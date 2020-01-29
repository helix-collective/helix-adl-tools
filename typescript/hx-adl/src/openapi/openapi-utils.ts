import * as AST from "../adl-gen/runtime/sys/adlast";
import {typeExprToString} from "../adl-gen/runtime/utils";
import {DeclResolver} from "../adl-gen/runtime/adl";
import {LoadedAdl, scopedNamesEqual, monomorphicDecl, getAnnotation, getStringAnnotation} from "../util";
import * as YAML from "js-yaml";
import {cloneDeep} from "lodash";

export interface JsonSchema { }
type JsonSchemaMap = { [key: string]: JsonSchema; };

export function yamlFromJsonSchema(schema: JsonSchema): string {
  return YAML.dump(schema);
}

export function schemaFromApi(apiscopedname: AST.ScopedName, loadedAdl: LoadedAdl): JsonSchema {
  // Find the api definition
  const api = loadedAdl.resolver(apiscopedname);

  const paths : JsonSchemaMap = {};
  const securitySchemes: JsonSchemaMap = {};
  const components : JsonSchemaMap = {};

  const declSchemas = new DeclSchemaCollector(loadedAdl);

  if (api.decl.type_.kind != 'struct_') {
    throw new Error("API declaration must be a struct");
  }

  const commonResponses = getAnnotation(api.decl.annotations, OPENAPI_OTHER_RESPONSES);

  // Each field in the API struct is a request
  for(const field of api.decl.type_.value.fields) {
    const otherResponses
       = getAnnotation(field.annotations, OPENAPI_OTHER_RESPONSES)
       || commonResponses
       || {};
    const apiRequest = decodeApiRequest(field, otherResponses);
    declSchemas.addTypeExpr(apiRequest.paramsType);
    switch(apiRequest.method) {
    case 'get':
      declSchemas.addTypeExpr(apiRequest.responseType);
      break;
    case 'post':
      declSchemas.addTypeExpr(apiRequest.bodyType);
      declSchemas.addTypeExpr(apiRequest.responseType);
      break;
    case 'delete':
      declSchemas.addTypeExpr(apiRequest.responseType);
      break;
    }
    paths[apiRequest.path] = schemaFromRequest(apiRequest, loadedAdl.resolver);

  }

  // Always include a security schema for JWTs
  securitySchemes.TokenAuth = securitySchemeFromAnnotation(getAnnotation(api.decl.annotations, SECURITY_SCHEME));

  components["securitySchemes"] = securitySchemes;
  components['schemas'] = declSchemas.schemas;

  return {
    openapi: "3.0.0",
    info: {
      version: "1.0.0",
      title: "API",
    },
    paths,
    components,
  };
}

type Roles = string[];

type Security
  = {kind: 'public'}
  | {kind: 'token'}
  ;

interface CommonRequest {
  name: string,
  description: string,
  path: string,
  security: Security,
  paramsType: AST.TypeExpr,
  responseType: AST.TypeExpr,
  otherResponses: JsonSchema,
};

interface GetRequest extends CommonRequest {
  method: 'get',
}

interface PostRequest extends CommonRequest {
  method: 'post',
  bodyType: AST.TypeExpr,
}

interface PutRequest extends CommonRequest {
  method: 'put',
  bodyType: AST.TypeExpr,
}

interface DeleteRequest extends CommonRequest {
  method: 'delete',
}

type ApiRequest
  = GetRequest
  | PutRequest
  | PostRequest
  | DeleteRequest
  ;


function decodeApiRequest(reqfield: AST.Field, otherResponses: JsonSchema): ApiRequest {
  const rtype = reqfield.typeExpr;
  if (rtype.typeRef.kind != 'reference') {
    throw new Error("API field types must be references");
  }
  const defv = reqfield.default;
  if (defv.kind != 'just' || defv.value == null) {
    throw new Error("Request details not provided as the field value");
  }
  const value = defv.value;
  const description : string = getStringAnnotation(reqfield.annotations, DOC) || "";

  if (scopedNamesEqual(rtype.typeRef.value, HTTP_GET)) {
    return {
      method: 'get',
      name: reqfield.name,
      description,
      path: pathFromFieldValue(value),
      security: securityFromFieldValue(value),
      paramsType: UNIT_TYPEEXPR,
      responseType: rtype.parameters[0],
      otherResponses: otherResponses || {},
    };
  } else if (scopedNamesEqual(rtype.typeRef.value, HTTP_GET2)) {
    return {
      method: 'get',
      name: reqfield.name,
      description,
      path: pathFromFieldValue(value),
      security: securityFromFieldValue(value),
      paramsType: rtype.parameters[0],
      responseType: rtype.parameters[1],
      otherResponses,
    };
  } else if (scopedNamesEqual(rtype.typeRef.value, HTTP_POST)) {
    return {
      method: 'post',
      name: reqfield.name,
      description,
      path: pathFromFieldValue(value),
      security: securityFromFieldValue(value),
      paramsType: UNIT_TYPEEXPR,
      bodyType: rtype.parameters[0],
      responseType: rtype.parameters[1],
      otherResponses,
    };
  } else if (scopedNamesEqual(rtype.typeRef.value, HTTP_POST2)) {
    return {
      method: 'post',
      name: reqfield.name,
      description,
      path: pathFromFieldValue(value),
      security: securityFromFieldValue(value),
      paramsType: rtype.parameters[0],
      bodyType: rtype.parameters[1],
      responseType: rtype.parameters[2],
      otherResponses,
    };
  } else if (scopedNamesEqual(rtype.typeRef.value, HTTP_PUT)) {
    return {
      method: 'put',
      name: reqfield.name,
      description,
      path: pathFromFieldValue(value),
      security: securityFromFieldValue(value),
      paramsType: UNIT_TYPEEXPR,
      bodyType: rtype.parameters[0],
      responseType: rtype.parameters[1],
      otherResponses,
    };
  } else if (scopedNamesEqual(rtype.typeRef.value, HTTP_PUT2)) {
    return {
      method: 'put',
      name: reqfield.name,
      description,
      path: pathFromFieldValue(value),
      security: securityFromFieldValue(value),
      paramsType: rtype.parameters[0],
      bodyType: rtype.parameters[1],
      responseType: rtype.parameters[2],
      otherResponses,
    };
  } else if (scopedNamesEqual(rtype.typeRef.value, HTTP_DELETE)) {
    return {
      method: 'delete',
      name: reqfield.name,
      description,
      path: pathFromFieldValue(value),
      security: securityFromFieldValue(value),
      paramsType: rtype.parameters[0],
      responseType: rtype.parameters[1],
      otherResponses,
    };
  }
  throw new Error("Unable to decode API field " + reqfield.name);
}

function pathFromFieldValue(value: {}): string {
  return value['path'];
}

function securityFromFieldValue(value: {}): Security {
  const v : {} = value['security'];
  if (typeof v == 'string' && v == 'public') {
    return {kind:'public'};
  }
  return {kind:'token'};
}

function securitySchemeFromAnnotation(annotation: {}|null|undefined): JsonSchema {
  const headerName = annotation && annotation['apiKey'] && annotation['apiKey']['headerName'];
  if (typeof(headerName) == 'string') {
    return {
      type: 'apiKey',
      'in': 'header',
      name: headerName
    }
  };
    
  // Else assume http bearer
  return {
    type: "http",
    scheme: "bearer",
  };
}

interface RequestParam {
  name: string,
  typeExpr: AST.TypeExpr,
  hasDefault: boolean,
  description: string
};

export function schemaFromRequest(apiRequest: ApiRequest, resolver: DeclResolver): JsonSchema {
  const schema: JsonSchemaMap = {};
  const properties: JsonSchemaMap = {};
  properties.operationId = apiRequest.name;
  if (apiRequest.description != undefined) {
    properties.description = apiRequest.description;
  }

  // Parameters
  properties.parameters = paramsFromType(apiRequest.paramsType, resolver).map( p => {
    const isPathParam = apiRequest.path.includes( '{' + p.name + '}' );
    const result = {
      'in':  isPathParam ? 'path' : 'query',
      name: p.name,
      required: isPathParam || !p.hasDefault,
      schema: schemaFromTypeExpr(p.typeExpr),
    };
    if (p.description) {
      result['description'] = p.description;
    }
    return result;
  });

  // Request body schema
  switch (apiRequest.method) {
  case 'post':
  case 'put':
    properties.requestBody = {
      content: {
        "application/json": {
          schema: schemaFromTypeExpr(apiRequest.bodyType),
        },
      },
    };
    break;
  }

  // Responses
  switch (apiRequest.method) {
  case 'put':
  case 'post':
  case 'get':
  case 'delete':
    properties.responses = cloneDeep(apiRequest.otherResponses);
    properties.responses[200] = {
      description: 'success',
      content: {
        "application/json": {
          schema: schemaFromTypeExpr(apiRequest.responseType),
        },
      },
    }
    break;
  }

  // security
  const publicEndpoint = apiRequest.security.kind == 'public';
  if (!publicEndpoint) {
    properties.security = [{ TokenAuth: [] }];
  }


  schema[apiRequest.method] = properties;
  return schema;
}

function paramsFromType(typeExpr: AST.TypeExpr, resolver: DeclResolver): RequestParam[] {
  if (typeExpr.typeRef.kind != 'reference') {
    throw new Error("request parameters must be a reference to a struct");
  }
  const decl = monomorphicDecl(typeExpr, typeExpr.typeRef.value, monomorphicName, resolver);
  if (decl.decl.type_.kind != 'struct_') {
    throw new Error("request parameters must be a reference to a struct");
  }
  return decl.decl.type_.value.fields.map( field => ({
    name: field.name,
    typeExpr: field.typeExpr,
    description: getStringAnnotation(field.annotations, DOC) || "",
    hasDefault: field.default.kind != 'nothing',
  }));
}


/**
 * Builds up a map of json schema objects from ADL declarations
 * and their recursive references.
 */
class DeclSchemaCollector {
  schemas: { [key: string]: JsonSchema; } = {};

  constructor(private readonly loadedAdl: LoadedAdl) {
  }

  addDecl(decl: AST.ScopedDecl) {
    const key = schemaFromScopedName({
      moduleName: decl.moduleName,
      name: decl.decl.name,
    });

    // Avoid infinite recursion by returning if this decl
    // is already present
    if (this.schemas[key] != undefined) {
      return;
    }

    const dtype = decl.decl.type_;
    switch (dtype.kind) {
      case "struct_":
        this.schemas[key] = schemaFromStruct(decl, dtype.value);
        dtype.value.fields.forEach((f) => {
          this.addTypeExpr(f.typeExpr);
        });
        break;
      case "union_":
        this.schemas[key] = schemaFromUnion(decl, dtype.value);
        dtype.value.fields.forEach((f) => {
          this.addTypeExpr(f.typeExpr);
        });
        break;
      case "newtype_":
        this.schemas[key] = schemaFromNewType(decl, dtype.value);
        this.addTypeExpr(dtype.value.typeExpr);
        break;
      case "type_":
        this.schemas[key] = schemaFromTypeAlias(decl, dtype.value);
        this.addTypeExpr(dtype.value.typeExpr);
    }
  }

  addTypeExpr(typeExpr: AST.TypeExpr) {
    typeExpr.parameters.forEach((p) => {
      this.addTypeExpr(p);
    });
    switch (typeExpr.typeRef.kind) {
      case "primitive":
      case "typeParam":
        break;
      case "reference":
        const decl = monomorphicDecl(typeExpr, typeExpr.typeRef.value, monomorphicName, this.loadedAdl.resolver);
        this.addDecl(decl);
    }
  }

}

export function schemaFromStruct(_decl: AST.ScopedDecl, struct: AST.Struct): JsonSchema {

  const properties: { [key: string]: JsonSchema; } = {};
  const required: string[] = [];
  struct.fields.forEach((f) => {
    properties[f.name] = schemaFromTypeExpr(f.typeExpr);
    if (f.default.kind == "nothing") {
      required.push(f.name);
    }
  });
  const result: { [key: string]: JsonSchema; } = {
    type: "object",
    properties,
  };
  if (required.length > 0) {
    result.required = required;
  }
  return result;
}

export function schemaFromUnion(_decl: AST.ScopedDecl, union: AST.Union): JsonSchema {
  const voidFields: string[] = [];
  const otherFields: AST.Field[] = [];
  union.fields.forEach((f) => {
    if (f.typeExpr.typeRef.kind == "primitive" && f.typeExpr.typeRef.value == "Void") {
      voidFields.push(f.name);
    } else {
      otherFields.push(f);
    }
  });

  if (otherFields.length == 0) {
    // We have an enum
    return {
      type: "string",
      enum: voidFields,
    };
  } else {
    // We have a union
    const alternatives: JsonSchema[] = [];
    if (voidFields.length) {
      alternatives.push({
        type: "string",
        enum: voidFields,
      });
    }
    otherFields.forEach((f) => {
      const properties: { [key: string]: JsonSchema; } = {};
      properties[f.name] = schemaFromTypeExpr(f.typeExpr);
      alternatives.push({
        type: "object",
        properties,
        required: [f.name],
      });
    });
    return {
      oneOf: alternatives,
    };
  }
}

export function schemaFromNewType(_decl: AST.ScopedDecl, newtype: AST.NewType): JsonSchema {
  return schemaFromTypeExpr(newtype.typeExpr);
}

export function schemaFromTypeAlias(_decl: AST.ScopedDecl, typealias: AST.TypeDef): JsonSchema {
  return schemaFromTypeExpr(typealias.typeExpr);
}

export function schemaFromTypeExpr(typeExpr: AST.TypeExpr): JsonSchema {
  if (typeExpr.typeRef.kind === "primitive" && typeExpr.typeRef.value == "Nullable") {
    return schemaFromTypeExpr1(typeExpr.parameters[0], true);
  } else {
    return schemaFromTypeExpr1(typeExpr, false);
  }
}

function schemaFromTypeExpr1(typeExpr: AST.TypeExpr, nullable: boolean): JsonSchema {
  switch (typeExpr.typeRef.kind) {
    case "primitive":
      const schema = (() => {
        if (typeExpr.typeRef.value == "Vector") {
          return {
            type: "array",
            items: schemaFromTypeExpr(typeExpr.parameters[0]),
          };
        } else if (typeExpr.typeRef.value == "StringMap") {
          return {
            type: "object",
            additionalProperties: schemaFromTypeExpr(typeExpr.parameters[0]),
          };
        } else {
          return schemaFromPrimitive(typeExpr.typeRef.value);
        }
      })();
      if (nullable) {
        /* tslint:disable:no-string-literal */
        schema["nullable"] = true;
        /* tslint:enable:no-string-literal */
      }
      return schema;
    case "reference":
      const scopedName = {
          moduleName: typeExpr.typeRef.value.moduleName,
          name: monomorphicName(typeExpr.typeRef.value.name, typeExpr.parameters),
      };
      if (nullable) {
        return {
          // see https://github.com/OAI/OpenAPI-Specification/issues/1368
          nullable: true,
          allOf: [
            { $ref: componentFromScopedName(scopedName) },
          ],
        };
      } else {
        return { $ref: componentFromScopedName(scopedName) };
      }
    case "typeParam":
          return {
            type: "Unimplemented: Type parameter: " + typeExprToString(typeExpr),
          };
  }
}

function schemaFromPrimitive(adlptype: string): JsonSchema {
  if (ADL_NUMERIC_TYPES.find((el) => el === adlptype)) {
    return { type: "number" };
  } else if (adlptype === "Bool") {
    return { type: "boolean" };
  } else if (adlptype === "Json") {
    return { type: "object" };
  } else {
    return { type: adlptype.toLowerCase() };
  }
}


const ADL_NUMERIC_TYPES: string[] = [
  "Int8",
  "Int16",
  "Int32",
  "Int64",
  "Word8",
  "Word16",
  "Word32",
  "Word64",
  "Float",
  "Double",
];

export function schemaFromScopedName(scopedName: AST.ScopedName): string {
  return (scopedName.moduleName + "." + scopedName.name).replace(/[.]/g, "_");
}

export function componentFromScopedName(scopedName: AST.ScopedName): JsonSchema {
  return "#/components/schemas/" + schemaFromScopedName(scopedName);
}

function monomorphicName(declName: string, typeParams: AST.TypeExpr[]): string {
  if (typeParams.length == 0) {
    return declName;
  }
  return declName + "_" + typeParams.map(te => typeExprToString(te)).join("_");
}

export const UNIT: AST.ScopedName = { moduleName: "common", name: "Unit" };
export const DOC: AST.ScopedName = { moduleName: "sys.annotations", name: "Doc" };
export const HTTP_GET: AST.ScopedName = { moduleName: "common.http", name: "HttpGet" };
export const HTTP_GET2: AST.ScopedName = { moduleName: "common.http", name: "HttpGet2" };
export const HTTP_POST: AST.ScopedName = { moduleName: "common.http", name: "HttpPost" };
export const HTTP_POST2: AST.ScopedName = { moduleName: "common.http", name: "HttpPost2" };
export const HTTP_PUT: AST.ScopedName = { moduleName: "common.http", name: "HttpPut" };
export const HTTP_PUT2: AST.ScopedName = { moduleName: "common.http", name: "HttpPut2" };
export const HTTP_DELETE: AST.ScopedName = { moduleName: "common.http", name: "HttpDelete" };
export const SECURITY_SCHEME: AST.ScopedName = { moduleName: "common.http", name: "SecurityScheme" };
export const OPENAPI_OTHER_RESPONSES: AST.ScopedName = { moduleName: "common.http", name: "OpenApiOtherResponses" };
export const UNIT_TYPEEXPR: AST.TypeExpr = {typeRef:{kind:'reference', value: UNIT}, parameters:[]};

