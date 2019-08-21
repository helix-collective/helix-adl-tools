import * as AST from "../adl-gen/runtime/sys/adlast";
import { AdlStore, DOC, getDeclStringAnnotation, getResponses, hasAnnotation, JWT_AUTH_HEADER, RequestDecl } from "./adl-utils";

const YAML = require("js-yaml");

export interface JsonSchema { }

export function yamlFromJsonSchema(schema: JsonSchema): string {
  return YAML.dump(schema);
}

export function schemaFromRequests(requestDecls: RequestDecl[], adlStore: AdlStore): JsonSchema {
  const paths: { [key: string]: JsonSchema; } = {};
  const securitySchemes: { [key: string]: JsonSchema } = {};

  const declSchemas = new DeclSchemaCollector(adlStore);


  requestDecls.forEach((rdecl) => {
    paths[rdecl.path] = schemaFromRequest(rdecl, adlStore);
    switch (rdecl.method) {
      case "post":
        declSchemas.addTypeExpr(rdecl.request);
        declSchemas.addTypeExpr(rdecl.response);
        break;
      case "get":
        declSchemas.addTypeExpr(rdecl.response);
        break;
      case "put":
        declSchemas.addTypeExpr(rdecl.request);
        declSchemas.addTypeExpr(rdecl.response);
    }

    getResponses(adlStore, rdecl, rdecl.response).responses.forEach((r) => {
      declSchemas.addTypeExpr(r.typeExpr);
    });

    if (hasAnnotation(rdecl.decl, JWT_AUTH_HEADER)) {
      securitySchemes.JwtBearerAuth = {
        type: "http",
        scheme: "bearer",
      };
    }
  });
  const components: { [key: string]: JsonSchema } = {};
  components.schemas = declSchemas.schemas;
  if (Object.keys(securitySchemes).length > 0) {
    components.securitySchemes = securitySchemes;
  }

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

export function schemaFromRequest(requestDecl: RequestDecl, adlStore: AdlStore): JsonSchema {
  const schema: { [key: string]: JsonSchema; } = {};
  const properties: { [key: string]: JsonSchema; } = {};
  let doc = getDeclStringAnnotation(requestDecl.decl, DOC);
  properties.operationId = requestDecl.decl.decl.name;
  if (doc != undefined) {
    const attributes = requestDecl.decl.decl.annotations.filter((ann) => adlStore.includeAnnotationInOpenApiDescription(ann.v1));
    if (attributes.length > 1) {
      doc = doc.trim() + "\n\nAttributes: " + attributes.map((ann) => ann.v1.name).join(", ");
    }
    properties.description = doc;
  }
  if (requestDecl.method == "post" || requestDecl.method == "put") {
    properties.requestBody = {
      content: {
        "application/json": {
          schema: schemaFromTypeExpr(requestDecl.request),
        },
      },
    };
  }

  if (hasAnnotation(requestDecl.decl, JWT_AUTH_HEADER)) {
    properties.security = [{ JwtBearerAuth: [] }];
  }

  properties.responses = {};

  const responses = getResponses(adlStore, requestDecl, requestDecl.response);
  responses.responses.forEach((resp) => {
    properties.responses[resp.status] = {
      description: resp.description,
      content: {
        "application/json": {
          schema: schemaFromTypeExpr(resp.typeExpr),
        },
      },
    };
  });

  schema[requestDecl.method] = properties;
  return schema;
}

/**
 * Builds up a map of json schema objects from ADL declarations
 * and their recursive references.
 */
class DeclSchemaCollector {
  schemas: { [key: string]: JsonSchema; } = {};

  constructor(private readonly adlStore: AdlStore) {
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
        this.addDecl(this.adlStore.resolve(typeExpr.typeRef.value));
    }
  }
}

export function schemaFromStruct(decl: AST.ScopedDecl, struct: AST.Struct): JsonSchema {

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

export function schemaFromUnion(decl: AST.ScopedDecl, union: AST.Union): JsonSchema {
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

export function schemaFromNewType(decl: AST.ScopedDecl, newtype: AST.NewType): JsonSchema {
  return schemaFromTypeExpr(newtype.typeExpr);
}

export function schemaFromTypeAlias(decl: AST.ScopedDecl, typealias: AST.TypeDef): JsonSchema {
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
      if (nullable) {
        return {
          // see https://github.com/OAI/OpenAPI-Specification/issues/1368
          nullable: true,
          allOf: [
            { $ref: componentFromScopedName(typeExpr.typeRef.value) },
          ],
        };
      } else {
        return { $ref: componentFromScopedName(typeExpr.typeRef.value) };
      }
    case "typeParam":
      return {
        type: "FIXME: typeParam",
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
