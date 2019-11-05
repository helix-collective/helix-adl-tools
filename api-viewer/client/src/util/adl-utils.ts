import { DeclResolver, declResolver, ScopedDecl } from "../adl-gen/runtime/adl";
import * as AST from "../adl-gen/runtime/sys/adlast";
import * as ADL from "../adl-gen/runtime/adl";
import { TypeRef } from "../adl-gen/sys/adlast";


export interface RequestGrouping<T> {
  title: string;
  requests: T;
}

type RequestFilter = (rdecl: RequestDecl) => boolean;

interface AnnotationDetails {
  scopedName: AST.ScopedName;
  docpage: string;
  showInSummary?: boolean;
}

export class AdlStore {
  private readonly resolver: DeclResolver;
  private readonly annotations: { [key: string]: AnnotationDetails } = {};

  constructor(
    // All known ADL declarations (generated by ADL compiler)
    private readonly allAdlDecls: { [key: string]: ScopedDecl },

    // Filter function for each displayed grouping
    private readonly groupings: RequestGrouping<RequestFilter>[],

    // Documentation links for annotations
    annotations: AnnotationDetails[],
  ) {
    this.resolver = declResolver(allAdlDecls);
    annotations.forEach((ann) => {
      this.annotations[this.scopedNameKey(ann.scopedName)] = ann;
    });
  }

  resolve(scopedName: AST.ScopedName): AST.ScopedDecl {
    return this.resolver(scopedName);
  }

  allDecls(): AST.ScopedDecl[] {
    const decls: AST.ScopedDecl[] = [];
    for (const key of Object.keys(this.allAdlDecls)) {
      decls.push(this.allAdlDecls[key]);
    }
    return decls;
  }

  allRequests(): RequestDecl[] {
    const rdecls: RequestDecl[] = [];
    for (const key of Object.keys(this.allAdlDecls)) {
      const decl = this.allAdlDecls[key];
      const rdecl = matchRequestDecl(decl);
      if (rdecl != undefined) {
        rdecls.push(rdecl);
      }
    }
    return rdecls;
  }

  requestsByGrouping(): RequestGrouping<RequestDecl[]>[] {
    const allRequests = this.allRequests();
    return this.groupings.map((g) => {
      return {
        title: g.title,
        requests: allRequests.filter(g.requests),
      };
    });
  }

  includeAnnotationInOpenApiDescription(scopedName: AST.ScopedName): boolean {
    const ann = this.annotations[this.scopedNameKey(scopedName)];
    return ann != undefined;
  }

  includeAnnotationInApiSummary(scopedName: AST.ScopedName): boolean {
    const ann = this.annotations[this.scopedNameKey(scopedName)];
    return ann && ann.showInSummary || false;
  }

  annotationDocumentationLink(scopedName: AST.ScopedName): string | undefined {
    const ann = this.annotations[this.scopedNameKey(scopedName)];
    return ann && ann.docpage || undefined;
  }

  scopedNameKey(scopedName: AST.ScopedName): string {
    return scopedName.moduleName + "." + scopedName.name;
  }
}

export function resolveDecl(adlStore: AdlStore, typeRef: TypeRef): AST.ScopedDecl | undefined {
  if (typeRef.kind == "reference") {
    return adlStore.resolve(typeRef.value);
  }
  return undefined;
}

export function getDeclStringAnnotation(decl: AST.ScopedDecl, atype: AST.ScopedName): string | undefined {
  return getStringAnnotation(decl.decl.annotations, atype);
}

export function getStringAnnotation(annotations: AST.Annotations, atype: AST.ScopedName): string | undefined {
  for (const ann of annotations) {
    if (scopedNamesEqual(ann.v1, atype)) {
      return typeof ann.v2 === "string" ? ann.v2 : undefined;
    }
  }
  return undefined;
}


export function getNumberAnnotation(annotations: AST.Annotations, atype: AST.ScopedName): number | undefined {
  for (const ann of annotations) {
    if (scopedNamesEqual(ann.v1, atype)) {
      return typeof ann.v2 === "number" ? ann.v2 : undefined;
    }
  }
  return undefined;
}

export function hasAnnotation(decl: AST.ScopedDecl, atype: AST.ScopedName): boolean {
  for (const ann of decl.decl.annotations) {
    if (scopedNamesEqual(ann.v1, atype)) {
      return true;
    }
  }
  return false;
}

// Holder type that contains an unpacked request decl.
export type RequestDecl
  = PostRequestDecl
  | PutRequestDecl
  | GetRequestDecl;

export interface PostRequestDecl {
  method: "post";
  decl: AST.ScopedDecl;
  path: string;
  request: AST.TypeExpr;
  response: AST.TypeExpr;
}

export interface PutRequestDecl {
  method: "put";
  path: string;
  decl: AST.ScopedDecl;
  request: AST.TypeExpr;
  response: AST.TypeExpr;
}

export interface GetRequestDecl {
  method: "get";
  path: string;
  decl: AST.ScopedDecl;
  response: AST.TypeExpr;
}

// Match
export function matchRequestDecl(decl: AST.ScopedDecl): RequestDecl | undefined {
  const dtype = decl.decl.type_;
  if (dtype.kind !== "type_") {
    return undefined;
  }
  const path = getDeclStringAnnotation(decl, PATH);
  if (path == undefined) {
    return undefined;
  }
  if (matchTypeRef(dtype.value.typeExpr, POST)) {
    return {
      method: "post",
      path,
      decl,
      request: dtype.value.typeExpr.parameters[0],
      response: dtype.value.typeExpr.parameters[1],
    };
  } else if (matchTypeRef(dtype.value.typeExpr, GET)) {
    return {
      method: "get",
      path,
      decl,
      response: dtype.value.typeExpr.parameters[0],
    };
  } else if (matchTypeRef(dtype.value.typeExpr, PUT)) {
    return {
      method: "put",
      path,
      decl,
      request: dtype.value.typeExpr.parameters[0],
      response: dtype.value.typeExpr.parameters[1],
    };
  } else {
    return undefined;
  }
}

export function matchPathPrefix(rdecl: RequestDecl, prefixes: string[]): boolean {
  for (const prefix of prefixes) {
    if (rdecl.path.startsWith(prefix)) {
      return true;
    }
  }
  return false;
}

export function matchTypeRef(typeExpr: AST.TypeExpr, scopedName: AST.ScopedName): boolean {
  return typeExpr.typeRef.kind === "reference" && scopedNamesEqual(typeExpr.typeRef.value, scopedName);
}

export function scopedNamesEqual(sn1: AST.ScopedName, sn2: AST.ScopedName) {
  return sn1.moduleName === sn2.moduleName
    && sn1.name === sn2.name;
}

export interface Responses {
  responses: Response[];
}

export interface Response {
  status: string;
  description: string;
  typeExpr: AST.TypeExpr;
}

export function getResponses(adlStore: AdlStore, rdecl: RequestDecl, typeExpr: AST.TypeExpr): Responses {
  const responses: Response[] = [];
  const unions = getSplitResponseUnions(adlStore, typeExpr);
  if (unions != undefined) {
    unions.forEach((ur) => {
      ur.union.fields.forEach((uf) => {
        const description = getStringAnnotation(uf.annotations, DOC) || uf.name;
        responses.push({
          status: ur.httpStatus,
          typeExpr: uf.typeExpr,
          description,
        });
      });
    });
  } else {
    responses.push({
      status: "200",
      description: "success",
      typeExpr,
    });
  }
  responses.push({
    status: "400",
    description: "The request couldn't be parsed or validated",
    typeExpr: texprPublicErrorData().value,
  });
  if (hasAnnotation(rdecl.decl, JWT_AUTH_HEADER)) {
    responses.push({
      status: "401",
      description: "The request lacked a valid Authentication Header",
      typeExpr: texprUnit().value,
    });
  }

  responses.sort((r1, r2) => r1.status < r2.status ? -1 : r1.status == r2.status ? 0 : 1);
  return {
    responses,
  };
}

function getSplitResponseUnions(adlStore: AdlStore, typeExpr: AST.TypeExpr): { httpStatus: string, union: AST.Union }[] | undefined {
  const decl = resolveDecl(adlStore, typeExpr.typeRef);
  if (decl != undefined && decl.decl.type_.kind == "union_") {
    const unions = splitUnionOnStatusAnnotations(decl.decl.type_.value);
    if (unions.length > 1) {
      return unions;
    }
  }
  return undefined;
}

/**
 *
 * If a union has any HttpStatus annotations, split it into a separate
 * union for each different status code.
 */
export function splitUnionOnStatusAnnotations(union: AST.Union)
  : { httpStatus: string, union: AST.Union }[] {

  const fieldsByHttpStatus: { [key: string]: AST.Field[] } = {};
  const otherFields: AST.Field[] = [];
  function addField(status: string, field: AST.Field) {
    if (fieldsByHttpStatus[status] == undefined) {
      fieldsByHttpStatus[status] = [field];
    } else {
      fieldsByHttpStatus[status].push(field);
    }
  }

  union.fields.forEach((field) => {
    const httpStatus = getNumberAnnotation(field.annotations, HTTP_STATUS);
    if (httpStatus != undefined) {
      addField(httpStatus + "", field);
    } else {
      otherFields.push(field);
    }
  });

  if (Object.keys(fieldsByHttpStatus).length == 0) {
    return [];
  } else {
    otherFields.forEach((field) => addField("200", field));
    return Object.keys(fieldsByHttpStatus).map((httpStatus) => {
      return {
        httpStatus,
        union: {
          typeParams: union.typeParams,
          fields: fieldsByHttpStatus[httpStatus],
        },
      };
    });
  }
}

function texprUnit() : ADL.ATypeExpr<{}> {
  return {value: {typeRef:{kind:'reference', value:UNIT}, parameters:[]}};
}

function texprPublicErrorData() : ADL.ATypeExpr<{}> {
  return {value: {typeRef:{kind:'reference', value:PUBLIC_ERROR_DATA}, parameters:[]}};
}


export const UNIT: AST.ScopedName = { moduleName: "common", name: "Unit" };
export const DOC: AST.ScopedName = { moduleName: "sys.annotations", name: "Doc" };
export const POST: AST.ScopedName = { moduleName: "common.http", name: "Post" };
export const GET: AST.ScopedName = { moduleName: "common.http", name: "Get" };
export const PUT: AST.ScopedName = { moduleName: "common.http", name: "Put" };
export const PATH: AST.ScopedName = { moduleName: "common.http", name: "Path" };
export const PUBLIC_ERROR_DATA: AST.ScopedName = { moduleName: "common.http", name: "PublicErrorData" }
export const JWT_AUTH_HEADER: AST.ScopedName = { moduleName: "common.http", name: "JwtAuthHeader" };
export const HTTP_STATUS: AST.ScopedName = { moduleName: "common.http", name: "HttpStatus" };
export const SAFE: AST.ScopedName = { moduleName: "common.http", name: "Safe" };
export const IDEMPOTENT: AST.ScopedName = { moduleName: "common.http", name: "Idempotent" };
export const IDEMPOTENT_WITH_KEY: AST.ScopedName = { moduleName: "common.http", name: "IdempotentWithKey" };