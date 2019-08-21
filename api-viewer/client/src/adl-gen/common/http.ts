/* @generated from adl module common.http */

import * as ADL from './../runtime/adl';

/**
 * A marker type to associate the output response
 * type for an http Get request.
 */
export type Get<_I> = null;

const Get_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["I"],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Get","version":{"kind":"nothing"}}};

export function texprGet<I>(texprI : ADL.ATypeExpr<I>): ADL.ATypeExpr<Get<I>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "Get"}}, parameters : [texprI.value]}};
}

/**
 * A marker type to associate the input request and output response
 * types for an http put request.
 */
export type Put<_I, _O> = null;

const Put_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["I","O"],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Put","version":{"kind":"nothing"}}};

export function texprPut<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<Put<I, O>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "Put"}}, parameters : [texprI.value, texprO.value]}};
}

/**
 * A marker type to associate the input request and output response
 * types for an http post request.
 */
export type Post<_I, _O> = null;

const Post_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["I","O"],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Post","version":{"kind":"nothing"}}};

export function texprPost<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<Post<I, O>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "Post"}}, parameters : [texprI.value, texprO.value]}};
}

/**
 * An annotation indicating the URL path for a request
 */
export type Path = string;

const Path_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Path","version":{"kind":"nothing"}}};

export function texprPath(): ADL.ATypeExpr<Path> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "Path"}}, parameters : []}};
}

/**
 * An annotation indicating that a request requires an Authorization http header
 */
export type AuthHeader = null;

const AuthHeader_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"AuthHeader","version":{"kind":"nothing"}}};

export function texprAuthHeader(): ADL.ATypeExpr<AuthHeader> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "AuthHeader"}}, parameters : []}};
}

/**
 * An annotation indicating that a request is idemponent, and can hence be safely
 * retried on failure.
 */
export type Idempotent = null;

const Idempotent_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Idempotent","version":{"kind":"nothing"}}};

export function texprIdempotent(): ADL.ATypeExpr<Idempotent> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "Idempotent"}}, parameters : []}};
}

/**
 * An annotation indicating that a request can be made idempotent if the caller
 * provides an Idempotency-Key header with a unique value.
 */
export type IdempotentWithKey = null;

const IdempotentWithKey_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"IdempotentWithKey","version":{"kind":"nothing"}}};

export function texprIdempotentWithKey(): ADL.ATypeExpr<IdempotentWithKey> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "IdempotentWithKey"}}, parameters : []}};
}

/**
 * The standard message body for errors
 */
export interface PublicErrorData {
  publicMessage: string;
}

export function makePublicErrorData(
  input: {
    publicMessage: string,
  }
): PublicErrorData {
  return {
    publicMessage: input.publicMessage,
  };
}

const PublicErrorData_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"publicMessage","default":{"kind":"nothing"},"name":"publicMessage","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"PublicErrorData","version":{"kind":"nothing"}}};

export function texprPublicErrorData(): ADL.ATypeExpr<PublicErrorData> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.http",name : "PublicErrorData"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.http.Get" : Get_AST,
  "common.http.Put" : Put_AST,
  "common.http.Post" : Post_AST,
  "common.http.Path" : Path_AST,
  "common.http.AuthHeader" : AuthHeader_AST,
  "common.http.Idempotent" : Idempotent_AST,
  "common.http.IdempotentWithKey" : IdempotentWithKey_AST,
  "common.http.PublicErrorData" : PublicErrorData_AST
};
