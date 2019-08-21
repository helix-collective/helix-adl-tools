/* @generated from adl module common.strings */

import * as ADL from './../runtime/adl';

/**
 * A string that isn't empty, and isn't only whitespace.
 */
export type StringNE = string;

const StringNE_AST : ADL.ScopedDecl =
  {"moduleName":"common.strings","decl":{"annotations":[{"v1":{"moduleName":"common.ui","name":"ValidRegex"},"v2":{"regex":"^.*\\S+.*$","description":"non empty"}}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"StringNE","version":{"kind":"nothing"}}};

export function texprStringNE(): ADL.ATypeExpr<StringNE> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.strings",name : "StringNE"}}, parameters : []}};
}

/**
 * An alphanumeric string, with hyphens for separation.
 */
export type StringANH = string;

const StringANH_AST : ADL.ScopedDecl =
  {"moduleName":"common.strings","decl":{"annotations":[{"v1":{"moduleName":"common.ui","name":"ValidRegex"},"v2":{"regex":"^[A-Za-z][A-Za-z0-9-]*$","description":"alphanumeric"}}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"StringANH","version":{"kind":"nothing"}}};

export function texprStringANH(): ADL.ATypeExpr<StringANH> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.strings",name : "StringANH"}}, parameters : []}};
}

/**
 * A multi line, free-form text string
 */
export type StringML = string;

const StringML_AST : ADL.ScopedDecl =
  {"moduleName":"common.strings","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"StringML","version":{"kind":"nothing"}}};

export function texprStringML(): ADL.ATypeExpr<StringML> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.strings",name : "StringML"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.strings.StringNE" : StringNE_AST,
  "common.strings.StringANH" : StringANH_AST,
  "common.strings.StringML" : StringML_AST
};
