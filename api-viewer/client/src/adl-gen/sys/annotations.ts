/* @generated from adl module sys.annotations */

import * as ADL from './../runtime/adl';

export type Doc = string;

const Doc_AST : ADL.ScopedDecl =
  {"moduleName":"sys.annotations","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Doc","version":{"kind":"nothing"}}};

export function texprDoc(): ADL.ATypeExpr<Doc> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.annotations",name : "Doc"}}, parameters : []}};
}

export type SerializedName = string;

const SerializedName_AST : ADL.ScopedDecl =
  {"moduleName":"sys.annotations","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"SerializedName","version":{"kind":"nothing"}}};

export function texprSerializedName(): ADL.ATypeExpr<SerializedName> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.annotations",name : "SerializedName"}}, parameters : []}};
}

export type CustomSerialization = boolean;

const CustomSerialization_AST : ADL.ScopedDecl =
  {"moduleName":"sys.annotations","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}},"name":"CustomSerialization","version":{"kind":"nothing"}}};

export function texprCustomSerialization(): ADL.ATypeExpr<CustomSerialization> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.annotations",name : "CustomSerialization"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.annotations.Doc" : Doc_AST,
  "sys.annotations.SerializedName" : SerializedName_AST,
  "sys.annotations.CustomSerialization" : CustomSerialization_AST
};
