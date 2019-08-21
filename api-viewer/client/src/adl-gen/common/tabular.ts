/* @generated from adl module common.tabular */

import * as ADL from './../runtime/adl';
import * as common_strings from './strings';

export type FieldName = common_strings.StringNE;

const FieldName_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}},"name":"FieldName","version":{"kind":"nothing"}}};

export function texprFieldName(): ADL.ATypeExpr<FieldName> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldName"}}, parameters : []}};
}

export interface FieldEquals {
  field: FieldName;
  value: string;
}

export function makeFieldEquals(
  input: {
    field: FieldName,
    value: string,
  }
): FieldEquals {
  return {
    field: input.field,
    value: input.value,
  };
}

const FieldEquals_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"FieldEquals","version":{"kind":"nothing"}}};

export function texprFieldEquals(): ADL.ATypeExpr<FieldEquals> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldEquals"}}, parameters : []}};
}

export interface FieldLike {
  field: FieldNameOrFunction;
  pattern: string;
  caseSensitive: boolean;
}

export function makeFieldLike(
  input: {
    field: FieldNameOrFunction,
    pattern: string,
    caseSensitive?: boolean,
  }
): FieldLike {
  return {
    field: input.field,
    pattern: input.pattern,
    caseSensitive: input.caseSensitive === undefined ? true : input.caseSensitive,
  };
}

const FieldLike_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldNameOrFunction"}},"parameters":[]}},{"annotations":[],"serializedName":"pattern","default":{"kind":"nothing"},"name":"pattern","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"caseSensitive","default":{"kind":"just","value":true},"name":"caseSensitive","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"FieldLike","version":{"kind":"nothing"}}};

export function texprFieldLike(): ADL.ATypeExpr<FieldLike> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldLike"}}, parameters : []}};
}

export interface FieldIn {
  field: FieldName;
  values: string[];
}

export function makeFieldIn(
  input: {
    field: FieldName,
    values: string[],
  }
): FieldIn {
  return {
    field: input.field,
    values: input.values,
  };
}

const FieldIn_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"values","default":{"kind":"nothing"},"name":"values","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"FieldIn","version":{"kind":"nothing"}}};

export function texprFieldIn(): ADL.ATypeExpr<FieldIn> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldIn"}}, parameters : []}};
}

export interface FieldIsNull {
  field: FieldName;
}

export function makeFieldIsNull(
  input: {
    field: FieldName,
  }
): FieldIsNull {
  return {
    field: input.field,
  };
}

const FieldIsNull_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}}]}},"name":"FieldIsNull","version":{"kind":"nothing"}}};

export function texprFieldIsNull(): ADL.ATypeExpr<FieldIsNull> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldIsNull"}}, parameters : []}};
}

export interface FieldComparison {
  field: FieldName;
  value: ValueOrFunction;
}

export function makeFieldComparison(
  input: {
    field: FieldName,
    value: ValueOrFunction,
  }
): FieldComparison {
  return {
    field: input.field,
    value: input.value,
  };
}

const FieldComparison_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"ValueOrFunction"}},"parameters":[]}}]}},"name":"FieldComparison","version":{"kind":"nothing"}}};

export function texprFieldComparison(): ADL.ATypeExpr<FieldComparison> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldComparison"}}, parameters : []}};
}

export interface FieldPredicate_Equals {
  kind: 'equals';
  value: FieldEquals;
}
export interface FieldPredicate_Like {
  kind: 'like';
  value: FieldLike;
}
export interface FieldPredicate_In {
  kind: 'in';
  value: FieldIn;
}
export interface FieldPredicate_Isnull {
  kind: 'isnull';
  value: FieldIsNull;
}
export interface FieldPredicate_And {
  kind: 'and';
  value: FieldPredicate[];
}
export interface FieldPredicate_Or {
  kind: 'or';
  value: FieldPredicate[];
}
export interface FieldPredicate_Not {
  kind: 'not';
  value: FieldPredicate;
}
export interface FieldPredicate_GreaterThan {
  kind: 'greaterThan';
  value: FieldComparison;
}
export interface FieldPredicate_LessThan {
  kind: 'lessThan';
  value: FieldComparison;
}
export interface FieldPredicate_EqualTo {
  kind: 'equalTo';
  value: FieldComparison;
}
export interface FieldPredicate_Literal {
  kind: 'literal';
  value: boolean;
}

export type FieldPredicate = FieldPredicate_Equals | FieldPredicate_Like | FieldPredicate_In | FieldPredicate_Isnull | FieldPredicate_And | FieldPredicate_Or | FieldPredicate_Not | FieldPredicate_GreaterThan | FieldPredicate_LessThan | FieldPredicate_EqualTo | FieldPredicate_Literal;

const FieldPredicate_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"equals","default":{"kind":"nothing"},"name":"equals","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldEquals"}},"parameters":[]}},{"annotations":[],"serializedName":"like","default":{"kind":"nothing"},"name":"like","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldLike"}},"parameters":[]}},{"annotations":[],"serializedName":"in","default":{"kind":"nothing"},"name":"in","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldIn"}},"parameters":[]}},{"annotations":[],"serializedName":"isnull","default":{"kind":"nothing"},"name":"isnull","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldIsNull"}},"parameters":[]}},{"annotations":[],"serializedName":"and","default":{"kind":"nothing"},"name":"and","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}]}},{"annotations":[],"serializedName":"or","default":{"kind":"nothing"},"name":"or","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}]}},{"annotations":[],"serializedName":"not","default":{"kind":"nothing"},"name":"not","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}},{"annotations":[],"serializedName":"greaterThan","default":{"kind":"nothing"},"name":"greaterThan","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldComparison"}},"parameters":[]}},{"annotations":[],"serializedName":"lessThan","default":{"kind":"nothing"},"name":"lessThan","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldComparison"}},"parameters":[]}},{"annotations":[],"serializedName":"equalTo","default":{"kind":"nothing"},"name":"equalTo","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldComparison"}},"parameters":[]}},{"annotations":[],"serializedName":"literal","default":{"kind":"nothing"},"name":"literal","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"FieldPredicate","version":{"kind":"nothing"}}};

export function texprFieldPredicate(): ADL.ATypeExpr<FieldPredicate> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldPredicate"}}, parameters : []}};
}

export interface ValueOrFunction_Value {
  kind: 'value';
  value: string;
}
export interface ValueOrFunction_IntValue {
  kind: 'intValue';
  value: number;
}
export interface ValueOrFunction_CurrentDate {
  kind: 'currentDate';
}

export type ValueOrFunction = ValueOrFunction_Value | ValueOrFunction_IntValue | ValueOrFunction_CurrentDate;

const ValueOrFunction_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"intValue","default":{"kind":"nothing"},"name":"intValue","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"currentDate","default":{"kind":"nothing"},"name":"currentDate","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"ValueOrFunction","version":{"kind":"nothing"}}};

export function texprValueOrFunction(): ADL.ATypeExpr<ValueOrFunction> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "ValueOrFunction"}}, parameters : []}};
}

export interface ConcatArgument_Name {
  kind: 'name';
  value: FieldName;
}
export interface ConcatArgument_Literal {
  kind: 'literal';
  value: string;
}

export type ConcatArgument = ConcatArgument_Name | ConcatArgument_Literal;

const ConcatArgument_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"literal","default":{"kind":"nothing"},"name":"literal","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"ConcatArgument","version":{"kind":"nothing"}}};

export function texprConcatArgument(): ADL.ATypeExpr<ConcatArgument> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "ConcatArgument"}}, parameters : []}};
}

export interface FieldNameOrFunction_Name {
  kind: 'name';
  value: FieldName;
}
export interface FieldNameOrFunction_Concat {
  kind: 'concat';
  value: ConcatArgument[];
}

export type FieldNameOrFunction = FieldNameOrFunction_Name | FieldNameOrFunction_Concat;

const FieldNameOrFunction_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"concat","default":{"kind":"nothing"},"name":"concat","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"ConcatArgument"}},"parameters":[]}]}}]}},"name":"FieldNameOrFunction","version":{"kind":"nothing"}}};

export function texprFieldNameOrFunction(): ADL.ATypeExpr<FieldNameOrFunction> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "FieldNameOrFunction"}}, parameters : []}};
}

export enum SortDirection {
  ascending,
  descending,
}

const SortDirection_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"ascending","default":{"kind":"nothing"},"name":"ascending","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"descending","default":{"kind":"nothing"},"name":"descending","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"SortDirection","version":{"kind":"nothing"}}};

export function texprSortDirection(): ADL.ATypeExpr<SortDirection> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "SortDirection"}}, parameters : []}};
}

export interface SortField {
  field: FieldName;
  direction: SortDirection;
}

export function makeSortField(
  input: {
    field: FieldName,
    direction: SortDirection,
  }
): SortField {
  return {
    field: input.field,
    direction: input.direction,
  };
}

const SortField_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"direction","default":{"kind":"nothing"},"name":"direction","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"SortDirection"}},"parameters":[]}}]}},"name":"SortField","version":{"kind":"nothing"}}};

export function texprSortField(): ADL.ATypeExpr<SortField> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "SortField"}}, parameters : []}};
}

export interface TableView {
  columns: FieldName[];
  filter: FieldPredicate;
  sorting: SortField[];
}

export function makeTableView(
  input: {
    columns: FieldName[],
    filter?: FieldPredicate,
    sorting?: SortField[],
  }
): TableView {
  return {
    columns: input.columns,
    filter: input.filter === undefined ? {kind : "literal", value : true} : input.filter,
    sorting: input.sorting === undefined ? [] : input.sorting,
  };
}

const TableView_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"columns","default":{"kind":"nothing"},"name":"columns","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}]}},{"annotations":[],"serializedName":"filter","default":{"kind":"just","value":{"literal":true}},"name":"filter","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}},{"annotations":[],"serializedName":"sorting","default":{"kind":"just","value":[]},"name":"sorting","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"SortField"}},"parameters":[]}]}}]}},"name":"TableView","version":{"kind":"nothing"}}};

export function texprTableView(): ADL.ATypeExpr<TableView> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "TableView"}}, parameters : []}};
}

export interface TableQuery {
  filter: FieldPredicate;
  sorting: SortField[];
  offset: number;
  count: number;
}

export function makeTableQuery(
  input: {
    filter?: FieldPredicate,
    sorting?: SortField[],
    offset?: number,
    count?: number,
  }
): TableQuery {
  return {
    filter: input.filter === undefined ? {kind : "literal", value : true} : input.filter,
    sorting: input.sorting === undefined ? [] : input.sorting,
    offset: input.offset === undefined ? 0 : input.offset,
    count: input.count === undefined ? -1 : input.count,
  };
}

const TableQuery_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"filter","default":{"kind":"just","value":{"literal":true}},"name":"filter","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}},{"annotations":[],"serializedName":"sorting","default":{"kind":"just","value":[]},"name":"sorting","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"SortField"}},"parameters":[]}]}},{"annotations":[],"serializedName":"offset","default":{"kind":"just","value":0},"name":"offset","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"count","default":{"kind":"just","value":-1},"name":"count","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"TableQuery","version":{"kind":"nothing"}}};

export function texprTableQuery(): ADL.ATypeExpr<TableQuery> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "TableQuery"}}, parameters : []}};
}

export interface SingleField<T> {
  value: T;
}

export function makeSingleField<T>(
  input: {
    value: T,
  }
): SingleField<T> {
  return {
    value: input.value,
  };
}

const SingleField_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"SingleField","version":{"kind":"nothing"}}};

export function texprSingleField<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<SingleField<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.tabular",name : "SingleField"}}, parameters : [texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.tabular.FieldName" : FieldName_AST,
  "common.tabular.FieldEquals" : FieldEquals_AST,
  "common.tabular.FieldLike" : FieldLike_AST,
  "common.tabular.FieldIn" : FieldIn_AST,
  "common.tabular.FieldIsNull" : FieldIsNull_AST,
  "common.tabular.FieldComparison" : FieldComparison_AST,
  "common.tabular.FieldPredicate" : FieldPredicate_AST,
  "common.tabular.ValueOrFunction" : ValueOrFunction_AST,
  "common.tabular.ConcatArgument" : ConcatArgument_AST,
  "common.tabular.FieldNameOrFunction" : FieldNameOrFunction_AST,
  "common.tabular.SortDirection" : SortDirection_AST,
  "common.tabular.SortField" : SortField_AST,
  "common.tabular.TableView" : TableView_AST,
  "common.tabular.TableQuery" : TableQuery_AST,
  "common.tabular.SingleField" : SingleField_AST
};
