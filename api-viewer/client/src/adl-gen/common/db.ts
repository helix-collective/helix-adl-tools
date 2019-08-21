/* @generated from adl module common.db */

import * as ADL from './../runtime/adl';

export interface DbTable {
  tableName: string;
  withIdPrimaryKey: boolean;
  withPrimaryKey: string[];
  indexes: string[][];
  uniquenessConstraints: string[][];
}

export function makeDbTable(
  input: {
    tableName?: string,
    withIdPrimaryKey?: boolean,
    withPrimaryKey?: string[],
    indexes?: string[][],
    uniquenessConstraints?: string[][],
  }
): DbTable {
  return {
    tableName: input.tableName === undefined ? "" : input.tableName,
    withIdPrimaryKey: input.withIdPrimaryKey === undefined ? false : input.withIdPrimaryKey,
    withPrimaryKey: input.withPrimaryKey === undefined ? [] : input.withPrimaryKey,
    indexes: input.indexes === undefined ? [] : input.indexes,
    uniquenessConstraints: input.uniquenessConstraints === undefined ? [] : input.uniquenessConstraints,
  };
}

const DbTable_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"tableName","default":{"kind":"just","value":""},"name":"tableName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"withIdPrimaryKey","default":{"kind":"just","value":false},"name":"withIdPrimaryKey","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"withPrimaryKey","default":{"kind":"just","value":[]},"name":"withPrimaryKey","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"indexes","default":{"kind":"just","value":[]},"name":"indexes","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}]}},{"annotations":[],"serializedName":"uniquenessConstraints","default":{"kind":"just","value":[]},"name":"uniquenessConstraints","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}]}}]}},"name":"DbTable","version":{"kind":"nothing"}}};

export function texprDbTable(): ADL.ATypeExpr<DbTable> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.db",name : "DbTable"}}, parameters : []}};
}

/**
 * Field level annotation to override the name of the
 * database column.
 */
export type DbColumnName = string;

const DbColumnName_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"DbColumnName","version":{"kind":"nothing"}}};

export function texprDbColumnName(): ADL.ATypeExpr<DbColumnName> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.db",name : "DbColumnName"}}, parameters : []}};
}

/**
 * Field level annotation to override the type of the
 * database column.
 */
export type DbColumnType = string;

const DbColumnType_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"DbColumnType","version":{"kind":"nothing"}}};

export function texprDbColumnType(): ADL.ATypeExpr<DbColumnType> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.db",name : "DbColumnType"}}, parameters : []}};
}

/**
 * A reference for a database stored value, referenced by a
 * string primary key.
 */
export type DbKey<_T> = string;

const DbKey_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":["T"],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"DbKey","version":{"kind":"nothing"}}};

export function texprDbKey<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<DbKey<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.db",name : "DbKey"}}, parameters : [texprT.value]}};
}

/**
 * A value of type T along with a unique db identifier
 */
export interface WithDbId<T> {
  id: DbKey<T>;
  value: T;
}

export function makeWithDbId<T>(
  input: {
    id: DbKey<T>,
    value: T,
  }
): WithDbId<T> {
  return {
    id: input.id,
    value: input.value,
  };
}

const WithDbId_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"WithDbId","version":{"kind":"nothing"}}};

export function texprWithDbId<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<WithDbId<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.db",name : "WithDbId"}}, parameters : [texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.db.DbTable" : DbTable_AST,
  "common.db.DbColumnName" : DbColumnName_AST,
  "common.db.DbColumnType" : DbColumnType_AST,
  "common.db.DbKey" : DbKey_AST,
  "common.db.WithDbId" : WithDbId_AST
};
