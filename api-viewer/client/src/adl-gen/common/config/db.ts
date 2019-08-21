/* @generated from adl module common.config.db */

import * as ADL from './../../runtime/adl';

export interface PostgreSqlConfig {
  host: string;
  port: number;
  dbname: string;
  user: string;
  password: string;
  poolSize: number;
  postgisEnabled: boolean;
}

export function makePostgreSqlConfig(
  input: {
    host: string,
    port: number,
    dbname: string,
    user: string,
    password: string,
    poolSize?: number,
    postgisEnabled?: boolean,
  }
): PostgreSqlConfig {
  return {
    host: input.host,
    port: input.port,
    dbname: input.dbname,
    user: input.user,
    password: input.password,
    poolSize: input.poolSize === undefined ? 5 : input.poolSize,
    postgisEnabled: input.postgisEnabled === undefined ? false : input.postgisEnabled,
  };
}

const PostgreSqlConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"host","default":{"kind":"nothing"},"name":"host","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"dbname","default":{"kind":"nothing"},"name":"dbname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"user","default":{"kind":"nothing"},"name":"user","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"poolSize","default":{"kind":"just","value":5},"name":"poolSize","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"postgisEnabled","default":{"kind":"just","value":false},"name":"postgisEnabled","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"PostgreSqlConfig","version":{"kind":"nothing"}}};

export function texprPostgreSqlConfig(): ADL.ATypeExpr<PostgreSqlConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.db",name : "PostgreSqlConfig"}}, parameters : []}};
}

export interface SqlServerConfig {
  host: string;
  port: number;
  dbname: string;
  user: string;
  password: string;
  poolSize: number;
}

export function makeSqlServerConfig(
  input: {
    host: string,
    port: number,
    dbname: string,
    user: string,
    password: string,
    poolSize?: number,
  }
): SqlServerConfig {
  return {
    host: input.host,
    port: input.port,
    dbname: input.dbname,
    user: input.user,
    password: input.password,
    poolSize: input.poolSize === undefined ? 5 : input.poolSize,
  };
}

const SqlServerConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"host","default":{"kind":"nothing"},"name":"host","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"dbname","default":{"kind":"nothing"},"name":"dbname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"user","default":{"kind":"nothing"},"name":"user","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"poolSize","default":{"kind":"just","value":5},"name":"poolSize","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"SqlServerConfig","version":{"kind":"nothing"}}};

export function texprSqlServerConfig(): ADL.ATypeExpr<SqlServerConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.db",name : "SqlServerConfig"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.db.PostgreSqlConfig" : PostgreSqlConfig_AST,
  "common.config.db.SqlServerConfig" : SqlServerConfig_AST
};
