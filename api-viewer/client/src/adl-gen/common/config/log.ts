/* @generated from adl module common.config.log */

import * as ADL from './../../runtime/adl';

export enum LogLevel {
  DEBUG,
  INFO,
  WARN,
  ERROR,
  OFF,
}

const LogLevel_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"DEBUG","default":{"kind":"nothing"},"name":"DEBUG","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"INFO","default":{"kind":"nothing"},"name":"INFO","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"WARN","default":{"kind":"nothing"},"name":"WARN","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"ERROR","default":{"kind":"nothing"},"name":"ERROR","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"OFF","default":{"kind":"nothing"},"name":"OFF","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"LogLevel","version":{"kind":"nothing"}}};

export function texprLogLevel(): ADL.ATypeExpr<LogLevel> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.log",name : "LogLevel"}}, parameters : []}};
}

export interface StdoutConfig {
  level: LogLevel;
}

export function makeStdoutConfig(
  input: {
    level: LogLevel,
  }
): StdoutConfig {
  return {
    level: input.level,
  };
}

const StdoutConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"level","default":{"kind":"nothing"},"name":"level","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogLevel"}},"parameters":[]}}]}},"name":"StdoutConfig","version":{"kind":"nothing"}}};

export function texprStdoutConfig(): ADL.ATypeExpr<StdoutConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.log",name : "StdoutConfig"}}, parameters : []}};
}

export interface FluentdConfig {
  level: LogLevel;
  hostname: string;
  port: number;
  tag: string;
}

export function makeFluentdConfig(
  input: {
    level: LogLevel,
    hostname: string,
    port: number,
    tag: string,
  }
): FluentdConfig {
  return {
    level: input.level,
    hostname: input.hostname,
    port: input.port,
    tag: input.tag,
  };
}

const FluentdConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"level","default":{"kind":"nothing"},"name":"level","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogLevel"}},"parameters":[]}},{"annotations":[],"serializedName":"hostname","default":{"kind":"nothing"},"name":"hostname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"tag","default":{"kind":"nothing"},"name":"tag","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"FluentdConfig","version":{"kind":"nothing"}}};

export function texprFluentdConfig(): ADL.ATypeExpr<FluentdConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.log",name : "FluentdConfig"}}, parameters : []}};
}

/**
 * Server side Rollbar config
 * NOTE: Name is `ServerRollbarConfig` due to being named prior to adding
 * `ClientRollbarConfig`
 */
export interface RollbarConfig {
  level: LogLevel;
  serverToken: string;
}

export function makeRollbarConfig(
  input: {
    level: LogLevel,
    serverToken: string,
  }
): RollbarConfig {
  return {
    level: input.level,
    serverToken: input.serverToken,
  };
}

const RollbarConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"level","default":{"kind":"nothing"},"name":"level","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogLevel"}},"parameters":[]}},{"annotations":[],"serializedName":"serverToken","default":{"kind":"nothing"},"name":"serverToken","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"RollbarConfig","version":{"kind":"nothing"}}};

export function texprRollbarConfig(): ADL.ATypeExpr<RollbarConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.log",name : "RollbarConfig"}}, parameters : []}};
}

/**
 * Server side logging, i.e. reporting errors originating from the server
 * NOTE: Name is not `ServerLogConfig` due to being named prior to adding
 * `ClientLogConfig`
 */
export interface LogConfig {
  stdout: (StdoutConfig|null);
  fluentd: (FluentdConfig|null);
  rollbar: (RollbarConfig|null);
}

export function makeLogConfig(
  input: {
    stdout?: (StdoutConfig|null),
    fluentd?: (FluentdConfig|null),
    rollbar?: (RollbarConfig|null),
  }
): LogConfig {
  return {
    stdout: input.stdout === undefined ? {level : 1} : input.stdout,
    fluentd: input.fluentd === undefined ? null : input.fluentd,
    rollbar: input.rollbar === undefined ? null : input.rollbar,
  };
}

const LogConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"stdout","default":{"kind":"just","value":{"level":"INFO"}},"name":"stdout","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"StdoutConfig"}},"parameters":[]}]}},{"annotations":[],"serializedName":"fluentd","default":{"kind":"just","value":null},"name":"fluentd","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"FluentdConfig"}},"parameters":[]}]}},{"annotations":[],"serializedName":"rollbar","default":{"kind":"just","value":null},"name":"rollbar","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"RollbarConfig"}},"parameters":[]}]}}]}},"name":"LogConfig","version":{"kind":"nothing"}}};

export function texprLogConfig(): ADL.ATypeExpr<LogConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.log",name : "LogConfig"}}, parameters : []}};
}

/**
 * Client side Rollbar config, i.e. from Rollbar JS
 */
export interface ClientRollbarConfig {
  accessToken: string;
}

export function makeClientRollbarConfig(
  input: {
    accessToken: string,
  }
): ClientRollbarConfig {
  return {
    accessToken: input.accessToken,
  };
}

const ClientRollbarConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"accessToken","default":{"kind":"nothing"},"name":"accessToken","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"ClientRollbarConfig","version":{"kind":"nothing"}}};

export function texprClientRollbarConfig(): ADL.ATypeExpr<ClientRollbarConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.log",name : "ClientRollbarConfig"}}, parameters : []}};
}

/**
 * Client side logging, i.e. reporting errors originating from the client
 */
export interface ClientLogConfig {
  rollbar: (ClientRollbarConfig|null);
}

export function makeClientLogConfig(
  input: {
    rollbar?: (ClientRollbarConfig|null),
  }
): ClientLogConfig {
  return {
    rollbar: input.rollbar === undefined ? null : input.rollbar,
  };
}

const ClientLogConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"rollbar","default":{"kind":"just","value":null},"name":"rollbar","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"ClientRollbarConfig"}},"parameters":[]}]}}]}},"name":"ClientLogConfig","version":{"kind":"nothing"}}};

export function texprClientLogConfig(): ADL.ATypeExpr<ClientLogConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.log",name : "ClientLogConfig"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.log.LogLevel" : LogLevel_AST,
  "common.config.log.StdoutConfig" : StdoutConfig_AST,
  "common.config.log.FluentdConfig" : FluentdConfig_AST,
  "common.config.log.RollbarConfig" : RollbarConfig_AST,
  "common.config.log.LogConfig" : LogConfig_AST,
  "common.config.log.ClientRollbarConfig" : ClientRollbarConfig_AST,
  "common.config.log.ClientLogConfig" : ClientLogConfig_AST
};
