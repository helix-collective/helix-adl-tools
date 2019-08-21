/* @generated from adl module helix.protoapp.uiconfig */

import * as ADL from './../../runtime/adl';
import * as common_config_log from './../../common/config/log';

/**
 * Configuration for the web frontend.
 */
export interface UiConfig {
  /**
   * Application title
   */
  title: string;
  /**
   * Environment name, e.g. dev, uat, prod
   */
  environment: string;
  /**
   * Name of the release / code version
   */
  releaseName: string;
  /**
   * Client side Rollbar config
   */
  rollbar: (common_config_log.ClientRollbarConfig|null);
}

export function makeUiConfig(
  input: {
    title?: string,
    environment: string,
    releaseName: string,
    rollbar?: (common_config_log.ClientRollbarConfig|null),
  }
): UiConfig {
  return {
    title: input.title === undefined ? "Welcome to the ProtoApp" : input.title,
    environment: input.environment,
    releaseName: input.releaseName,
    rollbar: input.rollbar === undefined ? null : input.rollbar,
  };
}

const UiConfig_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.uiconfig","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"title","default":{"kind":"just","value":"Welcome to the ProtoApp"},"name":"title","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"environment","default":{"kind":"nothing"},"name":"environment","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"releaseName","default":{"kind":"nothing"},"name":"releaseName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"rollbar","default":{"kind":"just","value":null},"name":"rollbar","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"ClientRollbarConfig"}},"parameters":[]}]}}]}},"name":"UiConfig","version":{"kind":"nothing"}}};

export function texprUiConfig(): ADL.ATypeExpr<UiConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.uiconfig",name : "UiConfig"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "helix.protoapp.uiconfig.UiConfig" : UiConfig_AST
};
