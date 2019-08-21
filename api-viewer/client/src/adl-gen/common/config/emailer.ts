/* @generated from adl module common.config.emailer */

import * as ADL from './../../runtime/adl';
import * as common_config_aws from './aws';

export interface EmailerConfig_Fake {
  kind: 'fake';
}
export interface EmailerConfig_Ses {
  kind: 'ses';
  value: SesConfig;
}

/**
 * Configuration for the emailer used 
 */
export type EmailerConfig = EmailerConfig_Fake | EmailerConfig_Ses;

const EmailerConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.emailer","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"fake","default":{"kind":"nothing"},"name":"fake","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"ses","default":{"kind":"nothing"},"name":"ses","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.emailer","name":"SesConfig"}},"parameters":[]}}]}},"name":"EmailerConfig","version":{"kind":"nothing"}}};

export function texprEmailerConfig(): ADL.ATypeExpr<EmailerConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.emailer",name : "EmailerConfig"}}, parameters : []}};
}

/**
 * Configuration for AWS SES emailer implementation
 */
export interface SesConfig {
  credentials: common_config_aws.AwsCredentialsProvider;
  region: common_config_aws.AwsRegionProvider;
}

export function makeSesConfig(
  input: {
    credentials: common_config_aws.AwsCredentialsProvider,
    region: common_config_aws.AwsRegionProvider,
  }
): SesConfig {
  return {
    credentials: input.credentials,
    region: input.region,
  };
}

const SesConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.emailer","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"credentials","default":{"kind":"nothing"},"name":"credentials","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsCredentialsProvider"}},"parameters":[]}},{"annotations":[],"serializedName":"region","default":{"kind":"nothing"},"name":"region","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsRegionProvider"}},"parameters":[]}}]}},"name":"SesConfig","version":{"kind":"nothing"}}};

export function texprSesConfig(): ADL.ATypeExpr<SesConfig> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common.config.emailer",name : "SesConfig"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.emailer.EmailerConfig" : EmailerConfig_AST,
  "common.config.emailer.SesConfig" : SesConfig_AST
};
