/* @generated from adl module helix.protoapp.db */

import * as ADL from './../../runtime/adl';
import * as common_strings from './../../common/strings';

/**
 * Details for a user
 */
export interface AppUser {
  username: common_strings.StringNE;
  fullname: common_strings.StringNE;
  email: common_strings.StringNE;
  isAdmin: boolean;
  hashedPassword: common_strings.StringNE;
}

export function makeAppUser(
  input: {
    username: common_strings.StringNE,
    fullname: common_strings.StringNE,
    email: common_strings.StringNE,
    isAdmin: boolean,
    hashedPassword?: common_strings.StringNE,
  }
): AppUser {
  return {
    username: input.username,
    fullname: input.fullname,
    email: input.email,
    isAdmin: input.isAdmin,
    hashedPassword: input.hashedPassword === undefined ? "" : input.hashedPassword,
  };
}

const AppUser_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.db","decl":{"annotations":[{"v1":{"moduleName":"common.db","name":"DbTable"},"v2":{"withIdPrimaryKey":true,"indexes":[["username"]]}}],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"username","default":{"kind":"nothing"},"name":"username","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"fullname","default":{"kind":"nothing"},"name":"fullname","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"isAdmin","default":{"kind":"nothing"},"name":"isAdmin","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"hashedPassword","default":{"kind":"just","value":""},"name":"hashedPassword","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}]}},"name":"AppUser","version":{"kind":"nothing"}}};

export function texprAppUser(): ADL.ATypeExpr<AppUser> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.db",name : "AppUser"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "helix.protoapp.db.AppUser" : AppUser_AST
};
