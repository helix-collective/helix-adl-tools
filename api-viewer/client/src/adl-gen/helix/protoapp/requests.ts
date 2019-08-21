/* @generated from adl module helix.protoapp.requests */

import * as ADL from './../../runtime/adl';
import * as common from './../../common';
import * as common_db from './../../common/db';
import * as common_http from './../../common/http';
import * as common_strings from './../../common/strings';
import * as common_tabular from './../../common/tabular';
import * as helix_protoapp_db from './db';

/**
 * Returns a custom friendly greeting response
 */
export type Hello = common_http.Post<HelloReq, HelloResp>;

const Hello_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/protected/hello"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Post"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.requests","name":"HelloReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.requests","name":"HelloResp"}},"parameters":[]}]}}},"name":"Hello","version":{"kind":"nothing"}}};

export function texprHello(): ADL.ATypeExpr<Hello> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "Hello"}}, parameters : []}};
}

export interface HelloReq {
  name: string;
}

export function makeHelloReq(
  input: {
    name: string,
  }
): HelloReq {
  return {
    name: input.name,
  };
}

const HelloReq_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"HelloReq","version":{"kind":"nothing"}}};

export function texprHelloReq(): ADL.ATypeExpr<HelloReq> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "HelloReq"}}, parameters : []}};
}

export interface HelloResp {
  time: common.Instant;
  message: string;
}

export function makeHelloResp(
  input: {
    time: common.Instant,
    message: string,
  }
): HelloResp {
  return {
    time: input.time,
    message: input.message,
  };
}

const HelloResp_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"time","default":{"kind":"nothing"},"name":"time","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Instant"}},"parameters":[]}},{"annotations":[],"serializedName":"message","default":{"kind":"nothing"},"name":"message","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"HelloResp","version":{"kind":"nothing"}}};

export function texprHelloResp(): ADL.ATypeExpr<HelloResp> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "HelloResp"}}, parameters : []}};
}

/**
 * Returns the current time
 */
export type CurrentTime = common_http.Get<common.Instant>;

const CurrentTime_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/debug/time"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Get"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Instant"}},"parameters":[]}]}}},"name":"CurrentTime","version":{"kind":"nothing"}}};

export function texprCurrentTime(): ADL.ATypeExpr<CurrentTime> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "CurrentTime"}}, parameters : []}};
}

/**
 * Throws a dummy exception with a public message
 */
export type DummyException = common_http.Post<string, common.Unit>;

const DummyException_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/debug/dummy-exception"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Post"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}}},"name":"DummyException","version":{"kind":"nothing"}}};

export function texprDummyException(): ADL.ATypeExpr<DummyException> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "DummyException"}}, parameters : []}};
}

/**
 * Details for a user
 */
export interface UserReq {
  username: string;
  fullname: string;
  email: string;
  password: string;
}

export function makeUserReq(
  input: {
    username: string,
    fullname: string,
    email: string,
    password: string,
  }
): UserReq {
  return {
    username: input.username,
    fullname: input.fullname,
    email: input.email,
    password: input.password,
  };
}

const UserReq_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"username","default":{"kind":"nothing"},"name":"username","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"fullname","default":{"kind":"nothing"},"name":"fullname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"UserReq","version":{"kind":"nothing"}}};

export function texprUserReq(): ADL.ATypeExpr<UserReq> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "UserReq"}}, parameters : []}};
}

/**
 * Create a new user
 */
export type CreateUser = common_http.Post<UserReq, common_db.DbKey<helix_protoapp_db.AppUser>>;

const CreateUser_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/users/create"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Post"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.requests","name":"UserReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.db","name":"AppUser"}},"parameters":[]}]}]}}},"name":"CreateUser","version":{"kind":"nothing"}}};

export function texprCreateUser(): ADL.ATypeExpr<CreateUser> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "CreateUser"}}, parameters : []}};
}

/**
 * Update an existing user
 */
export type UpdateUser = common_http.Post<common_db.WithDbId<UserReq>, common.Unit>;

const UpdateUser_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/users/update"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Post"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"WithDbId"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.requests","name":"UserReq"}},"parameters":[]}]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}}},"name":"UpdateUser","version":{"kind":"nothing"}}};

export function texprUpdateUser(): ADL.ATypeExpr<UpdateUser> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "UpdateUser"}}, parameters : []}};
}

/**
 * Delete an existing user
 */
export type DeleteUser = common_http.Post<common_db.DbKey<helix_protoapp_db.AppUser>, common.Unit>;

const DeleteUser_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/users/delete"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Post"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.db","name":"AppUser"}},"parameters":[]}]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}}},"name":"DeleteUser","version":{"kind":"nothing"}}};

export function texprDeleteUser(): ADL.ATypeExpr<DeleteUser> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "DeleteUser"}}, parameters : []}};
}

/**
 * Query existing users sorted and filters according to the
 * TableQuery request.
 */
export type QueryUsers = common_http.Post<common_tabular.TableQuery, common.Paginated<common_db.WithDbId<helix_protoapp_db.AppUser>>>;

const QueryUsers_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/users/query"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Post"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"TableQuery"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Paginated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"WithDbId"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.db","name":"AppUser"}},"parameters":[]}]}]}]}}},"name":"QueryUsers","version":{"kind":"nothing"}}};

export function texprQueryUsers(): ADL.ATypeExpr<QueryUsers> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "QueryUsers"}}, parameters : []}};
}

/**
 * Logs in
 */
export interface LoginReq {
  username: common_strings.StringNE;
  password: common_strings.StringNE;
}

export function makeLoginReq(
  input: {
    username: common_strings.StringNE,
    password: common_strings.StringNE,
  }
): LoginReq {
  return {
    username: input.username,
    password: input.password,
  };
}

const LoginReq_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"username","default":{"kind":"nothing"},"name":"username","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}]}},"name":"LoginReq","version":{"kind":"nothing"}}};

export function texprLoginReq(): ADL.ATypeExpr<LoginReq> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "LoginReq"}}, parameters : []}};
}

export interface LoginResp_AccessToken {
  kind: 'accessToken';
  value: common_strings.StringNE;
}
export interface LoginResp_InvalidCredentials {
  kind: 'invalidCredentials';
}

export type LoginResp = LoginResp_AccessToken | LoginResp_InvalidCredentials;

const LoginResp_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"accessToken","default":{"kind":"nothing"},"name":"accessToken","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"invalidCredentials","default":{"kind":"nothing"},"name":"invalidCredentials","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"LoginResp","version":{"kind":"nothing"}}};

export function texprLoginResp(): ADL.ATypeExpr<LoginResp> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "LoginResp"}}, parameters : []}};
}

export type Login = common_http.Post<LoginReq, LoginResp>;

const Login_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/login"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Post"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.requests","name":"LoginReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.requests","name":"LoginResp"}},"parameters":[]}]}}},"name":"Login","version":{"kind":"nothing"}}};

export function texprLogin(): ADL.ATypeExpr<Login> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "Login"}}, parameters : []}};
}

/**
 * Gets the logged in user details
 */
export type WhoAmI = common_http.Get<common_db.WithDbId<helix_protoapp_db.AppUser>>;

const WhoAmI_AST : ADL.ScopedDecl =
  {"moduleName":"helix.protoapp.requests","decl":{"annotations":[{"v1":{"moduleName":"common.http","name":"Path"},"v2":"/protected/whoami"}],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"Get"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"WithDbId"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"helix.protoapp.db","name":"AppUser"}},"parameters":[]}]}]}}},"name":"WhoAmI","version":{"kind":"nothing"}}};

export function texprWhoAmI(): ADL.ATypeExpr<WhoAmI> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "helix.protoapp.requests",name : "WhoAmI"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "helix.protoapp.requests.Hello" : Hello_AST,
  "helix.protoapp.requests.HelloReq" : HelloReq_AST,
  "helix.protoapp.requests.HelloResp" : HelloResp_AST,
  "helix.protoapp.requests.CurrentTime" : CurrentTime_AST,
  "helix.protoapp.requests.DummyException" : DummyException_AST,
  "helix.protoapp.requests.UserReq" : UserReq_AST,
  "helix.protoapp.requests.CreateUser" : CreateUser_AST,
  "helix.protoapp.requests.UpdateUser" : UpdateUser_AST,
  "helix.protoapp.requests.DeleteUser" : DeleteUser_AST,
  "helix.protoapp.requests.QueryUsers" : QueryUsers_AST,
  "helix.protoapp.requests.LoginReq" : LoginReq_AST,
  "helix.protoapp.requests.LoginResp" : LoginResp_AST,
  "helix.protoapp.requests.Login" : Login_AST,
  "helix.protoapp.requests.WhoAmI" : WhoAmI_AST
};
