/* @generated from adl module helix.protoapp.requests-rtypes */

import * as ADL from './../../runtime/adl';
import * as JSON from './../../runtime/json';
import * as common from './../../common';
import * as common_db from './../../common/db';
import * as common_tabular from './../../common/tabular';
import * as helix_protoapp_db from './db';
import * as helix_protoapp_requests from './requests';

export interface PostRequest<I,O> {
  path: string,
  reqJB: JSON.JsonBinding<I>,
  respJB: JSON.JsonBinding<O>,
};

export interface GetRequest<O> {
  path: string,
  respJB: JSON.JsonBinding<O>,
};

export interface PutRequest<I,O> {
  path: string,
  reqJB: JSON.JsonBinding<I>,
  respJB: JSON.JsonBinding<O>,
};

export interface RequestTypes {
  createUser: PostRequest<helix_protoapp_requests.UserReq, common_db.DbKey<helix_protoapp_db.AppUser>>,
  currentTime: GetRequest<common.Instant>,
  deleteUser: PostRequest<common_db.DbKey<helix_protoapp_db.AppUser>, common.Unit>,
  dummyException: PostRequest<string, common.Unit>,
  hello: PostRequest<helix_protoapp_requests.HelloReq, helix_protoapp_requests.HelloResp>,
  login: PostRequest<helix_protoapp_requests.LoginReq, helix_protoapp_requests.LoginResp>,
  queryUsers: PostRequest<common_tabular.TableQuery, common.Paginated<common_db.WithDbId<helix_protoapp_db.AppUser>>>,
  updateUser: PostRequest<common_db.WithDbId<helix_protoapp_requests.UserReq>, common.Unit>,
  whoAmI: GetRequest<common_db.WithDbId<helix_protoapp_db.AppUser>>,
};

export function makeRequestTypes(resolver: ADL.DeclResolver): RequestTypes {
  /**
   * Create a new user
   */
  const createUser = {
    path: "/users/create",
    reqJB: JSON.createJsonBinding(resolver, helix_protoapp_requests.texprUserReq()),
    respJB: JSON.createJsonBinding(resolver, common_db.texprDbKey(helix_protoapp_db.texprAppUser())),
  }

  /**
   * Returns the current time
   */
  const currentTime = {
    path: "/debug/time",
    respJB: JSON.createJsonBinding(resolver, common.texprInstant()),
  }

  /**
   * Delete an existing user
   */
  const deleteUser = {
    path: "/users/delete",
    reqJB: JSON.createJsonBinding(resolver, common_db.texprDbKey(helix_protoapp_db.texprAppUser())),
    respJB: JSON.createJsonBinding(resolver, common.texprUnit()),
  }

  /**
   * Throws a dummy exception with a public message
   */
  const dummyException = {
    path: "/debug/dummy-exception",
    reqJB: JSON.createJsonBinding(resolver, ADL.texprString()),
    respJB: JSON.createJsonBinding(resolver, common.texprUnit()),
  }

  /**
   * Returns a custom friendly greeting response
   */
  const hello = {
    path: "/protected/hello",
    reqJB: JSON.createJsonBinding(resolver, helix_protoapp_requests.texprHelloReq()),
    respJB: JSON.createJsonBinding(resolver, helix_protoapp_requests.texprHelloResp()),
  }

  const login = {
    path: "/login",
    reqJB: JSON.createJsonBinding(resolver, helix_protoapp_requests.texprLoginReq()),
    respJB: JSON.createJsonBinding(resolver, helix_protoapp_requests.texprLoginResp()),
  }

  /**
   * Query existing users sorted and filters according to the
   * TableQuery request.
   */
  const queryUsers = {
    path: "/users/query",
    reqJB: JSON.createJsonBinding(resolver, common_tabular.texprTableQuery()),
    respJB: JSON.createJsonBinding(resolver, common.texprPaginated(common_db.texprWithDbId(helix_protoapp_db.texprAppUser()))),
  }

  /**
   * Update an existing user
   */
  const updateUser = {
    path: "/users/update",
    reqJB: JSON.createJsonBinding(resolver, common_db.texprWithDbId(helix_protoapp_requests.texprUserReq())),
    respJB: JSON.createJsonBinding(resolver, common.texprUnit()),
  }

  /**
   * Gets the logged in user details
   */
  const whoAmI = {
    path: "/protected/whoami",
    respJB: JSON.createJsonBinding(resolver, common_db.texprWithDbId(helix_protoapp_db.texprAppUser())),
  }
  return {
    createUser,
    currentTime,
    deleteUser,
    dummyException,
    hello,
    login,
    queryUsers,
    updateUser,
    whoAmI,
  }
};
