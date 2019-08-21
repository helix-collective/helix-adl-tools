/* @generated from adl */
import { declResolver, ScopedDecl } from "./runtime/adl";
import { _AST_MAP as common } from "./common";
import { _AST_MAP as common_config_aws } from "./common/config/aws";
import { _AST_MAP as common_config_db } from "./common/config/db";
import { _AST_MAP as common_config_emailer } from "./common/config/emailer";
import { _AST_MAP as common_config_google } from "./common/config/google";
import { _AST_MAP as common_config_jwt } from "./common/config/jwt";
import { _AST_MAP as common_config_log } from "./common/config/log";
import { _AST_MAP as common_config_server } from "./common/config/server";
import { _AST_MAP as common_db } from "./common/db";
import { _AST_MAP as common_http } from "./common/http";
import { _AST_MAP as common_strings } from "./common/strings";
import { _AST_MAP as common_tabular } from "./common/tabular";
import { _AST_MAP as common_ui } from "./common/ui";
import { _AST_MAP as helix_protoapp_db } from "./helix/protoapp/db";
import { _AST_MAP as helix_protoapp_requests } from "./helix/protoapp/requests";
import { _AST_MAP as helix_protoapp_uiconfig } from "./helix/protoapp/uiconfig";
import { _AST_MAP as sys_adlast } from "./sys/adlast";
import { _AST_MAP as sys_annotations } from "./sys/annotations";
import { _AST_MAP as sys_dynamic } from "./sys/dynamic";
import { _AST_MAP as sys_types } from "./sys/types";

export const ADL: { [key: string]: ScopedDecl } = {
  ...common,
  ...common_config_aws,
  ...common_config_db,
  ...common_config_emailer,
  ...common_config_google,
  ...common_config_jwt,
  ...common_config_log,
  ...common_config_server,
  ...common_db,
  ...common_http,
  ...common_strings,
  ...common_tabular,
  ...common_ui,
  ...helix_protoapp_db,
  ...helix_protoapp_requests,
  ...helix_protoapp_uiconfig,
  ...sys_adlast,
  ...sys_annotations,
  ...sys_dynamic,
  ...sys_types,
};

export const RESOLVER = declResolver(ADL);
