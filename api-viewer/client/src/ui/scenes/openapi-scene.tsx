import * as React from "react";
import { AdlStore, RequestDecl } from "../../util/adl-utils";

import { renderCodeBlock } from "../../util/adl-renderers";
import * as OAPI from "../../util/openapi-utils";

export interface OpenApiSceneProps {

  // The ADL to be shown as openapi
  adlStore: AdlStore;
}

export class OpenApiScene extends React.Component<OpenApiSceneProps> {
  render() {
    const requestDecls: RequestDecl[] = [];
    this.props.adlStore.requestsByGrouping().forEach((rg) => {
      rg.requests.forEach((r) => requestDecls.push(r));
    });
    const schema = OAPI.schemaFromRequests(requestDecls, this.props.adlStore);
    const text = OAPI.yamlFromJsonSchema(schema);
    return renderCodeBlock([text]);
  }
}
