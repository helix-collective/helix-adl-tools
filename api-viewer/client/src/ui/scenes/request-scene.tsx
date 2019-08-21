import * as React from "react";
import * as AST from "../../adl-gen/runtime/sys/adlast";
import { pathFromRoute } from "../../routing/routes";
import { renderCodeBlock, renderDeclDocMarkdown, renderTypeExpr } from "../../util/adl-renderers";
import { AdlStore, GetRequestDecl, getResponses, matchRequestDecl, PostRequestDecl, PutRequestDecl, RequestDecl } from "../../util/adl-utils";
import * as OAPI from "../../util/openapi-utils";

const styles = require("./request-scene.css");

export interface RequestSceneProps {
  adlStore: AdlStore;
  scopedName: AST.ScopedName;
}

export class RequestScene extends React.Component<RequestSceneProps> {
  render() {
    const decl = this.props.adlStore.resolve(this.props.scopedName);
    const rdecl = matchRequestDecl(decl);
    if (rdecl == undefined) {
      return "???";
    }
    switch (rdecl.method) {
      case "post": return this.renderPostRequest(rdecl);
      case "get": return this.renderGetRequest(rdecl);
      case "put": return this.renderPutRequest(rdecl);
    }
  }

  renderPostRequest(rdecl: PostRequestDecl): JSX.Element {
    const decl = rdecl.decl;
    const name = decl.moduleName + "." + decl.decl.name;
    const schema = OAPI.schemaFromRequest(rdecl, this.props.adlStore);
    return (
      <div key={name}>
        <h1>POST {rdecl.path}</h1>
        <p>{renderDeclDocMarkdown(decl)}</p>
        <div><b>Attributes:</b> {this.renderAttributes(rdecl)}</div>
        <div><b>Request Body</b>: {renderTypeExpr(rdecl.request)}</div>
        <div><b>Responses</b>: {this.renderResponses(rdecl, rdecl.response)}</div>
        <h2>OpenAPI schema</h2>
        {renderOpenApi(schema)}
      </div>
    );
  }

  renderGetRequest(rdecl: GetRequestDecl): JSX.Element {
    const decl = rdecl.decl;
    const name = decl.moduleName + "." + decl.decl.name;
    return (
      <div key={name}>
        <h1>GET {rdecl.path}</h1>
        <div>{renderDeclDocMarkdown(decl)}</div>
        <div><b>Responses</b>: {this.renderResponses(rdecl, rdecl.response)}</div>
      </div>
    );
  }

  renderPutRequest(rdecl: PutRequestDecl): JSX.Element {
    const decl = rdecl.decl;
    const name = decl.moduleName + "." + decl.decl.name;
    const schema = OAPI.schemaFromRequest(rdecl, this.props.adlStore);
    return (
      <div key={name}>
        <h1>PUT {rdecl.path}</h1>
        <p>{renderDeclDocMarkdown(decl)}</p>
        <div><b>Request Body</b>: {renderTypeExpr(rdecl.request)}</div>
        <div><b>Responses</b>: {this.renderResponses(rdecl, rdecl.response)}</div>
        <h2>OpenAPI schema</h2>
        {renderOpenApi(schema)}
      </div>
    );
  }

  renderAttributes(rdecl: RequestDecl): JSX.Element {
    const attributes: (JSX.Element | string)[] = [];
    rdecl.decl.decl.annotations.forEach((ad) => {
      const docpage = this.props.adlStore.annotationDocumentationLink(ad.v1);
      if (docpage) {
        if (attributes.length > 0) {
          attributes.push(", ");
        }
        attributes.push(
          <a href={pathFromRoute({ root: "docs", page: docpage })}>
            {ad.v1.name}
          </a>);
      }
    });

    return <span>{attributes}</span>;
  }

  renderResponses(rdecl: RequestDecl, response: AST.TypeExpr): JSX.Element {
    const rows: JSX.Element[] = [];
    const responses = getResponses(this.props.adlStore, rdecl, response);
    responses.responses.forEach((resp) => {
      rows.push(
        <tr>
          <td>{resp.status}</td>
          <td>{renderTypeExpr(resp.typeExpr)}</td>
          <td>{resp.description}</td>
        </tr>,
      );
    });
    return (
      <table className={styles.responsetable}>
        <tr>
          <th>Status</th>
          <th>Content</th>
          <th>Description</th>
        </tr>
        {rows}
      </table >
    );
  }
}

function renderOpenApi(schema: OAPI.JsonSchema): JSX.Element {
  const text = OAPI.yamlFromJsonSchema(schema);
  return renderCodeBlock([text]);
}
