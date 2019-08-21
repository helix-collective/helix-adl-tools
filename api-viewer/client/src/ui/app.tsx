import { observer } from "mobx-react";
import * as React from "react";
import { Router } from "../routing/router";
import { getRoute, pathFromRoute, Route } from "../routing/routes";

import { MARKDOWN_DOCS } from "../markdown-docs";
import { DeclScene } from "../ui/scenes/decl-scene";
import { RequestScene } from "../ui/scenes/request-scene";
import { AdlStore } from "../util/adl-utils";
import { ApiSummaryScene as ApiSummaryScene } from "./scenes/api-summary-scene";
import { DocPageScene } from "./scenes/doc-page-scene";
import { OpenApiScene } from "./scenes/openapi-scene";


require("../ui/common/fonts.css");
const styles = require("./app.css");

/** Props for the component */
interface AppProps {
  origin: string;
  router: Router;
  adlStore: AdlStore;
  showDeclAdl: boolean;
  docs: { [page: string]: string };
}

/**
 * Renders the appropriate app page depending on the navigation state.
 */
@observer
export class App extends React.Component<AppProps> {

  constructor(props: AppProps) {
    super(props);
  }

  /** Renders the app */
  render() {
    let route = getRoute(this.props.router);

    if (route === undefined) {
      const api: Route = { root: "api_summary" };
      this.props.router.go(pathFromRoute(api));
      route = api;
    }

    const content = (() => {
      switch (route.root) {
        case "api_summary":
          return (
            <ApiSummaryScene
              adlStore={this.props.adlStore}
            />
          );
        case "openapi":
          return (
            <OpenApiScene
              adlStore={this.props.adlStore}
            />
          );
        case "docs":
          return (
            <DocPageScene
              markdownText={this.props.docs[route.page]}
            />
          );
        case "decl":
          return (
            <DeclScene
              adlStore={this.props.adlStore}
              showDeclAdl={this.props.showDeclAdl}
              scopedName={route.scopedName}
            />
          );
        case "request":
          return (
            <RequestScene
              adlStore={this.props.adlStore}
              scopedName={route.scopedName}
            />
          );
      }
    })();

    return (
      <div>
        <div className={styles.contentcontainer}>
          {this.renderMenu()}
          {content}
        </div>
      </div>
    );
  }

  renderMenu(): JSX.Element {
    const links: JSX.Element[] = [];
    links.push(<li><a href={pathFromRoute({ root: "api_summary" })}>API summary</a></li>);
    for(const page of Object.keys(MARKDOWN_DOCS)) {
      const title = page.replace("_", " ").replace(/\b\w/g, l => l.toUpperCase());
      links.push(<li><a href={pathFromRoute({ root: "docs", page })}>{title}</a></li>);
    }
    links.push(<li><a href={pathFromRoute({ root: "openapi" })}>OpenAPI 3.0 yaml</a></li>);
    return (
      <div className={styles.menuwrapper}>
        <div className={styles.menucontent}>
          <ul>{links}</ul>
        </div>
        <div className={styles.menuparent}><i className="fas fa-chevron-down"></i> Documentation</div>
      </div>
    );
  }

}
