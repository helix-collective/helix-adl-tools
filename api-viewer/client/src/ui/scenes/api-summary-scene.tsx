import * as React from "react";
import { pathFromRoute } from "../../routing/routes";
import { markdownFirstParagraph, renderMarkdown } from "../../util/adl-renderers";
import { AdlStore, DOC, getDeclStringAnnotation, RequestDecl } from "../../util/adl-utils";

const styles = require("./api-summary-scene.css");

export interface ApiSummarySceneProps {
  adlStore: AdlStore;
}

export class ApiSummaryScene extends React.Component<ApiSummarySceneProps> {
  render() {
    const sectionElements = this.props.adlStore.requestsByGrouping().map((section) => {
      const requests: JSX.Element[] = [];
      section.requests.forEach((rdecl) => {
        const decl = rdecl.decl;
        const name = decl.moduleName + "." + decl.decl.name;
        const scopedName = {
          moduleName: decl.moduleName,
          name: decl.decl.name,
        };
        const doc = getDeclStringAnnotation(decl, DOC);
        const header = doc && markdownFirstParagraph(doc);

        const markdown = header && renderMarkdown(header) || <br></br>;
        requests.push(
          <div className={styles.requestblock} key={name}>
            <div>
              <div><a className={styles.requestlink} href={pathFromRoute({ root: "request", scopedName })}><b>{rdecl.method} {rdecl.path}</b></a></div>
              <div className={styles.requestattributes}>{this.renderAttributes(rdecl)}</div>
            </div>
            <div className={styles.requestblockdescription}>
              {markdown}
            </div>
          </div>,
        );
      });

      return (
        <div>
          <h2>{section.title}</h2>
          {requests}
        </div>
      );
    });


    return (
      <div>
        <h1>API Summary</h1>
        {sectionElements}
      </div>
    );
  }

  renderAttributes(rdecl: RequestDecl): JSX.Element {
    const attributes: (JSX.Element | string)[] = [];
    rdecl.decl.decl.annotations.forEach((ad) => {
      if (this.props.adlStore.includeAnnotationInApiSummary(ad.v1)) {
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
      }
    });

    return <span>{attributes}</span>;
  }
}


