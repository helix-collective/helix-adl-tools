import * as React from "react";
import { renderMarkdown } from "../../util/adl-renderers";

export interface DocPageSceneProps {
  markdownText: string;
}

export class DocPageScene extends React.Component<DocPageSceneProps> {
  render() {
    return renderMarkdown(this.props.markdownText);
  }
}
