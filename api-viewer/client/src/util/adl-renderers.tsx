import * as React from "react";
import * as ReactMarkdown from "react-markdown";
import * as AST from "../adl-gen/runtime/sys/adlast";
import { pathFromRoute, Route } from "../routing/routes";
import { DOC, getDeclStringAnnotation, getStringAnnotation } from "./adl-utils";

const styles = require("./adl-renderers.css");

export function renderTypeExpr(typeExpr: AST.TypeExpr): JSX.Element {
  const typeElement = (() => {
    switch (typeExpr.typeRef.kind) {
      case "primitive":
        return (<span>{typeExpr.typeRef.value}</span>);
      case "reference":
        return (renderScopedName(typeExpr.typeRef.value));
      case "typeParam":
        return (<span>{typeExpr.typeRef.value}</span>);
    }
  })();
  const paramElements: JSX.Element[] = [];

  typeExpr.parameters.forEach((te) => {
    if (paramElements.length > 0) {
      paramElements.push(<span>,</span>);
    }
    paramElements.push(renderTypeExpr(te));
  });
  if (paramElements.length == 0) {
    return typeElement;
  } else {
    return <span>{typeElement}&lt;{paramElements}&gt;</span>;
  }
}

export function renderDeclDocMarkdown(scopedDecl: AST.ScopedDecl): JSX.Element[] {
  const elements: JSX.Element[] = [];
  const doc = getDeclStringAnnotation(scopedDecl, DOC);
  if (doc != undefined) {
    elements.push(<p>{renderMarkdown(doc)}</p>);
  }
  return elements;
}

export function renderFieldDocMarkdown(field: AST.Field): JSX.Element[] {
  const elements: JSX.Element[] = [];
  const doc = getStringAnnotation(field.annotations, DOC);
  if (doc != undefined) {
    elements.push(<p>{renderMarkdown(doc)}</p>);
  }
  return elements;
}

export function renderScopedName(scopedName: AST.ScopedName): JSX.Element {
  const route: Route = { root: "decl", scopedName };
  return <a href={pathFromRoute(route)}>{scopedName.name}</a>;
}

export function renderCodeBlock(elements: (JSX.Element | string)[]): JSX.Element {
  return <pre className={styles.codeblock}>{elements}</pre>;
}

export function renderMarkdown(markdown: string): JSX.Element {
  return <ReactMarkdown escapeHtml={false} source={markdown} />;
}

export function markdownFirstParagraph(markdown: string): string {
  // The first paragraph is delimited by a double newline
  return markdown.split("\n\n")[0];
}
