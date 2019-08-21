import * as React from "react";
import * as AST from "../../adl-gen/runtime/sys/adlast";
import { renderCodeBlock, renderDeclDocMarkdown, renderFieldDocMarkdown, renderTypeExpr } from "../../util/adl-renderers";
import { AdlStore } from "../../util/adl-utils";
import * as OAPI from "../../util/openapi-utils";

const styles = require("./decl-scene.css");

export interface DeclSceneProps {
  adlStore: AdlStore;
  scopedName: AST.ScopedName;
  showDeclAdl: boolean;
}

export class DeclScene extends React.Component<DeclSceneProps> {
  render() {
    const decl = this.props.adlStore.resolve(this.props.scopedName);
    const details = (() => {
      switch (decl.decl.type_.kind) {
        case "struct_":
          return this.renderStruct(decl, decl.decl.type_.value);
        case "newtype_":
          return this.renderNewType(decl, decl.decl.type_.value);
        case "union_":
          return this.renderUnion(decl, decl.decl.type_.value);
        case "type_":
          return this.renderTypeAlias(decl, decl.decl.type_.value);
      }
    })();
    return (
      <div>
        <h1>{decl.decl.name}</h1>
        {details}
      </div>
    );
  }

  renderStruct(decl: AST.ScopedDecl, struct: AST.Struct): JSX.Element {
    const jsonSchema = OAPI.schemaFromStruct(decl, struct);
    return (
      <div>
        {this.props.showDeclAdl ? structAdlText(decl, struct) : undefined}
        {renderDeclDocMarkdown(decl)}
        <h2>Fields</h2>
        {renderStructFieldDocs(struct.fields)}
        <h2>OpenAPI schema</h2>
        {renderJsonSchema(jsonSchema)}
      </div>
    );
  }

  renderUnion(decl: AST.ScopedDecl, union: AST.Union): JSX.Element {
    const jsonSchema = {
      components: {
        schemas: {},
      },
    };
    const key = OAPI.schemaFromScopedName({
      moduleName: decl.moduleName,
      name: decl.decl.name,
    });
    jsonSchema.components.schemas[key] = OAPI.schemaFromUnion(decl, union);

    return (
      <div>
        {this.props.showDeclAdl ? unionAdlText(decl, union) : undefined}
        {renderDeclDocMarkdown(decl)}
        <h2>Variants</h2>
        {renderUnionFieldDocs(union.fields)}
        <h2>OpenAPI schema</h2>
        {renderJsonSchema(jsonSchema)}
      </div>
    );
  }

  renderTypeAlias(decl: AST.ScopedDecl, typeAlias: AST.TypeDef): JSX.Element {
    const jsonSchema = OAPI.schemaFromTypeAlias(decl, typeAlias);
    return (
      <div>
        {this.props.showDeclAdl ? typeAliasAdlText(decl, typeAlias) : undefined}
        {renderDeclDocMarkdown(decl)}
        <h2>OpenAPI schema</h2>
        {renderJsonSchema(jsonSchema)}
      </div>
    );
  }

  renderNewType(decl: AST.ScopedDecl, newtype: AST.NewType): JSX.Element {
    const jsonSchema = OAPI.schemaFromNewType(decl, newtype);
    return (
      <div>
        {this.props.showDeclAdl ? newtypeAdlText(decl, newtype) : undefined}
        {renderDeclDocMarkdown(decl)}
        <h2>OpenAPI schema</h2>
        {renderJsonSchema(jsonSchema)}
      </div>
    );
  }
}

function renderStructFieldDocs(fields: AST.Field[]): JSX.Element {
  const fielddocs = fields.map((f) => {
    const elements: JSX.Element[] = [];
    if (f.default.kind === "nothing") {
      elements.push(<div><b>Required Field</b>: {f.name}</div>);
    } else {
      elements.push(<div><b>Field</b>: {f.name}</div>);
    }
    elements.push(<div><b>Type</b>: {renderTypeExpr(f.typeExpr)}</div>);
    if (f.default.kind === "just") {
      elements.push(<div><b>Default Value</b>: {JSON.stringify(f.default.value)}</div>);
    }
    const doc = renderFieldDocMarkdown(f);
    if (doc.length > 0) {
      elements.push(<div><b>Description</b>: {doc}</div>);
    }

    return (
      <div className={styles.fieldblock}>
        {elements}
      </div>
    );
  });
  return (
    <div>
      {fielddocs}
    </div>
  );
}

function renderUnionFieldDocs(fields: AST.Field[]): JSX.Element {
  const fielddocs = fields.map((f) => {
    const elements: JSX.Element[] = [];
    elements.push(<div><b>Variant</b>: {f.name}</div>);
    if (!(f.typeExpr.typeRef.kind === "primitive" && f.typeExpr.typeRef.value === "Void")) {
      elements.push(<div><b>Associated Type</b>: {renderTypeExpr(f.typeExpr)}</div>);
    }
    const doc = renderFieldDocMarkdown(f);
    if (doc.length > 0) {
      elements.push(<div><b>Description</b>: {doc}</div>);
    }

    return (
      <div className={styles.fieldblock}>
        {elements}
      </div>
    );
  });
  return (
    <div>
      {fielddocs}
    </div>
  );
}

function renderJsonSchema(schema: OAPI.JsonSchema): JSX.Element {
  const text = OAPI.yamlFromJsonSchema(schema);
  return renderCodeBlock([text]);
}

function structAdlText(decl: AST.ScopedDecl, struct: AST.Struct): JSX.Element {
  const elements: (JSX.Element | string)[] = [];
  elements.push("struct " + decl.decl.name + " {\n");
  addAdlFieldsText(elements, struct.fields);
  elements.push("};\n");
  return renderCodeBlock(elements);
}

function unionAdlText(decl: AST.ScopedDecl, union: AST.Union): JSX.Element {
  const elements: (JSX.Element | string)[] = [];
  elements.push("union " + decl.decl.name + " {\n");
  addAdlFieldsText(elements, union.fields);
  elements.push("};\n");
  return renderCodeBlock(elements);
}

function addAdlFieldsText(elements: (JSX.Element | string)[], fields: AST.Field[]) {
  fields.forEach((f) => {
    elements.push("    ");
    elements.push(renderTypeExpr(f.typeExpr));
    if (f.default.kind == "nothing") {
      elements.push(" " + f.name + ";\n");
    } else {
      elements.push(" " + f.name + " = " + JSON.stringify(f.default.value) + ";\n");
    }
  });
}

function newtypeAdlText(decl: AST.ScopedDecl, newtype: AST.NewType): JSX.Element {
  const elements: (JSX.Element | string)[] = [];
  elements.push("newtype " + decl.decl.name + " = ");
  elements.push(renderTypeExpr(newtype.typeExpr));
  elements.push(";\n");
  return renderCodeBlock(elements);
}

function typeAliasAdlText(decl: AST.ScopedDecl, typealias: AST.TypeDef): JSX.Element {
  const elements: (JSX.Element | string)[] = [];
  elements.push("type " + decl.decl.name + " = ");
  elements.push(renderTypeExpr(typealias.typeExpr));
  elements.push(";\n");
  return renderCodeBlock(elements);
}
