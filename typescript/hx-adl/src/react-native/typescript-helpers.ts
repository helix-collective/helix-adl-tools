import { camelCase } from "change-case";
import * as adlast from "../adl-gen/sys/adlast";
import { ImportingHelper } from "../ts-services/import-helper";
import { LoadedAdl } from "../util";
import {
  getDepFilename,
  getScopedName,
  IndentableWriter,
} from "./react-native-utils";
import * as path from "path";

const supportedTsType = (type: string): string => {
  switch (type) {
    case "Bool":
      return "boolean";
    case "String":
      return "string";
    case "Double":
      return "number";
    case "Vector":
    case "StringMap":
    case "Nullable":
    case "ByteVector":
      throw new Error(`TODO: support ${type}`);
    case "Void":
      return "void";
    case "Int8":
    case "Int16":
    case "Int32":
    case "Int64":
    case "Word8":
    case "Word16":
    case "Word32":
    case "Word64":
    case "Float":
    default:
      throw new Error(
        `${type} is not supported by React Native, use Double instead`
      );
  }
};

export const writeTsBridges = (
  field: adlast.Field,
  writer: IndentableWriter
) => {
  writer.writeIndent();
  writer.writeRaw(`${field.name} = (`);

  // write params
  let reqParamName = "";
  const reqTypeExpr = field.typeExpr.parameters[0];
  if (
    !(
      reqTypeExpr.typeRef.kind === "primitive" &&
      reqTypeExpr.typeRef.value === "Void"
    )
  ) {
    if (reqTypeExpr.typeRef.kind === "primitive") {
      reqParamName = "req";
      writer.writeRaw(
        `${reqParamName}: ${supportedTsType(reqTypeExpr.typeRef.value)}`
      );
    } else if (reqTypeExpr.typeRef.kind === "reference") {
      reqParamName = camelCase(reqTypeExpr.typeRef.value.name);
      writer.writeRaw(`${reqParamName}: string`);
    }
  }

  writer.writeRaw(`) => {\n`);

  writer.indent();
  writer.writeLn(`this.module._${field.name}(${reqParamName});`);
  writer.unindent();

  writer.writeLn(`}`);
  writer.blankLn();
};

export const writeTsInterface = (
  field: adlast.Field,
  writer: IndentableWriter
) => {
  writer.writeIndent();
  writer.writeRaw(`_${field.name}:(`);

  let reqParamName = "";
  const reqTypeExpr = field.typeExpr.parameters[0];
  if (
    !(
      reqTypeExpr.typeRef.kind === "primitive" &&
      reqTypeExpr.typeRef.value === "Void"
    )
  ) {
    if (reqTypeExpr.typeRef.kind === "primitive") {
      reqParamName = "req";
      writer.writeRaw(
        `${reqParamName}: ${supportedTsType(reqTypeExpr.typeRef.value)}`
      );
    } else if (reqTypeExpr.typeRef.kind === "reference") {
      reqParamName = camelCase(reqTypeExpr.typeRef.value.name);
      writer.writeRaw(`${reqParamName}: string`);
    }
  }

  writer.writeRaw(`) => `);

  const resTypeExpr = field.typeExpr.parameters[1];
  if (resTypeExpr.typeRef.kind === "primitive") {
    writer.writeRaw(`Promise<${supportedTsType(resTypeExpr.typeRef.value)}>;`);
  } else if (resTypeExpr.typeRef.kind === "reference") {
    writer.writeRaw(`Promise<${resTypeExpr.typeRef.value.name}>;`);
  }

  writer.blankLn();
}; // write abstract bridging fields

export const writeDep = (
  allAdl: LoadedAdl,
  i: adlast.TypeExpr,
  writer: IndentableWriter,
  outdir: string,
  parentFilename: string
) => {
  if (i.typeRef.kind === "reference") {
    const scopedDecl = allAdl.allAdlDecls[getScopedName(i)];
    const importHelper = new ImportingHelper();

    if (scopedDecl.decl.type_.kind === "struct_") {
      scopedDecl.decl.type_.value.fields.forEach((field) => {
        if (field.typeExpr.typeRef.kind === "primitive") {
        } else if (field.typeExpr.typeRef.kind === "reference") {
          const depFilename = getDepFilename(
            outdir,
            field.typeExpr.typeRef.value
          );

          const file = path.relative(path.dirname(parentFilename), depFilename);
          if (path.dirname(parentFilename) === path.dirname(depFilename)) {
            writer.writeLn(
              `import { ${
                field.typeExpr.typeRef.value.name
              } } from './${file.substr(0, file.lastIndexOf("."))}';`
            );
          } else {
            writer.writeLn(
              `import { ${
                field.typeExpr.typeRef.value.name
              } from '${file.substr(0, file.lastIndexOf("."))}';`
            );
          }
        }
      });

      writer.blankLn();

      writer.writeLn(`export interface ${i.typeRef.value.name} {`);
      writer.indent();

      scopedDecl.decl.type_.value.fields.forEach((field) => {
        if (field.typeExpr.typeRef.kind === "primitive") {
          writer.writeLn(
            `${field.name}: ${importHelper.asReferencedName(field.typeExpr)};`
          );
        } else if (field.typeExpr.typeRef.kind === "reference") {
          writer.writeLn(
            `${field.name}: ${field.typeExpr.typeRef.value.name};`
          );
        }
      });
    }

    writer.unindent();
    writer.writeLn(`}`);
  }

  writer.close();
};
