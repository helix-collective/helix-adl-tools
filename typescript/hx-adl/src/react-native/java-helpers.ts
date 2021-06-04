import { camelCase } from "change-case";
import * as adlast from "../adl-gen/sys/adlast";
import { RNBridge } from "../gen-javarnmodule";
import { IndentableWriter } from "./react-native-utils";

export const writeBridgingField = (
  field: adlast.Field,
  writer: IndentableWriter
) => {
  writer.writeLn(`@ReactMethod`);
  const reqTypeExpr = field.typeExpr.parameters[0];
  const resTypeExpr = field.typeExpr.parameters[1];

  writer.writeIndent();
  writer.writeRaw(`public final void _${field.name}(`);

  let hasReqParam = true;
  let hasReturn = false;
  let reqParamName = "";
  if (
    reqTypeExpr.typeRef.kind === "primitive" &&
    reqTypeExpr.typeRef.value === "Void"
  ) {
    hasReqParam = false;
  } else {
    if (reqTypeExpr.typeRef.kind === "primitive") {
      reqParamName = "req";
      writer.writeRaw(`${reqTypeExpr.typeRef.value} ${reqParamName}`);
    } else if (reqTypeExpr.typeRef.kind === "reference") {
      reqParamName = camelCase(reqTypeExpr.typeRef.value.name);
      writer.writeRaw(`String ${reqParamName}`);
    }
  }

  if (
    resTypeExpr.typeRef.kind === "primitive" &&
    resTypeExpr.typeRef.value === "Void"
  ) {
  } else {
    hasReturn = true;
    writer.writeRaw(`${hasReqParam ? ", " : ""}Promise resPromise`);
  }
  writer.writeRaw(`) {\n`);
  writer.indent();

  // function body to construct POJOs
  if (reqTypeExpr.typeRef.kind === "primitive") {
  } else if (reqTypeExpr.typeRef.kind === "reference") {
    const typeName = reqTypeExpr.typeRef.value.name;
    writer.writeLn(
      `${typeName} req = ${typeName}.jsonBinding().fromJsonString(${camelCase(
        typeName
      )});`
    );
  }

  if (hasReturn) {
    writer.writeLn(
      `resPromise.resolve(this.${field.name}(${hasReqParam ? "req" : ""}));`
    );
  } else {
    writer.writeLn(`this.${field.name}(${reqParamName});`);
  }

  writer.unindent();
  writer.writeLn("}");
  writer.blankLn();
};

const supportedJavaType = (type: string): string => {
  switch (type) {
    case "Bool":
      return "boolean";
    case "String":
      return "String";
    case "Double":
      return "Double";
    case "Void":
      return "void";
    case "Vector":
    case "StringMap":
    case "Nullable":
    case "ByteVector":
      throw new Error(`TODO: support ${type}`);
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

export const writeAbstractImplementation = (
  field: adlast.Field,
  writer: IndentableWriter
) => {
  const reqTypeExpr = field.typeExpr.parameters[0];
  const resTypeExpr = field.typeExpr.parameters[1];

  writer.writeIndent();

  let reqParamName = "";
  let returnType = "void";

  if (resTypeExpr.typeRef.kind === "primitive") {
    returnType = supportedJavaType(resTypeExpr.typeRef.value);
  } else if (resTypeExpr.typeRef.kind === "reference") {
    returnType = resTypeExpr.typeRef.value.name;
  }

  writer.writeRaw(`public abstract ${returnType} ${field.name}(`);

  if (
    !(
      reqTypeExpr.typeRef.kind === "primitive" &&
      reqTypeExpr.typeRef.value === "Void"
    )
  ) {
    if (reqTypeExpr.typeRef.kind === "primitive") {
      reqParamName = "req";
      writer.writeRaw(`${reqTypeExpr.typeRef.value} ${reqParamName}`);
    } else if (reqTypeExpr.typeRef.kind === "reference") {
      reqParamName = camelCase(reqTypeExpr.typeRef.value.name);
      writer.writeRaw(`${reqTypeExpr.typeRef.value.name} ${reqParamName}`);
    }
  }
  writer.writeRaw(`);\n`);
  writer.blankLn();
};
