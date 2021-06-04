import * as fs from "fs";
import * as path from "path";
import * as adlast from "../adl-gen/sys/adlast";
import * as mkdirp from "mkdirp";
import { LoadedAdl } from "../util";
import { camelCase } from "change-case";
import { RNBridge } from "../gen-javarnmodule";
import {
  writeAbstractImplementation,
  writeBridgingField,
} from "./java-helpers";
import { ImportingHelper } from "../ts-services/import-helper";
import _ from "lodash";
import {
  writeDep,
  writeTsBridges,
  writeTsInterface,
} from "./typescript-helpers";

const REACT_NATIVE_MODULE = "onederful.mobile.reactnative";

export const isNativeFunction = (field: adlast.Field): boolean => {
  return (
    field.typeExpr.typeRef.kind === "reference" &&
    field.typeExpr.typeRef.value.moduleName === REACT_NATIVE_MODULE &&
    field.typeExpr.typeRef.value.name === "NativeFunction"
  );
};

export const isNativeCallback = (field: adlast.Field): boolean => {
  return (
    field.typeExpr.typeRef.kind === "reference" &&
    field.typeExpr.typeRef.value.moduleName === REACT_NATIVE_MODULE &&
    field.typeExpr.typeRef.value.name === "NativeCallback"
  );
};

/**
 *
 * Returns all NativeFunctions and NativeCallbacks
 */
export const getBridgedMethods = (bridge: RNBridge): adlast.Field[] => {
  return bridge.struct.value.fields.filter(
    (field) => isNativeCallback(field) || isNativeFunction(field)
  );
};

export const getJavaFileName = (
  outdir: string,
  nativeBridge: RNBridge,
  packageName: string
) => {
  return (
    outdir +
    packageName.split(".").reduce((prev, current) => {
      return prev.concat(`/${current}`);
    }, "") +
    nativeBridge.scopedDecl.moduleName.split(".").reduce((prev, current) => {
      return prev.concat(`/${current}`);
    }, "") +
    "/" +
    nativeBridge.scopedDecl.decl.name +
    ".java"
  );
};

export const getTsFileName = (outdir: string, nativeBridge: RNBridge) => {
  // TODO: this may collide with identically named bridges? (don't do it?)
  return (
    outdir +
    nativeBridge.scopedDecl.moduleName.split(".").reduce((prev, current) => {
      return prev.concat(`/${current}`);
    }, "") +
    "/" +
    nativeBridge.scopedDecl.decl.name +
    ".ts"
  );
};

export const getDepFilename = (
  outdir: string,
  scopedDecl: adlast.ScopedName
) => {
  return (
    outdir +
    scopedDecl.moduleName.split(".").reduce((prev, current) => {
      return prev.concat(`/${current}`);
    }, "") +
    "/" +
    scopedDecl.name +
    ".ts"
  );
};

export const getScopedName = (type: adlast.TypeExpr) => {
  if (type.typeRef.kind === "reference") {
    return type.typeRef.value.moduleName + "." + type.typeRef.value.name;
  }
  return "";
};

export const findUniqueImports = (bridge: RNBridge): adlast.TypeExpr[] => {
  const imports: adlast.TypeExpr[] = [];

  // TODO handle imports from different modules with same name
  const fields = getBridgedMethods(bridge);

  fields.forEach((field) => {
    const req = field.typeExpr.parameters[0];
    const res = field.typeExpr.parameters[1];

    if (!imports.find((i) => getScopedName(i) === getScopedName(req))) {
      if (req.typeRef.kind !== "primitive") imports.push(req);
    }

    if (!imports.find((i) => getScopedName(i) === getScopedName(res))) {
      if (res.typeRef.kind !== "primitive") {
        imports.push(res);
      }
    }
  });

  return imports;
};

const flattenType = (
  adlTree: LoadedAdl,
  moduleName: string,
  prefix: string = ""
): string => {
  let result = "";

  // console.log(reqType);
  // console.log("***********");
  const sDecl = adlTree.allAdlDecls[moduleName];
  if (sDecl.decl.type_.kind === "struct_") {
    const fields = sDecl.decl.type_.value.fields;
    fields.forEach((field) => {
      if (field.typeExpr.typeRef.kind === "primitive") {
        // TODO: actually map value to java types
        result += ` ${field.typeExpr.typeRef.value} ${prefix}${field.name},`;
      }
      if (field.typeExpr.typeRef.kind === "reference") {
        result +=
          flattenType(
            adlTree,
            field.typeExpr.typeRef.value.moduleName +
              "." +
              field.typeExpr.typeRef.value.name,
            prefix + field.name + "_"
          ) + ",";
      }
      // console.log(field);
    });
  }

  result = result.trim();
  const trim = result.replace(/(^,)|(,$)/g, "");
  return trim;
};

export class IndentableWriter {
  constructor(
    outfile: string,
    private writer = fs.createWriteStream(outfile),
    private indentLevel = 0
  ) {}

  indent = () => {
    this.indentLevel += 1;
  };

  unindent = () => {
    this.indentLevel = Math.max(0, this.indentLevel - 1);
  };

  writeLn = (line: string) => {
    //todo this can be more elegant
    for (let i = 0; i < this.indentLevel; i++) {
      this.writer.write("  ");
    }
    this.writer.write(`${line}\n`);
  };

  writeIndent = () => {
    for (let i = 0; i < this.indentLevel; i++) {
      this.writer.write("  ");
    }
  };

  writeRaw = (str: string) => {
    this.writer.write(str);
  };

  blankLn = () => {
    this.writer.write("\n");
  };

  close = () => {
    this.writer.close();
  };
}

export const writeNativeBridge = (
  nativeBridge: RNBridge,
  params: {
    outdir: string;
    nativeBridges: RNBridge[];
    packageName: string;
    loadedAdl: LoadedAdl;
  }
): void => {
  const { outdir, loadedAdl, nativeBridges, packageName } = params;

  const fileName = getJavaFileName(outdir, nativeBridge, packageName);
  const writer = new IndentableWriter(fileName);

  writer.writeLn(`// Class autogenerated by adl for native bridge`);
  writer.writeLn(
    `package ${packageName}.${nativeBridge.scopedDecl.moduleName};`
  );
  writer.blankLn();

  // import required adl types (if they are from a separate module)
  const imports = findUniqueImports(nativeBridge);
  imports
    .sort((t1, t2) =>
      getScopedName(t1) < getScopedName(t2)
        ? -1
        : getScopedName(t1) > getScopedName(t2)
        ? 1
        : 0
    )
    .filter((i) => {
      if (
        i.typeRef.kind === "reference" &&
        i.typeRef.value.moduleName === nativeBridge.scopedDecl.moduleName
      ) {
        return false;
      }
      return true;
    })
    .forEach((i) => {
      writer.writeLn(`import ${packageName}.${getScopedName(i)};`);
    });
  writer.blankLn();

  // import react-native requirements
  writer.writeLn("import com.facebook.react.bridge.Promise;");
  writer.writeLn(
    "import com.facebook.react.bridge.ReactContextBaseJavaModule;"
  );
  writer.writeLn("import com.facebook.react.bridge.ReactMethod;");
  writer.blankLn();

  writer.writeLn("import org.jetbrains.annotations.NotNull;");
  writer.blankLn();

  // ================ write class definition
  writer.writeLn(
    `public abstract class ${
      nativeBridge.name
    } extends ReactContextBaseJavaModule {`
  );

  // ================ class body
  writer.indent();
  nativeBridge.struct.value.fields
    .filter((field) => field.typeExpr.typeRef.kind === "reference")
    .forEach((field) => {
      writeBridgingField(field, writer); // write abstract bridging fields
      writeAbstractImplementation(field, writer); // write functions to map abstract flattened to adl objects
    });

  // write getName as a concrete method
  writer.writeLn(`@NotNull`);
  writer.writeLn(`@Override`);
  writer.writeLn(`public final String getName() {`);
  writer.indent();
  writer.writeLn(
    `// Overriding this function will prevent the generated typescript code from registering with the correct module`
  );
  writer.writeLn(`return "${nativeBridge.name}";`);
  writer.unindent();
  writer.writeLn(`}`);
  writer.blankLn();

  // close class
  writer.unindent();
  writer.writeLn("}");
  writer.close();
};

export const writeTypeScriptBridge = (
  nativeBridge: RNBridge,
  params: {
    outdir: string;
    nativeBridges: RNBridge[];
    packageName: string;
    loadedAdl: LoadedAdl;
  }
): void => {
  const { outdir, loadedAdl } = params;
  const importHelper = new ImportingHelper();

  const filename = getTsFileName(outdir, nativeBridge);
  mkdirp.sync(path.dirname(filename));
  const writer = new IndentableWriter(filename);

  writer.writeLn(`// Class autogenerated by adl for native bridge`);
  writer.writeLn(`import { NativeModules } from "react-native";`);
  writer.writeLn(
    `const { ${nativeBridge.name}: NativeModule } = NativeModules;`
  );
  writer.blankLn();

  const imports = findUniqueImports(nativeBridge);
  imports
    .sort((t1, t2) =>
      getScopedName(t1) < getScopedName(t2)
        ? -1
        : getScopedName(t1) > getScopedName(t2)
        ? 1
        : 0
    )
    .forEach((i) => {
      // this can produce duplicated imports across files
      // TODO: solve this importing problem

      if (i.typeRef.kind === "reference") {
        const depFilename = getDepFilename(outdir, i.typeRef.value);
        mkdirp.sync(path.dirname(depFilename));
        const depWriter = new IndentableWriter(depFilename);

        writeDep(loadedAdl, i, depWriter, outdir, filename);

        // write the import into the file
        const file = path.relative(path.dirname(filename), depFilename);
        if (path.dirname(filename) === path.dirname(depFilename)) {
          writer.writeLn(
            `import { ${i.typeRef.value.name} } from './${file.substr(
              0,
              file.lastIndexOf(".")
            )}';`
          );
        } else {
          writer.writeLn(
            `import { ${i.typeRef.value.name} } from '${file.substr(
              0,
              file.lastIndexOf(".")
            )}';`
          );
        }
      }
    });
  writer.blankLn();

  writer.writeLn(`interface NativeInterface {`);
  writer.indent();

  nativeBridge.struct.value.fields
    .filter((field) => field.typeExpr.typeRef.kind === "reference")
    .forEach((field) => {
      writeTsInterface(field, writer); // write abstract bridging fields
    });

  writer.unindent();
  writer.writeLn("}");
  writer.blankLn();

  writer.writeLn(`class ${nativeBridge.name} {`);
  writer.indent();

  writer.writeLn(
    `constructor(private module: NativeInterface = NativeModule) {}`
  );
  writer.blankLn();

  nativeBridge.struct.value.fields
    .filter((field) => field.typeExpr.typeRef.kind === "reference")
    .forEach((field) => {
      writeTsBridges(field, writer); // write abstract bridging fields
    });

  writer.unindent();
  writer.writeLn("}");
  writer.blankLn();

  writer.writeLn(`export default new ${nativeBridge.name}(NativeModule);`);
};
