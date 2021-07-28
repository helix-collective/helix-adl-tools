import { camelCase, pascalCase } from "change-case";
import { ScopedName, TypeExpr } from "../adl-gen/runtime/sys/adlast";
import { LoadedAdl } from "../util";

export class StringMapSet {
  private readonly map: Map<string, Set<string>>;

  clear(): void {
    this.map.clear();
  }

  delete(key: string): boolean {
    return this.map.delete(key);
  }

  forEach(callbackfn: (value: Set<string>, key: string) => void): void {
    this.map.forEach(callbackfn);
  }

  get(key: string): Set<string> | undefined {
    return this.map.get(key);
  }

  has(key: string): boolean {
    return this.map.has(key);
  }

  constructor() {
    this.map = new Map<string, Set<string>>();
  }

  add(key: string, val: string) {
    let currentSet = this.get(key);

    if (currentSet === undefined) {
      currentSet = new Set<string>();
      this.map.set(key, currentSet);
    }

    currentSet.add(val);
  }
}

export class ScopedNameBiMapSet {
  scopedNames: ScopedName[] = [];
  forward: StringMapSet = new StringMapSet();
  reverse: StringMapSet = new StringMapSet();

  add(sn: ScopedName) {
    this.scopedNames.push(sn);
    this.forward.add(sn.moduleName, sn.name);
    this.reverse.add(sn.name, sn.moduleName);
  }
}

/** Helper for resolving how to import types from modules
 * esp if there are duplicate types in different modules
 * tuned for how typescript can rename imports via: import { foo as bar } from blah */
export class ImportingHelper {
  moduleTypes = new ScopedNameBiMapSet();
  javaImports = new Set<string>();

  asImportedNames: Map<ScopedName, string> = new Map<ScopedName, string>();

  modulesImports: StringMapSet = new StringMapSet();

  importSns: Set<ScopedName> = new Set();
  importMakeFns: Set<ScopedName> = new Set();

  addTypeParams(type: TypeExpr, loadedAdl: LoadedAdl) {
    // recurse into the type params:
    for (const tp of type.parameters) {
      this.addType(tp,loadedAdl);
    }
  }

  addType(type: TypeExpr, loadedAdl: LoadedAdl) {
    if (type.typeRef.kind === "primitive") {
      switch (type.typeRef.value) {
        // primitives that have type params:
        case 'Vector':
          this.javaImports.add("java.util.List");
          this.addTypeParams(type, loadedAdl)
          break;
        case 'Nullable':
          this.javaImports.add("java.util.Optional");
          this.addTypeParams(type, loadedAdl)
          break;
        case 'StringMap':
          this.javaImports.add("java.util.Map");
          this.addTypeParams(type, loadedAdl)
          break;
        case 'Void':
          this.javaImports.add("au.com.helixta.adl.runtime.AdlVoid");
          break;
        case 'Json':
          this.javaImports.add("com.google.gson.JsonElement");
          break;
        case 'String':
        case 'Bool':
        case 'Int8':
        case 'Int16':
        case 'Int32':
        case 'Int64':
        case 'Word8':
        case 'Word16':
        case 'Word32':
        case 'Word64':
        case 'Float':
        case 'Double':
          break;

        default:
          throw new Error("Unexpected primitive '" + type.typeRef.value + "'");
      }
    }
    if (type.typeRef.kind === "reference") {
      // If it's an adl `type X = Y`
      const adlType = loadedAdl.allAdlDecls[`${type.typeRef.value.moduleName}.${type.typeRef.value.name}`];
      if(adlType.decl.type_.kind === "type_") {
        console.log("ADL type", `${type.typeRef.value.moduleName}.${type.typeRef.value.name}` )
        const tp = adlType.decl.type_.value.typeExpr
        this.addType(tp,loadedAdl);
      }
      // tslint:disable-next-line: no-console
      this.moduleTypes.add(type.typeRef.value);
      this.addTypeParams(type, loadedAdl)
    }
    if (type.typeRef.kind === "typeParam") {
      throw new Error("Unexpected type.typeRef.kind === 'typeParam'");
    }
  }

  resolveImports() {
    this.asImportedNames.clear();


    for (const sn of this.moduleTypes.scopedNames) {
      // tslint:disable-next-line: no-non-null-assertion
      const mods = Array.from(this.moduleTypes.reverse.get(sn.name)!);

      // figure out which scopedNames have ambiguous names (ie in common between several modules)
      if (mods.length === 1) {
        // name is unique across modules: can import and use as-is
        this.asImportedNames.set(sn, sn.name);
        this.modulesImports.add(sn.moduleName, sn.name);

        if(this.importSns.has(sn)) {
          this.modulesImports.add(sn.moduleName, `sn${pascalCase(sn.name)}`);
        }
        if(this.importMakeFns.has(sn)) {
          this.modulesImports.add(sn.moduleName, `make${pascalCase(sn.name)}`);
        }
      } else {
        // name is ambiguous across modules
        // use "import {name as PascalCaseTheModuleWithName} from blah"
        const asImportedName = pascalCase(`${sn.moduleName}${sn.name}`);
        this.asImportedNames.set(sn, asImportedName);
        this.modulesImports.add(sn.moduleName, `${sn.name} as ${asImportedName}`);

        if(this.importSns.has(sn)) {
          this.modulesImports.add(sn.moduleName, `sn${pascalCase(sn.name)} as sn${asImportedName}`);
        }
        if(this.importSns.has(sn)) {
          this.modulesImports.add(sn.moduleName, `make${pascalCase(sn.name)} as make${asImportedName}`);
        }
      }
    }
  }

  /** Get a typeExpr in form as imported (possibly as a different name)
   * also in the form used by typescript
   **/
  asReferencedName(type: TypeExpr, loadedAdl: LoadedAdl): string {
    if (type.typeRef.kind === "primitive") {

      switch (type.typeRef.value) {
        case 'Vector':
          if (type.parameters.length !== 1) {
            throw new Error("Expected only 1 tparam");
          }
          return `List<${this.asReferencedName(type.parameters[0],loadedAdl)}>`;

        case 'Nullable':
          if (type.parameters.length !== 1) {
            throw new Error("Expected only 1 tparam");
          }
          return `Optional<${this.asReferencedName(type.parameters[0],loadedAdl)}>`;

        case 'StringMap':
          if (type.parameters.length !== 1) {
            throw new Error("Expected only 1 tparam");
          }
          return `Map<String,${this.asReferencedName(type.parameters[0],loadedAdl)}>`;
        case 'Void':
          return "AdlVoid";
        case 'String':
          return "String";
        case 'Bool':
          return "Boolean";
        case 'Int8':
        case 'Word8':
            return "Byte";
        case 'Int16':
        case 'Word16':
            return "Short";
        case 'Int32':
        case 'Word32':
            return "Integer";
        case 'Int64':
        case 'Word64':
          return "Long";
        case 'Float':
          return "Float";
        case 'Double':
          return "Double";
        case 'Json':
          return "JsonElement";

        default:
          throw new Error("Unexpected primitive " + type.typeRef.value);
      }
    }
    if (type.typeRef.kind === "reference") {
      let asImported = this.asImportedNames.get(type.typeRef.value);
      if (asImported === undefined) {
        throw new Error(`No asImported name found - ${type.typeRef.value.name}`);
      }
      // If it's an adl `type X = Y`
      const adlType = loadedAdl.allAdlDecls[`${type.typeRef.value.moduleName}.${type.typeRef.value.name}`];
      if(adlType.decl.type_.kind === "type_") {
        console.log("ADL type", `${type.typeRef.value.moduleName}.${type.typeRef.value.name}` )
        const tp = adlType.decl.type_.value.typeExpr
        asImported = this.asReferencedName(tp,loadedAdl);
        console.log("ADL type", asImported)
        if (asImported === undefined) {
          throw new Error(`No asImported name found - ${type.typeRef.value.name}`);
        }
      }

      if (type.parameters.length > 0) {
        // generic type with type parameters:
        const typeParamsNames = type.parameters.map((tp) => this.asReferencedName(tp,loadedAdl));
        return `${asImported}<${typeParamsNames.join(",")}>`;
      } else {
        // ordinary ADL referred type
        return asImported;
      }
    }
    if (type.typeRef.kind === "typeParam") {
      throw new Error("Unexpected type.typeRef.kind === 'typeParam'");
    }
    throw new Error("Unhandled type.typeRef.kind");
  }
}
