import * as adlast from './adl-gen/sys/adlast';
import { RESOLVER }  from "./adl-gen/resolver";
import { createJsonBinding, JsonBinding } from "./adl-gen/runtime/json";
import * as adl from "./adl-gen/runtime/adl";
import * as tmp from "tmp";
import { execFile } from "child_process";
import * as fs from "fs";
import { promisify } from "util";

type AdlModuleMap = {[key:string]:adlast.Module};

export interface LoadedAdl {
  modules: AdlModuleMap,
  resolver: adl.DeclResolver
};

/**
 * Load and parse the specified ADL files (and their dependencies) into
 * an AST map.
 *
 * Runs the adl compiler as a subprocess, using the environment variable ADLC to
 * specify the path.
 */
export async function parseAdl(adlFiles: string[], adlSearchPath: string[]): Promise<LoadedAdl> {
  const moduleMapJB = createJsonBinding(RESOLVER, adl.texprStringMap(adlast.texprModule()));

  // Work in a temporary directory
  const workdir = await tmpDirP();

  // run the ADL ast parser, outputing to a temporary file
  const outfile = workdir.name + "/output.json";
  const adlc = process.env.ADLC || "/usr/local/bin/adlc";
  let args = ["ast", `--combined-output=${outfile}`]
  for(const dir of adlSearchPath) {
    args = args.concat(['-I', dir]);
  }
  args = args.concat(adlFiles);
  try {
    await execFileP( adlc, args, {});
  } catch (e) {
    console.log("adl compiler failed:");
    console.log(e.stdout);
    console.log(e.stderr);
    throw e;
  }

  // Parse the module map json
  const jv = JSON.parse(await readFileP(outfile, 'utf8'));
  const modules = moduleMapJB.fromJsonE(jv);
  resolveRelativeModuleRefs(modules);

  // Build the resolver
  const astMap = {};
  forEachDecl(modules, scopedDecl => {
    astMap[scopedDecl.moduleName+"."+scopedDecl.decl.name] = scopedDecl;
  });
  const resolver : adl.DeclResolver = adl.declResolver(astMap);

  // Cleanup
  await unlinkP(outfile);
  workdir.removeCallback();

  return {modules, resolver};
}

const execFileP = promisify(execFile);
const readFileP = promisify(fs.readFile);
const unlinkP = promisify(fs.unlink);

function tmpDirP(): Promise<tmp.DirResult> {
  return new Promise((resolve, reject) => {
    tmp.dir({}, (err, name, removeCallback) => {
      if (err) {
        reject(err);
      } else {
        resolve({name,removeCallback});
      }
    })
  });
}

/**
 *  Convert module relative scoped names into absolute scoped namees
 */
export function resolveRelativeModuleRefs(moduleMap: AdlModuleMap) {
  forEachDecl(moduleMap, (sdecl: adlast.ScopedDecl) => {
    function resolve(typeExpr: adlast.TypeExpr) {
      if (typeExpr.typeRef.kind == 'reference' && typeExpr.typeRef.value.moduleName == '') {
        typeExpr.typeRef.value.moduleName = sdecl.moduleName;
      }
      typeExpr.parameters.forEach(p => resolve(p));
    }
    const dtype = sdecl.decl.type_;
    switch (dtype.kind) {
    case 'newtype_':
      resolve(dtype.value.typeExpr);
      break;
    case 'struct_':
      dtype.value.fields.forEach( f => resolve(f.typeExpr) );
      break;
    case 'union_':
      dtype.value.fields.forEach( f => resolve(f.typeExpr) );
      break;
    case 'type_':
      resolve(dtype.value.typeExpr);
      break;
    }
  });
}


export function substituteTypeVariable(typeExpr: adlast.TypeExpr,  tparam: string, tvalue: adlast.TypeExpr): adlast.TypeExpr {
  if (typeExpr.typeRef.kind == 'typeParam' && typeExpr.typeRef.value == tparam  && typeExpr.parameters.length == 0) {
    return tvalue;
  }
  return {
    typeRef: typeExpr.typeRef,
    parameters: typeExpr.parameters.map( p => substituteTypeVariable(p, tparam, tvalue) )
  };
}

export function substituteTypeVariables(typeExpr0: adlast.TypeExpr,  tparams: string[], tvalues: adlast.TypeExpr[]): adlast.TypeExpr {
  let typeExpr = typeExpr0;
  for(let i = 0; i < tparams.length; i++) {
    typeExpr = substituteTypeVariable(typeExpr, tparams[0], tvalues[0]);
  }
  return typeExpr;
}

export function expandTypeAlias(resolver: adl.DeclResolver, typeExpr: adlast.TypeExpr): adlast.TypeExpr | null {
  if (typeExpr.typeRef.kind == 'reference') {
    const sdecl = resolver(typeExpr.typeRef.value);
    const dtype = sdecl.decl.type_;
    if (dtype.kind == 'type_') {
      const tparams = dtype.value.typeParams;
      const tvalues = typeExpr.parameters;
      return  substituteTypeVariables(dtype.value.typeExpr, tparams, tvalues);
    }
  }
  return null;
}

export function expandNewType(resolver: adl.DeclResolver, typeExpr: adlast.TypeExpr): adlast.TypeExpr | null {
  if (typeExpr.typeRef.kind == 'reference') {
    const sdecl = resolver(typeExpr.typeRef.value);
    const dtype = sdecl.decl.type_;
    if (dtype.kind == 'newtype_') {
      const tparams = dtype.value.typeParams;
      const tvalues = typeExpr.parameters;
      return  substituteTypeVariables(dtype.value.typeExpr, tparams, tvalues);
    }
  }
  return null;
}

/**
 * Recursively xpand type aliases and/or newtypes by substitution
 */
export function expandTypes(resolver: adl.DeclResolver, typeExpr: adlast.TypeExpr, options: ExpandTypeOptions): adlast.TypeExpr {
  switch(typeExpr.typeRef.kind) {
  case "primitive":
    break;
  case "reference":
    let texpr2 = null;
    if (options.expandTypeAliases) {
      texpr2 = texpr2 || expandTypeAlias(resolver, typeExpr);
    }
    if (options.expandNewType) {
      texpr2 = texpr2 || expandNewType(resolver, typeExpr);
    }
    if (texpr2) {
      return expandTypes(resolver, texpr2, options);
    }
   break;
  case "typeParam":
    break;
  }
  return typeExpr;
}

interface ExpandTypeOptions {
  expandTypeAliases?: boolean;
  expandNewType?: boolean
};

/**
 * Execute the given function for each ADL declaration
 */
export function forEachDecl(moduleMap: AdlModuleMap, fn: (sdecl: adlast.ScopedDecl) => void): void {
  for(const moduleName of Object.keys(moduleMap)) {
    const module : adlast.Module = moduleMap[moduleName];
    for (const declName of Object.keys(module.decls)) {
      const decl = module.decls[declName];
      fn({moduleName,decl});
    }
  }
}

/**
 * Return the json value for the given annotation type for the given decl. Return undefined if the
 * decl doesn't have that annotation
 */
export function getAnnotation(annotations: adlast.Annotations, annotationType: adlast.ScopedName): {}|null|undefined {
  for(const ann of annotations) {
    if (scopedNamesEqual(ann.v1, annotationType)) {
      return ann.v2;
    }
  }
  return undefined;
}

export type DecodedTypeExpr
  = {kind: "Void"}
  | {kind: "String"}
  | {kind: "Bool"}
  | {kind: "Json"}
  | {kind: "Int8" }
  | {kind: "Int16" }
  | {kind: "Int32" }
  | {kind: "Int64" }
  | {kind: "Word8" }
  | {kind: "Word16" }
  | {kind: "Word32" }
  | {kind: "Word64" }
  | {kind: "Float" }
  | {kind: "Double" }
  | {kind: "Vector", elemType: DecodedTypeExpr}
  | {kind: "StringMap", elemType: DecodedTypeExpr}
  | {kind: "Nullable", elemType: DecodedTypeExpr}
  | {kind: "Reference", refScopedName: adlast.ScopedName, parameters: DecodedTypeExpr[]}
  | {kind: "Other", typeExpr: adlast.TypeExpr}

export function decodeTypeExpr(typeExpr: adlast.TypeExpr): DecodedTypeExpr {
  switch(typeExpr.typeRef.kind) {
  case 'primitive':
    const primitive = typeExpr.typeRef.value;
    switch (primitive) {
      case "Void": return {kind: primitive};
      case "String": return {kind: primitive};
      case "Bool" : return {kind: primitive};
      case "Json": return {kind: primitive};
      case "Int8": return {kind: primitive};
      case "Int16": return {kind: primitive};
      case "Int32": return {kind: primitive};
      case "Int64": return {kind: primitive};
      case "Word8": return {kind: primitive};
      case "Word16": return {kind: primitive};
      case "Word32": return {kind: primitive};
      case "Word64": return {kind: primitive};
      case "Float" : return {kind: primitive}
      case "Double" : return {kind: primitive}
      case "Vector" : return {kind: "Vector", elemType: decodeTypeExpr(typeExpr.parameters[0])};
      case "StringMap" : return {kind: "StringMap", elemType: decodeTypeExpr(typeExpr.parameters[0])};
      case "Nullable" : return {kind: "Nullable", elemType: decodeTypeExpr(typeExpr.parameters[0])};
    }
    break;
  case 'reference':
      return {kind:"Reference", refScopedName: typeExpr.typeRef.value, parameters:typeExpr.parameters.map(decodeTypeExpr)};
  }
  return {kind:"Other", typeExpr};
}

export function scopedName(moduleName:string, name: string): adlast.ScopedName {
  return {moduleName,name};
}

export function scopedNamesEqual(n1: adlast.ScopedName, n2: adlast.ScopedName): boolean {
  return n1.moduleName == n2.moduleName && n1.name == n2.name;
}

/**
 * Helper for command line processing
 */
export function collect (val:string, memo:string[]): string[] {
    memo.push(val);
    return memo;
}


export async function execHxAdlHs(args: string[]) {
  const hxadlhs = process.env.HXADLHS || "/usr/local/bin/hx-adl-hs";
  try {
    await execFileP( hxadlhs, args, {});
  } catch (e) {
    console.log("adl compiler failed:");
    console.log(e.stdout);
    console.log(e.stderr);
    throw e;
  }}



