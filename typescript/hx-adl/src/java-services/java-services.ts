// import { camelCase } from 'change-case';
import { Command } from 'commander';
import * as fsx from 'fs-extra';

import * as sys_types from '../adl-gen/runtime/sys/types';
import { Annotations, Module, ScopedDecl, TypeExpr, Struct } from '../adl-gen/runtime/sys/adlast';
import { collect, LoadedAdl, parseAdl } from '../util';
import { CodeGen } from './code-gen';
import { ImportingHelper } from './import-helper';

export function configureCli(program: Command) {
  program
    .command("java-services [adlFiles...]")
    .option('-I, --searchdir <path>', 'Add to adl searchpath', collect, [])
    .option('--apimodule <str>', 'ADL module name for the struct that has the API defs')
    .option('--apiname <str>', 'ADL struct name for the struct that has the API defs')
    .option('--apifield <str>', 'ADL field inside the apiname struct', '')
    .option('--package <path>', 'default package to prepend to module name if no package speced in <file>.adl-java', '')
    .option('--javasrcroot <path>', 'root directory for java src')
    .option('--servicepackage <str>', 'Java package for the service')
    .option('--serviceclass <str>', 'Class name for the service', 'AppService')

    .description('Generate a java api service interface file.')
    .action((adlFiles: string[], cmd: {}) => {
      const adlSearchPath: string[] = cmd["searchdir"];
      const javasrcroot: string = cmd["javasrcroot"];
      const defpackage: string = cmd["package"]
      const servicepackage: string = cmd["servicepackage"];
      const apimodule: string = cmd["apimodule"];
      const apiname: string = cmd["apiname"];
      const apifield: string = cmd["apifield"];
      const serviceclass: string = cmd["serviceclass"];
      generateTypescriptService({
        adlSearchPath,
        defpackage,
        javasrcroot,
        servicepackage,
        apimodule,
        apiname,
        apifield,
        adlFiles,
        serviceclass
      });
    });
}

interface Annotable {
  annotations: Annotations;
}

function getJavaPackage(item: Annotable): string | null {
  for (const anno of item.annotations) {
    if (anno.key.moduleName === 'adlc.config.java' && anno.key.name === "JavaPackage") {
      return anno.value as string;
    }
  }
  return null;
}

function getComment(item: Annotable): string | null {
  let comment: string | null = null;
  for (const anno of item.annotations) {
    if (anno.key.name === "Doc") {
      comment = anno.value as string;
      comment = comment.replace(/\n/g, " ");
      comment = comment.trim();
    }
  }
  return comment;
}

type CodeGenType = "collect" | "decl" | "ctor" | "impl";

function addCode(
  importingHelper: ImportingHelper,
  loadedAdl: LoadedAdl,
  codeGenType: CodeGenType,
  codeGen: CodeGen,
  typeExpr: TypeExpr,
  name: string,
  comment: string | null,
  getValue: (name: string) => sys_types.Maybe<{}|null>
) {
  if (typeExpr.typeRef.kind !== "reference") {
    throw new Error("Unexpected - typeExpr.typeRef.kind !== reference");
  }
  if (typeExpr.typeRef.value.name === "HttpPost") {
    if (typeExpr.parameters.length !== 2) {
      throw new Error("Unexpected - typeExpr.parameters.length != 2");
    }
    const requestType = typeExpr.parameters[0];
    const responseType = typeExpr.parameters[1];
    switch (codeGenType) {
      case "collect": {
        importingHelper.addType(requestType, loadedAdl);
        importingHelper.addType(responseType, loadedAdl);
        return;
      }
      case "decl": {
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        codeGen.add(
          `${importingHelper.asReferencedName(responseType, loadedAdl)} ${name}(HxContext ctx, ${importingHelper.asReferencedName(requestType, loadedAdl)} req);`
        );
        codeGen.add("");
        return;
      }
      case "ctor": {
        const Name = name[0].toUpperCase() + name.substr(1) 
        codeGen.add(`// register ${name} as a post`);
        codeGen.add(`// ${JSON.stringify(getValue(name))}`);
        codeGen.add(`adl(REQUESTS.get${Name}(), (i, o) -> {`);
        codeGen.add(`  handle(REQUESTS.get${Name}(), i, o, (a, t) -> t.send(impl.${name}(ctx, a)));`);
        codeGen.add(`});`);
        codeGen.add("");
        return;
      }
      case "impl": {
        codeGen.add("");
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        return;
      }
    }
  }
  if (typeExpr.typeRef.value.name === "HttpGet") {
    if (typeExpr.parameters.length !== 1) {
      throw new Error("Unexpected - typeExpr.parameters.length != 1");
    }
    const responseType = typeExpr.parameters[0];
    switch (codeGenType) {
      case "collect": {
        importingHelper.addType(responseType, loadedAdl);
        return;
      }
      case "decl": {
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        codeGen.add(
          `${importingHelper.asReferencedName(responseType, loadedAdl)} ${name}(HxContext ctx);`
        );
        codeGen.add("");
        return;
      }
      case "ctor": {
        const Name = name[0].toUpperCase() + name.substr(1) 
        codeGen.add(`// register ${name} as a post`);
        codeGen.add(`// ${JSON.stringify(getValue(name))}`);
        codeGen.add(`adl(REQUESTS.get${Name}(), (i, o) -> {`);
        codeGen.add(`  handle(REQUESTS.get${Name}(), i, o, (t) -> t.send(impl.${name}(ctx)));`);
        codeGen.add(`});`);
        codeGen.add("");
        return;
      }
      case "impl": {
        codeGen.add("");
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        return;
      }
    }

  }
  if (typeExpr.typeRef.value.name === "HttpGetStream") {
    if (typeExpr.parameters.length !== 1) {
      throw new Error("Unexpected - typeExpr.parameters.length != 1");
    }
    const responseType = typeExpr.parameters[0];
    switch (codeGenType) {
      case "collect": {
        importingHelper.addType(responseType, loadedAdl);
        return;
      }
      case "decl": {
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        codeGen.add(`TODO`);
        // codeGen.add(
        //   `${camelCase("get " + name)}: GetStreamFn<${importingHelper.asReferencedName(responseType, loadedAdl)}>;`
        // );
        codeGen.add("");
        return;
      }
      case "ctor": {
        codeGen.add(`TODO`);
        return;
      }
      case "impl": {
        codeGen.add("");
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        return;
      }
    }
  }
  const adlType = loadedAdl.allAdlDecls[`${typeExpr.typeRef.value.moduleName}.${typeExpr.typeRef.value.name}`];
  if (adlType) {
    if (adlType.decl.type_.kind === "type_" || adlType.decl.type_.kind === "newtype_") {
      // add the type of the field to the import helper
      importingHelper.addType(adlType.decl.type_.value.typeExpr, loadedAdl);

      if (adlType.decl.type_.value.typeParams.length !== 0) {
        //TODO(garym:202104) Implement generics for type and newtype
        // console.log("typeExpr", JSON.stringify(typeExpr, null, 2))
        // const param2typeExpr: MAP PARAM NAME to DECL
        // if( adlType.decl.type_.value.typeParams.length === typeExpr.parameters.length ) {
        //   adlType.decl.type_.value.typeParams.forEach((t,i) => {
        //    fill in map
        //   })
        // }
        // console.log("Param", adlType.decl.type_.value.typeParams, JSON.stringify(adlType, null, 2))
        throw new Error("ERROR: 'type' or 'newtype' with generic params not implemented in hx-adl java-services.ts");
      }
      addCode(importingHelper, loadedAdl, codeGenType, codeGen, adlType.decl.type_.value.typeExpr, name, comment, getValue);
    }
    return;
  }
  if (codeGenType === "collect") {
    console.warn(`java-services: unrecognized field ${typeExpr.typeRef.value.name}`);
  }
}

async function generateTypescriptService(params: {
  adlSearchPath: string[];
  apimodule: string;
  apiname: string;
  apifield: string;
  defpackage: string;
  javasrcroot: string;
  servicepackage: string;
  serviceclass: string;
  adlFiles: string[];
}) {
  const { adlSearchPath, javasrcroot, defpackage, servicepackage, apimodule, apiname, apifield, adlFiles, serviceclass } = params;

  // Load the ADL based upon command line arguments
  const loadedAdl = await parseAdl(adlFiles, adlSearchPath, "adl-java");

  let apistructSn: string = ""
  let apiRequests: ScopedDecl | undefined
  let apiRequestsStruct: Struct | undefined
  let apiClass: string = "?????"
  let apiReqsTypeExpr: TypeExpr | undefined
  let getValue: (name: string) => sys_types.Maybe<{}|null> = (name: string ) => {
    console.log("@!#$@#$@#!")
    return {kind: "nothing"}
  }
  if( apifield !== "" ) {
    // special case where field inside struct is specified
    const structContainerReq = loadedAdl.allAdlDecls[`${apimodule}.${apiname}`];
    if (structContainerReq === undefined) {
      throw new Error(`Scoped name Container not found: ${structContainerReq}`);
    }
    // console.log("!!!", structContainerReq.decl.type_.value)
    if (structContainerReq.decl.type_.kind !== "struct_") {
      throw new Error(`Unexpected - ${structContainerReq} is not a struct`);
    }
    for (const f of structContainerReq.decl.type_.value.fields) {
      if( f.name === apifield ) {
        if( f.typeExpr.typeRef.kind !== "reference" ) {
          throw new Error(`Unexpected - expected a ref '${apiname}::${apifield}'`);
        }
        getValue = (name: string) => {
          if( f.default.kind === "just" ) {
            if( f.default.value === null ) {
              return {kind: "just", value: null}
            }
            return {kind: "just", value: f.default.value[name]}
          }
          return {kind: "nothing"}
        }
        apiClass = f.typeExpr.typeRef.value.name
        apiRequests = loadedAdl.allAdlDecls[`${f.typeExpr.typeRef.value.moduleName}.${f.typeExpr.typeRef.value.name}`];
        apiReqsTypeExpr = f.typeExpr
      }
    }
    if (apiRequests === undefined) {
      throw new Error(`Scoped name field not found: '${apiname}::${apifield}'`);
    }
    if (apiRequests.decl.type_.kind !== "struct_") {
      throw new Error(`Unexpected - not a struct -> "${apistructSn}"`);
    }
    apiRequestsStruct = apiRequests.decl.type_.value;  
  } else {
    // Normal case
    apistructSn = `${apimodule}.${apiname}`;
    apiClass = apiname
    apiRequests = loadedAdl.allAdlDecls[apistructSn];
    if (apiRequests === undefined) {
      throw new Error(`Scoped name not found: "${apistructSn}"`);
    }
    if (apiRequests.decl.type_.kind !== "struct_") {
      throw new Error("Unexpected - apiRequests is not a struct");
    }
    apiRequestsStruct = apiRequests.decl.type_.value;
    apiReqsTypeExpr = {
      typeRef: {
        kind: 'reference',
        value: {
          moduleName: apimodule,
          name: apiname
        }
      },
      parameters: []
    };
    getValue = (name: string) => {
      if(apiRequestsStruct) {
        for(const e of apiRequestsStruct.fields) {
          if( e.name === name ) {
            return e.default
          }
        }
      } else {
        console.log("????")
      }
      return {kind: "nothing"}
    }
  }

  if(apiReqsTypeExpr === undefined) {
    throw new Error("!@#$%^&*()")
  }

  const importingHelper = new ImportingHelper();
  importingHelper.addType(apiReqsTypeExpr, loadedAdl);

  // start rendering code:
  const code = new CodeGen();
  code.add(`/* @generated from adl module ${apimodule} apiname: ${apiname}${apifield === "" ? "" : "::" + apifield}*/`);
  code.add(`package ${servicepackage};`);
  code.add("");

  // load all apiEntry referenced types into importingHelper to disambiguate imports:
  // it also recurses into all the type params of those types.
  for (const apiEntry of apiRequestsStruct.fields) {
    addCode(importingHelper,
      loadedAdl,
      "collect",
      code,
      apiEntry.typeExpr,
      apiEntry.name,
      getComment(apiEntry),
      getValue
    );
  }

  // all required imports are now known.
  // resolve the duplicates.
  importingHelper.resolveImports();

  const importStrs: string[] = []
  importingHelper.modulesImports.forEach((imports_: Set<string>, module: string) => {
    const apiModule: Module | undefined = loadedAdl.modules[module]
    const jp = module === "sys.types" 
      ? "au.com.helixta.adl.runtime.sys.types"
      : getJavaPackage(apiModule)

    // const importedModuleFrom = jp === null ? module : jp;
    const modImports: string[] = [];
    for (const imp_ of Array.from(imports_)) {
      modImports.push(imp_);
    }
    // console.log("ANN", module, jp, modImports)
    modImports.forEach(m => {
      const pkg = jp !== null 
        ? jp 
        : (defpackage === "" ? module : defpackage + "." + module);
      if( servicepackage === pkg ) {
        return;
      }
      const adlType = loadedAdl.allAdlDecls[`${module}.${m}`];
      if(adlType.decl.type_.kind === "type_") {
        return;
      }
      importStrs.push(`import ${pkg}.${m};`);
    })
  });
  importStrs.sort().forEach(imp => {
    code.add(imp);
  })

  // hardcoded common imports
  code.add("");
  importingHelper.javaImports.forEach(imp => {
   code.add(`import ${imp};`);
  })
  code.add("");
  code.add(`import au.com.helixta.service.http.core.HttpUtil.HxContext;`);
  code.add(`import static au.com.onederful.servers.HandlerUtil.adl;`);
  code.add(`import static au.com.helixta.adl.custom.HelixRequestHandlers.handle;`);
  code.add("");

  // generating the service class:
  const comment = getComment(apiRequests.decl);
  if (comment) {
    code.add(`/** ${comment} */`);
  }
  code.add(`@SuppressWarnings("all")`);
  code.add(`public interface ${serviceclass} {`);
  code.add("");
  const classBody = code.inner();
  code.add("");
  code.add(`  static final public ${apiClass} REQUESTS = new ${apiClass}();`)
  code.add("}");

  // api endpoints metadata class members:
  for (const apiEntry of apiRequestsStruct.fields) {
    addCode(
      importingHelper,
      loadedAdl,
      "decl",
      classBody,
      apiEntry.typeExpr,
      apiEntry.name,
      getComment(apiEntry),
      getValue
    );
  }

  // generate constructor
  classBody.add(`public static void register(${serviceclass} impl, HxContext ctx) {`);
  // const ctorArgs = classBody.inner();
  const ctorBody = classBody.inner();
  // constructor body, initialisers for api endpoints metadata class members
  for (const apiEntry of apiRequestsStruct.fields) {
    addCode(
      importingHelper,
      loadedAdl,
      "ctor",
      ctorBody,
      apiEntry.typeExpr,
      apiEntry.name,
      getComment(apiEntry),
      getValue
    );
  }
  classBody.add("}");

  // // member functions: The main async functions used to operate the API from the app:
  // for (const apiEntry of apiRequestsStruct.fields) {
  //   addCode(
  //     importingHelper,
  //     loadedAdl,
  //     "impl",
  //     classBody,
  //     apiEntry.typeExpr,
  //     apiEntry.name,
  //     getComment(apiEntry)
  // );
  // }
  // code.add("");

  const outfile = `${javasrcroot}/${servicepackage.replace(/\./g, "/")}/${serviceclass}.java`
  await fsx.writeFile(outfile, code.write().join('\n'));
}
