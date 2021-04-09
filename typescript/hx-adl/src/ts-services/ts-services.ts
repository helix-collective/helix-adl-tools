import { camelCase } from 'change-case';
import { Command } from 'commander';
import * as fsx from 'fs-extra';

import { Annotations, ScopedDecl, TypeExpr } from '../adl-gen/runtime/sys/adlast';
import { collect, LoadedAdl, parseAdl } from '../util';
import { CodeGen } from './code-gen';
import { ImportingHelper } from './import-helper';

export function configureCli(program: Command) {
  program
    .command("typescript-services [adlFiles...]")
    .option('-I, --searchdir <path>', 'Add to adl searchpath', collect, [])
    .option('--adlgendirrel <path>', 'relative path to adl-gen dir for ts imports in the resulting class code.')
    .option('--outfile <path>', 'the output app-service.ts file')
    .option('--apimodule <str>', 'ADL module name for the struct that has the API defs')
    .option('--apiname <str>', 'ADL struct name for the struct that has the API defs')
    .option('--serviceclass <str>', 'Class name for the service', 'AppService')

    .description('Generate a typescript API service class file.')
    .action((adlFiles: string[], cmd: {}) => {
      const adlSearchPath: string[] = cmd["searchdir"];
      const outfile: string = cmd["outfile"];
      const apimodule: string = cmd["apimodule"];
      const apiname: string = cmd["apiname"];
      const adlgendirrel: string = cmd["adlgendirrel"];
      const serviceclass: string = cmd["serviceclass"];
      generateTypescriptService({
        adlSearchPath,
        outfile,
        apimodule,
        apiname,
        adlFiles,
        adlgendirrel,
        serviceclass
      });
    });
}

interface Annotable {
  annotations: Annotations;
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

function addCode(importingHelper: ImportingHelper, loadedAdl: LoadedAdl, codeGenType: CodeGenType, codeGen: CodeGen, typeExpr: TypeExpr, name: string, comment: string | null) {
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
        importingHelper.addType(requestType);
        importingHelper.addType(responseType);
        return;
      }
      case "decl": {
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        codeGen.add(
          `${camelCase("post " + name)}: PostFn<${importingHelper.asReferencedName(
            requestType
          )}, ${importingHelper.asReferencedName(responseType)}>;`
        );
        codeGen.add("");
        return;
      }
      case "ctor": {
        codeGen.add(`this.${camelCase("post " + name)} = this.mkPostFn(api.${name});`);
        return;
      }
      case "impl": {
        codeGen.add("");
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }

        codeGen.add(
          `async ${name}(req: ${importingHelper.asReferencedName(
            requestType
          )}): Promise<${importingHelper.asReferencedName(responseType)}> {`
        );
        codeGen.add(`  return this.${camelCase("post " + name)}.call(req);`);
        codeGen.add(`}`);
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
        importingHelper.addType(responseType);
        return;
      }
      case "decl": {
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        codeGen.add(`${camelCase("get " + name)}: GetFn<${importingHelper.asReferencedName(responseType)}>;`);
        codeGen.add("");
        return;
      }
      case "ctor": {
        codeGen.add(`this.${camelCase("get " + name)} = this.mkGetFn(api.${name});`);
        return;
      }
      case "impl": {
        codeGen.add("");
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        codeGen.add(`async ${name}(): Promise<${importingHelper.asReferencedName(responseType)}> {`);
        codeGen.add(`  return this.${camelCase("get " + name)}.call();`);
        codeGen.add(`}`);
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
        importingHelper.addType(responseType);
        return;
      }
      case "decl": {
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }
        codeGen.add(
          `${camelCase("get " + name)}: GetStreamFn<${importingHelper.asReferencedName(responseType)}>;`
        );
        codeGen.add("");
        return;
      }
      case "ctor": {
        codeGen.add(`this.${camelCase("get " + name)} = this.mkGetStreamFn(api.${name});`);
        return;
      }
      case "impl": {
        codeGen.add("");
        if (comment) {
          codeGen.add(`/** ${comment} */`);
        }

        codeGen.add(`async ${name}(): Promise<${importingHelper.asReferencedName(responseType)}[]> {`);
        codeGen.add(`  return this.${camelCase("get " + name)}.call();`);
        codeGen.add(`}`);
        return;
      }
    }
  }
  const adlType = loadedAdl.allAdlDecls[`${typeExpr.typeRef.value.moduleName}.${typeExpr.typeRef.value.name}`];
  if (adlType) {
    if (adlType.decl.type_.kind === "type_" || adlType.decl.type_.kind === "newtype_") {

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
        throw new Error("ERROR: 'type' or 'newtype' with generic params not implemented in hx-adl ts-services.ts");
      }
      addCode(importingHelper, loadedAdl, codeGenType, codeGen, adlType.decl.type_.value.typeExpr, name, comment);
    }
    return;
  }
  if (codeGenType === "collect") {
    console.warn(`typescript-services: unrecognized field ${typeExpr.typeRef.value.name}`);
  }
}

async function generateTypescriptService(params: {
  adlSearchPath: string[];
  outfile: string;
  apimodule: string;
  apiname: string;
  adlFiles: string[];
  adlgendirrel: string;
  serviceclass: string;
}) {
  const { adlSearchPath, outfile, apimodule, apiname, adlFiles, adlgendirrel, serviceclass } = params;

  // Load the ADL based upon command line arguments
  const loadedAdl = await parseAdl(adlFiles, adlSearchPath);

  const apistructSn = `${apimodule}.${apiname}`;

  const apiRequests: ScopedDecl | undefined = loadedAdl.allAdlDecls[apistructSn];
  if (apiRequests === undefined) {
    throw new Error(`Scoped name not found: ${apistructSn}`);
  }
  if (apiRequests.decl.type_.kind !== "struct_") {
    throw new Error("Unexpected - apiRequests is not a struct");
  }
  const apiRequestsStruct = apiRequests.decl.type_.value;

  const apiReqsTypeExpr: TypeExpr = {
    typeRef: {
      kind: 'reference',
      value: {
        moduleName: apimodule,
        name: apiname
      }
    },
    parameters: []
  };

  const importingHelper = new ImportingHelper();

  importingHelper.addType(apiReqsTypeExpr, true, true);

  // start rendering code:
  const code = new CodeGen();
  code.add("// tslint:disable: ordered-imports");

  // load all apiEntry referenced types into importingHelper to disambiguate imports:
  // it also recurses into all the type params of those types.
  for (const apiEntry of apiRequestsStruct.fields) {
    addCode(importingHelper, loadedAdl, "collect", code, apiEntry.typeExpr, apiEntry.name, getComment(apiEntry));
  }

  // all required imports are now known.
  // resolve the duplicates.

  importingHelper.resolveImports();

  // get the as-referenced name of the struct that holds the runtime definition of the API:
  const apiReqAsRefd = importingHelper.asReferencedName(apiReqsTypeExpr);
  const apiReqSn = `sn${apiReqAsRefd}`;
  const apiReqMaker = `make${apiReqAsRefd}`;

  // typescript: import {foo as bar} from "blah"
  importingHelper.modulesImports.forEach((imports_: Set<string>, module: string) => {
    const importedModuleFrom = `${adlgendirrel}/${module.replace(/\./g, "/")}`;

    const modImports: string[] = [];
    for (const imp_ of Array.from(imports_)) {
      modImports.push(imp_);
    }

    code.add(`import { ${modImports.join(', ')} } from "${importedModuleFrom}";`);
  });

  // hardcoded common imports
  code.add('import { HttpServiceBase } from "@adltools/service/http-service-base";');
  code.add('import { HttpServiceError } from "@adltools/service/http-service-error";');
  code.add('import { GetFn, PostFn } from "@adltools/service/types";');
  code.add('import { HttpFetch } from "@hx/hx/service/http";');
  code.add("");
  code.add(`import { DeclResolver } from "${adlgendirrel}/runtime/adl";`);
  code.add("");

  // generating the service class:
  const comment = getComment(apiRequests.decl);
  if (comment) {
    code.add(`/** ${comment} */`);
  }
  code.add(`export class ${serviceclass} extends HttpServiceBase {`);
  const classBody = code.inner();
  code.add("};");

  // api endpoints metadata class members:
  // eg:/** Login a user */
  //    postLogin: PostFn<LoginReq, LoginResp>;
  for (const apiEntry of apiRequestsStruct.fields) {
    addCode(importingHelper, loadedAdl, "decl", classBody, apiEntry.typeExpr, apiEntry.name, getComment(apiEntry));
  }

  // generate constructor
  classBody.add("constructor(");
  const ctorArgs = classBody.inner();
  ctorArgs
    .add("/** Fetcher over HTTP */")
    .add("http: HttpFetch,")
    .add("/** Base URL of the API endpoints */")
    .add("baseUrl: string,")
    .add("/** Resolver for ADL types */")
    .add("resolver: DeclResolver,")
    .add("/** The authentication token (if any) */")
    .add("authToken: string | undefined,")
    .add("/** Error handler to allow for cross cutting concerns, e.g. authorization errors */")
    .add("handleError: (error: HttpServiceError) => void");

  classBody.add(") {");

  const ctorBody = classBody.inner();

  ctorBody.add("super(http, baseUrl, resolver, authToken, handleError);");
  ctorBody.add(`const api = this.annotatedApi(${apiReqSn}, ${apiReqMaker}({}));`);

  // constructor body, initialisers for api endpoints metadata class members
  for (const apiEntry of apiRequestsStruct.fields) {
    addCode(importingHelper, loadedAdl, "ctor", ctorBody, apiEntry.typeExpr, apiEntry.name, getComment(apiEntry));
  }
  classBody.add("}");

  // member functions: The main async functions used to operate the API from the app:
  // eg:/** Login a user */
  //    async login(req: LoginReq): Promise<LoginResp> {
  //      return this.postLogin.call(req);
  //    }
  for (const apiEntry of apiRequestsStruct.fields) {
    addCode(importingHelper, loadedAdl, "impl", classBody, apiEntry.typeExpr, apiEntry.name, getComment(apiEntry));
  }
  code.add("");

  await fsx.writeFile(outfile, code.write().join('\n'));
}
