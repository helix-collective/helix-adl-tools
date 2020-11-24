import { camelCase } from "change-case";
import { Command } from "commander";
import { Annotations, Decl, Field, makeScopedDecl, ScopedDecl, TypeExpr } from "../adl-gen/runtime/sys/adlast";
import { collect, parseAdl } from "../util";
import { CodeGen } from "./code-gen";
import { ImportingHelper } from "./import-helper";
import * as fsx from 'fs-extra';

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
    .action( (adlFiles:string[], cmd:{}) => {
      const adlSearchPath : string[] = cmd["searchdir"];
      const outfile : string = cmd["outfile"];
      const apimodule : string = cmd["apimodule"];
      const apiname : string = cmd["apiname"];
      const adlgendirrel : string = cmd["adlgendirrel"];
      const serviceclass : string = cmd["serviceclass"];
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

function getComment(item: Annotable) : string|null {
  let comment: string | null = null;
  for (const anno of item.annotations) {
    if (anno.v1.name === "Doc") {
      comment = anno.v2 as string;
      comment = comment.replace(/\n/g, " ");
      comment = comment.trim();
    }
  }
  return comment;
}

async function generateTypescriptService(params: {
  adlSearchPath: string[];
  outfile: string;
  apimodule: string;
  apiname: string;
  adlFiles: string[];
  adlgendirrel: string;
  serviceclass: string
}) {
  const {adlSearchPath, outfile, apimodule, apiname, adlFiles, adlgendirrel, serviceclass} = params;

  // Load the ADL based upon command line arguments
  const loadedAdl = await parseAdl(adlFiles, adlSearchPath);

  const apistructSn = `${apimodule}.${apiname}`;

  const apiRequests : ScopedDecl|undefined = loadedAdl.allAdlDecls[apistructSn];
  if(apiRequests===undefined) {
    throw new Error(`Scoped name not found: ${apistructSn}`);
  }
  if (apiRequests.decl.type_.kind !== "struct_") {
    throw new Error("Unexpected - apiRequests is not a struct");
  }
  const apiRequestsStruct = apiRequests.decl.type_.value;

  const apiReqsTypeExpr : TypeExpr = {
    typeRef: {
      kind: 'reference',
      value: {
        moduleName: apimodule,
        name: apiname
      }
    },
    parameters: []
  }


  const importingHelper = new ImportingHelper();

  importingHelper.addType(apiReqsTypeExpr, true, true);

  // load all apiEntry referenced types into importingHelper to disambiguate imports:
  // it also recurses into all the type params of those types.
  for (const apiEntry of apiRequestsStruct.fields) {
    if (apiEntry.typeExpr.typeRef.kind !== "reference") {
      throw new Error("Unexpected - apiEntry.typeExpr.typeRef.kind !== reference");
    }

    if (apiEntry.typeExpr.typeRef.value.name === "HttpPost") {
      if (apiEntry.typeExpr.parameters.length !== 2) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 2");
      }

      const requestType = apiEntry.typeExpr.parameters[0];
      const responseType = apiEntry.typeExpr.parameters[1];

      importingHelper.addType(requestType);
      importingHelper.addType(responseType);
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGet") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }

      const responseType = apiEntry.typeExpr.parameters[0];
      importingHelper.addType(responseType);
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGetStream") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }

      const responseType = apiEntry.typeExpr.parameters[0];
      importingHelper.addType(responseType);
    }
  }

  // all required imports are now known.
  // resolve the duplicates.

  importingHelper.resolveImports();

  // get the as-referenced name of the struct that holds the runtime definition of the API:
  const apiReqAsRefd = importingHelper.asReferencedName(apiReqsTypeExpr);
  const apiReqSn = `sn${apiReqAsRefd}`;
  const apiReqMaker = `make${apiReqAsRefd}`;

  // start rendering code:
  const code = new CodeGen();
  code.add("// tslint:disable: ordered-imports");

  // typescript: import {foo as bar} from "blah"
  importingHelper.modulesImports.forEach((imports_: Set<string>, module: string) => {
    const importedModuleFrom = `${adlgendirrel}/${module.replace(/\./g, "/")}`;

    const modImports : string[] = [];
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
  if(comment) {
    code.add(`/** ${comment} */`);
  }
  code.add(`export class ${serviceclass} extends HttpServiceBase {`);
  const classBody = code.inner();
  code.add("};");

  // api endpoints metadata class members:
  // eg:/** Login a user */
  //    postLogin: PostFn<LoginReq, LoginResp>;
  for (const apiEntry of apiRequestsStruct.fields) {
    if (apiEntry.typeExpr.typeRef.kind !== "reference") {
      throw new Error("Unexpected - apiEntry.typeExpr.typeRef.kind !== reference");
    }

    if (apiEntry.typeExpr.typeRef.value.name === "HttpPost") {
      if (apiEntry.typeExpr.parameters.length !== 2) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 2");
      }
      const requestType = apiEntry.typeExpr.parameters[0];
      const responseType = apiEntry.typeExpr.parameters[1];

      const comment = getComment(apiEntry);
      if(comment) {
        classBody.add(`/** ${comment} */`);
      }
      classBody.add(
        `${camelCase("post " + apiEntry.name)}: PostFn<${importingHelper.asReferencedName(
          requestType
        )}, ${importingHelper.asReferencedName(responseType)}>;`
      );
      classBody.add("");
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGet") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }
      const responseType = apiEntry.typeExpr.parameters[0];

      const comment = getComment(apiEntry);
      if(comment) {
        classBody.add(`/** ${comment} */`);
      }
      classBody.add(`${camelCase("get " + apiEntry.name)}: GetFn<${importingHelper.asReferencedName(responseType)}>;`);
      classBody.add("");
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGetStream") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }
      const responseType = apiEntry.typeExpr.parameters[0];

      const comment = getComment(apiEntry);
      if(comment) {
        classBody.add(`/** ${comment} */`);
      }
      classBody.add(
        `${camelCase("get " + apiEntry.name)}: GetStreamFn<${importingHelper.asReferencedName(responseType)}>;`
      );
      classBody.add("");
    }
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
    .add("handleError: (error: HttpServiceError) => void")

  classBody.add(") {");

  const ctorBody = classBody.inner()

  ctorBody.add("super(http, baseUrl, resolver, authToken, handleError);");
  ctorBody.add(`const api = this.annotatedApi(${apiReqSn}, ${apiReqMaker}({}));`);

  // constructor body, initialisers for api endpoints metadata class members
  for (const apiEntry of apiRequestsStruct.fields) {
    if (apiEntry.typeExpr.typeRef.kind !== "reference") {
      throw new Error("Unexpected - apiEntry.typeExpr.typeRef.kind !== reference");
    }

    if (apiEntry.typeExpr.typeRef.value.name === "HttpPost") {
      if (apiEntry.typeExpr.parameters.length !== 2) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 2");
      }

      ctorBody.add(`this.${camelCase("post " + apiEntry.name)} = this.mkPostFn(api.${apiEntry.name});`);
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGet") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }

      ctorBody.add(`this.${camelCase("get " + apiEntry.name)} = this.mkGetFn(api.${apiEntry.name});`);
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGetStream") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }

      ctorBody.add(`this.${camelCase("get " + apiEntry.name)} = this.mkGetStreamFn(api.${apiEntry.name});`);
    }
  }
  classBody.add("}");

  // member functions: The main async functions used to operate the API from the app:
  // eg:/** Login a user */
  //    async login(req: LoginReq): Promise<LoginResp> {
  //      return this.postLogin.call(req);
  //    }
  for (const apiEntry of apiRequestsStruct.fields) {
    if (apiEntry.typeExpr.typeRef.kind !== "reference") {
      throw new Error("Unexpected - apiEntry.typeExpr.typeRef.kind !== reference");
    }

    if (apiEntry.typeExpr.typeRef.value.name === "HttpPost") {
      if (apiEntry.typeExpr.parameters.length !== 2) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 2");
      }
      const requestType = apiEntry.typeExpr.parameters[0];
      const responseType = apiEntry.typeExpr.parameters[1];

      const comment = getComment(apiEntry);
      classBody.add("");
      if (comment) {
        classBody.add(`/** ${comment} */`);
      }

      classBody.add(
        `async ${apiEntry.name}(req: ${importingHelper.asReferencedName(
          requestType
        )}): Promise<${importingHelper.asReferencedName(responseType)}> {`
      );
      classBody.add(`  return this.${camelCase("post " + apiEntry.name)}.call(req);`);
      classBody.add(`}`);
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGet") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }

      const responseType = apiEntry.typeExpr.parameters[0];
      const comment = getComment(apiEntry);
      classBody.add("");
      if (comment) {
        classBody.add(`/** ${comment} */`);
      }

      classBody.add(`async ${apiEntry.name}(): Promise<${importingHelper.asReferencedName(responseType)}> {`);
      classBody.add(`  return this.${camelCase("get " + apiEntry.name)}.call();`);
      classBody.add(`}`);
    }
    if (apiEntry.typeExpr.typeRef.value.name === "HttpGetStream") {
      if (apiEntry.typeExpr.parameters.length !== 1) {
        throw new Error("Unexpected - apiEntry.typeExpr.parameters.length != 1");
      }

      const responseType = apiEntry.typeExpr.parameters[0];
      const comment = getComment(apiEntry);
      classBody.add("");
      if (comment) {
        classBody.add(`/** ${comment} */`);
      }

      classBody.add(`async ${apiEntry.name}(): Promise<${importingHelper.asReferencedName(responseType)}[]> {`);
      classBody.add(`  return this.${camelCase("get " + apiEntry.name)}.call();`);
      classBody.add(`}`);
    }
  }

  code.add("");

  await fsx.writeFile(outfile, code.write().join('\n'));
}
