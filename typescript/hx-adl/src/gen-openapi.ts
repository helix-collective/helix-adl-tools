import * as fs from "fs";
import * as mkdirp from "mkdirp";
import * as path from 'path';
import * as OAPI from "./openapi/openapi-utils";

import { Command } from "commander";
import { AdlStore, RequestDecl } from "./openapi/adl-utils";
import { collect, parseAdl } from "./util";

export function configureCli(program: Command) {
  program
   .command("openapi [adlFiles...]")
   .option('-I, --searchdir <path>', 'Add to adl searchpath', collect, [])
   .option('--outfile <path>', 'the resulting openapi file', 'openapi.yaml')
   .description('Generate an openapi specfication from ADL request definitions')
   .action( (adlFiles:string[], cmd:{}) => {
     const adlSearchPath: string[] = cmd['searchdir'];
     let outfile: string = cmd['outfile'];
     if (cmd['outputdir']) {
       outfile = cmd['outputdir'] + '/openapi.yaml';
     }
     generateOpenApiSpec({adlFiles, adlSearchPath, outfile});
   });
}

export interface Params {
  adlFiles: string[];
  adlSearchPath: string[];
  outfile: string;
};

export async function generateOpenApiSpec(params: Params): Promise<void> {
  // Load the ADL based upon command line arguments
  const loadedAdl = await parseAdl(params.adlFiles, params.adlSearchPath);

  const adlStore: AdlStore = new AdlStore(
    loadedAdl.allAdlDecls,
    [{title:'all', requests: _r => true}],
    []
  );
  const requestDecls: RequestDecl[] = [];
  adlStore.requestsByGrouping().forEach((rg) => {
    rg.requests.forEach((r) => requestDecls.push(r));
  });
  const schema = OAPI.schemaFromRequests(requestDecls, adlStore);
  const text : string = OAPI.yamlFromJsonSchema(schema);
  mkdirp.sync(path.dirname(params.outfile));
  const writer = fs.createWriteStream(params.outfile);
  writer.write(text);
  writer.write('\n');
}
