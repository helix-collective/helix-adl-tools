import * as fs from "fs";
import * as mkdirp from "mkdirp";
import * as path from 'path';
import * as OAPI from "./openapi/openapi-utils";

import { Command } from "commander";
import { collect, parseAdl, scopedNameFromString  } from "./util";
import * as adlast from "./adl-gen/runtime/sys/adlast";

export function configureCli(program: Command) {
  program
   .command("openapi <apiscopedname> [adlFiles...]")
   .option('-I, --searchdir <path>', 'Add to adl searchpath', collect, [])
   .option('--outfile <path>', 'the resulting openapi file', 'openapi.yaml')
   .description('Generate an openapi specfication from an ADL specified htttp API')
   .action( (apiscopedname, adlFiles:string[], cmd:{}) => {
     const adlSearchPath: string[] = cmd['searchdir'];
     let outfile: string = cmd['outfile'];
     if (adlFiles.length == 0) {
       throw new Error("No adl files specifed");
     }
     generateOpenApiSpec( {
       apiscopedname: scopedNameFromString(apiscopedname),
       adlFiles,
       adlSearchPath,
       outfile
     });
   });
}

export interface Params {
  apiscopedname: adlast.ScopedName;
  adlFiles: string[];
  adlSearchPath: string[];
  outfile: string;
};


export async function generateOpenApiSpec(params: Params): Promise<void> {
  // Load the ADL based upon command line arguments
  const loadedAdl = await parseAdl(params.adlFiles, params.adlSearchPath);

  const schema = OAPI.schemaFromApi(params.apiscopedname, loadedAdl);
  const text
    = params.outfile.endsWith('.json')
    ? JSON.stringify(schema, null, 2)
    : OAPI.yamlFromJsonSchema(schema);
  mkdirp.sync(path.dirname(params.outfile));
  const writer = fs.createWriteStream(params.outfile);
  writer.write(text);
  writer.write('\n');
}
