import program from "commander";
import * as sqlalterformat  from "./gen-sql-alter-format";
import * as sqlschema from "./gen-sqlschema";
import * as javatables from "./gen-javatables";
import * as javahttpreqs from "./gen-javahttpreqs";
import * as typescripthttpreqs from "./gen-typescripthttpreqs";
import * as typescriptservices from "./ts-services/ts-services";

import * as openapi from "./gen-openapi";

program.name("hx-adl");
program.version("1.0.0");
sqlalterformat.configureCli(program);
sqlschema.configureCli(program);
javatables.configureCli(program);
javahttpreqs.configureCli(program);
typescripthttpreqs.configureCli(program);
typescriptservices.configureCli(program);
openapi.configureCli(program);

// error on unknown commands
program.on('command:*', function () {
  console.error('Invalid command: %s\nSee --help for a list of available commands.', program.args.join(' '));
  process.exit(1);
});


program.parse(process.argv);

