import program from "commander";
import * as sqlschema from "./gen-sqlschema";
import * as javatables from "./gen-javatables";
import * as javahttpreqs from "./gen-javahttpreqs";
import * as typescripthttpreqs from "./gen-typescripthttpreqs";
import * as openapi from "./gen-openapi";

program.name("hx-adl");
program.version("1.0.0");
sqlschema.configureCli(program);
javatables.configureCli(program);
javahttpreqs.configureCli(program);
typescripthttpreqs.configureCli(program);
openapi.configureCli(program);

// error on unknown commands
program.on('command:*', function () {
  console.error('Invalid command: %s\nSee --help for a list of available commands.', program.args.join(' '));
  process.exit(1);
});


program.parse(process.argv);

