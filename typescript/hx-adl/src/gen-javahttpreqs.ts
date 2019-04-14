import { collect, execHxAdlHs } from "./util";
import { Command } from "commander";

export function configureCli(program: Command) {
  program
   .command("java-http-reqs [adlFiles...]")
   .option('-I, --searchdir <path>', 'Add to adl searchpath')
   .option('-O, --outputdir <dir>', 'Set the directory where generated code is written')
   .option('--merge-adlext <ext>', 'Add the specified adl file extension to be merged on loading')
   .option('--verbose', 'Print extra diagnostic information, especially about files being read/written')
   .option('--no-overwrite', 'Don\'t update files that haven\'t changed')
   .option('--rtpackage <package>', 'The  package where the ADL runtime can be found')
   .option('--package <package>', 'The  package into which the generated ADL code will be placed')
   .description('Generate java http request definitions')
   .action( (_adlFiles:string[], cmd) => {
     // Defer to the existing haskell implementation
     execHxAdlHs(cmd.parent.rawArgs.slice(2));
   });
}
