import { Command } from "commander";

import * as adlast from "./adl-gen/sys/adlast";
import { collect, forEachDecl, parseAdl, getAnnotation } from "./util";
import {
  getModuleName,
  RN_MODULE,
  writeNativeBridge,
  writeTypeScriptBridge,
} from "./react-native/react-native-utils";

export interface RNBridge {
  scopedDecl: adlast.ScopedDecl;
  struct: adlast.DeclType_Struct_;
  ann: {} | null;
  name: string;
}

export function configureCli(program: Command) {
  program
    .command("react-native-android [adlFiles...]")
    .description(
      "Generate matching Java and TypeScript code for react-native bridging modules on Android"
    )
    .option("-I, --searchdir <path>", "Add to adl searchpath", collect, [])
    .option("--javadir <path>", "where the resulting java files end up")
    .option("--tsdir <path>", "where the resulting typescript files end up")
    .option(
      "--package <package>",
      "The package into which the generated adl is placed",
      "adl"
    )
    .action((adlFiles: string[], cmd) => {
      const adlSearchPath: string[] = cmd["searchdir"];
      const javaOutdir: string = cmd["javadir"];
      const tsOutdir: string = cmd["tsdir"];
      const packageName: string = cmd["package"];

      const nativeBridges: RNBridge[] = [];

      (async () => {
        const loadedAdl = await parseAdl(adlFiles, adlSearchPath);

        // Collect all the native bridges that have been annotated
        forEachDecl(loadedAdl.modules, (scopedDecl) => {
          if (scopedDecl.decl.type_.kind == "struct_") {
            const struct = scopedDecl.decl.type_;
            const ann = getAnnotation(scopedDecl.decl.annotations, RN_MODULE);
            if (ann != undefined) {
              const name = getModuleName(scopedDecl);
              nativeBridges.push({ scopedDecl, struct, ann, name });
            }
          }
        });
        nativeBridges.sort((t1, t2) =>
          t1.name < t2.name ? -1 : t1.name > t2.name ? 1 : 0
        );

        // Write Java Native bridge
        nativeBridges.forEach((b) =>
          writeNativeBridge(b, {
            outdir: javaOutdir,
            nativeBridges,
            packageName,
            loadedAdl,
          })
        );

        // Write Typescript Bridges
        nativeBridges.forEach((b) =>
          writeTypeScriptBridge(b, {
            outdir: tsOutdir,
            nativeBridges,
            packageName,
            loadedAdl,
          })
        );
      })();
    });
}
