import * as adlast from './adl-gen/sys/adlast';
import * as adl from "./adl-gen/runtime/adl";
import { createJsonBinding } from "./adl-gen/runtime/json";
import { collect, scopedName, scopedNamesEqual, expandTypes, expandNewType, expandTypeAlias, parseAdl, forEachDecl, getAnnotation, decodeTypeExpr, LoadedAdl } from "./util";
import * as fs from "fs";
import * as mustache from "mustache";
import { isEnum, typeExprToStringUnscoped } from './adl-gen/runtime/utils';
import { Command } from "commander";
import { snakeCase } from "change-case";

export function configureCli(program: Command) {
  program
   .command("sql-alter-format [adlFiles...]")
   .option('-I, --searchdir <path>', 'Add to adl searchpath', collect, [])
   .option('--outfile <path>', 'the resulting sql file', 'create.sql')
   .option('--outputdir <dir>', 'the directory into which the sql is written (deprecated)')
   .option('--outmetadata <path>', 'sql to insert the model metadata')
   .option('--outtemplatesql <paths>', 'generate extra sql from a mustache template', collect, [])
   .option('--postgres', 'Generate sql for postgres')
   .option('--postgres-v2', 'Generate sql for postgres (model version 2)')
   .option('--mssql', 'Generate sql for microsoft sqlserver')
   .option('--extension <ext>', 'Add to included sql extensions', collect, [])
   .option('--migration', 'Generate sql for migration')
   .description('Generate a db schema from ADL files')
   .action( (adlFiles:string[], cmd:{}) => {
     const adlSearchPath: string[] = cmd['searchdir'];
     const extensions: string[] = cmd['extension'];
     const templates: Template[] = parseTemplates(cmd['outtemplatesql'] || []);

     let outfile: string = cmd['outfile'];
     if (cmd['outputdir']) {
       outfile = cmd['outputdir'] + '/create.sql';
     }

     let outmetadata: string | null = cmd['outmetadata'] || null;

     let dbProfile = postgresDbProfile;
     if (cmd['postgresV2']) {
       dbProfile = postgres2DbProfile;
     }
     if (cmd['mssql']) {
       dbProfile = mssql2DbProfile;
     }

     generateSql({adlFiles, adlSearchPath, outfile, outmetadata, extensions, templates, dbProfile});
   });
}

export interface Params {
  adlFiles: string[];
  adlSearchPath: string[];
  outfile: string;
  outmetadata: string | null;
  extensions: string[];
  templates: Template[];
  dbProfile: DbProfile;
};

interface Template {
  template: string;
  outfile: string;
};

export interface DbTable {
  scopedDecl: adlast.ScopedDecl;
  struct: adlast.DeclType_Struct_;
  ann: {}|null;
  name: string;
};

export async function generateSql(params: Params): Promise<void> {
  // Load the ADL based upon command line arguments
  const loadedAdl = await parseAdl(params.adlFiles, params.adlSearchPath);

  // Find all of the struct declarations that have a DbTable annotation
  const dbTables: DbTable[]  = [];

  forEachDecl(loadedAdl.modules, scopedDecl => {
    if (scopedDecl.decl.type_.kind == 'struct_') {
      const struct = scopedDecl.decl.type_;
      const ann = getAnnotation(scopedDecl.decl.annotations, DB_TABLE);
      if (ann != undefined) {
        const name = getTableName(scopedDecl);
        dbTables.push({scopedDecl, struct, ann, name});
      }
    }
  });
  dbTables.sort( (t1, t2) => t1.name < t2.name ? -1 : t1.name > t2.name ? 1 : 0);
  await generateSqlSchema(params, loadedAdl, dbTables);
  if (params.outmetadata !== null) {
    await generateMetadata(params.outmetadata, params, loadedAdl, dbTables);
  }
  for(const t of params.templates) {
    await generateTemplate(t, dbTables);
  }
}

async function generateSqlSchema(params: Params, loadedAdl: LoadedAdl, dbTables: DbTable[]): Promise<void> {
  // Now generate the SQL file
  const writer = fs.createWriteStream(params.outfile);
  const moduleNames : Set<string> = new Set(dbTables.map(dbt => dbt.scopedDecl.moduleName));
  writer.write( `-- Schema auto-generated from adl modules: ${Array.from(moduleNames.keys()).join(', ')}\n` );
  writer.write( `--\n` );

  if (params.extensions.length > 0) {
    writer.write('\n');
    params.extensions.forEach( e => {
      writer.write( `create extension ${e};\n` );
    });
  }

  const constraints: string[] = [];
  let allExtraSql: string[] = [];

  writer.write('\n');
  writer.write("-- uncomment -- begin; to run sql queries inside a transaction");
  writer.write('\n');
  writer.write("-- begin;");
  writer.write('\n');

  // Output the tables
  for(const t of dbTables) {
    const withIdPrimaryKey: boolean  = t.ann && t.ann['withIdPrimaryKey'] || false;
    const withPrimaryKey: string[] = t.ann && t.ann['withPrimaryKey'] || [];
    const indexes: string[][] = t.ann && t.ann['indexes'] || [];
    const uniquenessConstraints: string[][] = t.ann && t.ann['uniquenessConstraints'] || [];
    const extraSql: string[] = t.ann && t.ann['extraSql'] || [];

    const lines: {code:string, comment?:string}[] = [];
    const pkLines: string[] = [];

    if (withIdPrimaryKey) {
      lines.push({code: `id ${params.dbProfile.idColumnType} not null`});
      pkLines.push(`alter table ${quoteReservedName(t.name)} add primary key(id)`);
    } else if (withPrimaryKey.length > 0) {
      const cols = withPrimaryKey.map(findColName);
      pkLines.push(`alter table ${quoteReservedName(t.name)} add primary key(${cols.join(',')})`);
    }
    for(const f of t.struct.value.fields) {
      const columnName = getColumnName(f);
      const columnType = getColumnType(loadedAdl.resolver, f, params.dbProfile);
      lines.push({
        code: `${columnName} ${columnType.sqltype}`,
        comment: typeExprToStringUnscoped(f.typeExpr),
      });
      if (columnType.fkey) {
        constraints.push(`alter table ${quoteReservedName(t.name)} add constraint ${t.name}_${columnName}_fk foreign key (${columnName}) references ${quoteReservedName(columnType.fkey.table)}(${columnType.fkey.column});`);
      }
    }

    function findColName(s: string):string {
      for(const f of t.struct.value.fields) {
        if (f.name == s) {
          return getColumnName(f);
        }
      }
      return s;
    }

    for(let i = 0; i < indexes.length; i++) {
      const cols = indexes[i].map(findColName);
      constraints.push(`create index if not exists ${t.name}_${i+1}_idx on ${quoteReservedName(t.name)}(${cols.join(', ')});`);
    }
    for(let i = 0; i < uniquenessConstraints.length; i++) {
      const cols = uniquenessConstraints[i].map(findColName);
      constraints.push(`alter table ${quoteReservedName(t.name)} add constraint ${t.name}_${i+1}_con unique (${cols.join(', ')});`);
    }
    if (withIdPrimaryKey) {
      lines.push({code:`alter table ${quoteReservedName(t.name)} add primary key(id);`});
    } else if (withPrimaryKey.length > 0) {
      const cols = withPrimaryKey.map(findColName);
      lines.push({code:`alter table ${quoteReservedName(t.name)} add primary key(${cols.join(',')});`});
    }

    writer.write('\n');
    writer.write("-- create table");
    writer.write('\n');
    writer.write( `create table if not exists ${quoteReservedName(t.name)} ();`);
    writer.write('\n');
    for(let i = 0; i < lines.length; i++) {
      let line = lines[i].code;
      if (i < lines.length-1) {
        line = `alter table ${quoteReservedName(t.name)} add column if not exists ${line};`;
        writer.write('  ' + line + '\n');
      }
    }

    for(let i = 0; i < pkLines.length; i++) {
      let line = pkLines[i];
      line = `${line};`;
      writer.write('  ' + line + '\n');
    }
    allExtraSql = allExtraSql.concat(extraSql);
  }

  writer.write('\n');
  writer.write("-- uncomment -- commit; to run sql queries inside a transation");
  writer.write('\n');
  writer.write("-- commit;");
  writer.write('\n');

  if(constraints.length > 0) {
    writer.write('\n');
  }

  for(const constraint of constraints) {
    writer.write(constraint + '\n');
  }

  if(allExtraSql.length > 0) {
    writer.write('\n');
  }

  // And any sql
  for(const sql of allExtraSql) {
    writer.write(sql + '\n');
  }
}

/**
 *  Returns the SQL name for the table
 */
function getTableName(scopedDecl: adlast.ScopedDecl): string {
  const ann = getAnnotation(scopedDecl.decl.annotations, DB_TABLE);
  if (ann && typeof ann['tableName'] == 'string') {
    return ann['tableName'];
  }
  return snakeCase(scopedDecl.decl.name);
}

/**
 *  Returns the singular primary key for the table
 */
function getPrimaryKey(scopedDecl: adlast.ScopedDecl): string {
  const ann = getAnnotation(scopedDecl.decl.annotations, DB_TABLE);
  if (ann && ann['withIdPrimaryKey']) {
    return "id";
  }
  if (ann && ann["withPrimaryKey"] && ann["withPrimaryKey"].length == 1) {
    return ann["withPrimaryKey"][0];
  }
  throw new Error(`No singular primary key for ${scopedDecl.decl.name}`);
  return "??";
}

/**
 * Returns the SQL name for a column corresponding to a field
 */
function getColumnName(field: adlast.Field): string {
  const ann = getAnnotation(field.annotations, DB_COLUMN_NAME);
  if (typeof ann === "string") {
    return ann;
  }
  return snakeCase(field.name);
}

const RESERVED_NAMES : {[name:string] : boolean} = {};
[
  // TODO: Add other names here
  "user",
].forEach( n => {RESERVED_NAMES[n] = true;});

function quoteReservedName(s:string) {
  if (RESERVED_NAMES[s]) {
    return `"${s}"`;
  } else {
    return s;
  }
}


interface ColumnType {
  sqltype: string;
  fkey? : {
    table: string,
    column: string
  };
};


function getColumnType(resolver: adl.DeclResolver, field: adlast.Field,  dbProfile: DbProfile): ColumnType {
  const ann = getAnnotation(field.annotations, DB_COLUMN_TYPE);
  const annctype: string | undefined = typeof ann === "string" ? ann : undefined;
  
  const typeExpr = field.typeExpr;

  // For Maybe<T> and Nullable<T> the sql column will allow nulls
  const dtype = decodeTypeExpr(typeExpr);
  if(dtype.kind == 'Nullable' ||
     dtype.kind == 'Reference' && scopedNamesEqual(dtype.refScopedName, MAYBE)
    ) {
    return {
      sqltype: annctype || getColumnType1(resolver, typeExpr.parameters[0], dbProfile),
      fkey: getForeignKeyRef(resolver, typeExpr.parameters[0])
    };
  }

  // For all other types, the column will not allow nulls
  return {
    sqltype: (annctype || getColumnType1(resolver, typeExpr, dbProfile)) + " not null",
    fkey: getForeignKeyRef(resolver, typeExpr)
  };
}

function getColumnType1(resolver: adl.DeclResolver, typeExpr: adlast.TypeExpr, dbProfile: DbProfile): string {
  const dtype = decodeTypeExpr(typeExpr);
  switch(dtype.kind) {
    case "Reference":
      const sdecl = resolver(dtype.refScopedName);

      const ann = getAnnotation(sdecl.decl.annotations, DB_COLUMN_TYPE);
      if (typeof(ann) === 'string') {
        return ann;
      }

      if (scopedNamesEqual(dtype.refScopedName, INSTANT)) {
        return "timestamp";
      } else if (scopedNamesEqual(dtype.refScopedName, LOCAL_DATE)) {
        return "date";
      } else if (scopedNamesEqual(dtype.refScopedName, LOCAL_TIME)) {
        return "time";
      } else if (scopedNamesEqual(dtype.refScopedName, LOCAL_DATETIME)) {
        return "timestamp";
      } else if (sdecl.decl.type_.kind == 'union_' && isEnum(sdecl.decl.type_.value)) {
        return dbProfile.enumColumnType;
      }
      // If we have a reference to a newtype or type alias, resolve
      // to the underlying type
      let texpr2 = null;
      texpr2 = texpr2 || expandTypeAlias(resolver, typeExpr);
      texpr2 = texpr2 || expandNewType(resolver, typeExpr);
      if (texpr2) {
        return getColumnType1(resolver, texpr2, dbProfile);
      }
    default:
      return dbProfile.primColumnType(dtype.kind);
  }
}

function getForeignKeyRef(resolver: adl.DeclResolver, typeExpr0: adlast.TypeExpr): {table:string, column:string} | undefined {
  const typeExpr = expandTypes(resolver, typeExpr0, {expandTypeAliases:true});
  const dtype = decodeTypeExpr(typeExpr);
  if (dtype.kind == 'Reference' && scopedNamesEqual(dtype.refScopedName, DB_KEY)) {
    const param0 = dtype.parameters[0];
    if (param0.kind == 'Reference') {
      const decl = resolver(param0.refScopedName);
      return {table:getTableName(decl), column:getPrimaryKey(decl)};
    }
  }
  return undefined;
}


// Contains customizations for the db mapping
interface DbProfile {
  idColumnType : string;
  enumColumnType: string;
  primColumnType(ptype: string): string;
};

const postgresDbProfile: DbProfile = {
  idColumnType: "text",
  enumColumnType: "text",
  primColumnType(ptype: string): string {
    switch (ptype) {
    case "String": return "text";
    case "Bool": return "boolean";
    case "Json": return "json";
    case "Int8" : return "smallint";
    case "Int16" : return "smallint";
    case "Int32" : return "integer";
    case "Int64" : return "bigint";
    case "Word8" : return "smallint";
    case "Word16" : return "smallint";
    case "Word32" : return "integer";
    case "Word64" : return "bigint";
    case "Float": return "real";
    case "Double": return "double precision";
    }
    return "json";
  }
};

const postgres2DbProfile: DbProfile = {
  idColumnType: "text",
  enumColumnType: "text",
  primColumnType(ptype: string): string {
    switch (ptype) {
    case "String": return "text";
    case "Bool": return "boolean";
    case "Json": return "jsonb";
    case "Int8" : return "smallint";
    case "Int16" : return "smallint";
    case "Int32" : return "integer";
    case "Int64" : return "bigint";
    case "Word8" : return "smallint";
    case "Word16" : return "smallint";
    case "Word32" : return "integer";
    case "Word64" : return "bigint";
    case "Float": return "real";
    case "Double": return "double precision";
    }
    return "jsonb";
  }
};


const mssql2DbProfile: DbProfile = {
  idColumnType: "nvarchar(64)",
  enumColumnType: "nvarchar(64)",
  primColumnType(ptype: string): string {
    switch (ptype) {
    case "String": return "nvarchar(max)";
    case "Int8" : return "smallint";
    case "Int16" : return "smallint";
    case "Int32" : return "int";
    case "Int64" : return "bigint"
    case "Word8" : return "smallint";
    case "Word16" : return "smallint";
    case "Word32" : return "int";
    case "Word64" : return "bigint";
    case "Float": return "float(24)";
    case "Double": return "float(53)";
    case "Bool": return "bit";
    }
    return "nvarchar(max)";
  }
};

export async function generateMetadata(outmetadata: string, params: Params, loadedAdl: LoadedAdl, dbTables0: DbTable[]): Promise<void> {
  const jbDecl = createJsonBinding(loadedAdl.resolver,adlast.texprDecl());
  const writer = fs.createWriteStream(outmetadata);

  // Exclude metadata for the metadata tables
  const dbTables = dbTables0.filter(dbt => dbt.name != 'meta_table' && dbt.name !== 'meta_adl_decl');

  writer.write('delete from meta_table;\n');
  for(const dbTable of dbTables) {
    const docAnn = getAnnotation(dbTable.scopedDecl.decl.annotations, DOC);
    const description = typeof docAnn === "string" ? docAnn : "";
    writer.write( `insert into meta_table(name,description,decl_module_name, decl_name) values (${dbstr(dbTable.name)},${dbstr(description)},${dbstr(dbTable.scopedDecl.moduleName)},${dbstr(dbTable.scopedDecl.decl.name)});\n` );
  }

  writer.write('\n');

  writer.write('delete from meta_adl_decl;\n');
  insertDecls(loadedAdl.resolver, writer, dbTables.map(dbt => dbt.scopedDecl));
  writer.end();
}

function insertDecls(resolver: adl.DeclResolver, writer: fs.WriteStream, sdecls:adlast.ScopedDecl[]) {
  const done: {[name: string]:boolean}= {};
  const jbDecl = createJsonBinding(resolver,adlast.texprDecl());

  function insertDecl(sdecl:adlast.ScopedDecl) {
     const name = sdecl.moduleName + '.' + sdecl.decl.name;
     if (done[name] === undefined ) {
       const jsdecl = JSON.stringify(jbDecl.toJson(sdecl.decl));
       writer.write(`insert into meta_adl_decl(module_name,name,decl) values (${dbstr(sdecl.moduleName)},${dbstr(sdecl.decl.name)}, ${dbstr(jsdecl)});\n`);
       done[name] = true;
       switch(sdecl.decl.type_.kind) {
       case 'struct_':
       case 'union_':
         for(const field of sdecl.decl.type_.value.fields) {
           insertTypeExpr(field.typeExpr);
         }
       break;
       case 'newtype_':
       case 'type_':
         insertTypeExpr(sdecl.decl.type_.value.typeExpr);
       break;
       }
     }
  }

  function insertTypeExpr(texpr:adlast.TypeExpr) {
    switch(texpr.typeRef.kind) {
      case 'reference':
        const sname = texpr.typeRef.value;
        const decl = resolver(sname);
        insertDecl(decl);
        break;
      case 'primitive':
      case 'typeParam':
        break;
    }
    texpr.parameters.forEach(te => insertTypeExpr(te));
  }

  sdecls.forEach( insertDecl );
}


function generateTemplate(template: Template, dbtables: DbTable[]) {
  const templateStr : string = fs.readFileSync(template.template, {encoding:'utf-8'});
  const view : {} = {
    tables: dbtables.map( dbtable => {
      const attributes: {[key : string]: {}|null} = {};
      attributes['tablename'] = dbtable.name;
      for(const annotation of dbtable.scopedDecl.decl.annotations) {
        attributes[annotation.v1.name] = annotation.v2;
      }
      return attributes;
    }),
  };
  const outStr : string = mustache.render(templateStr, view);
  fs.writeFileSync(template.outfile, outStr);
}

function dbstr(s: string) {
  return "'" + s.replace(/'/g, "''") + "'";
}

function parseTemplates(ss: string[]): Template[] {
  return ss.map( s => {
    const paths = s.split(":");
    if (paths.length != 2) {
      throw new Error("outtemplatesql parameter must be a pair of paths, separated by :");
    }
    return {template: paths[0], outfile: paths[1]};
  });
}



const DOC = scopedName("sys.annotations", "Doc");
const MAYBE = scopedName("sys.types", "Maybe");
const DB_TABLE = scopedName("common.db", "DbTable");
const DB_COLUMN_NAME = scopedName("common.db", "DbColumnName")
const DB_COLUMN_TYPE = scopedName("common.db", "DbColumnType")
const DB_KEY = scopedName("common.db", "DbKey")
const INSTANT = scopedName("common", "Instant");
const LOCAL_DATE = scopedName("common", "LocalDate");
const LOCAL_TIME = scopedName("common", "LocalTime");
const LOCAL_DATETIME = scopedName("common", "LocalDateTime");

