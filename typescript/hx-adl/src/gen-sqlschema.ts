import * as adlast from './adl-gen/sys/adlast';
import * as adl from "./adl-gen/runtime/adl";
import { collect, scopedName, scopedNamesEqual, expandNewType, expandTypeAlias, parseAdl, forEachDecl, getAnnotation, decodeTypeExpr, DecodedTypeExpr } from "./util";
import * as fs from "fs";
import { isEnum, typeExprToStringUnscoped } from './adl-gen/runtime/utils';
import { Command } from "commander";
import { snakeCase } from "change-case";
import { execHxAdlHs } from "./util";

export function configureCli(program: Command) {
  program
   .command("sql [adlFiles...]")
   .option('-I, --searchdir <path>', 'Add to adl searchpath', collect, [])
   .option('--outfile <path>', 'the resulting sql file', 'create.sql')
   .option('--outputdir <dir>', 'the directory into which the sql is written (deprecated)')
   .option('--postgres', 'Generate sql for postgres')
   .option('--postgres-v2', 'Generate sql for postgres (model version 2)')
   .option('--mssql', 'Generate sql for microsoft sqlserver')
   .description('Generate a db schema from ADL files')
   .action( (_adlFiles:string[], cmd:{}) => {
// Continue to use the haskell sql generator until the output of the code below
// is made consistent
//
//     const adlSearchPath: string[] = cmd['searchdir'];
//     let outfile: string = cmd['outfile'];
//     if (cmd['outputdir']) {
//       outfile = cmd['outputdir'] + '/create.sql';
//     }
//  generateSqlSchema({adlFiles, adlSearchPath, outfile});
     execHxAdlHs(cmd['parent'].rawArgs.slice(2));
   });
}

export interface Params {
  adlFiles: string[];
  adlSearchPath: string[];
  outfile: string;
};

type DbTableBasic = {
  scopedDecl: adlast.ScopedDecl;
  struct: adlast.DeclType_Struct_;
  ann:{}|null;
  name:string;
};

type DbField = {
  fieldOrderNumber: number;
  tableName: string;
  columnName: string;
  columnType: ColumnType;
  references?: DbField;         // foreign key reference graph represented as js object reference graph
  typeExpr: adlast.TypeExpr;
};

type DbTableWithIdPrimaryKey = {
  kind: "withIdPrimaryKey"
};
type DbTableWithPrimaryKey = {
  kind: "withPrimaryKey",
  withPrimaryKey: string[]    // single column name or composite key of column names
};
type DbTableNoPrimaryKey = {
  kind: "withNoPrimaryKey";   // some tables may have no PK.
};

type DbTablePrimaryKey = DbTableWithIdPrimaryKey | DbTableWithPrimaryKey | DbTableNoPrimaryKey;

type DbTable = DbTableBasic & {
  primaryKey: DbTablePrimaryKey;
  indexes: string[][];
  uniquenessConstraints: string[][];
  extraSql: string[];

  fieldsMap: {
    [columnName: string] : DbField
  }
};

function makeDbTable(resolver: adl.DeclResolver, t: DbTableBasic) : DbTable {

  const noPrimaryKey : DbTableNoPrimaryKey = {
    kind: "withNoPrimaryKey"
  };
  const withIdPrimaryKey : DbTableWithIdPrimaryKey|undefined = t.ann && t.ann['withIdPrimaryKey'] ? {
    kind: "withIdPrimaryKey"
  } : undefined;
  const withPrimaryKey : DbTableWithPrimaryKey|undefined = t.ann && t.ann['withPrimaryKey'] ? {
    kind: "withPrimaryKey",
    withPrimaryKey: t.ann && t.ann['withPrimaryKey']
  } : undefined;
  const primaryKey : DbTablePrimaryKey = withPrimaryKey || withIdPrimaryKey || noPrimaryKey;

  const indexes: string[][] = t.ann && t.ann['indexes'] || [];
  const uniquenessConstraints: string[][] = t.ann && t.ann['uniquenessConstraints'] || [];
  const extraSql: string[] = t.ann && t.ann['extraSql'] || [];

  // TODO: ensure withPrimaryKey columns exist

  const fieldsMap : {
    [columnName: string] : DbField
  } = {};

  let fieldOrderNumber = 0;
  for(const f of t.struct.value.fields) {
    const columnName = getColumnName(f);
    const columnType = getColumnType(resolver, f.typeExpr);

    const dbf: DbField = {
      fieldOrderNumber,
      columnName,
      columnType,
      tableName: t.name,
      typeExpr: f.typeExpr,
    };

    fieldsMap[columnName] = dbf;
    fieldOrderNumber += 1
  }

  return {
    ...t,
    primaryKey,
    indexes,
    uniquenessConstraints,
    extraSql,
    fieldsMap
  };
}

/// Return the values in the map sorted by a key
function mapValuesSortedBy<Obj, K extends keyof Obj>(map : { [key:string] : Obj}, k : K) : Obj[] {
  const vals = Object.values(map);
  vals.sort((a, b) => a[k] < b[k] ? -1 : a[k] > b[k] ? 1 : 0);
  return vals;
}

export async function generateSqlSchema(params: Params): Promise<void> {
  // Load the ADL based upon command line arguments
  const loadedAdl = await parseAdl(params.adlFiles, params.adlSearchPath);

  // Find all of the struct declarations that have a DbTable annotation
  const dbTablesMap : {
    [tablename:string] : DbTable
  } = {};

  forEachDecl(loadedAdl.modules, scopedDecl => {
    if (scopedDecl.decl.type_.kind == 'struct_') {
      const struct = scopedDecl.decl.type_;
      const ann = getAnnotation(scopedDecl.decl.annotations, DB_TABLE);
      if (ann != undefined) {
        const name = getTableName(scopedDecl);
        dbTablesMap[name] = makeDbTable(loadedAdl.resolver, {scopedDecl, struct, ann, name});
      }
    }
  });

  // link dbTables:
  console.log('Linking dbTables');
  for(const dbTable of Object.values(dbTablesMap)) {
    console.log('Linking dbTables', dbTable.name);
    for(const field of Object.values(dbTable.fieldsMap)) {
      console.log('Linking dbTables', dbTable.name, field.columnName);
      if(field.columnType.fkey) {
        const referredTableName = field.columnType.fkey.table;
        const referredTable : DbTable|undefined = dbTablesMap[referredTableName];

        if(referredTable === undefined) {
          throw new Error('Foreign key referene to non-existent table found:' + referredTableName)
        }

        let referredTablePkColumn = 'id';
        if(referredTable.primaryKey.kind === 'withNoPrimaryKey') {
          throw new Error("Unable to map DbKey to a table with no primary key");
        }
        if(referredTable.primaryKey.kind === 'withPrimaryKey') {
          if(referredTable.primaryKey.withPrimaryKey.length === 1) {
            referredTablePkColumn = referredTable.primaryKey.withPrimaryKey[0];
          } else {
            throw new Error("Unable to map one DbKey to a table with a composite primary key");
          }
        }

        field.columnType.fkey.column = referredTablePkColumn;
      }
    }
  }

  const dbTables: DbTable[] = mapValuesSortedBy(dbTablesMap,"name");

  // Now generate the SQL file
  const writer = fs.createWriteStream(params.outfile);
  const moduleNames : Set<string> = new Set(dbTables.map(dbt => dbt.scopedDecl.moduleName));
  writer.write( `-- Schema autogenerated from adl modules: ${Array.from(moduleNames.keys()).join(', ')}\n` );
  writer.write( `-- \n` );
  writer.write( `-- column comments show original ADL types\n` );


  const constraints: string[] = [];
  let allExtraSql: string[] = [];

  // Output the tables
  for(const t of dbTables) {
    const lines: {code:string, comment?:string}[] = [];
    if (t.primaryKey.kind === 'withIdPrimaryKey') {
      lines.push({code: 'id text not null'});
    }

    for(const f of mapValuesSortedBy(t.fieldsMap,"fieldOrderNumber")) {
      const columnName = f.columnName;
      const columnType = f.columnType;
      lines.push({
        code: `${columnName} ${columnType.sqltype}`,
        comment: typeExprToStringUnscoped(f.typeExpr),
      });
      if (columnType.fkey) {
        constraints.push(`alter table ${t.name} add constraint ${t.name}_${columnName}_fk foreign key (${columnName}) references ${columnType.fkey.table}(${columnType.fkey.column});`);
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

    for(let i = 0; i < t.indexes.length; i++) {
      const cols = t.indexes[i].map(findColName);
      constraints.push(`create index ${t.name}_${i+1}_idx on ${t.name}(${cols.join(', ')});`);
    }
    for(let i = 0; i < t.uniquenessConstraints.length; i++) {
      const cols = t.uniquenessConstraints[i].map(findColName);
      constraints.push(`alter table ${t.name} add constraint ${t.name}_${i+1}_con unique (${cols.join(', ')});`);
    }
    if (t.primaryKey.kind === 'withIdPrimaryKey') {
      lines.push({code:'primary key(id)'});
    } else if (t.primaryKey.kind === 'withPrimaryKey') {
      const cols = t.primaryKey.withPrimaryKey.map(findColName);
      lines.push({code:`primary key(${cols.join(',')})`});
    } else if (t.primaryKey.kind === 'withNoPrimaryKey') {
      lines.push({code:'-- (no primary key)'});
      throw new Error("No primary key table not supported");    // it needs to consume the preceding comma
      /*
      create table look_ma_no_primary_key(
        name text not null,                  -- String        <-- superfluous comma not consumed here
        -- (no primary key)
      );
      */
    }

    writer.write('\n');
    writer.write( `create table ${t.name}(\n` );
    for(let i = 0; i < lines.length; i++) {
      let line = lines[i].code;
      if (i < lines.length-1) {
        line += ',';
      }
      if (lines[i].comment) {
        line = line.padEnd(36, ' ');
        line = line + " -- " + lines[i].comment;
      }
      writer.write('  ' + line + '\n');
    }
    writer.write( `);\n` );
    allExtraSql = allExtraSql.concat(t.extraSql);
  }

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
  for(const sql in allExtraSql) {
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
 * Returns the SQL name for a column corresponding to a field
 */
function getColumnName(field: adlast.Field): string {
  const ann = getAnnotation(field.annotations, DB_COLUMN_NAME);
  if (typeof ann === "string") {
    return ann;
  }
  return snakeCase(field.name);
}

interface ColumnType {
  sqltype: string;
  fkey? : {
    table: string,
    column?: string
  };
};


function getColumnType(resolver: adl.DeclResolver, typeExpr: adlast.TypeExpr): ColumnType {
  // For Maybe<T> and Nullable<T> the sql column will allow nulls
  const dtype = decodeTypeExpr(typeExpr);
  if(dtype.kind == 'Nullable' ||
     dtype.kind == 'Reference' && scopedNamesEqual(dtype.refScopedName, MAYBE)
    ) {
    return {
      sqltype: getColumnType1(resolver, typeExpr.parameters[0]),
      fkey: getForeignKeyRef(resolver, typeExpr.parameters[0])
    };
  }

  // For all other types, the column will not allow nulls
  return {
    sqltype: getColumnType1(resolver, typeExpr) + " not null",
    fkey: getForeignKeyRef(resolver, typeExpr)
  };
}

function getColumnType1(resolver: adl.DeclResolver, typeExpr: adlast.TypeExpr): string {
  const dtype = decodeTypeExpr(typeExpr);
  switch(dtype.kind) {
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
    case "Reference":
      const sdecl = resolver(dtype.refScopedName);

      if (scopedNamesEqual(dtype.refScopedName, INSTANT)) {
        return "timestamp";
      } else if (scopedNamesEqual(dtype.refScopedName, LOCAL_DATE)) {
        return "date";
      } else if (scopedNamesEqual(dtype.refScopedName, LOCAL_DATETIME)) {
        return "timestamp";
      } else if (sdecl.decl.type_.kind == 'union_' && isEnum(sdecl.decl.type_.value)) {
        return "text";
      }
      // If we have a reference to a newtype or type alias, resolve
      // to the underlying type
      let texpr2 = null;
      texpr2 = texpr2 || expandTypeAlias(resolver, typeExpr);
      texpr2 = texpr2 || expandNewType(resolver, typeExpr);
      if (texpr2) {
        return getColumnType1(resolver, texpr2);
      }
  }
  return "json";
}

function getForeignKeyRef(resolver: adl.DeclResolver, typeExpr: adlast.TypeExpr): {table:string} | undefined {
  const dtype = decodeTypeExpr(typeExpr);
  if (dtype.kind == 'Reference' && scopedNamesEqual(dtype.refScopedName, DB_KEY)) {
    const param0 = dtype.parameters[0];
    if (param0.kind == 'Reference') {
      return { table:getTableName(resolver(param0.refScopedName)) };
    }
  }
  return undefined;
}


const MAYBE = scopedName("sys.types", "Maybe");
const DB_TABLE = scopedName("common.db", "DbTable");
const DB_COLUMN_NAME = scopedName("common.db", "DbColumnName")
const DB_KEY = scopedName("common.db", "DbKey")
const INSTANT = scopedName("common", "Instant");
const LOCAL_DATE = scopedName("common", "LocalDate");
const LOCAL_DATETIME = scopedName("common", "LocalDateTime");

