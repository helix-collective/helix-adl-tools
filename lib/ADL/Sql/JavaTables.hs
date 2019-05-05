{-# LANGUAGE OverloadedStrings #-}

module ADL.Sql.JavaTables(
    generateJavaTables
  , javaTableOptions
  , defaultJavaTableFlags
  , JavaTableFlags(..)
  ) where

import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified ADL.Compiler.AST as AST
import qualified ADL.Compiler.Backends.Java.Internal as J
import qualified ADL.Compiler.Backends.Java.Json as J
import qualified ADL.Compiler.Backends.Java as J
import qualified ADL.Sql.SchemaUtils as SC
import qualified ADL.Sql.Schema as SC

import ADL.Compiler.EIO
import ADL.Compiler.Primitive
import ADL.Compiler.Processing(AdlFlags(..),ResolvedType, RModule,RDecl,defaultAdlFlags,loadAndCheckModule1,removeModuleTypedefs, expandModuleTypedefs, associateCustomTypes, refEnumeration, refNewtype, ResolvedTypeT(..))
import ADL.Compiler.Utils(FileWriter,writeOutputFile)
import ADL.Compiler.Flags(Flags(..),parseArguments,standardOptions, addToMergeFileExtensions)
import ADL.Utils.IndentedCode
import ADL.Utils.Format(template,formatText)
import Cases(snakify)
import Control.Monad(when)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict
import Data.Char(toUpper, isUpper)
import Data.Foldable(for_)
import Data.Traversable(for)
import Data.List(intersperse,find)
import Data.Monoid
import Data.Maybe(mapMaybe)
import System.Directory(createDirectoryIfMissing)
import System.FilePath(takeDirectory,(</>))
import System.Console.GetOpt(OptDescr(..), ArgDescr(..))

data JavaTableFlags = JavaTableFlags {
  jt_rtpackage :: T.Text,
  jt_package :: T.Text,
  jt_crudfns :: Bool
}

defaultJavaTableFlags = JavaTableFlags "adl.runtime" "adl" False

javaTableOptions =
  [ Option "" ["rtpackage"]
      (ReqArg (\s f -> f{f_backend=(f_backend f){jt_rtpackage=T.pack s}}) "PACKAGE")
      "The  package where the ADL runtime can be found"
  , Option "" ["package"]
      (ReqArg (\s f -> f{f_backend=(f_backend f){jt_package=T.pack s}}) "PACKAGE")
      "The  package into which the generated ADL code will be placed"
  , Option "" ["crudfns"]
      (NoArg (\f -> f{f_backend=(f_backend f){jt_crudfns=True}}))
      "Generate CRUD helper functions"
  ]

type DBTable = (J.CDecl,AST.Struct J.CResolvedType,SC.Table,JS.Value)

-- | CLI sub command to read arguments and a list ADL files
-- and generate java code mappinging between ADL objects
-- and database tables
generateJavaTables :: [String] -> EIO T.Text ()
generateJavaTables args = do
  let header = "Usage: generate.hs java-tables ...args..."
      options =  standardOptions <> javaTableOptions
  (flags0,paths) <- parseArguments header defaultAdlFlags defaultJavaTableFlags options args
  let fileWriter = writeOutputFile (f_output flags)
      flags = addToMergeFileExtensions "adl-java" flags0
      cgp = J.defaultCodeGenProfile{J.cgp_runtimePackage=(J.javaPackage (jt_rtpackage (f_backend flags)))}
  for_ paths $ \path -> do
    (mod0,moddeps) <- loadAndCheckModule1 (f_adl flags) path
    let javaPackageFn = J.mkJavaPackageFn cgp (mod0:moddeps) (J.javaPackage (jt_package (f_backend flags)))
        schema = SC.schemaFromAdl SC.postgresDbProfile mod0
        mod = ( associateCustomTypes J.getCustomType (AST.m_name mod0)
              . removeModuleTypedefs
              . expandModuleTypedefs
              ) mod0
    liftIO $ writeModuleJavaTables fileWriter (f_backend flags) cgp javaPackageFn schema mod

-- | Generate the java table mapping code for a resolved ADL module
writeModuleJavaTables :: FileWriter -> JavaTableFlags -> J.CodeGenProfile -> J.JavaPackageFn -> SC.Schema -> J.CModule -> IO ()
writeModuleJavaTables writeFile jtflags cgp javaPackageFn schema rmod = do
  let tableDecls = mapMaybe (matchDBTable schema) (M.elems (AST.m_decls rmod))
  for_ tableDecls $ \(decl,struct,table,annotation) -> do
    let filePath = J.javaClassFilePath (J.javaClass (javaPackageFn (AST.m_name rmod)) (tableClassName decl))
        classfile = generateJavaModel jtflags cgp javaPackageFn rmod (decl,struct,table,annotation)
        text = (T.intercalate "\n" (codeText Nothing (J.classFileCode classfile)))
    writeFile filePath (LBS.fromStrict (T.encodeUtf8 text))

lookupTable :: T.Text -> SC.Schema -> SC.Table
lookupTable name schema = case find (\t -> SC.table_name t == name) (SC.schema_tables schema) of
  Nothing -> error ("Can't find schema table for " <> T.unpack name)
  Just t -> t

generateJavaModel :: JavaTableFlags -> J.CodeGenProfile -> J.JavaPackageFn -> J.CModule -> DBTable -> J.ClassFile
generateJavaModel jtflags cgp javaPackageFn mod (decl,struct,table,dbTableAnnotation) = execState gen state0
  where
    state0 = J.classFile cgp (AST.m_name mod) javaPackageFn classDecl
    tableClassNameT = tableClassName decl
    tableInstanceNameT = T.toUpper (snakify (AST.d_name decl))
    classDecl = "@SuppressWarnings(\"all\")\npublic class " <> tableClassNameT <> " extends Table"
    javaClassNameT = javaClassName decl
    dbTableNameT = dbTableName decl
    gen = do
      J.addImport "au.com.helixta.nofrills.sql.Dsl"
      J.addImport "au.com.helixta.nofrills.sql.Dsl.Table"
      J.addImport "au.com.helixta.nofrills.sql.Dsl.FieldRef"
      J.addImport "au.com.helixta.nofrills.sql.Dsl.TypedField"
      J.addImport "au.com.helixta.nofrills.sql.impl.DbResults"
      J.addImport "au.com.helixta.util.sql.QueryHelper"
      J.addImport "au.com.helixta.adl.runtime.JsonBindings"
      J.addImport "com.google.common.collect.ImmutableMap"
      J.addImport "com.google.common.collect.Maps"
      J.addImport "com.google.gson.JsonElement"
      J.addImport "com.google.gson.JsonPrimitive"
      J.addImport "javax.annotation.Nullable"
      J.addImport "java.util.Map"
      J.addImport "java.util.Optional"
      J.addImport "java.util.function.Function"
      J.addImport "java.util.function.Supplier"
      J.addImport "au.com.helixta.util.common.collect.Mapx"

      J.addMethod (ctemplate "public static final $1 $2 = new $1();" [tableClassNameT, tableInstanceNameT])

      J.addMethod (cblock (template "public $1()" [tableClassNameT]) (
        ctemplate "super(\"$1\");" [dbTableNameT]
        ))

      J.addMethod (cblock (template "public $1(String alias)" [tableClassNameT]) (
        ctemplate "super(\"$1\", alias);" [dbTableNameT]
        ))

      J.addMethod
        (  cline "//exposed to enable postgres views that extend tables"
        <> cblock (template "protected $1(String tablename, @Nullable String alias)" [tableClassNameT]) (
             ctemplate "super(tablename, alias);" [dbTableNameT]
           )
        )

      let withIdPrimaryKey = length (SC.table_columns table) /= length (AST.s_fields struct)
          dbColumns = mkDbColumns withIdPrimaryKey (SC.table_columns table) (AST.s_fields struct)


      for_ dbColumns $ \dbc -> do
        let javaFieldNameT = javaFieldName dbc
            (columnName,javaDbTypeT) = case dbc of
              (IdColumn col) -> (SC.column_name col,"String")
              (DbColumn col field) -> (SC.column_name col, javaDbType col field)
        J.addMethod
          (  ctemplate "private final TypedField<$2> $1 = f(\"$3\", $2.class);"
                        [javaFieldNameT, javaDbTypeT, columnName]
          <> ctemplate "public TypedField<$2> $1() { return $1; }"
                        [javaFieldNameT, javaDbTypeT]
          )

      J.addMethod
        ( ctemplate "public static final ImmutableMap<String, Function<$1, TypedField<?>>> FIELDS = ImmutableMap.copyOf(Mapx.m("
                     [tableClassNameT]
        <> indent (mconcat [ ctemplate "Mapx.e(\"$1\", t -> t.$1)$2" [javaFieldName dbc,mcomma]
                           | (dbc,mcomma) <- withCommas dbColumns])
        <> cline "));"
        )

      J.addMethod
        ( cblock "public Map<String, TypedField<?>> allFields()"
          (cline "return Maps.transformValues(FIELDS, f -> f.apply(this));")
        )

      let withIdPrimaryKey = case getAnnotationField dbTableAnnotation "withIdPrimaryKey" of
            (Just (JS.Bool True)) -> True
            _ -> False

      if withIdPrimaryKey
        then do
          let fields = [(dbc, col,field) | dbc@(DbColumn col field) <- dbColumns]
          genFromDbResultsWithIdKey fields
          genDbMappingWithIdKey fields

          when (jt_crudfns jtflags) $ do
            crudHelperFns

        else do
          genFromDbResults dbColumns
          genDbMapping dbColumns

    genFromDbResults dbColumns = do
      ctorargs <- for dbColumns $ \dbc -> case dbc of
        (DbColumn col field) -> do
          adlFromDbExpr col field (template "res.get($1())" [AST.f_name field])
        (IdColumn col) -> return "res.get(id()"

      J.addMethod
        ( cblock (template "public $1 fromDbResults(DbResults res)" [javaClassNameT])
          (  ctemplate "return new $1(" [javaClassNameT]
          <> indent (mconcat [ cline (ctorarg <> mcomma) | (ctorarg,mcomma) <- withCommas ctorargs] )
          <> cline ");"
          )
        )

    genDbMapping dbColumns = do
      getters <- for dbColumns $ \dbc -> case dbc of
        (DbColumn col field) -> do
          dbFromAdlExpr col field (template "value.get$1()" [J.javaCapsFieldName field])
        (IdColumn col) -> return ("value.getId()")

      J.addMethod
        ( cblock (template "public Map<? extends FieldRef, ? extends Object> dbMapping($1 value)" [javaClassNameT])
          (  cline "return Mapx.m("
          <> indent (mconcat [ctemplate "Mapx.e($1(), $2)$3" [javaFieldName dbc, getter, mcomma] | ((dbc,getter),mcomma) <- withCommas (zip dbColumns getters)])
          <> cline ");"
          )
        )

    genFromDbResultsWithIdKey fields = do
      withDbIdI <- J.addImport "au.com.helixta.adl.common.db.WithDbId"
      dbKeyI <- J.addImport "au.com.helixta.adl.common.db.DbKey"

      ctorargs <- for fields $ \(dbc,col,field) -> do
        adlFromDbExpr col field (template "res.get($1())" [AST.f_name field])

      J.addMethod
        ( cblock (template "public $1<$2> fromDbResults(DbResults res)" [withDbIdI,javaClassNameT])
          (  ctemplate "$1 result = new $1(" [javaClassNameT]
          <> indent (mconcat [ cline (ctorarg <> mcomma) | (ctorarg,mcomma) <- withCommas ctorargs] )
          <> cline ");"
          <> ctemplate "return new $1<$3>(new $2<$3>(res.get(id())), result);" [withDbIdI,dbKeyI,javaClassNameT]
          )
        )

    genDbMappingWithIdKey fields = do
      dbKeyI <- J.addImport "au.com.helixta.adl.common.db.DbKey"
      getters <- for fields $ \(dbc,col,field) -> do
          dbFromAdlExpr col field (template "value.get$1()" [J.javaCapsFieldName field])

      J.addMethod
        ( cblock (template "public Map<? extends FieldRef, ? extends Object> dbMapping($1<$2> id, $2 value)" [dbKeyI,javaClassNameT])
          (  cline "return Mapx.m("
          <> indent (cline "Mapx.e(id(), id.getValue()),")
          <> indent (mconcat [ctemplate "Mapx.e($1(), $2)$3" [javaFieldName dbc, getter, mcomma] | (((dbc,_,_),getter),mcomma) <- withCommas (zip fields getters)])
          <> cline ");"
          )
        )

    crudHelperFns = do
      dbKeyI <- J.addImport "au.com.helixta.adl.common.db.DbKey"

      J.addMethod
        ( cblock (template "public $1<$2> create(Supplier<String> idSupplier, QueryHelper.Context ctx, $2 value)" [dbKeyI, javaClassNameT])
          (  ctemplate "  $1<$2> id = new $1<>(idSupplier.get());" [dbKeyI, javaClassNameT]
          <> cline "  Dsl.Insert insert = Dsl.insert(this).mappings(dbMapping(id, value));"
          <> cline "  ctx.execute(insert);"
          <> cline "  return id;"
          )
        )

      J.addMethod
        ( cblock (template " public Optional<$2> read(QueryHelper.Context ctx, $1<$2> id)" [dbKeyI,javaClassNameT])
          (  cline "  Dsl.Select select ="
          <> cline "          Dsl.select(allFields().values())"
          <> cline "                  .from(this)"
          <> cline "                  .where(id().eq(id.getValue()));"
          <> cline "  DbResults dbResults = ctx.query(select);"
          <> cline "  while (dbResults.next()) {"
          <> cline "    return Optional.of(fromDbResults(dbResults).getValue());"
          <> cline "  }"
          <> cline "  return Optional.empty();"
          )
        )

      J.addMethod
        ( cblock (template "public void update(QueryHelper.Context ctx, $1<$2> id, $2 value)" [dbKeyI,javaClassNameT])
          (  cline "  Dsl.Update update ="
          <> cline "    Dsl.update(this)"
          <> cline "    .set(this.dbMapping(id,value))"
          <> cline "    .where(id().eq(id.getValue()));"
          <> cline "  ctx.execute(update);"
          )
        )

      J.addMethod
        ( cblock (template "public void delete(QueryHelper.Context ctx, $1<$2> id)" [dbKeyI,javaClassNameT])
          (  cline "  Dsl.Delete delete ="
          <> cline "    Dsl.delete(this)"
          <> cline "    .where(id().eq(id.getValue()));"
          <> cline "  ctx.execute(delete);"
          )
        )

data DbColumn
  = IdColumn SC.Column
  | DbColumn SC.Column (AST.Field J.CResolvedType)

mkDbColumns :: Bool -> [SC.Column] -> [AST.Field J.CResolvedType] -> [DbColumn]
mkDbColumns False columns fields = zipWith DbColumn columns fields
mkDbColumns True columns fields = (IdColumn (head columns)): mkDbColumns False(tail columns) fields

javaFieldName :: DbColumn -> T.Text
javaFieldName (IdColumn _) = "id"
javaFieldName (DbColumn _ field) = J.unreserveWord (AST.f_name field)

javaDbType :: SC.Column -> AST.Field J.CResolvedType -> T.Text
javaDbType col field
  | refEnumeration (AST.f_type field) = "String"
  | SC.column_ctype col == "text" = "String"
  | "varchar" `T.isPrefixOf` SC.column_ctype col = "String"
  | "nvarchar" `T.isPrefixOf` SC.column_ctype col = "String"
  | SC.column_ctype col == "boolean" = "Boolean"
  | SC.column_ctype col == "json" = "JsonElement"
  | SC.column_ctype col == "jsonb" = "JsonElement"
  | SC.column_ctype col == "bigint" = "Long"
  | SC.column_ctype col == "integer" = "Integer"
  | SC.typeExprReferences SC.instantType te  = "java.time.Instant"
  | SC.typeExprReferences SC.localDateTimeType te = "java.time.LocalDateTime"
  | SC.typeExprReferences SC.localDateType te = "java.time.LocalDate"
  | SC.typeExprReferences SC.geographyType te = "org.postgis.PGgeometry"
  | SC.typeExprReferences SC.geographyGeoJsonType te = "org.postgis.PGgeometry"
  | SC.typeExprReferences SC.geometryWKTType te = "org.postgis.PGgeometry"
  | SC.column_ctype col == "double precision" = "Double"
  | "float" `T.isPrefixOf` SC.column_ctype col = "Double"
  | otherwise = "unimp:" <> SC.column_ctype col
  where
    te = SC.columnTypeFromField SC.postgresDbProfile field

-- Generate an expression converting a db value into an ADL value
adlFromDbExpr :: SC.Column -> AST.Field J.CResolvedType -> T.Text -> J.CState T.Text
adlFromDbExpr col field expr = do
  let ftype = AST.f_type field
  cgp <- fmap J.cf_codeProfile get
  fdetails <- J.genFieldDetails field
  case (SC.column_nullable col,SC.column_references col, SC.column_ctype col,ftype) of
    (True, _, "json", AST.TypeExpr _ [te]) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp te
      return (template "Optional.ofNullable($1).map($2::fromJson)" [expr,jbindingExpr])
    (True, _, "jsonb", AST.TypeExpr _ [te]) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp te
      return (template "Optional.ofNullable($1).map($2::fromJson)" [expr,jbindingExpr])
    (True, _, _, AST.TypeExpr _ [te]) -> do
      expr1 <- adlFromDbExpr col{SC.column_nullable=False} field{AST.f_type=te,AST.f_default=Nothing} "v"
      let mapExpr = case expr1 of
            "v" -> ""
            _ -> template ".map(v -> $1)" [expr1]
      return (template "Optional.ofNullable($1)$2" [expr,mapExpr])
    (False,_,"geography",_) -> return expr
    (False,_,"geometry",_) -> return expr
    (False,Just _,_,_) -> do
      return (template "new $1($2)" [J.fd_typeExprStr fdetails,expr])
    (False,_,"timestamp",_) -> return expr
    (False,_,"date",_) -> return expr
    (False,_,"json",_) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp ftype
      return (template "$1.fromJson($2)" [jbindingExpr,expr])
    (False,_,"jsonb",_) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp ftype
      return (template "$1.fromJson($2)" [jbindingExpr,expr])
    (False,_,_,_)
            | refEnumeration ftype -> return (template "$1.fromString($2)" [J.fd_typeExprStr fdetails,expr])
            | otherwise -> case refNewtype ftype of
                  (Just n) -> return (template "new $1($2)" [J.fd_typeExprStr fdetails,expr])
                  Nothing -> return expr

-- Generate an expression converting an ADL value into a db value
dbFromAdlExpr :: SC.Column -> AST.Field J.CResolvedType -> T.Text -> J.CState T.Text
dbFromAdlExpr col field expr = do
  let ftype = AST.f_type field
  cgp <- fmap J.cf_codeProfile get
  fdetails <- J.genFieldDetails field
  case (SC.column_nullable col,SC.column_references col, SC.column_ctype col,ftype) of
    (True, _, "json", AST.TypeExpr _ [te]) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp te
      return (template "($1.isPresent() ? $2.toJson($1.get()) : null)" [expr, jbindingExpr])
    (True, _, "jsonb", AST.TypeExpr _ [te]) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp te
      return (template "($1.isPresent() ? $2.toJson($1.get()) : null)" [expr, jbindingExpr])
    (True, _, _, AST.TypeExpr _ [te]) -> do
      expr1 <- dbFromAdlExpr col{SC.column_nullable=False} field{AST.f_type=te,AST.f_default=Nothing} "v"
      let mapExpr = case expr1 of
            "v" -> ""
            _ -> template ".map(v -> $1)" [expr1]
      return (template "$1$2.orElse(null)" [expr,mapExpr])
    (False,_,"geography",_) -> return expr
    (False,_,"geometry",_) -> return expr
    (False,Just _,_,_) -> do
      return (template "$1.getValue()" [expr])
    (False,_,"timestamp",_) -> return expr
    (False,_,"date",_) -> return expr
    (False,_,"json",_) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp ftype
      return (template "$1.toJson($2)" [jbindingExpr,expr])
    (False,_,"jsonb",_) -> do
      jbindingExpr <- J.genJsonBindingExpr cgp ftype
      return (template "$1.toJson($2)" [jbindingExpr,expr])
    (False,_,_,_)
            | refEnumeration ftype -> return (template "$1.toString()" [expr])
            | otherwise -> case refNewtype ftype of
                  (Just n) -> return (template "$1.getValue()" [expr])
                  Nothing -> return expr

dbTableType = AST.ScopedName (AST.ModuleName ["common","db"]) "DbTable"

-- ----------------------------------------------------------------------
-- -- helper stuff

tableClassName decl = J.unreserveWord (AST.d_name decl) <> "Table"

-- match structs that are annotated to be
-- database tables
matchDBTable :: SC.Schema -> J.CDecl -> Maybe DBTable
matchDBTable schema decl = case AST.d_type decl of
  (AST.Decl_Struct struct) ->
    case getAnnotation (AST.d_annotations decl) dbTableType of
      Nothing -> Nothing
      (Just annotation) -> case find (\t -> SC.table_name t == dbTableName decl) (SC.schema_tables schema) of
        Nothing -> error ("Can't find schema table for oth" <> T.unpack (dbTableName decl))
        Just table -> Just (decl,struct,table,annotation)
  _ -> Nothing

dbTableName :: J.CDecl -> T.Text
dbTableName decl = case getAnnotation (AST.d_annotations decl) dbTableType of
  (Just (JS.Object hm)) -> case HM.lookup "tableName" hm of
    (Just (JS.String t)) -> t
    _ -> dbName (AST.d_name decl)
  _ -> dbName (AST.d_name decl)

javaTableClassName :: J.CDecl -> T.Text
javaTableClassName decl = AST.d_name decl <> "Table"

javaClassName :: J.CDecl -> T.Text
javaClassName decl = AST.d_name decl

getAnnotationField :: JS.Value -> T.Text -> Maybe JS.Value
getAnnotationField (JS.Object hm) field = HM.lookup field hm
getAnnotationField _ _ = Nothing

withCommas :: [a] -> [(a,T.Text)]
withCommas [] = []
withCommas [l] = [(l," ")]
withCommas (l:ls) = (l,","):withCommas ls

dbName :: T.Text -> T.Text
dbName = snakify

getAnnotation :: AST.Annotations J.CResolvedType -> AST.ScopedName -> Maybe JS.Value
getAnnotation annotations annotationName = snd <$> M.lookup annotationName annotations
