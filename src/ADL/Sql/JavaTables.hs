{-# LANGUAGE OverloadedStrings #-}

module ADL.Sql.JavaTables(
    generateJavaTables
  , writeJavaTables
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

import ADL.Compiler.EIO
import ADL.Compiler.Primitive
import ADL.Compiler.Processing(AdlFlags(..),RModule,defaultAdlFlags,loadAndCheckModule1)
import ADL.Compiler.Utils(FileWriter,writeOutputFile)
import ADL.Compiler.ExternalAST(moduleToA2)
import ADL.Compiler.Flags(Flags(..),parseArguments,standardOptions)
import ADL.Utils.IndentedCode
import ADL.Utils.Format(template,formatText)
import ADL.Sys.Adlast
import Cases(snakify)
import Control.Monad.Trans(liftIO)
import Data.Char(toUpper)
import Data.Foldable(for_)
import Data.List(intersperse,find)
import Data.Monoid
import Data.Maybe(mapMaybe)
import System.Directory(createDirectoryIfMissing)
import System.FilePath(takeDirectory,(</>))
import System.Console.GetOpt(OptDescr(..), ArgDescr(..))

data JavaTableFlags = JavaTableFlags {
  jt_rtpackage :: T.Text,
  jt_package :: T.Text
}

defaultJavaTableFlags = JavaTableFlags "adl.runtime" "adl"

javaTableOptions =
  [ Option "" ["rtpackage"]
      (ReqArg (\s f -> f{f_backend=(f_backend f){jt_rtpackage=T.pack s}}) "PACKAGE")
      "The  package where the ADL runtime can be found"
  , Option "" ["package"]
      (ReqArg (\s f -> f{f_backend=(f_backend f){jt_package=T.pack s}}) "PACKAGE")
      "The  package into which the generated ADL code will be placed"
  ]

type DBTable = (Decl,Struct,Literal)

generateJavaTables :: [String] -> EIO T.Text ()
generateJavaTables args = do
  (flags,paths) <- parseArguments header defaultAdlFlags defaultJavaTableFlags options args
  let fileWriter = writeOutputFile (f_output flags)
      adlFlags = (f_adl flags){af_mergeFileExtensions=[".adl-java"] <> af_mergeFileExtensions (f_adl flags)}
  for_ paths $ \path -> do
    (mod,moddeps) <- loadAndCheckModule1 adlFlags path
    let getJavaPackage = findJavaPackage (jt_package (f_backend flags)) (mod:moddeps)
    liftIO $ writeJavaTables fileWriter (f_backend flags) getJavaPackage mod
  where
    header = "Usage: generate.hs java-tables ...args..."
    options =  standardOptions <> javaTableOptions

-- FIXME: This code is currently written in terms of the external ast (ie ADL.Sys.Adlast).
-- It would be better in terms of the internal AST (ADL.Compiler.AST) and or schema class.

writeJavaTables :: FileWriter -> JavaTableFlags -> (T.Text -> T.Text) -> RModule -> IO ()
writeJavaTables writeFile flags getJavaPackage rmod = for_ tableDecls $ \(decl,struct,annotation) -> do
  let code = generateJavaModel (jt_rtpackage flags) javaPackage commonDbPackage mod (decl,struct,annotation)
      text = (T.intercalate "\n" (codeText 1000 code))
      outputPath = pathFromPackage (javaPackage <> "." <> module_name mod <> "." <> javaTableClassName decl) <> ".java"
  writeFile outputPath (LBS.fromStrict (T.encodeUtf8 text))
  where
    commonDbPackage = getJavaPackage "common.db"
    javaPackage = jt_package flags
    mod = moduleToA2 rmod
    pathFromPackage pkg = T.unpack (T.replace "." "/" pkg)
    tableDecls = mapMaybe matchDBTable ( M.elems (module_decls mod))

generateJavaModel :: T.Text -> T.Text -> T.Text -> Module -> DBTable -> Code
generateJavaModel rtPackage javaPackage commonDbPackage mod (decl,struct,annotation)
  =  cline "// Copyright 2017 Helix Collective Pty. Ltd."
  <> cline "// *GENERATED CODE*"
  <> cline ""
  <> ctemplate "package $1;" [javaPackage <> "." <> module_name mod]
  <> cline ""
  <> cline "import static au.com.helixta.util.common.collect.Mapx.e;"
  <> cline "import static au.com.helixta.util.common.collect.Mapx.m;"
  <> cline ""
  <> cline "import au.com.helixta.nofrills.sql.Dsl.Table;"
  <> cline "import au.com.helixta.nofrills.sql.Dsl.FieldRef;"
  <> cline "import au.com.helixta.nofrills.sql.Dsl.TypedField;"
  <> cline "import au.com.helixta.nofrills.sql.impl.DbResults;"
  <> cline ""
  <> ctemplate "import $1.JsonBindings;" [rtPackage]
  <> ctemplate "import $1.DbKey;" [commonDbPackage]
  <> ctemplate "import $1.WithDbId;" [commonDbPackage]
  <> cline ""
  <> cline "import com.google.common.collect.ImmutableMap;"
  <> cline "import com.google.common.collect.Maps;"
  <> cline "import com.google.gson.JsonElement;"
  <> cline "import com.google.gson.JsonPrimitive;"
  <> cline ""
  <> cline "import javax.annotation.Nullable;"
  <> cline ""
  <> cline "import java.util.Map;"
  <> cline "import java.util.Optional;"
  <> cline "import java.util.function.Function;"
  <> cline ""
  <> cline "@SuppressWarnings(\"all\")"
  <> ctemplate "public class $1 extends Table {" [tableClassName]
  <> indent
    (  ctemplate "public static final $1 $2 = new $1();" [tableClassName,staticVarName]
    <> cline ""
    <> ctemplate "public $1() {" [tableClassName]
    <> indent (ctemplate "super(\"$1\");" [tableName])
    <> cline "}"
    <> cline ""
    <> ctemplate "public $1(String alias) {" [tableClassName]
    <> indent (ctemplate "super(\"$1\", alias);" [tableName])
    <> cline "}"
    <> cline ""
    <> cline "//exposed to enable postgres views that extend tables"
    <> ctemplate "protected $1(String tablename, @Nullable String alias) {" [tableClassName]
    <> indent (cline "super(tablename, alias);")
    <> cline "}"
    <> cline ""
    <> mconcat
      [  ctemplate "private final TypedField<$1> $2 = f(\"$3\", $1.class);" [fieldType,unreserveWord fieldName,colName]
      <> ctemplate "public TypedField<$1> $2() { return $2; }" [fieldType,unreserveWord fieldName]
      <> cline ""
      |  (colName,fieldName,fieldType,_,_) <- allColumns]
    <> cline ""
    <> ctemplate "public static final ImmutableMap<String, Function<$1, TypedField<?>>> FIELDS = ImmutableMap.copyOf(m(" [tableClassName]
    <> indent (mconcat
      [ ctemplate "e(\"$1\", t -> t.$2)$3" [fieldName,unreserveWord fieldName,mcomma] | ((colName,fieldName,fieldType,_,_),mcomma) <- withCommas allColumns
      ])
    <> cline "));"
    <> cline ""
    <> cline "public Map<String, TypedField<?>> allFields() {"
    <> indent (cline "return Maps.transformValues(FIELDS, f -> f.apply(this));")
    <> cline "}"
    <> cline ""
    <> (if hasPrimaryKey then fromDbResultsWithIdCode else fromDbResultsCode)
    <> cline ""
    <> (if hasPrimaryKey then dbMappingWithIdCode else dbMappingCode)
    )
  <> cline "};"
  where
    fromDbResultsCode
      = ctemplate "public $1 fromDbResults(DbResults res) {" [className]
      <> indent
        (  ctemplate "$1 result = new $1();" [className]
        <> mconcat [ ctemplate "result.set$1($2);" [upper1 fieldName,fromDb (template "res.get($1())" [unreserveWord fieldName])]
                   | (colName,fieldName,fieldType,fromDb,toDb) <- columns ]
        <> cline "return result;"
        )
      <> cline "}"

    fromDbResultsWithIdCode
      = ctemplate "public WithDbId<$1> fromDbResults(DbResults res) {" [className]
      <> indent
        (  ctemplate "$1 result = new $1();" [className]
        <> mconcat [ ctemplate "result.set$1($2);" [upper1 fieldName,fromDb (template "res.get($1())" [unreserveWord fieldName])]
                   | (colName,fieldName,fieldType,fromDb,toDb) <- columns ]
        <> ctemplate "return new WithDbId<$1>(new DbKey<$1>(res.get(id())), result);" [className]
        )
      <> cline "}"

    dbMappingCode
      = ctemplate "public Map<? extends FieldRef, ? extends Object> dbMapping($1 value) {" [className]
      <> indent
         (  cline "return m("
         <> indent
            (  mconcat [ cline (mapping <> mcomma) | (mapping,mcomma) <- withCommas dbMappings]
            )
         <> cline ");"
         )
      <> cline "}"

    dbMappingWithIdCode
      = ctemplate "public Map<? extends FieldRef, ? extends Object> dbMapping(DbKey<$1> id, $1 value) {" [className]
      <> indent
         (  cline "return m("
         <> indent
            (  mconcat [ cline (mapping <> mcomma) | (mapping,mcomma) <- withCommas (["e(id(), id.getValue())"] <> dbMappings)]
            )
         <> cline ");"
         )
      <> cline "}"

    hasPrimaryKey = case getLiteralField annotation "withIdPrimaryKey" of
      (Just (Literal_boolean True)) -> True
      _ -> False

    allColumns = primaryKeyColumn <> columns
    primaryKeyColumn = if hasPrimaryKey then [("id","id","String",id,id)] else []
    columns = map generateCol (struct_fields struct)

    dbMappings
      =  [ template "e($1(), $2)" [unreserveWord fieldName, toDb (template "value.get$1()" [upper1 fieldName])]
         | (colName,fieldName,fieldType,fromDb,toDb) <- columns ]

    generateCol :: Field -> (T.Text,T.Text,T.Text,T.Text->T.Text,T.Text->T.Text)
    generateCol field = (colName,fieldName,fieldType,fromDb,toDb)
      where
        colName = dbName (field_name field)
        fieldName = field_name field
        fieldType = javaFieldType (field_typeExpr field)
        fromDb expr = fromDbExpr (field_typeExpr field) expr
        toDb expr = toDbExpr (field_typeExpr field) expr

    tableClassName = javaTableClassName decl
    className = javaClassName decl
    tableName = dbTableName decl
    staticVarName = T.toUpper (dbName (decl_name decl))

    javaFieldType :: TypeExpr -> T.Text
    javaFieldType te = javaType (snd (dbType mod te))

    javaType (Primitive _ _ jt) = jt
    javaType (Ref _) = "String"
    javaType Enumeration = "String"
    javaType Timestamp = "java.time.Instant"
    javaType Date = "java.time.LocalDate"
    javaType (DbNewtype _ dbt) = javaType dbt
    javaType (Json _) = "JsonElement"

    fromDbExpr :: TypeExpr -> T.Text -> T.Text
    fromDbExpr te expr = case dbType mod te of
      (Required,dbt) -> fromDbExpr' dbt expr
      (Nullable,Json te) -> template "Optional.ofNullable($1).map($2::fromJson)" [expr,jsonBindingExpr javaPackage te]
      (Nullable,dbt) -> template "Optional.ofNullable($1)" [fromDbExpr' dbt expr]
      where
        fromDbExpr' :: DbType0 -> T.Text -> T.Text
        fromDbExpr' (Primitive _ _ _) expr = expr
        fromDbExpr' Timestamp expr = expr
        fromDbExpr' Date expr = expr
        fromDbExpr' (DbNewtype (ScopedName _ name) dbt) expr = template "new $1($2)" [name,expr]
        fromDbExpr' (Ref te') expr = template "new DbKey<$1>($2)" [localClassName te',expr]
        fromDbExpr' Enumeration expr = template "$1.fromString($2)" [localClassName te, expr]
        fromDbExpr' (Json te) expr = template "$1.fromJson($2)" [jsonBindingExpr javaPackage te, expr]

    toDbExpr :: TypeExpr -> T.Text -> T.Text
    toDbExpr te expr = case dbType mod te of
      (Required,dbt) -> toDbExpr' dbt expr
      (Nullable,Json te) -> template "($1.isPresent() ? $2.toJson($1.get()) : null)" [expr,jsonBindingExpr javaPackage te]
      (Nullable,dbt) -> template "$1.orElse(null)" [toDbExpr' dbt expr]
      where
        toDbExpr' :: DbType0 -> T.Text -> T.Text
        toDbExpr' (Primitive _ _ _) expr = expr
        toDbExpr' Timestamp expr = expr
        toDbExpr' Date expr = expr
        toDbExpr' (DbNewtype (ScopedName _ name) dbt) expr = template "$1.getValue()" [expr]
        toDbExpr' (Ref _) expr = template "$1.getValue()" [expr]
        toDbExpr' Enumeration expr = template "$1.toString()" [expr]
        toDbExpr' (Json te) expr = template "$1.toJson($2)" [jsonBindingExpr javaPackage te, expr]

dbTableType = ScopedName "common.db" "DbTable"
dbKeyType = ScopedName "common.db" "DbKey"
instantType = ScopedName "common" "Instant"
localDateType = ScopedName "common" "LocalDate"
maybeType = ScopedName "sys.types" "Maybe"

----------------------------------------------------------------------
-- helper stuff

-- match structs that are annotated to be
-- database tables
matchDBTable :: Decl -> Maybe DBTable
matchDBTable decl = case decl_type_ decl of
  (DeclType_struct_ struct) ->
    case getAnnotation decl dbTableType of
      Nothing -> Nothing
      (Just annotation) -> Just (decl,struct,annotation)
  _ -> Nothing

dbTableName :: Decl -> T.Text
dbTableName decl = case getAnnotation decl dbTableType of
  (Just (Literal_object m)) -> case M.lookup "tableName" m of
    (Just (Literal_string t)) -> t
    _ -> dbName (decl_name decl)
  _ -> dbName (decl_name decl)

javaTableClassName :: Decl -> T.Text
javaTableClassName decl = decl_name decl <> "Table"

javaClassName :: Decl -> T.Text
javaClassName decl = decl_name decl

upper1 :: T.Text -> T.Text
upper1 t = case T.uncons t of
  Nothing -> t
  Just (c,t') -> T.cons (toUpper c) t'


localClassName :: TypeExpr -> T.Text
localClassName (TypeExpr (TypeRef_reference (ScopedName mname name)) []) | T.null mname = name
localClassName _ = "UNIMPLEMENTED(X)"

jsonBindingExpr :: T.Text -> TypeExpr -> T.Text
jsonBindingExpr javaPackage (TypeExpr rt tes) =
  case rt of
    (TypeRef_reference (ScopedName mname name))
      | T.null mname -> template "$1.jsonBinding($2)" [name,bparams]
      | otherwise    -> template "$1.$2.$3.jsonBinding($4)" [javaPackage, mname, name,bparams]
    (TypeRef_primitive p) | p == "Vector"
      -> template "JsonBindings.arrayList($1)" [bparams]
    _ -> template "UNIMPLEMENTED($1)" [T.pack (show rt)]
  where
    bparams = T.intercalate ", " (map (jsonBindingExpr javaPackage) tes)

-- Column type mapping:
--    Anything that is a Maybe<T> or Nullable<T> maps to a nullable column
--    Newtypes are expanded
--    Enunerations (ie unions with only void branches) are stored as text
--    DBRef<T> is a foreign key reference
--    Primitives are handled explicitly below
--    everything else is stored as json

data Nullable = Nullable | Required
type DbType = (Nullable, DbType0)

data DbType0
  = Primitive Ident T.Text T.Text
  | Ref TypeExpr
  | Enumeration
  | Timestamp
  | Date
  | DbNewtype ScopedName DbType0
  | Json TypeExpr

dbType :: Module -> TypeExpr -> DbType
dbType mod (TypeExpr ref [te]) | ref == TypeRef_reference maybeType = (Nullable, dbType0 mod te)
                               | ref == TypeRef_primitive "Nullable" = (Nullable, dbType0 mod te)
dbType mod te@(TypeExpr ref@(TypeRef_reference sn) []) = case resolveTypeDef mod sn of
  (Just te') -> dbType mod te'
  Nothing -> (Required, dbType0 mod te)
dbType mod te = (Required, dbType0 mod te)

dbType0 :: Module -> TypeExpr -> DbType0
dbType0 mod te = case resolveNewType mod te of
  Just (sn,te') -> DbNewtype sn (dbType0 mod te')
  Nothing -> dbType1 mod te

dbType1 :: Module -> TypeExpr -> DbType0

dbType1 mod (TypeExpr (TypeRef_reference sn) [te])
  | sn ==  dbKeyType = Ref te

dbType1 mod te@(TypeExpr ref@(TypeRef_reference sn) [])
  | sn == instantType = Timestamp
  | sn == localDateType = Date
  | isEnumeration mod ref = Enumeration
  | otherwise = case resolveTypeDef mod sn of
      (Just te') -> dbType1 mod te'
      Nothing -> Json te

dbType1 mod te@(TypeExpr (TypeRef_primitive p) _)
 | p == "String" = Primitive p "text"      "String"
 | p == "Int8"   = Primitive p "smalllint" "Integer"
 | p == "Int16"  = Primitive p "smalllint" "Integer"
 | p == "Int32"  = Primitive p "integer"   "Integer"
 | p == "Int64"  = Primitive p "bigint"    "Long"
 | p == "Word8"  = Primitive p "smalllint" "Integer"
 | p == "Word16" = Primitive p "smalllint" "Integer"
 | p == "Word32" = Primitive p "integer"   "Integer"
 | p == "Word64" = Primitive p "bigint"    "Long"
 | p == "Float"  = Primitive p "real"      "Float"
 | p == "Double" = Primitive p "double"    "Double"
 | p == "Bool"   = Primitive p "boolean"   "Boolean"
 | p == "Json"   = Primitive p "json"      "JsonElement"
 | otherwise = Json te

dbType1 mod te = Json te

-- | A type is an enumeration if it's a union with all void fields.
isEnumeration :: Module -> TypeRef -> Bool
isEnumeration mod (TypeRef_reference (ScopedName mname name)) | T.null mname
  = case M.lookup name (module_decls mod) of
     Just Decl{decl_type_=DeclType_union_ Union{union_fields=fields}} -> all (isVoidType . field_typeExpr) fields
     _ -> False
isEnumeration _ _ = False

resolveNewType :: Module -> TypeExpr -> Maybe (ScopedName,TypeExpr)
resolveNewType mod (TypeExpr (TypeRef_reference sn@(ScopedName mname name)) []) | T.null mname
  = case M.lookup name (module_decls mod) of
     Just Decl{decl_type_=DeclType_newtype_ (NewType [] te _)} -> Just (sn,te)
     _ -> Nothing
resolveNewType _ _ = Nothing

resolveTypeDef :: Module -> ScopedName -> Maybe TypeExpr
resolveTypeDef mod sn@(ScopedName mname name) | T.null mname
  = case M.lookup name (module_decls mod) of
     Just Decl{decl_type_=DeclType_type_ (TypeDef [] te )} -> Just te
     _ -> Nothing
resolveTypeDef _ _ = Nothing

isVoidType :: TypeExpr -> Bool
isVoidType (TypeExpr (TypeRef_primitive p) []) = p == "Void"
isVoidType _ = False

getLiteralField :: Literal -> T.Text -> Maybe Literal
getLiteralField (Literal_object map) field = M.lookup field map
getLiteralField _ _ = Nothing

withCommas :: [a] -> [(a,T.Text)]
withCommas [] = []
withCommas [l] = [(l," ")]
withCommas (l:ls) = (l,","):withCommas ls

typeExprText :: TypeExpr -> T.Text
typeExprText (TypeExpr tr [])  = typeRefText tr
typeExprText (TypeExpr tr tes)  = typeRefText tr <> "<" <>  T.intercalate "," (map typeExprText tes) <> ">"

typeRefText :: TypeRef -> T.Text
typeRefText (TypeRef_primitive p) = p
typeRefText (TypeRef_typeParam p) = p
typeRefText (TypeRef_reference (ScopedName mname name))
   | mname == "" = name
   | otherwise   = mname <> "." <> name

dbName :: T.Text -> T.Text
dbName = snakify

getAnnotation :: Decl -> ScopedName -> Maybe Literal
getAnnotation decl annotationName = M.lookup annotationName (decl_annotations decl)

findJavaPackage :: T.Text -> [RModule] -> T.Text -> T.Text
findJavaPackage defJavaPackage mods adlPackage
  = case find (\mod -> formatText (AST.m_name mod) == adlPackage) mods of
     Nothing -> defJavaPackage
     (Just mod) -> case M.lookup snJavaPackage (AST.m_annotations mod) of
       Just (_,JS.String pkg) -> pkg
       Nothing -> defJavaPackage
  where
    snJavaPackage = AST.ScopedName (AST.ModuleName ["adlc","config","java"]) "JavaPackage"

reservedWords :: S.Set T.Text
reservedWords = S.fromList
 [ "abstract"
 , "assert"
 , "boolean"
 , "break"
 , "byte"
 , "case"
 , "catch"
 , "char"
 , "class"
 , "const"
 , "continue"
 , "default"
 , "do"
 , "double"
 , "else"
 , "enum"
 , "extends"
 , "false"
 , "final"
 , "finally"
 , "float"
 , "for"
 , "goto"
 , "if"
 , "implements"
 , "import"
 , "instanceof"
 , "int"
 , "interface"
 , "long"
 , "native"
 , "new"
 , "null"
 , "package"
 , "private"
 , "protected"
 , "public"
 , "return"
 , "short"
 , "static"
 , "strictfp"
 , "super"
 , "switch"
 , "synchronized"
 , "this"
 , "throw"
 , "throws"
 , "transient"
 , "true"
 , "try"
 , "void"
 , "volatile"
 , "while"
 ]

unreserveWord :: T.Text -> T.Text
unreserveWord n | S.member n reservedWords = T.append n "_"
                | otherwise = n
