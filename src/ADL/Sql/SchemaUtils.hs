{-# LANGUAGE OverloadedStrings #-}
module ADL.Sql.SchemaUtils
  ( schemaFromAdl
  , sqlFromSchema
  , columnFromField
  ) where

import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import ADL.Compiler.Primitive
import ADL.Compiler.AST
import ADL.Compiler.Processing
import ADL.Utils.IndentedCode
import ADL.Utils.Format
import ADL.Sql.Schema
import Data.List(intersperse)
import Data.Monoid
import Cases(snakify)
import Data.Maybe(mapMaybe)

type RTypeExpr = TypeExpr ResolvedType
type RField = Field ResolvedType
type DBTable = (RDecl,Struct ResolvedType,JS.Value)

-- Build an abstract db schema from an ADL module
schemaFromAdl :: (RField -> Column) -> RModule -> Schema
schemaFromAdl mkColumn mod = Schema
  { schema_adlModules = [formatText (m_name mod)]
  , schema_tables = tables
  }
  where
    tables = map (mkTable mkColumn) (mapMaybe matchDBTable ( M.elems ( m_decls mod)))

instance Monoid Schema where
  mempty = Schema [] []
  mappend (Schema m1 t1) (Schema m2 t2) = Schema (m1 <> m2) (t1 <> t2)

-- Produce the SQL corresponding to the schema
sqlFromSchema :: Schema -> Code
sqlFromSchema schema =  sql
  where
    sql
      =  ctemplate "-- Schema auto-generated from adl modules: $1" [T.intercalate ", " (schema_adlModules schema)]
      <> cline "--"
      <> cline "-- column comments show original ADL types"
      <> cline ""
      <> mconcat (intersperse (cline "") (map tableDefSql (schema_tables schema)))
      <> cline ""
      <> mconcat (map tableConstraintsSql (schema_tables schema))

tableDefSql :: Table -> Code
tableDefSql table
  =  ctemplate "create table $1(" [table_name table]
  <> indent (mconcat
       [ sqlFromColumn col mcomma
       | (col,mcomma) <- withCommas (table_columns table)]
     )
  <> cline ");"
  where
    sqlFromColumn col mcomma = cline (column_name col <> " " <> sqltype <> mcomma <> comment)
      where
        sqltype = column_ctype col <> pkey <> nullable
        pkey | column_primaryKey col = " primary key"
             | otherwise = ""
        nullable | column_nullable col = ""
                 | otherwise = " not null"
        comment | column_comment col == "" = ""
                | otherwise                = template "$1-- $2" [pad,column_comment col]
        pad = T.replicate (max 1 (35 - T.length (column_name col <> sqltype))) " "

tableConstraintsSql :: Table -> Code
tableConstraintsSql table = rconstraints <> uconstraints
  where
    rconstraints = mconcat [ ctemplate "alter table $1 add constraint $1_$2_fk foreign key ($2) references $3($4);"
                             [table_name table, column_name col,ftable,fname]
                           | col@Column{column_references=(Just (ForeignKeyRef ftable fname))} <- table_columns table]
    uconstraints = mconcat [ ctemplate "alter table $1 add constraint $1_$2_con unique ($3);"
                             [table_name table, T.pack (show i), T.intercalate ", " (map dbName uc)]
                           | (UniqueConstraint uc,i) <- zip (table_uniqueConstraints table) [1,2..]]


mkTable :: (RField -> Column) -> DBTable -> Table
mkTable mkColumn (decl,struct,ann) = Table
  { table_name = dbName (d_name decl)
  , table_columns = idColumn <> map (setPrimaryKey . mkColumn) (s_fields struct)
  , table_uniqueConstraints = map UniqueConstraint uconstraints
  , table_annotation = ann
  }
  where
    uconstraints :: [[T.Text]]
    uconstraints = case getLiteralField ann "uniquenessConstraints" of
      Nothing -> []
      Just lit ->  fromLitArray (fromLitArray fromLitString) lit

    idColumn :: [Column]
    idColumn = case getLiteralField ann "withIdPrimaryKey" of
      Nothing -> []
      Just lit ->  [Column "id" "text" "" True True Nothing]

    setPrimaryKey :: Column -> Column
    setPrimaryKey col = case getLiteralField ann "withPrimaryKey" of
      Nothing -> col
      Just lit ->  col{column_primaryKey=dbName (fromLitString lit) == column_name col}

    fromLitArray f (JS.Array a) = map f (V.toList a)
    fromLitString (JS.String s) = s
    fromLitBool (JS.Bool b) = b

columnFromField :: RField -> Column
columnFromField field = mkColumn (f_type field)
  where
    mkColumn :: RTypeExpr -> Column
    mkColumn  (TypeExpr (RT_Named (sn,_)) [te]) | sn == maybeType = (mkColumn1 te){column_nullable=True}
    mkColumn (TypeExpr (RT_Primitive P_Nullable) [te]) = (mkColumn1 te){column_nullable=True}
    mkColumn te = mkColumn1 te

    mkColumn1 :: RTypeExpr -> Column
    mkColumn1 te@(TypeExpr (RT_Primitive p) _) =
      mkColumn2 (primColumnType p)
    mkColumn1 (TypeExpr (RT_Named (sn1,_)) [TypeExpr (RT_Named (sn2,_)) []]) | sn1 ==  dbKeyType =
      (mkColumn2 "text"){column_references=Just (ForeignKeyRef (dbName (sn_name sn2)) "id")}
    mkColumn1 (TypeExpr (RT_Named (sn,_)) [])  | sn == instantType =
      mkColumn2 "timestamp"
    mkColumn1 (TypeExpr (RT_Named (sn,_)) [])  | sn == localDateType =
      mkColumn2 "date"
    mkColumn1 (TypeExpr ref               [])  | isEnumeration2 ref =
      mkColumn2 "text"
    mkColumn1 _ = mkColumn2 "json"

    mkColumn2 ctype = Column
     { column_name = dbName (f_name field)
     , column_ctype = ctype
     , column_comment = typeExprText (f_type field)
     , column_primaryKey = False
     , column_nullable = False
     , column_references = Nothing
     }

primColumnType p =  case p of
  P_String -> "text"
  P_Int8 -> "smalllint"
  P_Int16 -> "smalllint"
  P_Int32 -> "integer"
  P_Int64 -> "bigint"
  P_Word8 -> "smalllint"
  P_Word16 -> "smalllint"
  P_Word32 -> "integer"
  P_Word64 -> "bigint"
  P_Float -> "real"
  P_Double -> "double"
  P_Bool -> "boolean"
  _ -> "json"

-- match structs that are annotated to be
-- database tables
matchDBTable :: RDecl -> Maybe DBTable
matchDBTable decl = case d_type decl of
  (Decl_Struct struct) ->
    case getAnnotation decl dbTableType of
      Nothing -> Nothing
      (Just annotation) -> Just (decl,struct,annotation)
  _ -> Nothing

getAnnotation :: RDecl -> ScopedName -> Maybe JS.Value
getAnnotation decl annotationName = snd <$> (M.lookup annotationName (d_annotations decl))

typeExprText :: RTypeExpr -> T.Text
typeExprText (TypeExpr tr [])  = typeRefText tr
typeExprText (TypeExpr tr tes)  = typeRefText tr <> "<" <>  T.intercalate "," (map typeExprText tes) <> ">"

typeRefText :: ResolvedType -> T.Text
typeRefText (RT_Primitive p) = formatText p
typeRefText (RT_Param p) = p
typeRefText (RT_Named (ScopedName (ModuleName mname) name,_))
   | null mname = name
   | otherwise   = T.intercalate "." mname <> "." <> name
-- | A type is an enumeration if it's a union with all void fields.

isEnumeration2 :: ResolvedType -> Bool
isEnumeration2 (RT_Named (ScopedName mname name,Decl{d_type=Decl_Union u})) = isEnumeration u
isEnumeration2 _ = False

----------------------------------------------------------------------
dbName :: T.Text -> T.Text
dbName = snakify

getLiteralField :: JS.Value -> T.Text -> Maybe JS.Value
getLiteralField (JS.Object map) field = HM.lookup field map
getLiteralField _ _ = Nothing

withCommas :: [a] -> [(a,T.Text)]
withCommas [] = []
withCommas [l] = [(l," ")]
withCommas (l:ls) = (l,","):withCommas ls

dbTableType = ScopedName (ModuleName ["common","db"]) "DbTable"
dbKeyType = ScopedName (ModuleName ["common","db"]) "DbKey"
pkType = ScopedName (ModuleName ["common","db"]) "PK"
instantType = ScopedName (ModuleName ["common"]) "Instant"
localDateType = ScopedName (ModuleName ["common"]) "LocalDate"
maybeType = ScopedName (ModuleName ["sys","types"]) "Maybe"
