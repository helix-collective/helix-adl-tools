{-# LANGUAGE OverloadedStrings #-}

module ADL.Http.TypescriptReqs where

import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified ADL.Compiler.AST as AST
import qualified ADL.Compiler.Backends.Typescript.Internal as TS

import ADL.Compiler.EIO
import ADL.Compiler.Primitive
import ADL.Compiler.Processing(AdlFlags(..),ResolvedType, RModule,RDecl,defaultAdlFlags,loadAndCheckModule1,removeModuleTypedefs, expandModuleTypedefs, associateCustomTypes, refEnumeration, refNewtype, ResolvedTypeT(..), expandTypedefs, fullyScopedModule)
import ADL.Compiler.Utils(FileWriter,writeOutputFile)
import ADL.Compiler.Flags(Flags(..),parseArguments,standardOptions, addToMergeFileExtensions)
import ADL.Http.Utils(GetReq(..), PostReq(..), PutReq, RequestType(..), RequestDecl(..), getRequestDecl)
import ADL.Utils.IndentedCode
import ADL.Utils.Format(template,formatText)
import Cases(snakify)
import Control.Monad(when)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict
import Data.Char(toUpper, isUpper)
import Data.Foldable(for_)
import Data.Traversable(for)
import Data.Monoid
import Data.Maybe(mapMaybe)
import System.Directory(createDirectoryIfMissing)
import System.FilePath(takeDirectory,(</>),(<.>))
import System.Console.GetOpt(OptDescr(..), ArgDescr(..))


data TypescriptReqFlags = TypescriptReqFlags {
  tsRuntimeDir :: FilePath
}

typescriptReqOptions =
  [ Option "" ["runtime-dir"]
      (ReqArg (\s f -> f{f_backend=(f_backend f){tsRuntimeDir=s}}) "DIR")
      "The  package where the typescript runtime can be found"
  ]

generateTypescriptReqs :: [String] -> EIO T.Text ()
generateTypescriptReqs args = do
  let header = "Usage: hx-adl typescript-http-reqs ...args..."
      options =  standardOptions <> typescriptReqOptions
      defaultReqFlags = TypescriptReqFlags ""

  (flags0,paths) <- parseArguments header defaultAdlFlags defaultReqFlags options args
  let fileWriter = writeOutputFile (f_output flags)
      flags = addToMergeFileExtensions "adl-ts" flags0

  for_ paths $ \path -> do
    (mod0,moddeps) <- loadAndCheckModule1 (f_adl flags) path
    let m' = fullyScopedModule mod0
    generateModule (f_backend flags) fileWriter m'

generateModule :: TypescriptReqFlags ->
                  FileWriter ->
                  RModule ->
                  EIO T.Text ()
generateModule tf fileWriter m0 = do
  let moduleName = requestsModuleName (AST.m_name m)
      m = associateCustomTypes TS.getCustomType moduleName m0
      cgp = TS.CodeGenProfile {TS.cgp_includeAst = False, TS.cgp_includeAstAnnotation = const False}
      requestDecls = mapMaybe getRequestDecl (M.elems (AST.m_decls m))
      mf = execState (genRequestsModule m requestDecls) (TS.emptyModuleFile moduleName cgp)
      filepath = TS.moduleFilePath (AST.unModuleName moduleName) <.> "ts"
  case requestDecls of
    [] -> return ()
    _ -> liftIO $ fileWriter filepath (TS.genModuleCode "hx-adl" mf)

type CRequestDecl = RequestDecl (Maybe TS.CustomType)

genRequestsModule :: TS.CModule ->  [CRequestDecl] -> TS.CState ()
genRequestsModule m requestDecls = do
  TS.addImport "ADL" (TS.TSImport "ADL" ["runtime","adl"])
  TS.addImport "JSON" (TS.TSImport "JSON" ["runtime","json"])

  TS.addDeclaration
    $  cline "export interface PostRequest<I,O> {"
    <> indent
       (  cline "path: string,"
       <> cline "reqJB: JSON.JsonBinding<I>,"
       <> cline "respJB: JSON.JsonBinding<O>,"
       )
    <> cline "};"

  TS.addDeclaration
    $  cline "export interface GetRequest<O> {"
    <> indent
       (  cline "path: string,"
       <> cline "respJB: JSON.JsonBinding<O>,"
       )
    <> cline "};"

  TS.addDeclaration
    $  cline "export interface PutRequest<I,O> {"
    <> indent
       (  cline "path: string,"
       <> cline "reqJB: JSON.JsonBinding<I>,"
       <> cline "respJB: JSON.JsonBinding<O>,"
       )
    <> cline "};"

  typeExprs <- mapM requestTypeExpr requestDecls

  TS.addDeclaration
    $  cline "export interface RequestTypes {"
    <> indent
       (  mconcat [ctemplate "$1: $2," [requestName rd, typeExpr] | (rd,typeExpr) <- zip requestDecls typeExprs]
       )
    <> cline "};"

  requestVariables <- mapM requestVariable requestDecls

  TS.addDeclaration
    $  cline "export function makeRequestTypes(resolver: ADL.DeclResolver): RequestTypes {"
    <> indent
       (  mconcat (L.intersperse (cline "") requestVariables)
       <> cline "return {"
       <> indent
          ( mconcat [ctemplate "$1," [requestName rd] | rd <-requestDecls]
          )
       <> cline "}"
       )
    <> cline "};"


requestsModuleName :: AST.ModuleName -> AST.ModuleName
requestsModuleName (AST.ModuleName []) = error "BUF: no module name"
requestsModuleName (AST.ModuleName ns) = AST.ModuleName (init ns <> [last ns <> "-rtypes"])

requestName :: RequestDecl r -> T.Text
requestName rd = lower1 (AST.d_name (rd_decl rd))

requestTypeExpr :: CRequestDecl -> TS.CState T.Text
requestTypeExpr rd@RequestDecl{rd_type=RT_Get (GetReq otype)} = do
  otexpr <- TS.genTypeExpr otype
  return (template "GetRequest<$1>" [otexpr])
requestTypeExpr rd@RequestDecl{rd_type=RT_Post (PostReq itype otype)} = do
  itexpr <- TS.genTypeExpr itype
  otexpr <- TS.genTypeExpr otype
  return (template "PostRequest<$1, $2>" [itexpr, otexpr])
requestTypeExpr rd@RequestDecl{rd_type=RT_Put (PostReq itype otype)} = do
  itexpr <- TS.genTypeExpr itype
  otexpr <- TS.genTypeExpr otype
  return (template "PutRequest<$1, $2>" [itexpr, otexpr])

requestVariable:: CRequestDecl -> TS.CState Code
requestVariable rd = do
  hpath <- case M.lookup snHttpPath (AST.d_annotations (rd_decl rd)) of
    Just (_,JS.String hpath) -> return hpath
    _ -> error ("Request " <> T.unpack (AST.d_name (rd_decl rd)) <> " is missing a Path attribute")

  fields <- getFields hpath (rd_type rd)
  return
    (  TS.renderCommentForDeclaration (rd_decl rd)
    <> ctemplate "const $1 = {" [requestName rd]
    <> indent fields
    <> cline "}"
    )
  where
    getFields hpath (RT_Post (PostReq itype otype)) = do
      itvexpr <- TS.genTypeValueExpr itype
      otvexpr <- TS.genTypeValueExpr otype
      return
        (  ctemplate "path: \"$1\"," [hpath]
        <> ctemplate "reqJB: JSON.createJsonBinding(resolver, $1)," [itvexpr]
        <> ctemplate "respJB: JSON.createJsonBinding(resolver, $1)," [otvexpr]
        )
    getFields hpath (RT_Get (GetReq otype)) = do
      otvexpr <- TS.genTypeValueExpr otype
      return
        (  ctemplate "path: \"$1\"," [hpath]
        <> ctemplate "respJB: JSON.createJsonBinding(resolver, $1)," [otvexpr]
        )
    getFields hpath (RT_Put (PostReq itype otype)) = do
      itvexpr <- TS.genTypeValueExpr itype
      otvexpr <- TS.genTypeValueExpr otype
      return
        (  ctemplate "path: \"$1\"," [hpath]
        <> ctemplate "reqJB: JSON.createJsonBinding(resolver, $1)," [itvexpr]
        <> ctemplate "respJB: JSON.createJsonBinding(resolver, $1)," [otvexpr]
        )

snHttpPath :: AST.ScopedName
snHttpPath = AST.ScopedName (AST.ModuleName ["common","http"]) "Path"

upper1,lower1 :: T.Text -> T.Text
upper1 t = T.toUpper (T.pack [(T.head t)]) `T.append` T.tail t
lower1 t = T.toLower (T.pack [(T.head t)]) `T.append` T.tail t
