{-# LANGUAGE OverloadedStrings #-}

module ADL.Http.JavaReqs where

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

import ADL.Compiler.EIO
import ADL.Compiler.Primitive
import ADL.Compiler.Processing(AdlFlags(..),ResolvedType, RModule,RDecl,defaultAdlFlags,loadAndCheckModule1,removeModuleTypedefs, expandModuleTypedefs, associateCustomTypes, refEnumeration, refNewtype, ResolvedTypeT(..), expandTypedefs)
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

data JavaReqFlags = JavaReqFlags {
  jt_rtpackage :: T.Text,
  jt_package :: T.Text
}

defaultJavaReqFlags = JavaReqFlags "adl.runtime" "adl"

javaReqOptions =
  [ Option "" ["rtpackage"]
      (ReqArg (\s f -> f{f_backend=(f_backend f){jt_rtpackage=T.pack s}}) "PACKAGE")
      "The  package where the ADL runtime can be found"
  , Option "" ["package"]
      (ReqArg (\s f -> f{f_backend=(f_backend f){jt_package=T.pack s}}) "PACKAGE")
      "The  package into which the generated ADL code will be placed"
  ]

generateJavaReqs :: [String] -> EIO T.Text ()
generateJavaReqs args = do
  let header = "Usage: generate.hs java-http-reqs ...args..."
      options =  standardOptions <> javaReqOptions

  (flags0,paths) <- parseArguments header defaultAdlFlags defaultJavaReqFlags options args
  let fileWriter = writeOutputFile (f_output flags)
      flags = addToMergeFileExtensions "adl-java" flags0
      cgp = J.defaultCodeGenProfile{J.cgp_runtimePackage=(J.javaPackage (jt_rtpackage (f_backend flags)))}

  for_ paths $ \path -> do
    (mod0,moddeps) <- loadAndCheckModule1 (f_adl flags) path
    let javaPackageFn = J.mkJavaPackageFn cgp (mod0:moddeps) (J.javaPackage (jt_package (f_backend flags)))
        filePath = J.javaClassFilePath (J.javaClass (javaPackageFn (AST.m_name mod0)) "Requests")
        mod = ( associateCustomTypes J.getCustomType (AST.m_name mod0)
              ) mod0
        requestDecls = mapMaybe getRequestDecl (M.elems (AST.m_decls mod))
        classfile = generateJavaReqsClassFile (f_backend flags) cgp javaPackageFn mod requestDecls
        text = (T.intercalate "\n" (codeText 1000 (J.classFileCode classfile)))
    when (not (null requestDecls)) $ do
      liftIO $ fileWriter filePath (LBS.fromStrict (T.encodeUtf8 text))

type RTypeExpr = AST.TypeExpr ResolvedType

data GetReq = GetReq {
  gr_req :: J.CTypeExpr,
  gr_resp :: J.CTypeExpr
}

data PostReq = PostReq {
  pr_req :: J.CTypeExpr,
  pr_resp :: J.CTypeExpr
}

data RequestType = RT_Get GetReq | RT_Post PostReq

data RequestDecl = RequestDecl {
  rd_decl :: J.CDecl,
  rd_type :: RequestType
}

-- matches a declaration of a post or get request, of the form:
--
--   type Hello = Post<HelloReq, HelloResp>;
getRequestDecl :: J.CDecl -> Maybe RequestDecl
getRequestDecl decl = case AST.d_type decl of
  (AST.Decl_Typedef (AST.Typedef [] (AST.TypeExpr (RT_Named (sn,_)) [req,resp])))
    | sn == getReqScopedName -> Just (RequestDecl decl (RT_Get (GetReq  (ex req) (ex resp))))
    | sn == postReqScopedName -> Just (RequestDecl decl (RT_Post (PostReq  (ex req) (ex resp))))
  _ -> Nothing
  where
    ex = expandTypedefs

-- | Generate the java http request helpers
generateJavaReqsClassFile :: JavaReqFlags -> J.CodeGenProfile -> J.JavaPackageFn-> J.CModule  -> [RequestDecl] -> J.ClassFile
generateJavaReqsClassFile flags cgp javaPackageFn mod requests = execState gen state0
  where
    state0 = J.classFile cgp (AST.m_name mod) javaPackageFn classDecl
    classDecl = "@SuppressWarnings(\"all\")\npublic class Requests"
    gen = do
      httpRequestsI <- J.addImport "au.com.helixta.adl.custom.HttpRequests"
      for_ requests $ \req -> do
        let name = requestName (AST.d_name (rd_decl req))
            docstring = J.generateDocString (AST.d_annotations (rd_decl req))
        case rd_type req of
          (RT_Get gr) -> do
            reqtype <- J.genTypeExpr (gr_req gr)
            resptype <- J.genTypeExpr (gr_resp gr)
            reqjb <- J.genJsonBindingExpr cgp (gr_req gr)
            respjb <- J.genJsonBindingExpr cgp (gr_resp gr)
            J.addMethod
              (  docstring
              <> ctemplate "public static final $1.AdlGetRequest<$2,$3> $4 = new $1.AdlGetRequest<>(" [httpRequestsI, reqtype, resptype, name]
              <> indent
                (  ctemplate "$1," [reqjb]
                <> ctemplate "$1" [respjb]
                )
              <> cline ");"
              )
          (RT_Post pr) -> do
            reqtype <- J.genTypeExpr (pr_req pr)
            resptype <- J.genTypeExpr (pr_resp pr)
            reqjb <- J.genJsonBindingExpr cgp (pr_req pr)
            respjb <- J.genJsonBindingExpr cgp (pr_resp pr)
            J.addMethod
              (  docstring
              <> ctemplate "public static final $1.AdlPostRequest<$2,$3> $4 = new $1.AdlPostRequest<>(" [httpRequestsI, reqtype, resptype, name]
              <> indent
                (  ctemplate "$1," [reqjb]
                <> ctemplate "$1" [respjb]
                )
              <> cline ");"
              )
      return ()

requestName :: T.Text -> T.Text
requestName = T.toUpper . snakify

getReqScopedName = AST.ScopedName (AST.ModuleName ["common","http"]) "Get"
postReqScopedName = AST.ScopedName (AST.ModuleName ["common","http"]) "Post"
