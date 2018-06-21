#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import ADL.Utils.IndentedCode(codeText)
import ADL.Compiler.EIO
import ADL.Compiler.Flags(parseArguments,standardOptions,Flags(..))
import ADL.Compiler.Processing(loadAndCheckModule,defaultAdlFlags)
import ADL.Compiler.Utils(writeOutputFile)
import ADL.Sql.SchemaUtils(schemaFromAdl,columnFromField,sqlFromSchema)
import ADL.Sql.JavaTables(generateJavaTables)
import ADL.Http.JavaReqs(generateJavaReqs)
import Control.Monad.Trans(liftIO)
import Data.Monoid((<>), mconcat, mempty)
import Data.Traversable(for)
import System.Environment(getArgs)
import System.Exit(exitWith,ExitCode(..))
import System.IO(stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
   ("sql":args) -> exitOnError $ generateSchema args
   ("java-tables":args) -> exitOnError $ generateJavaTables args
   ("java-http-reqs":args) -> exitOnError $ generateJavaReqs args
   _ -> do
     T.hPutStrLn stderr "Usage:"
     T.hPutStrLn stderr "    hx-adl sql ...args..."
     T.hPutStrLn stderr "    hx-adl java-tables ...args..."
     T.hPutStrLn stderr "    hx-adl java-http-reqs ...args..."
     exitWith (ExitFailure 1)

generateSchema :: [String] -> EIO T.Text ()
generateSchema args = do
  (flags,paths) <- parseArguments header defaultAdlFlags () standardOptions args

  -- Generate a schema for each adl file
  schemas <- for paths $ \path -> do
    rmodule <- loadAndCheckModule (f_adl flags) path
    return (schemaFromAdl columnFromField rmodule)

  -- Compute the SQL for the aggregate schema
  let schema = mconcat schemas
      sql = sqlFromSchema schema
      t = T.intercalate "\n" (codeText Nothing sql)

  -- Write it out
  liftIO $ writeOutputFile (f_output flags) "create.sql" (LBS.fromStrict (T.encodeUtf8 t))
  where
    header = "Usage: hx-adl sql ...args..."
