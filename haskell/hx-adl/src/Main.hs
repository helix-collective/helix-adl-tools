#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import ADL.Utils.IndentedCode(codeText)
import ADL.Compiler.EIO
import ADL.Compiler.Flags(parseArguments,standardOptions,updateBackendFlags,Flags(..))
import ADL.Compiler.Processing(loadAndCheckModule,defaultAdlFlags)
import ADL.Compiler.Utils(writeOutputFile)
import ADL.Sql.SchemaUtils(schemaFromAdl,sqlFromSchema, DbProfile, postgresDbProfile, postgresDbProfileV2, mssqlDbProfile)
import ADL.Sql.JavaTables(generateJavaTables)
import ADL.Http.JavaReqs(generateJavaReqs)
import ADL.Http.TypescriptReqs(generateTypescriptReqs)
import Control.Monad.Trans(liftIO)
import Data.Monoid((<>), mconcat, mempty)
import Data.Traversable(for)
import System.Environment(getArgs)
import System.Exit(exitWith,ExitCode(..))
import System.Console.GetOpt
import System.IO(stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
   ("sql":args) -> exitOnError $ generateSchema args
   ("java-tables":args) -> exitOnError $ generateJavaTables args
   ("java-http-reqs":args) -> exitOnError $ generateJavaReqs args
   ("typescript-http-reqs":args) -> exitOnError $ generateTypescriptReqs args
   _ -> do
     T.hPutStrLn stderr "Usage:"
     T.hPutStrLn stderr "    hx-adl sql ...args..."
     T.hPutStrLn stderr "    hx-adl java-tables ...args..."
     T.hPutStrLn stderr "    hx-adl java-http-reqs ...args..."
     T.hPutStrLn stderr "    hx-adl typescript-http-reqs ...args..."
     exitWith (ExitFailure 1)


data SchemaFlags = SchemaFlags {
  sf_dbProfile :: DbProfile
}

defaultSchemaFlags = SchemaFlags postgresDbProfile

schemaOptions :: [OptDescr (Flags SchemaFlags -> Flags SchemaFlags)]
schemaOptions
  =  [ Option "" ["postgres"] (NoArg (updateBackendFlags (\f -> f{sf_dbProfile=postgresDbProfile})))
       "Generate postgres compatible sql (default)"

     ,  Option "" ["postgres-v2"] (NoArg (updateBackendFlags (\f -> f{sf_dbProfile=postgresDbProfileV2})))
       "Generate postgres 9.6+ compatible sql (eg including jsonb)"

     ,  Option "" ["mssql"] (NoArg (updateBackendFlags (\f -> f{sf_dbProfile=mssqlDbProfile})))
       "Generate microsoft sqlserver compatible sql"
     ]
  <> standardOptions

generateSchema :: [String] -> EIO T.Text ()
generateSchema args = do
  (flags,paths) <- parseArguments header defaultAdlFlags defaultSchemaFlags schemaOptions args
  let dbp = sf_dbProfile (f_backend flags)

  -- Generate a schema for each adl file
  schemas <- for paths $ \path -> do
    rmodule <- loadAndCheckModule (f_adl flags) path
    return (schemaFromAdl dbp rmodule)

  -- Compute the SQL for the aggregate schema
  let schema = mconcat schemas
      sql = sqlFromSchema dbp schema
      t = T.intercalate "\n" (codeText Nothing sql)

  -- Write it out
  liftIO $ writeOutputFile (f_output flags) "create.sql" (LBS.fromStrict (T.encodeUtf8 t))
  where
    header = "Usage: hx-adl sql ...args..."
