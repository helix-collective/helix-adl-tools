{-# LANGUAGE OverloadedStrings #-}

module ADL.Http.Utils where

import qualified Data.Text as T
import qualified ADL.Compiler.AST as AST

import ADL.Compiler.Processing(ResolvedTypeT(..), expandTypedefs)

data GetReq r = GetReq {
  gr_resp :: AST.TypeExpr r
}

data PostReq r = PostReq {
  pr_req :: AST.TypeExpr r,
  pr_resp :: AST.TypeExpr r
}

type PutReq r = PostReq r

data RequestType r = RT_Get (GetReq r) | RT_Post (PostReq r) | RT_Put (PutReq r)

data RequestDecl ct = RequestDecl {
  rd_decl :: AST.Decl ct (ResolvedTypeT ct),
  rd_type :: RequestType (ResolvedTypeT ct)
}

-- matches a declaration of a post, put or get request, of the form:
--
--   type X = Get<HelloReq>;
--   type Y = Post<HelloReq, HelloResp>;
--   type Z = Put<HelloReq, HelloResp>;
getRequestDecl :: AST.Decl ct (ResolvedTypeT ct) -> Maybe (RequestDecl ct)
getRequestDecl decl = case AST.d_type decl of
  (AST.Decl_Typedef (AST.Typedef [] (AST.TypeExpr (RT_Named (sn,_)) [resp])))
    | sn == getReqScopedName -> Just (RequestDecl decl (RT_Get (GetReq  (ex resp))))
  (AST.Decl_Typedef (AST.Typedef [] (AST.TypeExpr (RT_Named (sn,_)) [req,resp])))
    | sn == postReqScopedName -> Just (RequestDecl decl (RT_Post (PostReq  (ex req) (ex resp))))
  (AST.Decl_Typedef (AST.Typedef [] (AST.TypeExpr (RT_Named (sn,_)) [req,resp])))
    | sn == putReqScopedName -> Just (RequestDecl decl (RT_Put (PostReq  (ex req) (ex resp))))
  _ -> Nothing
  where
    ex = expandTypedefs


getReqScopedName = AST.ScopedName (AST.ModuleName ["common","http"]) "Get"
postReqScopedName = AST.ScopedName (AST.ModuleName ["common","http"]) "Post"
putReqScopedName = AST.ScopedName (AST.ModuleName ["common","http"]) "Put"
