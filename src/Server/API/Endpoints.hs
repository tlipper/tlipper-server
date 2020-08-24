{-# LANGUAGE CPP #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.Endpoints where

import Control.Monad
import Data.Monoid
import Data.Proxy
import Data.Text (Text, pack)
import GHC.TypeLits
import Network.HTTP.Types (Method)
import Network.Wai
import Servant

-- | Following is taken from:
-- Commit: https://github.com/dminuoso/servant-prometheus/commit/13dca8df604fd45df768becf0f33748b8ccc0431
-- Link: https://github.com/dminuoso/servant-prometheus/blob/master/lib/Servant/Prometheus.hs#L129
class HasEndpoints a where
  getEndpoints :: Proxy a -> [([Text], Method)]
  getEndpoint :: Proxy a -> Request -> Maybe ([Text], Method)

instance (HasEndpoints (a :: *), HasEndpoints (b :: *)) =>
         HasEndpoints (a :<|> b) where
  getEndpoints _ =
    getEndpoints (Proxy :: Proxy a) ++ getEndpoints (Proxy :: Proxy b)
  getEndpoint _ req =
    getEndpoint (Proxy :: Proxy a) req `mplus`
    getEndpoint (Proxy :: Proxy b) req

instance (KnownSymbol (path :: Symbol), HasEndpoints (sub :: *)) =>
         HasEndpoints (path :> sub) where
  getEndpoints _ = do
    (end, method) <- getEndpoints (Proxy :: Proxy sub)
    return (pack (symbolVal (Proxy :: Proxy path)) : end, method)
  getEndpoint _ req =
    case pathInfo req of
      p:ps
        | p == pack (symbolVal (Proxy :: Proxy path)) -> do
          (end, method) <- getEndpoint (Proxy :: Proxy sub) req {pathInfo = ps}
          return (p : end, method)
      _ -> Nothing
#if MIN_VERSION_servant(0,13,0)
#define CAPTURE Capture' mods
#define HEADER Header' mods
#define QUERY_PARAM QueryParam' mods
#define REQ_BODY ReqBody' mods
#else
#define CAPTURE Capture
#define HEADER Header
#define QUERY_PARAM QueryParam
#define REQ_BODY ReqBody
#endif
instance (KnownSymbol (capture :: Symbol), HasEndpoints (sub :: *)) =>
         HasEndpoints (CAPTURE capture a :> sub) where
  getEndpoints _ = do
    (end, method) <- getEndpoints (Proxy :: Proxy sub)
    let p = pack $ (':' :) $ symbolVal (Proxy :: Proxy capture)
    return (p : end, method)
  getEndpoint _ req =
    case pathInfo req of
      _:ps -> do
        (end, method) <- getEndpoint (Proxy :: Proxy sub) req {pathInfo = ps}
        let p = pack $ (':' :) $ symbolVal (Proxy :: Proxy capture)
        return (p : end, method)
      _ -> Nothing

instance HasEndpoints (sub :: *) => HasEndpoints (AuthProtect t :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (BasicAuth r a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (HEADER h a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) =>
         HasEndpoints (QUERY_PARAM (h :: Symbol) a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) =>
         HasEndpoints (QueryParams (h :: Symbol) a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (QueryFlag h :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (REQ_BODY cts a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (RemoteHost :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (IsSecure :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (HttpVersion :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) => HasEndpoints (Vault :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) =>
         HasEndpoints (WithNamedContext x y sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoints (Verb method status cts a) where
  getEndpoints _ = [([], method)]
    where
      method = reflectMethod (Proxy :: Proxy method)
  getEndpoint _ req =
    case pathInfo req of
      []
        | requestMethod req == method -> Just ([], method)
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoints Raw where
  getEndpoints _ = pure ([], "RAW")
  getEndpoint _ _ = Just ([], "RAW")

instance HasEndpoints EmptyAPI where
  getEndpoints _ = pure ([], "EmptyAPI")
  getEndpoint _ _ = Just ([], "EmptyAPI")
#if MIN_VERSION_servant(0,8,1)
instance HasEndpoints (sub :: *) =>
         HasEndpoints (CaptureAll (h :: Symbol) a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif

#if MIN_VERSION_servant(0,13,0)
instance HasEndpoints (sub :: *) =>
         HasEndpoints (Servant.Description s :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: *) =>
         HasEndpoints (Servant.Summary s :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif
