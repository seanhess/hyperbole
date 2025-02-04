module Example.Cache where

import Network.HTTP.Types (Header)
import Network.Wai.Middleware.Static

clientCache :: IO Options
clientCache = do
  container <- initCaching PublicStaticCaching
  -- container <- initCaching (CustomCaching customCache)
  pure $ defaultOptions{cacheContainer = container}

-- for testing if caching is working
customCache :: FileMeta -> [Header]
customCache (FileMeta lm etag _file) = do
  [("Cache-Control", "no-transform,public,max-age=30"), ("Last-Modified", lm), ("Etag", etag)]
