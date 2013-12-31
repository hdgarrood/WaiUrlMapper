{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module WaiUrlMapper where

import Control.Monad.State
import Data.Monoid
import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

class ToLazyByteString a where
    toLBS :: a -> BL.ByteString

instance ToLazyByteString B.ByteString where
    toLBS = BL.fromChunks . (: [])

instance ToLazyByteString String where
    toLBS = BL.pack . map (fromIntegral . ord)

type Path = [Text]
type UrlMap = [(Path, Application)]
type UrlMapM = State UrlMap ()

prepend :: a -> State [a] ()
prepend x = modify (\s -> x : s)

mount :: ToApplication a => Path -> a -> UrlMapM
mount prefix thing = prepend (prefix, toApplication thing)

mount' :: ToApplication a => Text -> a -> UrlMapM
mount' prefix thing = mount [prefix] thing

mountRoot :: ToApplication a => a -> UrlMapM
mountRoot = mount []

runUrlMapM :: UrlMapM -> UrlMap
runUrlMapM x = reverse $ execState x []

-- try ["hello", "world"]
--      [(["hello"], lol)]
--
-- ==> Just (["world", lol])
try :: Eq a
    => [a]        -- ^ Path info of request
    -> [([a], b)] -- ^ List of applications to match
    -> Maybe ([a], b)
try xs tuples = foldl go Nothing tuples
    where
        go (Just x) _     = Just x
        go _ (prefix, y)  = stripPrefix prefix xs >>= \xs' -> return (xs', y)

class ToApplication a where
    toApplication :: a -> Application

instance ToApplication Application where
    toApplication = id

instance ToApplication UrlMap where
    toApplication urlMap = \req -> do
        case try (pathInfo req) urlMap of
            Just (newPath, app) ->
                app $ req { pathInfo = newPath
                          , rawPathInfo = makeRaw newPath
                          }
            Nothing ->
                return $ responseLBS
                    status500
                    [("content-type", "text/plain")]
                    ("WaiUrlMapper: no routes matched. Consider using " <>
                     "'mountRoot for the last mapping in the 'do' block.\n")

        where
        makeRaw :: [Text] -> B.ByteString
        makeRaw = ("/" `B.append`) . T.encodeUtf8 . T.intercalate "/"

instance ToApplication UrlMapM where
    toApplication = toApplication . runUrlMapM

-- A friendlier alias
mapUrls :: UrlMapM -> Application
mapUrls = toApplication

trivialApp :: BL.ByteString -> Application
trivialApp msg req = return $
    responseLBS
        status200
        [("content-type", "text/plain")]
        (msg <>
            "\nrawPathInfo: " <> toLBS (rawPathInfo req) <>
            "\npathInfo: " <> toLBS (show $ pathInfo req) <>
            "\n")

oneApp, twoApp, defApp :: Application
oneApp = trivialApp "one"
twoApp = trivialApp "two"
defApp = trivialApp "default"

main :: IO ()
main = run 3000 $ mapUrls $ do
    mount' "one" oneApp
    mount' "two" $ do
        -- Note that (by design) this cannot 'fall up' into the outer do
        -- block. So if we get here, it will have to either match the mapping
        -- below, or we'll get a 500 error.
        mount ["three", "four"] twoApp
    mountRoot defApp
