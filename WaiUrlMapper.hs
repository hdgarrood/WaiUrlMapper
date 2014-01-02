{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module WaiUrlMapper where

import Control.Monad.Writer
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

type Path = [Text]
type UrlMap = [(Path, Application)]
type UrlMapM = Writer UrlMap ()

mount :: ToApplication a => Path -> a -> UrlMapM
mount prefix thing = tell [(prefix, toApplication thing)]

-- A little helper function, since most of the time, apps are mounted under
-- a single path segment.
mount' :: ToApplication a => Text -> a -> UrlMapM
mount' prefix thing = mount [prefix] thing

-- Another little helper function. Use this for the last mounted
-- application in the block, to avoid 500 errors from none matching.
mountRoot :: ToApplication a => a -> UrlMapM
mountRoot = mount []

runUrlMapM :: UrlMapM -> UrlMap
runUrlMapM = execWriter

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
    toApplication urlMap = \req ->
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

mapUrls :: UrlMapM -> Application
mapUrls = toApplication

-- And here's some example code which uses it:

class ToLazyByteString a where
    toLBS :: a -> BL.ByteString

instance ToLazyByteString B.ByteString where
    toLBS = BL.fromChunks . (: [])

instance ToLazyByteString String where
    toLBS = BL.pack . map (fromIntegral . ord)

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

urlmap :: UrlMapM
urlmap = do
    mount' "one" oneApp
    mount' "two" $ do
        -- Note that (by design) this cannot 'fall up' into the outer do
        -- block. So if we get here, it will have to either match the mapping
        -- below, or we'll get a 500 error.
        mount ["three", "four"] twoApp
    mountRoot defApp

main :: IO ()
main = run 3000 $ mapUrls urlmap
