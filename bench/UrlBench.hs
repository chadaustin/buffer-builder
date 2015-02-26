{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main (main) where

import qualified Data.BufferBuilder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Criterion
import Criterion.Main
import Data.Word (Word16)
import Data.Monoid
import Control.Monad
import Network.HTTP.Types (urlEncode)

type Protocol = BS.ByteString
type PathPiece = BS.ByteString

type Hash = BS.ByteString

data SMaybe a = SNothing | SJust !a
    deriving (Eq, Show, Ord) 

data UrlHost = UrlHost
    { uhHostName :: !BS.ByteString
    , uhPort     :: {-# UNPACK #-} !Word16         -- 0 = no port
    , uhUserName :: !BS.ByteString  -- empty = no username
    , uhPasswd   :: !BS.ByteString  -- empty = no password
    } deriving (Show, Ord, Eq)

{- | The type of a URL's query arguments.
   The RFC does not specify any particular format for this section of a URL, but, since
   it is so common to use form encoding, we include a representation which is a list of
   parsed key=value&key=value pairs.

   When parsing a URL, we first assume that the query is form encoded.  If this parse fails,
   the 'UnparsedQuery' constructor is instead used to encode a URL whose query encoding is
   unknown.
-}
data Query
    = UnparsedQuery !BS.ByteString
    | ParsedQuery [(BS.ByteString, BS.ByteString)]
      deriving (Show, Ord, Eq)
               
{- | Symbolic URL type.
   It is intended that this type can capture all \"conventional\" URLs losslessly and symbolically.

   In short, if you ever find yourself resorting to direct string parsing or generation, you are
   either doing it wrong, or this library is missing something you need.
 -}

data UrlHead = FullyQualified !Protocol !UrlHost
             | ProtocolRelative !UrlHost
             | Absolute
             | Relative
             deriving (Eq, Ord, Show)

data Url = Url
    { urlHead     :: !UrlHead
    , urlPath     :: ![PathPiece]
    , urlQuery    :: !(SMaybe Query)
    , urlHash     :: !(SMaybe Hash)
    } deriving (Eq, Ord, Show)


builderToBS :: BSB.Builder -> BS.ByteString
builderToBS = BSL.toStrict . BSB.toLazyByteString

escapeString :: BS.ByteString -> BSB.Builder
escapeString = BSB.byteString . urlEncode False

toByteString :: Url -> BS.ByteString
toByteString = builderToBS . toBuilder

toBuilder :: Url -> BSB.Builder
toBuilder Url{..} =
    renderHead urlHead
    <> renderPath urlPath
    <> renderQuery urlQuery
    <> renderHash urlHash
  where
    toBB = BSB.byteString
    char = BSB.char7

    renderHead :: UrlHead -> BSB.Builder
    renderHead (FullyQualified protocol host) = toBB protocol <> char ':' <> renderHost host <> char '/'
    renderHead (ProtocolRelative host) = renderHost host <> char '/'
    renderHead Absolute = char '/'
    renderHead Relative = mempty

    renderHost :: UrlHost -> BSB.Builder
    renderHost UrlHost{..} = char '/' <> char '/' <> renderUser uhUserName uhPasswd <> renderHostName uhHostName uhPort

    renderPath :: [PathPiece] -> BSB.Builder
    renderPath [] = mempty
    renderPath path = interc (char '/') (map escapeString path)

    renderQuery :: SMaybe Query -> BSB.Builder
    renderQuery SNothing = mempty
    renderQuery (SJust (UnparsedQuery q)) = char '?' <> toBB q
    renderQuery (SJust (ParsedQuery pairs)) = char '?' <> interc (char '&') (map renderPair pairs)

    renderPair (k, v) = escapeString k <> char '=' <> escapeString v

    renderHash h = case h of
        SNothing -> mempty
        SJust hash -> char '#' <> toBB hash

    renderHostName hostName hnPort
        | 0 /= hnPort = toBB hostName <> char ':' <> BSB.intDec (fromIntegral hnPort)
        | otherwise   = toBB hostName

    renderUser uhName uhPasswd
        | BS.null uhName = mempty
        | BS.null uhPasswd = escapeString uhName <> char '@'
        | otherwise = escapeString uhName <> char ':' <> escapeString uhPasswd <> char '@'

    interc _ [] = mempty
    interc _ [x] = x
    interc sep (x:y:xs) = x <> sep <> interc sep (y:xs)

renderHost' :: UrlHost -> BB.BufferBuilder ()
renderHost' UrlHost{..} = do
    BB.appendChar8 '/'
    BB.appendChar8 '/'

    when (not $ BS.null uhUserName) $ do
        BB.appendBS $ urlEncode False uhUserName
        when (not $ BS.null uhPasswd) $ do
            BB.appendChar8 ':'
            BB.appendBS $ urlEncode False uhPasswd
        BB.appendChar8 '@'

    BB.appendBS uhHostName
    if (uhPort /= 0) then do
        BB.appendChar8 ':'
        BB.appendDecimalSignedInt (fromIntegral uhPort)
    else
        return ()
        
    BB.appendChar8 '/'

renderPair :: (BS.ByteString, BS.ByteString) -> BB.BufferBuilder ()
renderPair (key, value) = do
    BB.appendBS $ urlEncode False key
    BB.appendChar8 '='
    BB.appendBS $ urlEncode False value
    
render :: Url -> BB.BufferBuilder ()
render Url{..} = do
    case urlHead of
        (FullyQualified protocol host) -> do
            BB.appendBS protocol
            BB.appendChar8 ':'
            renderHost' host
        (ProtocolRelative host) -> renderHost' host
        Absolute -> BB.appendChar8 '/'
        Relative -> return ()

    case map (urlEncode False) urlPath of
        [] -> return ()
        (x:xs) -> do
            BB.appendBS x
            forM_ xs $ \ps -> do
                BB.appendChar8 '/'
                BB.appendBS ps

    case urlQuery of
        SNothing -> return ()
        (SJust (UnparsedQuery q)) -> do
            BB.appendChar8 '?'
            BB.appendBS q
        (SJust (ParsedQuery pairs)) -> do
            BB.appendChar8 '?'
            case pairs of
                [] -> return ()
                (x:xs) -> do
                    renderPair x
                    forM_ xs $ \pair -> do
                        BB.appendChar8 '&'
                        renderPair pair


    case urlHash of
        SNothing -> return ()
        (SJust h) -> do
            BB.appendChar8 '#'
            BB.appendBS h

viaBufferBuilder :: Url -> BS.ByteString
viaBufferBuilder = BB.runBufferBuilder . render

main :: IO ()
main = do
    let host = UrlHost
            { uhHostName = "example.com"
            , uhPort = 80
            , uhUserName = ""
            , uhPasswd = ""
            }

    let url = Url
            { urlHead = FullyQualified "http" host
            , urlPath = ["service", "one", "two", "three"]
            , urlQuery = SJust (ParsedQuery [("limit", "30"), ("next", "123456"), ("previous", "987654")])
            , urlHash = SJust "anchor"
            }

    BSC.putStrLn $ "bytestring builder: " <> toByteString url
    BSC.putStrLn $ "bufferbuilder: " <> viaBufferBuilder url

    defaultMain [ bgroup "render url"
                    [ bench "bufferbuilder" $ nf viaBufferBuilder url
                    , bench "bytestring builder" $ nf toByteString url
                    --, bench "vector text" $ nf Json.encodeJson (Vector.replicate 100000 ("hello world" :: Text))
                    ]
                ]
