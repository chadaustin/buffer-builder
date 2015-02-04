{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Criterion

import Criterion.Main

import Data.Semigroup ((<>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL

import qualified Data.BufferBuilder.Json as Json
import           Data.Text (Text)
import           Data.String (IsString)
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:))
import           Control.DeepSeq (NFData (..), force)
import qualified Data.ByteString as BS

newtype Utf8 = Utf8 { unUtf8 :: BS.ByteString }
    deriving (Show, Eq, IsString)

data EyeColor = Green | Blue | Brown
    deriving (Eq, Show)
data Gender = Male | Female
    deriving (Eq, Show)
data Fruit = Apple | Strawberry | Banana
    deriving (Eq, Show)
data Friend string = Friend
    { fId :: !Int
    , fName :: !string
    } deriving (Eq, Show)

data User string = User
    { uId       :: !string
    , uIndex    :: !Int
    , uGuid     :: !string
    , uIsActive :: !Bool
    , uBalance  :: !string
    , uPicture  :: !string
    , uAge      :: !Int
    , uEyeColor :: !EyeColor
    , uName     :: !string
    , uGender   :: !Gender
    , uCompany  :: !string
    , uEmail    :: !string
    , uPhone    :: !string
    , uAddress  :: !string
    , uAbout    :: !string
    , uRegistered   :: !string -- UTCTime?
    , uLatitude :: !Double
    , uLongitude    :: !Double
    , uTags :: ![Text]
    , uFriends  :: ![Friend string]
    , uGreeting :: !string
    , uFavouriteFruit   :: !Fruit
    } deriving (Eq, Show)

instance NFData EyeColor
instance NFData Gender
instance NFData Fruit

instance NFData str => NFData (Friend str) where
    rnf Friend {..} = (rnf fId) `seq` (rnf fName) `seq` ()

instance NFData a => NFData (User a) where
    rnf User {..} = (rnf uId) `seq` (rnf uIndex) `seq` (rnf uGuid) `seq` (rnf uIsActive) `seq` (rnf uBalance) `seq` (rnf uPicture) `seq` (rnf uAge) `seq` (rnf uEyeColor) `seq` (rnf uName) `seq` (rnf uGender) `seq` (rnf uCompany) `seq` (rnf uEmail) `seq` (rnf uPhone) `seq` (rnf uAddress) `seq` (rnf uAbout) `seq` (rnf uRegistered) `seq` (rnf uLatitude) `seq` (rnf uLongitude) `seq` (rnf uTags) `seq` (rnf uFriends) `seq` (rnf uGreeting) `seq` (rnf uFavouriteFruit) `seq` ()

eyeColorTable :: [(Text, EyeColor)]
eyeColorTable = [("brown", Brown), ("green", Green), ("blue", Blue)]

genderTable :: [(Text, Gender)]
genderTable = [("male", Male), ("female", Female)]

fruitTable :: [(Text, Fruit)]
fruitTable = [("apple", Apple), ("strawberry", Strawberry), ("banana", Banana)]

enumFromJson :: Monad m => String -> [(Text, enum)] -> (json -> m Text) -> json -> m enum
enumFromJson enumName table extract v = do
    s <- extract v
    case lookup s table of
        Just r -> return r
        Nothing -> fail $ "Bad " ++ enumName ++ ": " ++ show s

--- Aeson instances ---

instance Aeson.FromJSON EyeColor where
    parseJSON = enumFromJson "EyeColor" eyeColorTable Aeson.parseJSON

instance Aeson.FromJSON Gender where
    parseJSON = enumFromJson "Gender" genderTable Aeson.parseJSON

instance Aeson.FromJSON Fruit where
    parseJSON = enumFromJson "Fruit" fruitTable Aeson.parseJSON

instance Aeson.FromJSON str => Aeson.FromJSON (Friend str) where
    parseJSON = Aeson.withObject "Friend" $ \o -> do
        fId <- o .: "id"
        fName <- o .: "name"
        return Friend {..}

instance Aeson.FromJSON str => Aeson.FromJSON (User str) where
    parseJSON = Aeson.withObject "User" $ \o -> do
        uId <- o .: "_id"
        uIndex <- o .: "index"
        uGuid <- o .: "guid"
        uIsActive <- o .: "isActive"
        uBalance <- o .: "balance"
        uPicture <- o .: "picture"
        uAge <- o .: "age"
        uEyeColor <- o .: "eyeColor"
        uName <- o .: "name"
        uGender <- o .: "gender"
        uCompany <- o .: "company"
        uEmail <- o .: "email"
        uPhone <- o .: "phone"
        uAddress <- o .: "address"
        uAbout <- o .: "about"
        uRegistered <- o .: "registered"
        uLatitude <- o .: "latitude"
        uLongitude <- o .: "longitude"
        uTags <- o .: "tags"
        uFriends <- o .: "friends"
        uGreeting <- o .: "greeting"
        uFavouriteFruit <- o .: "favoriteFruit"
        return User {..}

instance Aeson.ToJSON EyeColor where
    toJSON ec = Aeson.toJSON $ case ec of
        Green -> "green" :: Text
        Blue -> "blue"
        Brown -> "brown"

instance Aeson.ToJSON Gender where
    toJSON g = Aeson.toJSON $ case g of
        Male -> "male" :: Text
        Female -> "female"

instance Aeson.ToJSON Fruit where
    toJSON f = Aeson.toJSON $ case f of
        Apple -> "apple" :: Text
        Banana -> "banana"
        Strawberry -> "strawberry"

instance Aeson.ToJSON str => Aeson.ToJSON (Friend str) where
    toJSON Friend {..} = Aeson.object
        [ "id" Aeson..= fId
        , "name" Aeson..= fName
        ]

instance Aeson.ToJSON str => Aeson.ToJSON (User str) where
    toJSON User{..} = Aeson.object
        [ "_id" Aeson..= uId
        , "index" Aeson..= uIndex
        , "guid" Aeson..= uGuid
        , "isActive" Aeson..= uIsActive
        , "balance" Aeson..= uBalance
        , "picture" Aeson..= uPicture
        , "age" Aeson..= uAge
        , "eyeColor" Aeson..= uEyeColor
        , "name" Aeson..= uName
        , "gender" Aeson..= uGender
        , "company" Aeson..= uCompany
        , "email" Aeson..= uEmail
        , "phone" Aeson..= uPhone
        , "address" Aeson..= uAddress
        , "about" Aeson..= uAbout
        , "registered" Aeson..= uRegistered
        , "latitude" Aeson..= uLatitude
        , "longitude" Aeson..= uLongitude
        , "tags" Aeson..= uTags
        , "friends" Aeson..= uFriends
        , "greeting" Aeson..= uGreeting
        , "favoriteFruit" Aeson..= uFavouriteFruit
        ]

--- BufferBuilder instances ---

instance Json.ToJson EyeColor where
    appendJson ec = Json.appendJson $ case ec of
        Green -> "green" :: Text
        Blue -> "blue"
        Brown -> "brown"

instance Json.ToJson Gender where
    appendJson g = Json.appendJson $ case g of
        Male -> "male" :: Text
        Female -> "female"

instance Json.ToJson Fruit where
    appendJson f = Json.appendJson $ case f of
        Apple -> "apple" :: Text
        Strawberry -> "strawberry"
        Banana -> "banana"

instance Json.ToJson str => Json.ToJson (Friend str) where
    appendJson Friend{..} = Json.appendJson $
            "_id" Json..= fId
            <> "name" Json..= fName

instance Json.ToJson str => Json.ToJson (User str) where
    appendJson User{..} = Json.appendJson $
            "_id" Json..= uId
            <> "index" Json..= uIndex
            <> "guid" Json..= uGuid
            <> "isActive" Json..= uIsActive
            <> "balance" Json..= uBalance
            <> "picture" Json..= uPicture
            <> "age" Json..= uAge
            <> "eyeColor" Json..= uEyeColor
            <> "name" Json..= uName
            <> "gender" Json..= uGender
            <> "company" Json..= uCompany
            <> "email" Json..= uEmail
            <> "phone" Json..= uPhone
            <> "address" Json..= uAddress
            <> "about" Json..= uAbout
            <> "registered" Json..= uRegistered
            <> "latitude" Json..= uLatitude
            <> "longitude" Json..= uLongitude
            <> "tags" Json..= uTags
            <> "friends" Json..= uFriends
            <> "greeting" Json..= uGreeting
            <> "favoriteFruit" Json..= uFavouriteFruit


--- ---

assumeSuccess :: Either a b -> b
assumeSuccess (Right r) = r
assumeSuccess _ = error "assumeSuccess"

main :: IO ()
main = do
    content <- B.readFile "test.json"
    let lazyContent = force $ BSL.fromChunks [content]

    let parsedUserList :: [User Text]
        Just parsedUserList = Aeson.decode lazyContent

    defaultMain [ bgroup "render"
                    [ bench "bufferbuilder" $ nf Json.encodeJson parsedUserList
                    , bench "aeson" $ nf Aeson.encode parsedUserList
                    ]
                ]
