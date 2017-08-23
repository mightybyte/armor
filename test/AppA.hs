{-# LANGUAGE DeriveGeneric #-}

module AppA where

------------------------------------------------------------------------------
import           Armor
import           Control.Lens
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Data.Typeable
import           Data.Word
import           GHC.Generics
import           Text.Read
------------------------------------------------------------------------------

showPrism :: (Read a, Show a) => Prism' ByteString a
showPrism =
    prism' (encodeUtf8 . T.pack . show) (readMaybe . T.unpack . decodeUtf8)

aesonPrism :: (FromJSON a, ToJSON a) => Prism' ByteString a
aesonPrism =
    prism' (toStrict . encode) (decode . fromStrict)

data Employee = Employee
    { personFirstName :: String
    , personLastName  :: String
    , personTenure    :: (Int, Int)
    } deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance FromJSON Employee
instance ToJSON Employee

instance Armored Employee where
    version = 1
    serializations = M.fromList
        [ ("show", showPrism)
        , ("aeson", aesonPrism)
        ]

data EmployeeLevel = Executive | Manager | Worker
  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance FromJSON EmployeeLevel
instance ToJSON EmployeeLevel

instance Armored EmployeeLevel where
    version = 0
    serializations = M.fromList
        [ ("show", showPrism)
        , ("aeson", aesonPrism)
        ]

