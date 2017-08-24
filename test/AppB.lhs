> {-# LANGUAGE DeriveGeneric #-}
>
> module AppB where
>
> ------------------------------------------------------------------------------
> import           Armor
> import           Control.Lens
> import           Data.Aeson
> import           Data.ByteString      (ByteString)
> import           Data.ByteString.Lazy (fromStrict, toStrict)
> import qualified Data.Map             as M
> import qualified Data.Text            as T
> import           Data.Text.Encoding
> import           Data.Typeable
> import           GHC.Generics
> import           Text.Read
> ------------------------------------------------------------------------------
> import           AppA                 (showPrism, aesonPrism)
> ------------------------------------------------------------------------------

Time goes by and we discover we need to add a new field to Employee.

> data Employee = Employee
>     { employeeFirstName :: String
>     , employeeLastName  :: String
>     , employeeTenure    :: Int
>     , employeeAge       :: Maybe Int
>     } deriving (Eq, Ord, Show, Read, Typeable, Generic)

The EmployeeLevel data type stays the same.

> data EmployeeLevel = Executive | Manager | Worker
>   deriving (Eq, Ord, Show, Read, Typeable, Generic)
>
> instance FromJSON Employee
> instance ToJSON Employee
>
> instance FromJSON EmployeeLevel
> instance ToJSON EmployeeLevel

We update the `Armored` instance to version 1. If you forget to updated the
version, the armor tests should still fail because the existing version 0
serialization files will not be overwritten.

NOTE / TODO: In the future we may be able to have explicit checking for this
situation and have special alerts to change the version number.

> instance Armored Employee where
>     version = 1
>     serializations = M.fromList
>         [ ("show", showPrism)
>         , ("aeson", aesonPrism)
>         ]
>
> instance Armored EmployeeLevel where
>     version = 0
>     serializations = M.fromList
>         [ ("show", showPrism)
>         , ("aeson", aesonPrism)
>         ]

