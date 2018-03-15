> {-# LANGUAGE DeriveGeneric #-}
>
> module AppB where
>
> ------------------------------------------------------------------------------
> import           Armor
> import           Data.Aeson
> import qualified Data.Map             as M
> import           Data.Typeable
> import           GHC.Generics
> ------------------------------------------------------------------------------
> import           AppA                 (showPrism, aesonPrism)
> ------------------------------------------------------------------------------

Time goes by and we discover we need to add a new field to Employee. Since we
care about backwards compatibility and there isn't a reasonable default for the
age field, we add it as a Maybe.

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
>     version = Version 1
>     serializations = M.fromList
>         [ ("show", showPrism)
>         , ("aeson", aesonPrism)
>         ]
>
> instance Armored EmployeeLevel where
>     version = Version 0
>     serializations = M.fromList
>         [ ("show", showPrism)
>         , ("aeson", aesonPrism)
>         ]

Now go to the TestAppB module to see how we update the tests for the new field.

https://github.com/mightybyte/armor/blob/master/test/TestAppB.lhs
