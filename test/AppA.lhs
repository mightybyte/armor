The easiest way to explain this package is to walk through a case study of using
it. This is a literate Haskell file in the test suite, so let's get some imports
out of the way first.

> {-# LANGUAGE DeriveGeneric #-}
>
> module AppA where
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
> import qualified Generics.SOP as SOP
> import           GHC.Generics
> import           Text.Read
> ------------------------------------------------------------------------------

Imagine you have the following data types:

> data Employee = Employee
>     { employeeFirstName :: String
>     , employeeLastName  :: String
>     , employeeTenure    :: Int
>     } deriving (Eq, Ord, Show, Read, Typeable, Generic)
>
> data EmployeeLevel = Executive | Manager | Worker
>   deriving (Eq, Ord, Show, Read, Typeable, Generic)

Ignore this for now.  It's just here for testing.

> instance SOP.Generic Employee
> instance SOP.HasDatatypeInfo Employee
>
> instance SOP.Generic EmployeeLevel
> instance SOP.HasDatatypeInfo EmployeeLevel

You want to store this data in your database as a serialized JSON blob. (That
might not very plausible for this example, but it's definitely a fairly common
thing, so suspend disbelief for a moment.)

> instance FromJSON Employee
> instance ToJSON Employee
>
> instance FromJSON EmployeeLevel
> instance ToJSON EmployeeLevel

Now, to use the armor package you need to define an `Armored` instance for your
data type. To do that you need to define two things. A version number and a list
of serializations you want armored. We'll discuss the serializations in more
detail below.

One notable point about the serializations is that we need to be able to create
a unique identifier for them later. So armor requires a `Map String (APrism'
ByteString a)` where the `String` is a unique and hopefully meaningful
identifier for this serialization.

> instance Armored Employee where
>     version = Version 0
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

This tutorial is a part of the armor test suite, and since we don't want armor
to depend on any specific serialization packages we're using Show as an example
of how armor supports any number of serialiaztions.

A serialization is simply a pair of a serialization function that converts your
data type to ByteString and a deserialization function that converts a
ByteString to a Maybe of your data type. If you're familiar with the lens
package, this is a prism, so that's what we use here.

> showPrism :: (Read a, Show a) => Prism' ByteString a
> showPrism =
>     prism' (encodeUtf8 . T.pack . show) (readMaybe . T.unpack . decodeUtf8)
>
> aesonPrism :: (FromJSON a, ToJSON a) => Prism' ByteString a
> aesonPrism =
>     prism' (toStrict . encode) (decode . fromStrict)
>

Once you have defined your `Armored` instances, the next step is to define your
tests.  To see an example of that go here:

https://github.com/TaktInc/armor/blob/master/test/TestAppA.lhs
