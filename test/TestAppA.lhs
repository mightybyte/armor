> module TestAppA where
>
> ------------------------------------------------------------------------------
> import           Test.HUnit
> import qualified Data.Map        as M
> ------------------------------------------------------------------------------
> import           Armor
> import           AppA
> ------------------------------------------------------------------------------

To actually enable the armoring of your data types, write tests as follows:

> aTests :: ArmorConfig -> Test
> aTests ac = TestList
>     [ testArmor ac "e1" (Employee "Bob" "Smith" 5)
>     , testArmorMany ac $ M.fromList
>         [("e", Executive) , ("m", Manager) , ("w", Worker)]
>     ]

See the documentation for the `testArmor` function for more detailed information.

With the above tests in your app, you'll see that a number of `.test` files will
be automatically created that store the serialized data and are used to check
that they can be correctly parsed.  Eventually you'll need to change the data
type in some way.  See this file for a case study of how that might play out:

https://github.com/mightybyte/armor/blob/master/test/AppB.lhs
