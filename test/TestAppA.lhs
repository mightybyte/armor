> module TestAppA where
>
> ------------------------------------------------------------------------------
> import Test.HUnit
> import Armor
> import AppA
> ------------------------------------------------------------------------------

To actually enable the armoring of your data types, write tests as follows:

> aTests :: ArmorConfig -> Test
> aTests ac = TestList
>     [ testArmor ac "e1" (Employee "Bob" "Smith" 5)
>     , testArmor ac "e" Executive
>     , testArmor ac "m" Manager
>     , testArmor ac "w" Worker
>     ]

See the documentation for the `testArmor` function for more detailed information.
