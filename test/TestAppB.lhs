> module TestAppB where
>
> ------------------------------------------------------------------------------
> import Test.HUnit
> import Armor
> import AppB
> ------------------------------------------------------------------------------

Since the new field is a Maybe, we update the old test with a Nothing value
because that's what we expect the serialization to do.  We also add a new test
exercising the new field.  This test should have a new value ID so it gets a
different .test file.

> bTests :: ArmorConfig -> Test
> bTests ac = TestList
>     [ testArmor ac "e1" (Employee "Bob" "Smith" 5 Nothing)
>     , testArmor ac "e2" (Employee "Jane" "Doe" 8 (Just 33))
>     , testArmor ac "e" Executive
>     , testArmor ac "m" Manager
>     , testArmor ac "w" Worker
>     ]

