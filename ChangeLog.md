# Revision history for armor

## 0.2      -- 2021-05-06

* Widen version bounds
* Support and test through GHC 9
* Expose more customizable test function
* Change default mechanics of FilePath generation

WARNING: Depending on how you use armor this is a potentially
backwards-incompatible change!  As a precaution we are doing a major version
bump even though the change would only technically need a C bump.

The best way to upgrade your app is to upgrade armor in a commit by itself
with no other changes to your app.  That way, if there armor has test case
failures, you know that they are innocuous because no part of the rest of your
app changed functionaly.  So you can simple delete the failing golds,
regenerate them, and check the new ones into source control.

## 0.1      -- 2018-03-14

* First version. Released on an unsuspecting world.
