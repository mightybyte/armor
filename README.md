# Armor

Armor yourself against backwards-incompatible serializations once and for all.

## Motivation

As almost everyone with significant experience managing production software
systems knows, backwards compatibility is incredibly important for any data that
is persisted by an application. If you make a change to a data structure that is
not backwards compatible with the existing serialized formats, your app will
break as soon as it encounters the existing format. Even if you have 100% test
coverage, your tests still might not catch this problem because it's not a
problem with your app at any single point in time, but a problem with how your
app evolves over time.

More subtly, if you deploy a backwards incompatible migration, your app may
persist some data in the new format before it crashes on the old format. This
can leave your system in the horrible state where not only will it not work with
the new code, but rolling back to the old code will also break because the old
code doesn't support the new serialized format! You have two incomptable
serializations active at the same time!

Proper migration systems can reduce the chances of this problem occurring, but
if your system has any kind of queueing system or message bus, your migrations
might not be applied to in-flight messages. Clearly we need something to help us
protect against this problem.  Enter `armor`.

For an overview of how to use this package, [check out the test suite](https://github.com/TaktInc/armor/blob/master/test/AppA.lhs).

## Credits

Inspiration for this package came from [Soostone's safecopy-hunit package](https://github.com/Soostone/safecopy-hunit).

Details were refined in production at [Formation](http://formation.ai/)
(previously Takt).
