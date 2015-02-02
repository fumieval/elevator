0.2.0.1
---------------------------------
* Re-exported some `extensible` combinators.
* Added `mapGondolas`.

0.2
---------------------------------
* Switched to use extensible. Things will be faster and saner.
* `elevate` is now more flexible.
* `elevate` no longer has eager strategy, revealing potential ambiguousness.

0.1.3
---------------------------------
* Prevent the reckress type checking of GHC so that it works on older GHC and shows sane signature.

0.1.2
---------------------------------
* Added `Identity` monad to the default definition of `Floors`.

0.1.1
---------------------------------
* Added `elevate = id` pragma for efficiency.
