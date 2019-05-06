# Text.InterpolatedString.QQ2

QuasiQuoter for QQ2-style multi-line interpolated strings with "q", "qq" and
"qc" support.

## Description

QuasiQuoter for interpolated strings using Perl 6 syntax.

The q form does one thing and does it well: It contains a multi-line string with
no interpolation at all:

```haskell
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
import Text.InterpolatedString.QQ2 (q)
foo :: String -- Text, ByteString etc also works
foo = [q|

Well here is a
    multi-line string!

|]
```

Any instance of the IsString class is permitted.

The qc form interpolates curly braces: expressions inside #{} will be
directly interpolated if it's a Char, String, Text or ByteString, or
it will have show called if it is not.

Escaping of '{' is done with backslash.

For interpolating numeric expressions without an explicit type signature,
use the ExtendedDefaultRules lanuage pragma, as shown below:

```haskell
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
import Text.InterpolatedString.QQ2 (qc)
bar :: String
bar = [qc| Well #{"hello" ++ " there"} #{6 * 7} |]
```

bar will have the value " Well hello there 42 ".

If you want control over how show works on your types, define a custom
ShowQ instance:

For example, this instance allows you to display interpolated lists of strings as
a sequence of words, removing those pesky brackets, quotes, and escape sequences.

```haskell
{-# LANGUAGE FlexibleInstances #-}
import Text.InterpolatedString.QQ2 (qc, ShowQ(..))
instance ShowQ [String] where
    showQ = unwords
```

qc permits output to any types with both IsString and Monoid
instances.

```haskell
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
import Text.InterpolatedString.QQ2 (qc)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
qux :: ByteString
qux = [qc| This will convert #{"Text" :: Text} to #{"ByteString" :: ByteString} |]
```
