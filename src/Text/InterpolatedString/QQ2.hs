{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
  UndecidableInstances, MultiParamTypeClasses #-}

-- | QuasiQuoter for interpolated strings using Perl 6 syntax.
--
-- The 'q' form does one thing and does it well: It contains a multi-line string with
-- no interpolation at all:
--
-- @
-- {-\# LANGUAGE QuasiQuotes, ExtendedDefaultRules \#-}
-- import Text.InterpolatedString.QQ2 (q)
-- foo :: String -- 'Text', 'ByteString' etc also works
-- foo = [q|
--
-- Well here is a
--     multi-line string!
--
-- |]
-- @
--
-- Any instance of the 'IsString' class is permitted.
--
-- The 'qc' form interpolates curly braces: expressions inside \#{} will be
-- directly interpolated if it's a 'Char', 'String', 'Text' or 'ByteString', or
-- it will have 'show' called if it is not.
--
-- Escaping of '\#{' is done with backslash.
--
-- For interpolating numeric expressions without an explicit type signature,
-- use the ExtendedDefaultRules lanuage pragma, as shown below:
--
-- @
-- {-\# LANGUAGE QuasiQuotes, ExtendedDefaultRules \#-}
-- import Text.InterpolatedString.QQ2 (qc)
-- bar :: String
-- bar = [qc| Well \#{\"hello\" ++ \" there\"} \#{6 * 7} |]
-- @
--
-- bar will have the value \" Well hello there 42 \".
--
-- If you want control over how 'show' works on your types, define a custom
-- 'ShowQ' instance:
--
-- For example, this instance allows you to display interpolated lists of strings as
-- a sequence of words, removing those pesky brackets, quotes, and escape sequences.
--
-- @
-- {-\# LANGUAGE FlexibleInstances \#-}
-- import Text.InterpolatedString.QQ2 (qc, ShowQ(..))
-- instance ShowQ [String] where
--     showQ = unwords
-- @
--
-- 'qc' permits output to any types with both 'IsString' and 'Monoid'
-- instances.
--
-- @
-- {-\# LANGUAGE QuasiQuotes, OverloadedStrings \#-}
-- import Text.InterpolatedString.QQ2 (qc)
-- import Data.Text (Text)
-- import Data.ByteString.Char8 (ByteString)
-- qux :: ByteString
-- qux = [qc| This will convert \#{\"Text\" :: Text} to \#{\"ByteString\" :: ByteString} |]
-- @

module Text.InterpolatedString.QQ2 (qc, q, ShowQ(..)) where

import           Data.ByteString.Char8 as Strict (ByteString, unpack)
import           Data.ByteString.Lazy.Char8 as Lazy (ByteString, unpack)
import           Data.Monoid (Monoid(..))
import           Data.Text as T (Text, unpack)
import           Data.Text.Lazy as LazyT(Text, unpack)
import           GHC.Exts (IsString(..))
import           Language.Haskell.Meta.Parse
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote

-- |A class for types that use special interpolation rules.
-- Instances of 'ShowQ' that are also instances of 'IsString' should obey the
-- following law:
--
-- @
-- fromString (showQ s) == s
-- @
--
-- because this library relies on this fact to optimize
-- away needless string conversions.
class ShowQ a where
    showQ :: a -> String

instance ShowQ Char where
    showQ = (:[])

instance ShowQ String where
    showQ = id

instance ShowQ Strict.ByteString where
    showQ = Strict.unpack

instance ShowQ Lazy.ByteString where
    showQ = Lazy.unpack

instance ShowQ T.Text where
    showQ = T.unpack

instance ShowQ LazyT.Text where
    showQ = LazyT.unpack

instance {-# OVERLAPPABLE #-} Show a => ShowQ a where
    showQ = show

-- todo: this should really be rewritten into RULES pragmas, but so far
-- I can't convince GHC to let the rules fire.
class QQ a string where
    toQQ :: a -> string

instance {-# INCOHERENT #-} IsString s => QQ s s where
    toQQ = id

instance {-# INCOHERENT #-} (ShowQ a, IsString s) => QQ a s where
    toQQ = fromString . showQ

data StringPart = Literal String | AntiQuote String deriving Show

unQC :: [Char] -> [Char] -> [StringPart]
unQC a []          = [Literal (reverse a)]
unQC a ('\\':x:xs) = unQC (x:a) xs
unQC a ('\\':[])   = unQC ('\\':a) []
unQC a ('}':xs)    = AntiQuote (reverse a) : parseQC [] xs
unQC a (x:xs)      = unQC (x:a) xs

parseQC :: [Char] -> [Char] -> [StringPart]
parseQC a []             = [Literal (reverse a)]
parseQC a ('\\':'\\':xs) = parseQC ('\\':a) xs
parseQC a ('\\':'#':xs)  = parseQC ('#':a) xs
parseQC a ('\\':[])      = parseQC ('\\':a) []
parseQC a ('#':'{':xs)   = Literal (reverse a) : unQC [] xs
parseQC a (x:xs)         = parseQC (x:a) xs

makeExpr :: [StringPart] -> TH.Q TH.Exp
makeExpr [] = [| mempty |]
makeExpr ((Literal a):xs)   = TH.appE [| mappend (fromString a) |]
                              $ makeExpr xs
makeExpr ((AntiQuote a):xs) = TH.appE [| mappend (toQQ $(reify a)) |]
                              $ makeExpr xs

reify :: String -> TH.Q TH.Exp
reify s =
    case parseExp s of
        Left s'  -> TH.reportError s' >> [| mempty |]
        Right e ->  return e

-- | QuasiQuoter for interpolating '\#{expr}' into a string literal. The pattern portion is undefined.
qc :: QuasiQuoter
qc = QuasiQuoter (makeExpr . parseQC [] . filter (/= '\r'))
                 (error "Cannot use qc as a pattern")
                 (error "Cannot use qc as a type")
                 (error "Cannot use qc as a dec")

-- | QuasiQuoter for a non-interpolating string literal. The pattern portion is undefined.
q :: QuasiQuoter
q = QuasiQuoter ((\a -> [|fromString a|]) . filter (/= '\r'))
                 (error "Cannot use q as a pattern")
                 (error "Cannot use q as a type")
                 (error "Cannot use q as a dec")
