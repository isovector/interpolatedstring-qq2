{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, OverloadedStrings, IncoherentInstances #-}

module Main where

import Data.ByteString.Char8 as BS(ByteString, pack)
import Data.Text as T(Text, pack)
import GHC.Exts(fromString)
import Test.HUnit
import Text.InterpolatedString.QQ2

data Foo = Foo Int String deriving Show

t1 :: String
t1 = "字元"

testEmpty       = assertBool "" ([qc||] == "")
testCharLiteral = assertBool "" ([qc|#{1+2}|] == "3")
testString      = assertBool "" ([qc|a string #{t1} is here|] == "a string 字元 is here")
testEscape      = assertBool "" ([qc|\#{ }|] == "#{ }")
testComplex     = assertBool "" ([qc|
        \ok
#{Foo 4 "Great!" : [Foo 3 "Scott!"]}
        then
|] == ("\n" ++
    "        \\ok\n" ++
    "[Foo 4 \"Great!\",Foo 3 \"Scott!\"]\n" ++
    "        then\n"))
testConvert = assertBool ""
              (([qc|#{fromString "a"::Text} #{fromString "b"::ByteString}|] :: String)
               == "a b")

tests = TestList
    [ TestLabel "Empty String"       $ TestCase testEmpty
    , TestLabel "Character Literal"  $ TestCase testCharLiteral
    , TestLabel "String Variable"    $ TestCase testString
    , TestLabel "Escape Sequences"   $ TestCase testEscape
    , TestLabel "Complex Expression" $ TestCase testComplex
    , TestLabel "String Conversion"  $ TestCase testConvert
    , TestLabel "ByteString Test"    $ TestCase testByteString
    , TestLabel "Text Test"          $ TestCase testText
    ]

main = runTestTT tests


-- the primary purpose of these tests is to ensure that
-- the Text and ByteString rewrite rules are firing, to avoid
-- needlessly converting string types
testByteString = assertBool "" $ [qc|#{"a" :: ByteString} #{"b" :: ByteString}|]
                 == BS.pack ("a b")
testText = assertBool "" $ [qc|#{"a" :: Text} #{"b" :: Text}|]
           == T.pack ("a b")

