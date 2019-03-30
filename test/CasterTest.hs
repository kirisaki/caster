module CasterTest where

import           System.Log.Caster
import           Test.QuickCheck.Instances

import qualified Data.ByteString             as SBS
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.FastBuilder as FB
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text                   as ST
import qualified Data.Text.Encoding          as STE
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LTE

prop_toBuilder_String :: String -> Bool
prop_toBuilder_String str = (LT.unpack . LTE.decodeUtf8 . FB.toLazyByteString $ toBuilder str) == str

prop_toBuilder_strict_Text :: ST.Text -> Bool
prop_toBuilder_strict_Text str = (LT.toStrict . LTE.decodeUtf8 . FB.toLazyByteString $ toBuilder str) == str

prop_toBuilder_lazy_Text :: LT.Text -> Bool
prop_toBuilder_lazy_Text str = (LTE.decodeUtf8 . FB.toLazyByteString $ toBuilder str) == str

prop_toBuilder_strict_ByteString :: SBS.ByteString -> Bool
prop_toBuilder_strict_ByteString str = FB.toStrictByteString (toBuilder str) == str

prop_toBuilder_lazy_ByteString :: LBS.ByteString -> Bool
prop_toBuilder_lazy_ByteString str = FB.toLazyByteString (toBuilder str) == str

prop_concat :: String -> String -> Bool
prop_concat str0 str1 = (LT.unpack . LTE.decodeUtf8 . FB.toLazyByteString $ str0 <:> str1) == str0 <> str1
