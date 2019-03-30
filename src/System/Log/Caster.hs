{-|
Module      : System.Log.Caster
Description : Multicast, thread-safe, and not slow logger.
Copyright   : (c) Akihito KIRISAKI

License     : BSD3
Maintainer  :  Akihito KIRISAKI
Stability   : experimental
-}
{-# LANGUAGE UndecidableInstances #-}
module System.Log.Caster where

import qualified Data.ByteString             as SBS
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.FastBuilder as FB
import qualified Data.ByteString.Lazy        as LBS
import           Data.String                 (IsString (..))
import qualified Data.Text                   as ST
import qualified Data.Text.Encoding          as STE
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LTE

log = undefined

-- | Types which are able to be converted into @'FB.Builder' Builder@
--   @toBuilde@ encodes @String@ and @Text@ as utf-8.
class IsString a => ToBuilder a where
  toBuilder :: a -> FB.Builder

instance ToBuilder FB.Builder where
  toBuilder = id

instance ToBuilder String where
  toBuilder = FB.stringUtf8

instance ToBuilder ST.Text where
  toBuilder = FB.byteString . STE.encodeUtf8

instance ToBuilder LT.Text where
  toBuilder = FB.byteString . LBS.toStrict . LTE.encodeUtf8

instance ToBuilder SBS.ByteString where
  toBuilder = FB.byteString

instance ToBuilder LBS.ByteString where
  toBuilder = FB.byteString . LBS.toStrict

instance ToBuilder BB.Builder where
  toBuilder = FB.byteString . LBS.toStrict . BB.toLazyByteString

instance {-# OVERLAPPABLE #-} (Show a, IsString a) => ToBuilder a where
  toBuilder = toBuilder . show

-- | If you turn @OverloadedStrings@ extension on, GHC can't deduce the type of string literal.
--   This operator fix the type to @'FB.Builder' Builder@
infixr 0 $:
($:) :: (FB.Builder -> b) -> FB.Builder -> b
($:) = ($)
