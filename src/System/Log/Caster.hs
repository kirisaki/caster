{- |
Description : Thread-safe, multicast, and not slow logger.
Module      : System.Log.Caster
Copyright   : Akihito KIRISAKI (c)
License     : BSD3

Maintainer  :  Akihito KIRISAKI
-}
module System.Log.Caster where

import qualified Data.ByteString.FastBuilder as FB
import           Data.String                 (IsString (..))

log = undefined

-- | Types which are able to be converted into @Builder@
class IsString a => LogStr a where
  toLogStr :: a -> FB.Builder

instance LogStr ST.Text where
  toLogStr = FB.byteString . STE.encodeUtf8

instance LogStr String where
  toLogStr = FB.stringUtf8

instance LogStr FB.Builder where
  toLogStr = id

