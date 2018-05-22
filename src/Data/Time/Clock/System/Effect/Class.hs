{-# LANGUAGE NoImplicitPrelude #-}
module Data.Time.Clock.System.Effect.Class (
    CS.systemEpochDay
  , CS.SystemTime(..)
  , CS.truncateSystemTimeLeapSecond
  , CS.systemToUTCTime
  , CS.utcToSystemTime
  , CS.systemToTAITime

  , GetSystemTime(..)
  ) where

import Prelude (IO)
import qualified Data.Time.Clock.System as CS

class GetSystemTime t where
  getSystemTime :: t CS.SystemTime

instance GetSystemTime IO where
  getSystemTime = CS.getSystemTime
