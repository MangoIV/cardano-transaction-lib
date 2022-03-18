module Types.Rational
  ( Rational
  ) where

import Data.Ratio (Ratio)
import Data.BigInt (BigInt)

type Rational = Ratio BigInt