{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Money
 ( Discrete(..)
 , fromDiscrete

 , Continuous(..)
 , fromContinuous
 , round
 , ceiling
 , floor

 , Scale
 , scale
 ) where

import GHC.TypeLits (Symbol, Nat, CmpNat, KnownNat)
import qualified GHC.TypeLits as GHC
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Prelude
  (Eq(..), Ord(..), Num(..), Show(..), Ordering(..), Fractional(..),
   Maybe(..), Integer, undefined)
import qualified Prelude

--------------------------------------------------------------------------------
-- | 'Continuous' represents a continuous monetary value for @currency@.
--
-- While monetary values associated with a particular currency are discrete, you
-- can still treat monetary values as continuous while operating on them. For
-- example, the half of @USD 3.41@ is @USD 1.705@, which is not an amount that
-- can't be represented as a number of USD cents (the smallest unit that can
-- represent USD amounts). Nevertheless, if you multiply @USD 1.705@ by @4@, for
-- example, you end up with @USD 6.82@, which is a value representable as USD
-- cents. In other words, 'Continuous' monetary values allow us to perform
-- precise calculations deferring the conversion to a 'Discrete' monetary values
-- as much as posible. Once you are ready to aproximate a 'Continuous' value to
-- a 'Discrete' value you can use one of 'round', 'floor' or 'ceiling'.
newtype Continuous (currency :: Symbol)
  = Continuous Scientific
    -- ^ If you want to represent @USD 12.5231@, then you can use:
    --
    -- @
    -- 'Continuous' ('fromInteger' 12531 * 10 '^^' (-3))
    -- @
    --
    -- In practice, it is more likely that you will be constructing 'Continuous'
    -- values from 'Discrete' values using 'fromDiscrete'.
  deriving (Eq, Ord, Show, Num)

-- | Obtain the underlying 'Scientific' representation for a 'Continuous'
-- monetary value, which is not guaranteed to be discrete and representable
-- within @currency@'s 'Scale'.
fromContinuous :: Continuous currency -> Scientific
fromContinuous = \(Continuous currency) -> currency
{-# INLINE fromContinuous #-}

-- | 'Discrete' represents a discrete monetary value for @currency@. That is, an
-- amount of money that is fully representable by the smallest unit of
-- @currency@.
newtype Discrete (currency :: Symbol)
  = Discrete Integer
    -- ^ If you want to represent @GBP 21.05@, where the smallest represetable
    -- unit for a GBP (United Kingdom Pound) is the /penny/, and 100 /pennies/
    -- equal 1 GBP (i.e., @'Scale' \"GBP\" ~ 100@), then you can use:
    --
    -- @
    -- 'Discrete' 2105
    -- @
    --
    -- Since @2015 / 100 == 20.15@.
  deriving (Eq, Ord, Show, Num)

instance
  ( GHC.TypeError
      (('GHC.Text "The ") 'GHC.:<>:
       ('GHC.ShowType Discrete) 'GHC.:<>:
       ('GHC.Text " type is deliberately not a ") 'GHC.:<>:
       ('GHC.ShowType Fractional) 'GHC.:$$:
       ('GHC.Text "instance. Convert the ") 'GHC.:<>:
       ('GHC.ShowType Discrete) 'GHC.:<>:
       ('GHC.Text " value to a ") 'GHC.:<>:
       ('GHC.ShowType Continuous) 'GHC.:$$:
       ('GHC.Text "value and use the ") 'GHC.:<>:
       ('GHC.ShowType Fractional) 'GHC.:<>:
       ('GHC.Text " features on it instead.")) )
  => Fractional (Discrete currency) where
  fromRational = undefined
  recip = undefined

-- | Convert currency 'Discrete' monetary value into a 'Continuous' monetary
-- value.
fromDiscrete
  :: (CmpNat 0 (Scale currency) ~ 'LT, KnownNat (Scale currency))
  => Discrete currency -> Continuous currency
fromDiscrete = \c@(Discrete i) ->
  Continuous (fromInteger i / fromInteger (scale c))
{-# INLINE fromDiscrete #-}

-- | Internal. Used to implement 'round', 'ceiling' and 'floor'.
roundf
  :: (CmpNat 0 (Scale currency) ~ 'LT, KnownNat (Scale currency))
  => (Scientific -> Integer)
  -- ^ 'Prelude.round', 'Prelude.ceiling' or similar.
  -> Continuous currency
  -> (Discrete currency, Maybe (Continuous currency))
roundf f = \m ->
  let ss = fromInteger (scale m)
      sx = fromContinuous m * ss
      iy = f sx
      sc = fromContinuous m - (fromInteger iy / ss)
      yc = if Scientific.isInteger sx
              then Nothing else Just (Continuous sc)
  in (Discrete iy, yc)
{-# INLINE roundf #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s 'Scale', which might be @x@ itself.
--
-- If @x@ is already fully representable in its @currency@'s 'Scale', then the
-- following holds:
--
-- @
-- 'round' x == (x, 'Nothing')
-- @
--
-- Otherwise, if the nearest value to @x@ that is fully representable in its
-- @currency@'s 'Scale' is greater than @x@, then the following holds:
--
-- @
-- 'round' == 'ceiling'
-- @
--
-- Otherwise, the nearest value to @x@ that is fully representable in its
-- @currency@'s 'Scale' is smaller than @x@, and the following holds:
--
-- @
-- 'round' == 'floor'
-- @
--
-- Proof that 'round' doesn't lose money:
--
-- @
-- x == case 'round' x of
--        (y, 'Nothing') -> y
--        (y, 'Just' z)  -> y + z
-- @
round
  :: (CmpNat 0 (Scale currency) ~ 'LT, KnownNat (Scale currency))
  => Continuous currency -> (Discrete currency, Maybe (Continuous currency))
round = roundf Prelude.round
{-# INLINE round #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s 'Scale' which is greater than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s 'Scale', then the
-- following holds:
--
-- @
-- 'ceiling' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s 'Scale', then the
-- following holds:
--
-- @
-- 'ceiling' x == (y, 'Just' z)
-- @
--
-- @
-- x /= y
-- @
--
-- @
-- z < 'zero'
-- @
--
-- Proof that 'ceiling' doesn't lose money:
--
-- @
-- x == case 'ceiling' x of
--        (y, 'Nothing') -> y
--        (y, 'Just' z)  -> y + z
-- @
ceiling
  :: (CmpNat 0 (Scale currency) ~ 'LT, KnownNat (Scale currency))
  => Continuous currency -> (Discrete currency, Maybe (Continuous currency))
ceiling = roundf Prelude.ceiling
{-# INLINE ceiling #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s 'Scale' which is smaller than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s 'Scale', then the
-- following holds:
--
-- @
-- 'floor' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s 'Scale', then the
-- following holds:
--
-- @
-- 'floor' x == (y, 'Just' z)
-- @
--
-- @
-- x /= y
-- @
--
-- @
-- z > 'zero'
-- @
--
-- Proof that 'floor' doesn't lose money:
--
-- @
-- x == case 'floor' x of
--        (y, 'Nothing') -> y
--        (y, 'Just' z)  -> y + z
-- @
floor
  :: (CmpNat 0 (Scale currency) ~ 'LT, KnownNat (Scale currency))
  => Continuous currency -> (Discrete currency, Maybe (Continuous currency))
floor = roundf Prelude.floor
{-# INLINE floor #-}

--------------------------------------------------------------------------------
-- | Monetary values in a particular currency are rational and discrete values,
-- and as such they can have an integral representation as the amount of
-- smallest representable units of that currency. For example, the smallest
-- representable unit for USD (United States Dollar) is the Cent, where 100 Cent
-- units equal 1 USD unit. For example, we can represent 42.65 USD units as 4265
-- Cent units.  Thus, we say that the 'Scale' for USD is 100.
--
-- @
-- type instance Scale \"USD\" = 100
-- @
--
-- For some other monetary values, such as precious metals, the smallest
-- representable unit is not obvious, since you can continue to split the
-- precious metal many times before it stops being a precious metal. Still,
-- for practical purposes we can make a sane arbitrary choice of smallest unit.
-- For example, the base unit for XAU (Gold) is the /troy ounce/, which is too
-- big to be considered the smallest unit, but we can arbitrarily choose the
-- /milligrain/ as our smallest unit, which is about as heavy as a single grain
-- of table salt and should be sufficiently precise for all monetary practical
-- purposes. A /troy ounce/ equals 480000 /milligrains/.
--
-- @
-- type instance Scale \"XAG\" = 480000
-- @
--
-- The 'Scale' will determine how to convert a 'Continuous' value into a
-- 'Discrete' value and vice-versa.
type family Scale (currency :: Symbol) :: Nat

-- | Term-level representation for @currency@'s 'Scale'.
--
-- For example, the 'Scale' for @\"USD\"@ is @100@, thus @'scale' ('Proxy' ::
-- 'Proxy' \"USD\") == 100@.
scale
  :: forall (currency :: Symbol) proxy
  .  (CmpNat 0 (Scale currency) ~ 'LT, KnownNat (Scale currency))
  => proxy currency
  -- ^ Use something like @'Proxy' \"EUR\"@, @'Continuous' \"EUR\"@, or
  -- @'Discrete' \"EUR\"@.
  -> Integer
scale = \_ -> GHC.natVal (Proxy :: Proxy (Scale currency))
{-# INLINE scale #-}

--------------------------------------------------------------------------------

type instance Scale "ARS" = Scale "ARS/centavo"
type instance Scale "ARS/centavo" = 100
type instance Scale "ARS/peso" = 1

type instance Scale "BTC" = Scale "BTC/satoshi"
type instance Scale "BTC/satoshi" = 100000000
type instance Scale "BTC/bitcoin" = 1

type instance Scale "GBP" = Scale "GBP/penny"
type instance Scale "GBP/penny" = 100
type instance Scale "GBP/pound" = 1

type instance Scale "JPY" = Scale "JPY/yen"
type instance Scale "JPY/yen" = 1

type instance Scale "USD" = Scale "USD/Cent"
type instance Scale "USD/cent" = 100
type instance Scale "USD/dollar" = 1

type instance Scale "XAU" = Scale "XAU/milligrain"
type instance Scale "XAU/micrograin" = 480000000
type instance Scale "XAU/milligrain" = 480000
type instance Scale "XAU/grain" = 480
