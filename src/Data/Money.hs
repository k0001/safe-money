{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Import this module qualified:
--
-- @
-- import qualified Data.Money as Money
-- @
module Data.Money
 ( Discrete
 , fromDiscrete

 , Continuous
 , continuous
 , round
 , ceiling
 , floor
 , truncate

 , Scale
 , Scale'
 , scale
 , scale'
 , ErrScaleNonCanonical
 ) where

import Control.Applicative (empty)
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), numerator, denominator)
import GHC.Real (infinity, notANumber)
import GHC.TypeLits
  (Symbol, Nat, CmpNat, KnownNat, KnownSymbol, symbolVal, natVal)
import qualified GHC.TypeLits as GHC
import Prelude hiding (round, ceiling, floor, truncate)
import qualified Prelude
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readPrec)

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
-- a 'Discrete' value you can use one of 'round', 'floor', 'ceiling' or
-- 'truncate'. Otherwise, using 'toRational' you can obtain a precise 'Rational'
-- representation.
--
-- Construct 'Continuous' monetary values using 'continuous', or
-- 'fromInteger'/'fromIntegral' if that suffices.
newtype Continuous (currency :: Symbol) = Continuous Rational
  deriving (Eq, Ord, Num, Real, Fractional)

instance
  forall (currency :: Symbol).
  ( KnownSymbol currency
  ) => Show (Continuous currency) where
  show = \(Continuous r0) -> mconcat
    [ "Continuous "
    , if r0 < 0 then "-" else "+"
    , show (abs (numerator r0))
    , "/"
    , show (abs (denominator r0))
    , " "
    , symbolVal (Proxy :: Proxy currency)
    ]

instance
  forall (currency :: Symbol).
  ( KnownSymbol currency
  ) => Read (Continuous currency) where
  readPrec = do
    _ <- ReadPrec.lift $ ReadP.string "Continuous "
    f <- ReadPrec.get >>= \case { '+' -> pure id; '-' -> pure negate; _ -> empty }
    n <- readPrec
    _ <- ReadPrec.lift $ ReadP.satisfy (== '/')
    d <- readPrec
    _ <- ReadPrec.lift $ ReadP.satisfy (== ' ')
    _ <- ReadPrec.lift $ ReadP.string (symbolVal (Proxy :: Proxy currency))
    maybe empty pure (continuous (f n % d))

-- | Build a 'Continuous' monetary value from a 'Rational' value.
--
-- For example, if you want to represent @USD 12.52316@, then you can use:
--
-- @
-- 'continuous' (125316 % 10000)
-- @
--
-- This function returns 'Nothing' in case the given 'Rational' is 'infinity' or
-- 'notANumber'.
--
-- In practice, it is more likely that you will be constructing 'Continuous'
-- values from 'Discrete' values using 'fromDiscrete'.
continuous :: Rational -> Maybe (Continuous currency)
continuous = \r0 ->
  if (infinity == r0 || notANumber == r0)
  then Nothing else Just (Continuous r0)
{-# INLINE continuous #-}

-- | 'Discrete' represents a discrete monetary value for a @currency@ expresed
-- as an integer amount of a particular @unit@. For example, with @currency ~
-- \"USD\"@ ad @unit ~ \"cent\"@ you can represent United States Dollars to
-- their full extent.
--
-- Construct 'Discrete' values using 'fromInteger'.
--
-- For example, if you want to represent @GBP 21.05@, where the smallest
-- represetable unit for a GBP (United Kingdom Pound) is the /penny/, and 100
-- /pennies/ equal 1 GBP (i.e., @'Scale'' \"GBP\" ~ 100@), then you can use:
--
-- @
-- 'fromInteger' 2105 :: Discrete "GBP" "penny"
-- @
--
-- Because @2015 / 100 == 20.15@.
newtype Discrete (currency :: Symbol) (unit :: Symbol) = Discrete Integer
  deriving (Eq, Ord, Enum, Num, Real, Integral)

instance
  forall (currency :: Symbol) (unit :: Symbol).
  ( CmpNat 0 (Scale' currency unit) ~ 'LT
  , KnownNat (Scale' currency unit)
  , KnownSymbol currency
  ) => Show (Discrete currency unit) where
  show = \(Discrete i0) -> mconcat
    [ "Discrete "
    , if i0 < 0 then "-" else "+"
    , show (abs i0)
    , " "
    , symbolVal (Proxy :: Proxy currency)
    , "/"
    , show (natVal (Proxy :: Proxy (Scale' currency unit)))
    ]

instance
  forall (currency :: Symbol) (unit :: Symbol).
  ( CmpNat 0 (Scale' currency unit) ~ 'LT
  , KnownNat (Scale' currency unit)
  , KnownSymbol currency
  ) => Read (Discrete currency unit) where
  readPrec = do
    _ <- ReadPrec.lift $ ReadP.string "Discrete "
    f <- ReadPrec.get >>= \case { '+' -> pure id; '-' -> pure negate; _ -> empty }
    i <- readPrec
    _ <- ReadPrec.lift $ ReadP.satisfy (== ' ')
    _ <- ReadPrec.lift $ ReadP.string (symbolVal (Proxy :: Proxy currency))
    _ <- ReadPrec.lift $ ReadP.satisfy (== '/')
    _ <- ReadPrec.lift $ ReadP.string $
       show (natVal (Proxy :: Proxy (Scale' currency unit)))
    pure (Discrete (f i))

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
  => Fractional (Discrete currency unit) where
  fromRational = undefined
  recip = undefined

-- | Convert currency 'Discrete' monetary value into a 'Continuous' monetary
-- value.
fromDiscrete
  :: ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => Discrete currency unit
  -> Continuous currency -- ^
fromDiscrete = \c@(Discrete i) -> Continuous (i % scale c)
{-# INLINE fromDiscrete #-}

-- | Internal. Used to implement 'round', 'ceiling', 'floor' and 'truncate'.
roundf
  :: ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => (Rational -> Integer) -- ^ 'Prelude.round', 'Prelude.ceiling' or similar.
  -> Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency))
roundf f = \c0 ->
  let r0 = toRational c0 :: Rational
      r1 = r0 * fromInteger (scale d2) :: Rational
      i2 = f r1 :: Integer
      r2 = fromInteger i2 % scale d2 :: Rational
      ycrest | r0 == r2  = Nothing
             | otherwise = Just (Continuous (r0 - r2))
      d2 = Discrete i2
  in (d2, ycrest)
{-# INLINE roundf #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s 'Scale'', which might be @x@ itself.
--
-- If @x@ is already fully representable in its @currency@'s 'Scale'', then the
-- following holds:
--
-- @
-- 'round' x == (x, 'Nothing')
-- @
--
-- Otherwise, if the nearest value to @x@ that is fully representable in its
-- @currency@'s 'Scale'' is greater than @x@, then the following holds:
--
-- @
-- 'round' == 'ceiling'
-- @
--
-- Otherwise, the nearest value to @x@ that is fully representable in its
-- @currency@'s 'Scale'' is smaller than @x@, and the following holds:
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
  :: ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency)) -- ^
round = roundf Prelude.round
{-# INLINE round #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s 'Scale'' which is greater than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s 'Scale'', then the
-- following holds:
--
-- @
-- 'ceiling' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s 'Scale'', then the
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
  :: ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency)) -- ^
ceiling = roundf Prelude.ceiling
{-# INLINE ceiling #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s 'Scale'' which is smaller than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s 'Scale'', then the
-- following holds:
--
-- @
-- 'floor' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s 'Scale'', then the
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
  :: ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency)) -- ^
floor = roundf Prelude.floor
{-# INLINE floor #-}

-- | Round a 'Continuous' value @x@ to the nearest value between zero and
-- @x@ (inclusive) which is fully representable in its @currency@'s 'Scale''.
--
-- If @x@ is already fully representable in its @currency@'s 'Scale'', then the
-- following holds:
--
-- @
-- 'truncate' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is positive, then the following holds:
--
-- @
-- 'truncate' == 'floor'
-- @
--
-- Otherwise, if @x@ is negative, the following holds:
--
-- @
-- 'truncate' == 'ceiling'
-- @
--
-- Proof that 'round' doesn't lose money:
--
-- @
-- x == case 'truncate' x of
--        (y, 'Nothing') -> y
--        (y, 'Just' z)  -> y + z
-- @
truncate
  :: ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency)) -- ^
truncate = roundf Prelude.truncate
{-# INLINE truncate #-}

--------------------------------------------------------------------------------

-- | Like 'Scale'', but the @currency@'s @unit@ is expected to be the smallest
-- discrete unit that can represent the it in its full extent. For example,
-- cents are the smallest unit that can represent United States Dollars, so:
--
-- @
-- 'Scale' \"USD\" ~ 'Scale'' \"USD\" \"USD\" ~ 'Scale'' \"USD\" \"cent\"
-- @
type Scale (currency :: Symbol) = Scale' currency currency

-- | Monetary values in a particular currency are rational and discrete values,
-- and as such they can have an integral representation as the amount of
-- smallest representable units of that currency. For example, the smallest
-- representable unit for USD (United States Dollar) is the Cent, where 100 Cent
-- units equal 1 USD unit. For example, we can represent 42.65 USD units as 4265
-- Cent units.  Thus, we say that the 'Scale'' for USD cents is 100.
--
-- @
-- type instance 'Scale'' \"USD\" \"cent\" = 100
-- @
--
-- You can pick other unit as your smallest, say the dollar for USD, if you
-- don't want to deal with smaller units.
--
-- @
-- type instance 'Scale'' \"USD\" \"dollar\" = 1
-- @
--
-- If there exists a cannonical smallest unit that can fully represent the
-- currency, then an instance @'Scale'' currency currency@ exists.
--
-- @
-- type instance 'Scale'' \"USD\" \"USD\" = Scale' \"USD\" \"cent\"
-- @
--
-- There is a convenient type synonym 'Scale' for this 'Scale'' case where the
-- @currency@ and @unit@ match:
--
-- @
-- 'Scale' \"USD\" ~ 'Scale'' \"USD\" \"USD\"
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
-- type instance Scale' \"XAG\" \"milligrain\" = 480000
-- @
--
-- If you try to use without an obvious smallest representable unit, like XAU,
-- as @'Scale' \"XAU\"@ (or @'Scale'' \"XAU\" \"XAU\"), you will get a compile
-- error.
--
-- The 'Scale'' will determine how to convert a 'Continuous' value into a
-- 'Discrete' value and vice-versa.
type family Scale' (currency :: Symbol) (unit :: Symbol) :: Nat

-- | Term-level representation for the @currency@'s @unit@ 'Scale''.
--
-- For example, the 'Scale'' for @\"USD\"@ in @\"cent\"@s is @100@.
scale
  :: forall (currency :: Symbol) (unit :: Symbol)
  .  ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => Discrete currency unit
  -> Integer
scale = \_ -> scale' (Proxy :: Proxy currency) (Proxy :: Proxy unit)
{-# INLINE scale #-}

-- | Like 'scale', but takes proxies (e.g., 'Proxy') instead of 'Discrete'.
scale'
  :: forall (currency :: Symbol) (unit :: Symbol) proxy1 proxy2
  .  ( CmpNat 0 (Scale' currency unit) ~ 'LT
     , KnownNat (Scale' currency unit) )
  => proxy1 currency
  -> proxy2 unit
  -> Integer
scale' = \_ _ -> GHC.natVal (Proxy :: Proxy (Scale' currency unit))
{-# INLINE scale' #-}

--------------------------------------------------------------------------------

type family ErrScaleNonCanonical (currency :: Symbol) :: k where
  ErrScaleNonCanonical c = GHC.TypeError
    ( 'GHC.Text c 'GHC.:<>:
      'GHC.Text " is not a currency with a canonical smallest unit," 'GHC.:$$:
      'GHC.Text "be explicit about the currency unit you want to use." )

type instance Scale' "ARS" "ARS" = Scale' "ARS" "centavo"
type instance Scale' "ARS" "peso" = 1
type instance Scale' "ARS" "centavo" = 100

type instance Scale' "BTC" "BTC" = Scale' "ARS" "satoshi"
type instance Scale' "BTC" "bitcoin" = 1
type instance Scale' "BTC" "satoshi" = 100000000

type instance Scale' "GBP" "GBP" = Scale' "ARS" "penny"
type instance Scale' "GBP" "pound" = 1
type instance Scale' "GBP" "penny" = 100

type instance Scale' "JPY" "JPY" = Scale' "JPY" "JPY"
type instance Scale' "JPY" "yen" = 1

type instance Scale' "USD" "USD" = Scale' "USD" "cent"
type instance Scale' "USD" "dollar" = 1
type instance Scale' "USD" "cent" = 100

type instance Scale' "XAU" "XAU" = ErrScaleNonCanonical "XAU"
type instance Scale' "XAU" "troy-ounce" = 1
type instance Scale' "XAU" "grain" = 480
type instance Scale' "XAU" "milligrain" = 480000
type instance Scale' "XAU" "micrograin" = 480000000


