{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
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
 , GoodScale
 , scale
 , scaleFromProxy
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
-- represent USD amounts). Nevertheless, if you eventually multiply @USD 1.705@
-- by @4@, for example, you end up with @USD 6.82@, which is again a value
-- representable as USD cents. In other words, 'Continuous' monetary values
-- allow us to perform precise calculations deferring the conversion to a
-- 'Discrete' monetary values as much as posible. Once you are ready to
-- aproximate a 'Continuous' value to a 'Discrete' value you can use one of
-- 'round', 'floor', 'ceiling' or 'truncate'. Otherwise, using 'toRational' you
-- can obtain a precise 'Rational' representation.
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
    , "/", show (abs (denominator r0))
    , " ", symbolVal (Proxy :: Proxy currency)
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
continuous :: Rational -> Maybe (Continuous currency)
continuous = \r0 ->
  if (infinity == r0 || notANumber == r0)
  then Nothing else Just (Continuous r0)
{-# INLINE continuous #-}

-- | 'Discrete' represents a discrete monetary value for a @currency@ expresed
-- as an integer amount of a particular @unit@. For example, with @currency ~
-- \"USD\"@ and @unit ~ \"cent\"@ you can represent United States Dollars to
-- their full extent.
--
-- Construct 'Discrete' values using 'fromInteger'.
--
-- For example, if you want to represent @GBP 21.05@, where the smallest
-- represetable unit for a GBP (United Kingdom Pound) is the /penny/, and 100
-- /pennies/ equal 1 GBP (i.e., @'Scale'' \"GBP\" ~ '(100, 1)@), then you can
-- use:
--
-- @
-- 'fromInteger' 2105 :: Discrete "GBP" "penny"
-- @
--
-- Because @2015 / 100 == 20.15@.
newtype Discrete (currency :: Symbol) (unit :: Symbol) = Discrete Integer
  deriving (Eq, Ord, Enum, Num, Real, Integral)

instance
  forall currency unit num den.
  ( GoodScale currency unit num den
  ) => Show (Discrete currency unit) where
  show = \(Discrete i0) -> mconcat
    [ "Discrete "
    , if i0 < 0 then "-" else "+"
    , show (abs i0)
    , " ", symbolVal (Proxy :: Proxy currency)
    , ":", show (natVal (Proxy :: Proxy num))
    , "/", show (natVal (Proxy :: Proxy den))
    ]

instance
  forall currency unit num den.
  ( GoodScale currency unit num den
  ) => Read (Discrete currency unit) where
  readPrec = do
    _ <- ReadPrec.lift $ ReadP.string "Discrete "
    f <- ReadPrec.get >>= \case { '+' -> pure id; '-' -> pure negate; _ -> empty }
    i <- readPrec
    _ <- ReadPrec.lift $ ReadP.satisfy (== ' ')
    _ <- ReadPrec.lift $ ReadP.string (symbolVal (Proxy :: Proxy currency))
    _ <- ReadPrec.lift $ ReadP.satisfy (== ':')
    _ <- ReadPrec.lift $ ReadP.string $ show (natVal (Proxy :: Proxy num))
    _ <- ReadPrec.lift $ ReadP.satisfy (== '/')
    _ <- ReadPrec.lift $ ReadP.string $ show (natVal (Proxy :: Proxy den))
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
  :: GoodScale currency unit num den
  => Discrete currency unit
  -> Continuous currency -- ^
fromDiscrete = \c@(Discrete i) -> Continuous (fromInteger i / scale c)
{-# INLINE fromDiscrete #-}

-- | Internal. Used to implement 'round', 'ceiling', 'floor' and 'truncate'.
roundf
  :: GoodScale currency unit num den
  => (Rational -> Integer) -- ^ 'Prelude.round', 'Prelude.ceiling' or similar.
  -> Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency))
roundf f = \c0 ->
  let r0 = toRational c0 :: Rational
      r1 = r0 * scale d2 :: Rational
      i2 = f r1 :: Integer
      r2 = fromInteger i2 / scale d2 :: Rational
      ycrest | r0 == r2  = Nothing
             | otherwise = Just (Continuous (r0 - r2))
      d2 = Discrete i2
  in (d2, ycrest)
{-# INLINE roundf #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s @unit@ 'Scale'', which might be @x@ itself.
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale'',
-- then the following holds:
--
-- @
-- 'round' x == (x, 'Nothing')
-- @
--
-- Otherwise, if the nearest value to @x@ that is fully representable in its
-- @currency@'s @unit@ 'Scale'' is greater than @x@, then the following holds:
--
-- @
-- 'round' == 'ceiling'
-- @
--
-- Otherwise, the nearest value to @x@ that is fully representable in its
-- @currency@'s @unit@ 'Scale'' is smaller than @x@, and the following holds:
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
  :: GoodScale currency unit num den
  => Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency)) -- ^
round = roundf Prelude.round
{-# INLINE round #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s @unit@ 'Scale'' which is greater than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale'',
-- then the following holds:
--
-- @
-- 'ceiling' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s @unit@ 'Scale'',
-- then the following holds:
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
  :: GoodScale currency unit num den
  => Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency)) -- ^
ceiling = roundf Prelude.ceiling
{-# INLINE ceiling #-}

-- | Round a 'Continuous' value @x@ to the nearest value fully representable in
-- its @currency@'s @unit@ 'Scale'' which is smaller than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale'',
-- then the following holds:
--
-- @
-- 'floor' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s @unit@ 'Scale'',
-- then the following holds:
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
  :: GoodScale currency unit num den
  => Continuous currency
  -> (Discrete currency unit, Maybe (Continuous currency)) -- ^
floor = roundf Prelude.floor
{-# INLINE floor #-}

-- | Round a 'Continuous' value @x@ to the nearest value between zero and
-- @x@ (inclusive) which is fully representable in its @currency@'s @unit@
-- 'Scale''.
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale'',
-- then the following holds:
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
  :: GoodScale currency unit nat dem
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
--
-- If you try to obtain the 'Scale' of a @currency@ without an obvious smallest
-- representable @unit@, like XAU, you will get a compile error.
type Scale (currency :: Symbol) = Scale' currency currency

-- | @'Scale'' currency unit@ is a rational number (expressed as @'(numerator,
-- denominator)@) indicating how many pieces of @unit@ fit in @currency@.
-- The 'Scale'' will determine how to convert a 'Continuous' value into a
-- 'Discrete' value and vice-versa.
--
-- For example, there are 100 USD cents in 1 USD, so the scale for this
-- relationship is:
--
-- @
-- type instance 'Scale'' \"USD\" \"cent\" = '(100, 1)
-- @
--
-- As another example, there is 1 dollar in USD, so the scale for this
-- relationship is:
--
-- @
-- type instance 'Scale'' \"USD\" \"dollar\" = '(1, 1)
-- @
--
-- When using 'Discrete' values to represent money, it will be impossible to
-- represent an amount of @currency@ smaller than @unit@. So, if you decide to
-- use @Scale' \"USD\" \"dollar\"@ as your scale, you will not be able to
-- represent values such as USD 3.50 or USD 21.87, since they are not exact
-- multiples of a dollar.
--
-- If there exists a cannonical smallest @unit@ that can fully represent the
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
-- type 'Scale' a = 'Scale'' a a
-- 'Scale' \"USD\" ~ 'Scale'' \"USD\" \"USD\"
-- @
--
-- For some monetary values, such as precious metals, the smallest representable
-- unit is not obvious, since you can continue to split the precious metal many
-- times before it stops being a precious metal. Still, for practical purposes
-- we can make a sane arbitrary choice of smallest unit. For example, the base
-- unit for XAU (Gold) is the /troy ounce/, which is too big to be considered
-- the smallest unit, but we can arbitrarily choose the /milligrain/ as our
-- smallest unit, which is about as heavy as a single grain of table salt and
-- should be sufficiently precise for all monetary practical purposes. A /troy
-- ounce/ equals 480000 /milligrains/.
--
-- @
-- type instance 'Scale'' \"XAG\" \"milligrain\" = '(480000, 1)
-- @
--
-- You can use other units such as /milligrams/ for measuring XAU, for example.
-- However, since the amount of /milligrams/ in a /troy ounce/ (31103.477) is
-- not integral, we need to use rational number to express it.
--
-- @
-- type instance 'Scale'' \"XAU\" \"milligram\" = '(31103477, 1000)
-- @
--
-- If you try to obtain the 'Scale' of a @currency@ without an obvious smallest
-- representable @unit@, like XAU, you will get a compile error.
type family Scale' (currency :: Symbol) (unit :: Symbol) :: (Nat, Nat)

-- | Constraints to @'Scale'' currency unit ~ '(num, den)@ expected to always be
-- satisfied. In particular, the scale is always guaranteed to be a positive
-- rational number ('infinity' and 'notANumber' are forbidden by 'GoodScale').
--
-- Notice that there is a functional dependency @currency unit -> num dem@,
-- which means you can leave @num@ and @den@ polymorphic in constraints.
type GoodScale (currency :: Symbol) (unit :: Symbol) (num :: Nat) (den :: Nat)
  = ( Scale' currency unit ~ '(num, den)
    , CmpNat 0 num ~ 'LT
    , CmpNat 0 den ~ 'LT
    , KnownNat num
    , KnownNat den
    , KnownSymbol currency
    , KnownSymbol unit
    )

-- | Term-level representation for the @currency@'s @unit@ 'Scale''.
--
-- For example, the 'Scale'' for @\"USD\"@ in @\"cent\"@s is @100/1@.
--
-- The returned 'Rational' is statically guaranteed to be a positive number, and
-- to be different from both 'notANumber' and 'infinity'.
scale
  :: forall currency unit num den
  .  GoodScale currency unit num den
  => Discrete currency unit
  -> Rational
scale = \_ -> natVal (Proxy :: Proxy num) % natVal (Proxy :: Proxy den)
{-# INLINE scale #-}

-- | Like 'scale', but takes proxies (e.g., 'Proxy') instead of 'Discrete'.
scaleFromProxy
  :: forall currency unit num den proxy1 proxy2
  .  GoodScale currency unit num den
  => proxy1 currency
  -> proxy2 unit
  -> Rational
scaleFromProxy = \_ _ ->
   natVal (Proxy :: Proxy num) % natVal (Proxy :: Proxy den)
{-# INLINE scaleFromProxy #-}

--------------------------------------------------------------------------------

type family ErrScaleNonCanonical (currency :: Symbol) :: k where
  ErrScaleNonCanonical c = GHC.TypeError
    ( 'GHC.Text c 'GHC.:<>:
      'GHC.Text " is not a currency with a canonical smallest unit," 'GHC.:$$:
      'GHC.Text "be explicit about the currency unit you want to use." )

type instance Scale' "ARS" "ARS" = Scale' "ARS" "centavo"
type instance Scale' "ARS" "peso" = '(1, 1)
type instance Scale' "ARS" "centavo" = '(100, 1)

type instance Scale' "BTC" "BTC" = Scale' "BTC" "satoshi"
type instance Scale' "BTC" "bitcoin" = '(1, 1)
type instance Scale' "BTC" "satoshi" = '(100000000, 1)

type instance Scale' "GBP" "GBP" = Scale' "GBP" "penny"
type instance Scale' "GBP" "pound" = '(1, 1)
type instance Scale' "GBP" "penny" = '(100, 1)

type instance Scale' "JPY" "JPY" = Scale' "JPY" "JPY"
type instance Scale' "JPY" "yen" = '(1, 1)

type instance Scale' "USD" "USD" = Scale' "USD" "cent"
type instance Scale' "USD" "dollar" = '(1, 1)
type instance Scale' "USD" "cent" = '(100, 1)

type instance Scale' "XAU" "XAU" = ErrScaleNonCanonical "XAU"
type instance Scale' "XAU" "troy-ounce" = '(1, 1)
type instance Scale' "XAU" "grain" = '(480, 1)
type instance Scale' "XAU" "milligrain" = '(480000, 1)
type instance Scale' "XAU" "micrograin" = '(480000000, 1)
type instance Scale' "XAU" "kilogram" = '(31103477, 1000000000)
type instance Scale' "XAU" "gram" = '(31103477, 1000000)
type instance Scale' "XAU" "milligram" = '(31103477, 1000)
type instance Scale' "XAU" "microgram" = '(31103477, 1)

