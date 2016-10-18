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
 ( -- * Dense monetary values
   Dense
 , dense
   -- * Discrete monetary values
 , Discrete
 , fromDiscrete
 , coerceUnit
 , round
 , ceiling
 , floor
 , truncate
   -- * Currency scales
 , Scale
 , Scale'
 , GoodScale
 , ErrScaleNonCanonical
 , scale
 , scaleFromProxy
   -- * Currency exchange
 , ExchangeRate
 , exchangeRate
 , fromExchangeRate
 , flipExchangeRate
 , exchange
 ) where

import Control.Applicative (empty)
import Data.Proxy (Proxy(..))
import Data.Ratio ((%))
import GHC.Real (infinity, notANumber)
import GHC.TypeLits
  (Symbol, Nat, CmpNat, KnownNat, natVal)
import qualified GHC.TypeLits as GHC
import Prelude hiding (round, ceiling, floor, truncate)
import qualified Prelude
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readPrec)

--------------------------------------------------------------------------------
-- | 'Dense' represents a dense monetary value for @currency@ (usually a
-- ISO-4217 currency code, but not necessarily).
--
-- While monetary values associated with a particular currency are discrete, you
-- can still treat monetary values as dense while operating on them. For
-- example, the half of @USD 3.41@ is @USD 1.705@, which is not an amount that
-- can't be represented as a number of USD cents (the smallest unit that can
-- represent USD amounts). Nevertheless, if you eventually multiply @USD 1.705@
-- by @4@, for example, you end up with @USD 6.82@, which is again a value
-- representable as USD cents. In other words, 'Dense' monetary values
-- allow us to perform precise calculations deferring the conversion to a
-- 'Discrete' monetary values as much as posible. Once you are ready to
-- aproximate a 'Dense' value to a 'Discrete' value you can use one of
-- 'round', 'floor', 'ceiling' or 'truncate'. Otherwise, using 'toRational' you
-- can obtain a precise 'Rational' representation.
--
-- Construct 'Dense' monetary values using 'dense', or
-- 'fromInteger'/'fromIntegral' if that suffices.
newtype Dense (currency :: Symbol) = Dense Rational
  deriving (Eq, Ord, Num, Real, Fractional, Show)

instance Read (Dense currency) where
  readPrec = do
    _ <- ReadPrec.lift (ReadP.string "Dense ")
    maybe empty pure =<< fmap dense readPrec

-- | Build a 'Dense' monetary value from a 'Rational' value.
--
-- For example, if you want to represent @USD 12.52316@, then you can use:
--
-- @
-- 'dense' (125316 % 10000)
-- @
--
-- This function returns 'Nothing' in case the given 'Rational' is 'infinity' or
-- 'notANumber'.
dense :: Rational -> Maybe (Dense currency)
dense = \r0 ->
  if (infinity == r0 || notANumber == r0)
  then Nothing else Just (Dense r0)
{-# INLINE dense #-}

-- | 'Discrete' represents a discrete monetary value for a @currency@ expresed
-- as an integer amount of a particular @unit@. For example, with @currency ~
-- \"USD\"@ and @unit ~ \"cent\"@ you can represent United States Dollars to
-- their full extent.
--
-- @currency@ is usually a ISO-4217 currency code, but not necessarily.
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
  deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

instance Read (Discrete currency unit) where
  readPrec = do
    _ <- ReadPrec.lift (ReadP.string "Discrete ")
    Discrete <$> readPrec

instance
  ( GHC.TypeError
      (('GHC.Text "The ") 'GHC.:<>:
       ('GHC.ShowType Discrete) 'GHC.:<>:
       ('GHC.Text " type is deliberately not a ") 'GHC.:<>:
       ('GHC.ShowType Fractional) 'GHC.:$$:
       ('GHC.Text "instance. Convert the ") 'GHC.:<>:
       ('GHC.ShowType Discrete) 'GHC.:<>:
       ('GHC.Text " value to a ") 'GHC.:<>:
       ('GHC.ShowType Dense) 'GHC.:$$:
       ('GHC.Text "value and use the ") 'GHC.:<>:
       ('GHC.ShowType Fractional) 'GHC.:<>:
       ('GHC.Text " features on it instead.")) )
  => Fractional (Discrete currency unit) where
  fromRational = undefined
  recip = undefined

-- | Convert currency 'Discrete' monetary value into a 'Dense' monetary
-- value.
fromDiscrete
  :: GoodScale currency unit
  => Discrete currency unit
  -> Dense currency -- ^
fromDiscrete = \c@(Discrete i) -> Dense (fromInteger i / scale c)
{-# INLINE fromDiscrete #-}

-- | Rename a 'Discrete''s @unit@, provided the new unit shares the same 'Scale'
-- as the original.
--
-- This is useful for converting between cases such as @'Discrete' \"USD\"
-- \"USD\"@ and @'Discrete' \"USD\" \"cent\"@, which have the same meaning yet
-- different types.
coerceUnit
  :: Scale' currency unit1 ~ Scale' currency unit2
  => Discrete currency unit1
  -> Discrete currency unit2
coerceUnit = \(Discrete i) -> Discrete i
{-# INLINE coerceUnit #-}

-- | Internal. Used to implement 'round', 'ceiling', 'floor' and 'truncate'.
roundf
  :: GoodScale currency unit
  => (Rational -> Integer) -- ^ 'Prelude.round', 'Prelude.ceiling' or similar.
  -> Dense currency
  -> (Discrete currency unit, Maybe (Dense currency))
roundf f = \c0 ->
  let r0 = toRational c0 :: Rational
      r1 = r0 * scale d2 :: Rational
      i2 = f r1 :: Integer
      r2 = fromInteger i2 / scale d2 :: Rational
      ycrest | r0 == r2  = Nothing
             | otherwise = Just (Dense (r0 - r2))
      d2 = Discrete i2
  in (d2, ycrest)
{-# INLINE roundf #-}

-- | Round a 'Dense' value @x@ to the nearest value fully representable in
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
  :: GoodScale currency unit
  => Dense currency
  -> (Discrete currency unit, Maybe (Dense currency)) -- ^
round = roundf Prelude.round
{-# INLINE round #-}

-- | Round a 'Dense' value @x@ to the nearest value fully representable in
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
  :: GoodScale currency unit
  => Dense currency
  -> (Discrete currency unit, Maybe (Dense currency)) -- ^
ceiling = roundf Prelude.ceiling
{-# INLINE ceiling #-}

-- | Round a 'Dense' value @x@ to the nearest value fully representable in
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
  :: GoodScale currency unit
  => Dense currency
  -> (Discrete currency unit, Maybe (Dense currency)) -- ^
floor = roundf Prelude.floor
{-# INLINE floor #-}

-- | Round a 'Dense' value @x@ to the nearest value between zero and
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
-- Proof that 'truncate' doesn't lose money:
--
-- @
-- x == case 'truncate' x of
--        (y, 'Nothing') -> y
--        (y, 'Just' z)  -> y + z
-- @
truncate
  :: GoodScale currency unit
  => Dense currency
  -> (Discrete currency unit, Maybe (Dense currency)) -- ^
truncate = roundf Prelude.truncate
{-# INLINE truncate #-}

--------------------------------------------------------------------------------

-- | Like 'Scale'', but the @currency@'s @unit@ is expected to be the smallest
-- discrete unit that can represent the it in its full extent. For example,
-- cents are the smallest unit that can represent United States Dollars, so:
--
-- @
-- 'Scale' \"USD\"  ~  'Scale'' \"USD\" \"USD\"  ~  'Scale'' \"USD\" \"cent\"
-- @
--
-- If you try to obtain the 'Scale' of a @currency@ without an obvious smallest
-- representable @unit@, like XAU, you will get a compile error.
type Scale (currency :: Symbol) = Scale' currency currency

-- | @'Scale'' currency unit@ is a rational number (expressed as @'(numerator,
-- denominator)@) indicating how many pieces of @unit@ fit in @currency@.
--
-- @currency@ is usually a ISO-4217 currency code, but not necessarily.
--
-- The 'Scale'' will determine how to convert a 'Dense' value into a
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

-- | Constraints to @'Scale'' currency unit@ expected to always be satisfied. In
-- particular, the scale is always guaranteed to be a positive rational number
-- ('infinity' and 'notANumber' are forbidden by 'GoodScale').
type GoodScale (currency :: Symbol) (unit :: Symbol)
  = ( CmpNat 0 (Fst (Scale' currency unit)) ~ 'LT
    , CmpNat 0 (Snd (Scale' currency unit)) ~ 'LT
    , KnownNat (Fst (Scale' currency unit))
    , KnownNat (Snd (Scale' currency unit))
    )

-- | Term-level representation for the @currency@'s @unit@ 'Scale''.
--
-- For example, the 'Scale'' for @\"USD\"@ in @\"cent\"@s is @100/1@.
--
-- The returned 'Rational' is statically guaranteed to be a positive number, and
-- to be different from both 'notANumber' and 'infinity'.
scale
  :: forall currency unit
  .  GoodScale currency unit
  => Discrete currency unit
  -> Rational
scale = \_ -> scaleFromProxy (Proxy :: Proxy currency) (Proxy :: Proxy unit)
{-# INLINE scale #-}

-- | Like 'scale', but takes proxies (e.g., 'Proxy') instead of 'Discrete'.
scaleFromProxy
  :: forall currency unit proxy1 proxy2
  .  GoodScale currency unit
  => proxy1 currency
  -> proxy2 unit
  -> Rational
scaleFromProxy = \_ _ ->
   natVal (Proxy :: Proxy (Fst (Scale' currency unit))) %
   natVal (Proxy :: Proxy (Snd (Scale' currency unit)))
{-# INLINE scaleFromProxy #-}

--------------------------------------------------------------------------------

-- | Exchange rate for converting monetary values of currency @src@ into
-- monetary values of currency @dst@ by multiplying for it.
--
-- For example, if in order to convert USD to GBP we have to multiply by 1.2345,
-- then we can represent this situaion using:
--
-- @
-- 'exchangeRate' (12345 % 10000) :: 'Maybe' ('ExchangeRate' \"USD\" \"GBP\")
-- @
newtype ExchangeRate (src :: Symbol) (dst :: Symbol) = ExchangeRate Rational
  deriving (Eq, Ord, Show)

instance Read (ExchangeRate (src :: Symbol) (dst :: Symbol)) where
  readPrec = maybe empty pure =<< fmap exchangeRate readPrec

-- | Obtain a 'Rational' representation of the 'ExchangeRate'.
--
-- This 'Rational' is statically guaranteed to be greater than 0, different
-- from 'infinity' and different from 'notANumber'.
fromExchangeRate :: ExchangeRate src dst -> Rational
fromExchangeRate = \(ExchangeRate r0) -> r0
{-# INLINE fromExchangeRate #-}

-- | Safely construct an 'ExchangeRate' from a 'Rational' number.
--
-- For construction to succeed, this 'Rational' must be greater than 0,
-- different from 'infinity' and different from 'notANumber'.
exchangeRate :: Rational -> Maybe (ExchangeRate src dst)
exchangeRate = \r0 ->
  if (r0 <= 0 || infinity == r0 || notANumber == r0)
  then Nothing else Just (ExchangeRate r0)
{-# INLINE exchangeRate #-}

-- | Flip the direction of an 'ExchangeRate'.
--
-- Identity law:
--
-- @
-- 'flipExchangeRate' . 'flipExchangeRate'   ==  'id'
-- @
flipExchangeRate :: ExchangeRate a b -> ExchangeRate b a
flipExchangeRate = \(ExchangeRate x) -> ExchangeRate (1 / x)
{-# INLINE flipExchangeRate #-}

-- | Apply the 'ExchangeRate' to the given @'Dense' src@ monetary value.
--
-- Identity law:
--
-- @
-- 'exchange' ('flipExchangeRate' x) . 'exchange' x  ==  'id'
-- @
--
-- Use the /Identity law/ for reasoning about going back and forth between @src@
-- and @dst@ in order to manage any leftovers that might not be representable as
-- a 'Discrete' monetary value of @src@.
exchange :: ExchangeRate src dst -> Dense src -> Dense dst
exchange = \(ExchangeRate r) -> \(Dense s) -> Dense (r * s)
{-# INLINE exchange #-}

--------------------------------------------------------------------------------
-- Miscellaneous

type family Fst (ab :: (ka, kb)) :: ka where Fst '(a,b) = a
type family Snd (ab :: (ka, kb)) :: ka where Snd '(a,b) = b

--------------------------------------------------------------------------------
-- Currency scales

-- | A friendly 'GHC.TypeError' to use for a @currency@ that doesn't have a
-- cannonical small unit.
type family ErrScaleNonCanonical (currency :: Symbol) :: k where
  ErrScaleNonCanonical c = GHC.TypeError
    ( 'GHC.Text c 'GHC.:<>:
      'GHC.Text " is not a currency with a canonical smallest unit," 'GHC.:$$:
      'GHC.Text "be explicit about the currency unit you want to use." )

-- | United Arab Emirates dirham
type instance Scale' "AED" "AED" = '(100, 1)
type instance Scale' "AED" "dirham" = '(1, 1)
type instance Scale' "AED" "fils" = '(100, 1)
-- | Afghan afghani
type instance Scale' "AFN" "AFN" = '(100, 1)
type instance Scale' "AFN" "afghani" = '(1, 1)
type instance Scale' "AFN" "pul" = '(100, 1)
-- | Albanian lek
type instance Scale' "ALL" "ALL" = '(100, 1)
type instance Scale' "ALL" "lek" = '(1, 1)
type instance Scale' "ALL" "qindarke" = '(100, 1)
-- | Armenian dram
type instance Scale' "AMD" "AMD" = '(100, 1)
type instance Scale' "AMD" "dram" = '(1, 1)
type instance Scale' "AMD" "luma" = '(100, 1)
-- | Netherlands Antillean guilder
type instance Scale' "ANG" "ANG" = '(100, 1)
type instance Scale' "ANG" "guilder" = '(1, 1)
type instance Scale' "ANG" "cent" = '(100, 1)
-- | Angolan kwanza
type instance Scale' "AOA" "AOA" = '(100, 1)
type instance Scale' "AOA" "kwanza" = '(1, 1)
type instance Scale' "AOA" "centimo" = '(100, 1)
-- | Argentine peso
type instance Scale' "ARS" "ARS" = '(100, 1)
type instance Scale' "ARS" "peso" = '(1, 1)
type instance Scale' "ARS" "centavo" = '(100, 1)
-- | Australian dollar
type instance Scale' "AUD" "AUD" = '(100, 1)
type instance Scale' "AUD" "dollar" = '(1, 1)
type instance Scale' "AUD" "cent" = '(100, 1)
-- | Aruban florin
type instance Scale' "AWG" "AWG" = '(100, 1)
type instance Scale' "AWG" "florin" = '(1, 1)
type instance Scale' "AWG" "cent" = '(100, 1)
-- | Azerbaijani manat
type instance Scale' "AZN" "AZN" = '(100, 1)
type instance Scale' "AZN" "manat" = '(1, 1)
type instance Scale' "AZN" "qəpik" = '(100, 1)
-- | Bosnia and Herzegovina convertible mark
type instance Scale' "BAM" "BAM" = '(100, 1)
type instance Scale' "BAM" "mark" = '(1, 1)
type instance Scale' "BAM" "fening" = '(100, 1)
-- | Barbadian dollar
type instance Scale' "BBD" "BBD" = '(100, 1)
type instance Scale' "BBD" "dollar" = '(1, 1)
type instance Scale' "BBD" "cent" = '(100, 1)
-- | Bangladeshi taka
type instance Scale' "BDT" "BDT" = '(100, 1)
type instance Scale' "BDT" "taka" = '(1, 1)
type instance Scale' "BDT" "paisa" = '(100, 1)
-- | Bulgarian lev
type instance Scale' "BGN" "BGN" = '(100, 1)
type instance Scale' "BGN" "lev" = '(1, 1)
type instance Scale' "BGN" "stotinka" = '(100, 1)
-- | Bahraini dinar
type instance Scale' "BHD" "BHD" = '(1000, 1)
type instance Scale' "BHD" "dinar" = '(1, 1)
type instance Scale' "BHD" "fils" = '(1000, 1)
-- | Burundi franc
type instance Scale' "BIF" "BIF" = '(100, 1)
type instance Scale' "BIF" "franc" = '(1, 1)
type instance Scale' "BIF" "centime" = '(100, 1)
-- | Bermudian dollar
type instance Scale' "BMD" "BMD" = '(100, 1)
type instance Scale' "BMD" "dollar" = '(1, 1)
type instance Scale' "BMD" "cent" = '(100, 1)
-- | Brunei dollar
type instance Scale' "BND" "BND" = '(100, 1)
type instance Scale' "BND" "dollar" = '(1, 1)
type instance Scale' "BND" "sen" = '(100, 1)
-- | Bolivian boliviano
type instance Scale' "BOB" "BOB" = '(100, 1)
type instance Scale' "BOB" "boliviano" = '(1, 1)
type instance Scale' "BOB" "centavo" = '(100, 1)
-- | Bolivian Mvdol
type instance Scale' "BOV" "BOV" = '(100, 1)
-- | Brazilian real
type instance Scale' "BRL" "BRL" = '(100, 1)
type instance Scale' "BRL" "real" = '(1, 1)
type instance Scale' "BRL" "centavo" = '(100, 1)
-- | Bahamian dollar
type instance Scale' "BSD" "BSD" = '(100, 1)
type instance Scale' "BSD" "dollar" = '(1, 1)
type instance Scale' "BSD" "cent" = '(1, 1)
-- | Bhutanese ngultrum
type instance Scale' "BTN" "BTN" = '(100, 1)
type instance Scale' "BTN" "ngultrum" = '(1, 1)
type instance Scale' "BTN" "chetrum" = '(100, 1)
-- | Botswana pula
type instance Scale' "BWP" "BWP" = '(100, 1)
type instance Scale' "BWP" "pula" = '(1, 1)
type instance Scale' "BWP" "thebe" = '(100, 1)
-- | Belarusian ruble
type instance Scale' "BYN" "BYN" = '(100, 1)
-- | Belarusian ruble
type instance Scale' "BYR" "BYR" = '(100, 1)
type instance Scale' "BYR" "ruble" = '(1, 1)
type instance Scale' "BYR" "kapyeyka" = '(100, 1)
-- | Belize dollar
type instance Scale' "BZD" "BZD" = '(100, 1)
type instance Scale' "BZD" "dollar" = '(1, 1)
type instance Scale' "BZD" "cent" = '(100, 1)
-- | Canadian dollar
type instance Scale' "CAD" "CAD" = '(100, 1)
type instance Scale' "CAD" "dollar" = '(1, 1)
type instance Scale' "CAD" "cent" = '(100, 1)
-- | Congolese franc
type instance Scale' "CDF" "CDF" = '(100, 1)
type instance Scale' "CDF" "franc" = '(1, 1)
type instance Scale' "CDF" "centime" = '(100, 1)
-- | WIR euro
type instance Scale' "CHE" "CHE" = '(100, 1)
-- | Swiss franc
type instance Scale' "CHF" "CHF" = '(100, 1)
type instance Scale' "CHF" "franc" = '(1, 1)
type instance Scale' "CHF" "rappen" = '(100, 1)
-- | WIR franc
type instance Scale' "CHW" "CHW" = '(100, 1)
-- | Chilean unidad de fomento
type instance Scale' "CLF" "CLF" = '(100, 1)
-- | Chilean peso
type instance Scale' "CLP" "CLP" = '(100, 1)
type instance Scale' "CLP" "peso" = '(1, 1)
type instance Scale' "CLP" "centavo" = '(100, 1)
-- | Chinese Renminbi
type instance Scale' "CNY" "CNY" = '(100, 1)
type instance Scale' "CNY" "yuan" = '(1, 1)
type instance Scale' "CNY" "fen" = '(100, 1)
-- | Colombian peso
type instance Scale' "COP" "COP" = '(100, 1)
type instance Scale' "COP" "peso" = '(1, 1)
type instance Scale' "COP" "centavo" = '(100, 1)
-- | Colombian unidad de valor real
type instance Scale' "COU" "COU" = '(100, 1)
-- | Costa Rican colon
type instance Scale' "CRC" "CRC" = '(100, 1)
type instance Scale' "CRC" "colon" = '(1, 1)
type instance Scale' "CRC" "centimo" = '(100, 1)
-- | Cuban peso convertible
type instance Scale' "CUC" "CUC" = '(100, 1)
type instance Scale' "CUC" "peso" = '(1, 1)
type instance Scale' "CUC" "centavo" = '(100, 1)
-- | Cuban peso
type instance Scale' "CUP" "CUP" = '(100, 1)
type instance Scale' "CUP" "peso" = '(1, 1)
type instance Scale' "CUP" "centavo" = '(100, 1)
-- | Cape Verdean escudo
type instance Scale' "CVE" "CVE" = '(100, 1)
type instance Scale' "CVE" "escudo" = '(1, 1)
type instance Scale' "CVE" "centavo" = '(100, 1)
-- | Czech koruna
type instance Scale' "CZK" "CZK" = '(100, 1)
type instance Scale' "CZK" "koruna" = '(1, 1)
type instance Scale' "CZK" "haler" = '(100, 1)
-- | Djiboutian franc
type instance Scale' "DJF" "DJF" = '(100, 1)
type instance Scale' "DJF" "franc" = '(1, 1)
type instance Scale' "DJF" "centime" = '(100, 1)
-- | Danish krone
type instance Scale' "DKK" "DKK" = '(100, 1)
type instance Scale' "DKK" "krone" = '(1, 1)
type instance Scale' "DKK" "ore" = '(100, 1)
-- | Dominican peso
type instance Scale' "DOP" "DOP" = '(100, 1)
type instance Scale' "DOP" "peso" = '(1, 1)
type instance Scale' "DOP" "centavo" = '(100, 1)
-- | Algerian dinar
type instance Scale' "DZD" "DZD" = '(100, 1)
type instance Scale' "DZD" "dinar" = '(1, 1)
type instance Scale' "DZD" "santeem" = '(100, 1)
-- | Egyptian pound
type instance Scale' "EGP" "EGP" = '(100, 1)
type instance Scale' "EGP" "pound" = '(1, 1)
type instance Scale' "EGP" "piastre" = '(100, 1)
-- | Eritrean nakfa
type instance Scale' "ERN" "ERN" = '(100, 1)
type instance Scale' "ERN" "nafka" = '(1, 1)
type instance Scale' "ERN" "cent" = '(100, 1)
-- | Ethiopian birr
type instance Scale' "ETB" "ETB" = '(100, 1)
type instance Scale' "ETB" "birr" = '(1, 1)
type instance Scale' "ETB" "santim" = '(100, 1)
-- | European euro
type instance Scale' "EUR" "EUR" = '(100, 1)
type instance Scale' "EUR" "euro" = '(1, 1)
type instance Scale' "EUR" "cent" = '(100, 1)
-- | Fijian dollar
type instance Scale' "FJD" "FJD" = '(100, 1)
-- | Falkland Islands pound
type instance Scale' "FKP" "FKP" = '(100, 1)
type instance Scale' "FKP" "pound" = '(1, 1)
type instance Scale' "FKP" "penny" = '(100, 1)
-- | Pound sterling
type instance Scale' "GBP" "GBP" = '(100, 1)
type instance Scale' "GBP" "pound" = '(1, 1)
type instance Scale' "GBP" "penny" = '(100, 1)
-- | Georgian lari
type instance Scale' "GEL" "GEL" = '(100, 1)
type instance Scale' "GEL" "lari" = '(1, 1)
type instance Scale' "GEL" "tetri" = '(100, 1)
-- | Ghanaian cedi
type instance Scale' "GHS" "GHS" = '(100, 1)
type instance Scale' "GHS" "cedi" = '(1, 1)
type instance Scale' "GHS" "pesewa" = '(100, 1)
-- | Gibraltar pound
type instance Scale' "GIP" "GIP" = '(100, 1)
type instance Scale' "GIP" "pound" = '(1, 1)
type instance Scale' "GIP" "penny" = '(100, 1)
-- | Gambian dalasi
type instance Scale' "GMD" "GMD" = '(100, 1)
type instance Scale' "GMD" "dalasi" = '(1, 1)
type instance Scale' "GMD" "butut" = '(100, 1)
-- | Guinean franc
type instance Scale' "GNF" "GNF" = '(100, 1)
type instance Scale' "GNF" "franc" = '(1, 1)
type instance Scale' "GNF" "centime" = '(100, 1)
-- | Guatemalan quetzal
type instance Scale' "GTQ" "GTQ" = '(100, 1)
type instance Scale' "GTQ" "quetzal" = '(1, 1)
type instance Scale' "GTQ" "centavo" = '(100, 1)
-- | Guyanese dollar
type instance Scale' "GYD" "GYD" = '(100, 1)
type instance Scale' "GYD" "dollar" = '(1, 1)
type instance Scale' "GYD" "cent" = '(100, 1)
-- | Hong Kong dollar
type instance Scale' "HKD" "HKD" = '(100, 1)
type instance Scale' "HKD" "dollar" = '(1, 1)
type instance Scale' "HKD" "cent" = '(100, 1)
-- | Honduran lempira
type instance Scale' "HNL" "HNL" = '(100, 1)
type instance Scale' "HNL" "lempira" = '(1, 1)
type instance Scale' "HNL" "centavo" = '(100, 1)
-- | Croatian kuna
type instance Scale' "HRK" "HRK" = '(100, 1)
type instance Scale' "HRK" "kuna" = '(1, 1)
type instance Scale' "HRK" "lipa" = '(100, 1)
-- | Haitian gourde
type instance Scale' "HTG" "HTG" = '(100, 1)
type instance Scale' "HTG" "gourde" = '(1, 1)
type instance Scale' "HTG" "centime" = '(1, 1)
-- | Hungarian forint
type instance Scale' "HUF" "HUF" = '(100, 1)
type instance Scale' "HUF" "forint" = '(1, 1)
type instance Scale' "HUF" "filler" = '(100, 1)
-- | Indonesian rupiah
type instance Scale' "IDR" "IDR" = '(100, 1)
type instance Scale' "IDR" "rupiah" = '(1, 1)
type instance Scale' "IDR" "sen" = '(100, 1)
-- | Israeli new shekel
type instance Scale' "ILS" "ILS" = '(100, 1)
type instance Scale' "ILS" "shekel" = '(1, 1)
type instance Scale' "ILS" "agora" = '(100, 1)
-- | Indian rupee
type instance Scale' "INR" "INR" = '(100, 1)
type instance Scale' "INR" "rupee" = '(1, 1)
type instance Scale' "INR" "paisa" = '(100, 1)
-- | Iraqi dinar
type instance Scale' "IQD" "IQD" = '(1000, 1)
type instance Scale' "IQD" "dinar" = '(1, 1)
type instance Scale' "IQD" "fils" = '(1000, 1)
-- | Iranian rial
type instance Scale' "IRR" "IRR" = '(100, 1)
type instance Scale' "IRR" "rial" = '(1, 1)
type instance Scale' "IRR" "dinar" = '(100, 1)
-- | Icelandic króna
type instance Scale' "ISK" "ISK" = '(100, 1)
type instance Scale' "ISK" "krona" = '(1, 1)
type instance Scale' "ISK" "eyir" = '(100, 1)
-- | Jamaican dollar
type instance Scale' "JMD" "JMD" = '(100, 1)
type instance Scale' "JMD" "dollar" = '(1, 1)
type instance Scale' "JMD" "cent" = '(100, 1)
-- | Jordanian dinar
type instance Scale' "JOD" "JOD" = '(100, 1)
type instance Scale' "JOD" "dinar" = '(1, 1)
type instance Scale' "JOD" "piastre" = '(100, 1)
-- | Japanese yen
type instance Scale' "JPY" "JPY" = '(1, 1)
type instance Scale' "JPY" "yen" = '(1, 1)
-- | Kenyan shilling
type instance Scale' "KES" "KES" = '(100, 1)
type instance Scale' "KES" "shilling" = '(1, 1)
type instance Scale' "KES" "cent" = '(100, 1)
-- | Kyrgyzstani som
type instance Scale' "KGS" "KGS" = '(100, 1)
type instance Scale' "KGS" "som" = '(1, 1)
type instance Scale' "KGS" "tyiyn" = '(100, 1)
-- | Cambodian riel
type instance Scale' "KHR" "KHR" = '(100, 1)
type instance Scale' "KHR" "riel" = '(1, 1)
type instance Scale' "KHR" "sen" = '(100, 1)
-- | Comorian franc
type instance Scale' "KMF" "KMF" = '(100, 1)
type instance Scale' "KMF" "franc" = '(1, 1)
type instance Scale' "KMF" "centime" = '(100, 1)
-- | North Korean won
type instance Scale' "KPW" "KPW" = '(100, 1)
type instance Scale' "KPW" "won" = '(1, 1)
type instance Scale' "KPW" "chon" = '(100, 1)
-- | South Korean won
type instance Scale' "KRW" "KRW" = '(100, 1)
type instance Scale' "KRW" "won" = '(1, 1)
type instance Scale' "KRW" "jeon" = '(100, 1)
-- | Kuwaiti dinar
type instance Scale' "KWD" "KWD" = '(1000, 1)
type instance Scale' "KWD" "dinar" = '(1, 1)
type instance Scale' "KWD" "fils" = '(1000, 1)
-- | Cayman Islands dollar
type instance Scale' "KYD" "KYD" = '(100, 1)
type instance Scale' "KYD" "dollar" = '(1, 1)
type instance Scale' "KYD" "cent" = '(100, 1)
-- | Kazakhstani tenge
type instance Scale' "KZT" "KZT" = '(100, 1)
type instance Scale' "KZT" "tenge" = '(1, 1)
type instance Scale' "KZT" "tiyin" = '(100, 1)
-- | Lao kip
type instance Scale' "LAK" "LAK" = '(100, 1)
type instance Scale' "LAK" "kip" = '(1, 1)
type instance Scale' "LAK" "att" = '(100, 1)
-- | Lebanese pound
type instance Scale' "LBP" "LBP" = '(100, 1)
type instance Scale' "LBP" "pound" = '(1, 1)
type instance Scale' "LBP" "piastre" = '(100, 1)
-- | Sri Lankan rupee
type instance Scale' "LKR" "LKR" = '(100, 1)
type instance Scale' "LKR" "rupee" = '(1, 1)
type instance Scale' "LKR" "cent" = '(100, 1)
-- | Liberian dollar
type instance Scale' "LRD" "LRD" = '(100, 1)
type instance Scale' "LRD" "dollar" = '(1, 1)
type instance Scale' "LRD" "cent" = '(100, 1)
-- | Lesotho loti
type instance Scale' "LSL" "LSL" = '(100, 1)
type instance Scale' "LSL" "loti" = '(1, 1)
type instance Scale' "LSL" "sente" = '(100, 1)
-- | Libyan dinar
type instance Scale' "LYD" "LYD" = '(100, 1)
type instance Scale' "LYD" "dinar" = '(1, 1)
type instance Scale' "LYD" "dirham" = '(1000, 1)
-- | Moroccan dirham
type instance Scale' "MAD" "MAD" = '(100, 1)
type instance Scale' "MAD" "dirham" = '(1, 1)
type instance Scale' "MAD" "centime" = '(100, 1)
-- | Moldovan leu
type instance Scale' "MDL" "MDL" = '(100, 1)
type instance Scale' "MDL" "leu" = '(100, 1)
type instance Scale' "MDL" "ban" = '(100, 1)
-- | Malagasy ariary
type instance Scale' "MGA" "MGA" = '(5, 1)
type instance Scale' "MGA" "ariary" = '(1, 1)
type instance Scale' "MGA" "iraimbilanja" = '(5, 1)
-- | Macedonian denar
type instance Scale' "MKD" "MKD" = '(100, 1)
type instance Scale' "MKD" "denar" = '(1, 1)
type instance Scale' "MKD" "deni" = '(100, 1)
-- | Myanmar kyat
type instance Scale' "MMK" "MMK" = '(100, 1)
type instance Scale' "MMK" "kyat" = '(1, 1)
type instance Scale' "MMK" "pya" = '(100, 1)
-- | Mongolian tugrik
type instance Scale' "MNT" "MNT" = '(100, 1)
type instance Scale' "MNT" "tugrik" = '(1, 1)
type instance Scale' "MNT" "mongo" = '(100, 1)
-- | Macanese pataca
type instance Scale' "MOP" "MOP" = '(100, 1)
type instance Scale' "MOP" "pataca" = '(1, 1)
type instance Scale' "MOP" "avo" = '(100, 1)
-- | Mauritanian ouguiya
type instance Scale' "MRO" "MRO" = '(5, 1)
type instance Scale' "MRO" "ouguiya" = '(1, 1)
type instance Scale' "MRO" "khoums" = '(5, 1)
-- | Mauritian rupee
type instance Scale' "MUR" "MUR" = '(100, 1)
type instance Scale' "MUR" "rupee" = '(1, 1)
type instance Scale' "MUR" "cent" = '(100, 1)
-- | Maldivian rufiyaa
type instance Scale' "MVR" "MVR" = '(100, 1)
type instance Scale' "MVR" "rufiyaa" = '(1, 1)
type instance Scale' "MVR" "laari" = '(100, 1)
-- | Malawian kwacha
type instance Scale' "MWK" "MWK" = '(100, 1)
type instance Scale' "MWK" "kwacha" = '(1, 1)
type instance Scale' "MWK" "tambala" = '(100, 1)
-- | Mexican peso
type instance Scale' "MXN" "MXN" = '(100, 1)
type instance Scale' "MXN" "peso" = '(1, 1)
type instance Scale' "MXN" "centavo" = '(100, 1)
-- | Mexican unidad de inversion
type instance Scale' "MXV" "MXV" = '(100, 1)
-- | Malaysian ringgit
type instance Scale' "MYR" "MYR" = '(100, 1)
type instance Scale' "MYR" "ringgit" = '(1, 1)
type instance Scale' "MYR" "sen" = '(100, 1)
-- | Mozambican metical
type instance Scale' "MZN" "MZN" = '(100, 1)
type instance Scale' "MZN" "metical" = '(1, 1)
type instance Scale' "MZN" "centavo" = '(100, 1)
-- | Namibian dollar
type instance Scale' "NAD" "NAD" = '(100, 1)
type instance Scale' "NAD" "dollar" = '(1, 1)
type instance Scale' "NAD" "cent" = '(100, 1)
-- | Nigerian naira
type instance Scale' "NGN" "NGN" = '(100, 1)
type instance Scale' "NGN" "naira" = '(1, 1)
type instance Scale' "NGN" "kobo" = '(100, 1)
-- | Nicaraguan cordoba
type instance Scale' "NIO" "NIO" = '(100, 1)
type instance Scale' "NIO" "cordoba" = '(1, 1)
type instance Scale' "NIO" "centavo" = '(100, 1)
-- | Norwegian krone
type instance Scale' "NOK" "NOK" = '(100, 1)
type instance Scale' "NOK" "krone" = '(1, 1)
type instance Scale' "NOK" "øre" = '(100, 1)
-- | Nepalese rupee
type instance Scale' "NPR" "NPR" = '(100, 1)
type instance Scale' "NPR" "rupee" = '(1, 1)
type instance Scale' "NPR" "paisa" = '(100, 1)
-- | New Zealand dollar
type instance Scale' "NZD" "NZD" = '(100, 1)
type instance Scale' "NZD" "dollar" = '(1, 1)
type instance Scale' "NZD" "cent" = '(100, 1)
-- | Omani rial
type instance Scale' "OMR" "OMR" = '(1000, 1)
type instance Scale' "OMR" "rial" = '(1, 1)
type instance Scale' "OMR" "baisa" = '(1000, 1)
-- | Panamenian balboa
type instance Scale' "PAB" "PAB" = '(100, 1)
type instance Scale' "PAB" "balboa" = '(1, 1)
type instance Scale' "PAB" "centesimo" = '(100, 1)
-- | Peruvian sol
type instance Scale' "PEN" "PEN" = '(100, 1)
type instance Scale' "PEN" "sol" = '(1, 1)
type instance Scale' "PEN" "centimo" = '(100, 1)
-- | Papua New Guinean kina
type instance Scale' "PGK" "PGK" = '(100, 1)
type instance Scale' "PGK" "kina" = '(1, 1)
type instance Scale' "PGK" "toea" = '(100, 1)
-- | Philippine peso
type instance Scale' "PHP" "PHP" = '(100, 1)
type instance Scale' "PHP" "peso" = '(1, 1)
type instance Scale' "PHP" "centavo" = '(100, 1)
-- | Pakistani rupee
type instance Scale' "PKR" "PKR" = '(100, 1)
type instance Scale' "PKR" "rupee" = '(1, 1)
type instance Scale' "PKR" "paisa" = '(100, 1)
-- | Polish zloty
type instance Scale' "PLN" "PLN" = '(100, 1)
type instance Scale' "PLN" "zloty" = '(1, 1)
type instance Scale' "PLN" "grosz" = '(100, 1)
-- | Paraguayan guarani
type instance Scale' "PYG" "PYG" = '(100, 1)
type instance Scale' "PYG" "guarani" = '(1, 1)
type instance Scale' "PYG" "centimo" = '(100, 1)
-- | Qatari riyal
type instance Scale' "QAR" "QAR" = '(100, 1)
type instance Scale' "QAR" "riyal" = '(1, 1)
type instance Scale' "QAR" "dirham" = '(100, 1)
-- | Romanian leu
type instance Scale' "RON" "RON" = '(100, 1)
type instance Scale' "RON" "leu" = '(1, 1)
type instance Scale' "RON" "ban" = '(100, 1)
-- | Serbian dinar
type instance Scale' "RSD" "RSD" = '(100, 1)
type instance Scale' "RSD" "dinar" = '(1, 1)
type instance Scale' "RSD" "para" = '(100, 1)
-- | Russian ruble
type instance Scale' "RUB" "RUB" = '(100, 1)
type instance Scale' "RUB" "ruble" = '(1, 1)
type instance Scale' "RUB" "kopek" = '(100, 1)
-- | Rwandan franc
type instance Scale' "RWF" "RWF" = '(100, 1)
type instance Scale' "RWF" "franc" = '(1, 1)
type instance Scale' "RWF" "centime" = '(100, 1)
-- | Saudi Arabian riyal
type instance Scale' "SAR" "SAR" = '(100, 1)
type instance Scale' "SAR" "riyal" = '(1, 1)
type instance Scale' "SAR" "halala" = '(100, 1)
-- | Solomon Islands dollar
type instance Scale' "SBD" "SBD" = '(100, 1)
type instance Scale' "SBD" "dollar" = '(100, 1)
type instance Scale' "SBD" "cent" = '(100, 1)
-- | Seychellois rupee
type instance Scale' "SCR" "SCR" = '(100, 1)
type instance Scale' "SCR" "rupee" = '(1, 1)
type instance Scale' "SCR" "cent" = '(100, 1)
-- | Sudanese pound
type instance Scale' "SDG" "SDG" = '(100, 1)
type instance Scale' "SDG" "pound" = '(1, 1)
type instance Scale' "SDG" "piastre" = '(100, 1)
-- | Swedish krona
type instance Scale' "SEK" "SEK" = '(100, 1)
type instance Scale' "SEK" "krona" = '(1, 1)
type instance Scale' "SEK" "ore" = '(100, 1)
-- | Singapore dollar
type instance Scale' "SGD" "SGD" = '(100, 1)
type instance Scale' "SGD" "dollar" = '(1, 1)
type instance Scale' "SGD" "cent" = '(100, 1)
-- | Saint Helena pound
type instance Scale' "SHP" "SHP" = '(100, 1)
type instance Scale' "SHP" "pound" = '(1, 1)
type instance Scale' "SHP" "penny" = '(100, 1)
-- | Sierra Leonean leone
type instance Scale' "SLL" "SLL" = '(100, 1)
type instance Scale' "SLL" "leone" = '(1, 1)
type instance Scale' "SLL" "cent" = '(100, 1)
-- | Somali shilling
type instance Scale' "SOS" "SOS" = '(100, 1)
type instance Scale' "SOS" "shilling" = '(1, 1)
type instance Scale' "SOS" "cent" = '(100, 1)
-- | Surinamese dollar
type instance Scale' "SRD" "SRD" = '(100, 1)
type instance Scale' "SRD" "dollar" = '(1, 1)
type instance Scale' "SRD" "cent" = '(100, 1)
-- | South Sudanese pound
type instance Scale' "SSP" "SSP" = '(100, 1)
type instance Scale' "SSP" "pound" = '(1, 1)
type instance Scale' "SSP" "piastre" = '(100, 1)
-- | Sao Tome and Principe dobra
type instance Scale' "STD" "STD" = '(100, 1)
type instance Scale' "STD" "dobra" = '(1, 1)
type instance Scale' "STD" "centimo" = '(100, 1)
-- | Salvadoran colon
type instance Scale' "SVC" "SVC" = '(100, 1)
type instance Scale' "SVC" "colon" = '(1, 1)
type instance Scale' "SVC" "centavo" = '(100, 1)
-- | Syrian pound
type instance Scale' "SYP" "SYP" = '(100, 1)
type instance Scale' "SYP" "pound" = '(1, 1)
type instance Scale' "SYP" "piastre" = '(100, 1)
-- | Swazi lilangeni
type instance Scale' "SZL" "SZL" = '(100, 1)
type instance Scale' "SZL" "lilangeni" = '(1, 1)
type instance Scale' "SZL" "cent" = '(100, 1)
-- | Thai baht
type instance Scale' "THB" "THB" = '(100, 1)
type instance Scale' "THB" "baht" = '(1, 1)
type instance Scale' "THB" "satang" = '(100, 1)
-- | Tajikistani somoni
type instance Scale' "TJS" "TJS" = '(100, 1)
type instance Scale' "TJS" "somoni" = '(1, 1)
type instance Scale' "TJS" "diram" = '(100, 1)
-- | Turkmen manat
type instance Scale' "TMT" "TMT" = '(100, 1)
type instance Scale' "TMT" "manat" = '(1, 1)
type instance Scale' "TMT" "tennesi" = '(100, 1)
-- | Tunisian dinar
type instance Scale' "TND" "TND" = '(1000, 1)
type instance Scale' "TND" "dinar" = '(1, 1)
type instance Scale' "TND" "millime" = '(1000, 1)
-- | Tongan pa’anga
type instance Scale' "TOP" "TOP" = '(100, 1)
type instance Scale' "TOP" "pa'anga" = '(1, 1)
type instance Scale' "TOP" "seniti" = '(100, 1)
-- | Turkish lira
type instance Scale' "TRY" "TRY" = '(100, 1)
type instance Scale' "TRY" "lira" = '(1, 1)
type instance Scale' "TRY" "kuruş" = '(100, 1)
-- | Tobago Trinidad and Tobago dollar
type instance Scale' "TTD" "TTD" = '(100, 1)
type instance Scale' "TTD" "dollar" = '(1, 1)
type instance Scale' "TTD" "cent" = '(100, 1)
-- | New Taiwan dollar
type instance Scale' "TWD" "TWD" = '(100, 1)
type instance Scale' "TWD" "dollar" = '(1, 1)
type instance Scale' "TWD" "cent" = '(100, 1)
-- | Tanzanian shilling
type instance Scale' "TZS" "TZS" = '(100, 1)
type instance Scale' "TZS" "shilling" = '(1, 1)
type instance Scale' "TZS" "cent" = '(100, 1)
-- | Ukrainian hryvnia
type instance Scale' "UAH" "UAH" = '(100, 1)
type instance Scale' "UAH" "hryvnia" = '(1, 1)
type instance Scale' "UAH" "kopiyka" = '(100, 1)
-- | Ugandan shilling
type instance Scale' "UGX" "UGX" = '(100, 1)
type instance Scale' "UGX" "shilling" = '(1, 1)
type instance Scale' "UGX" "cent" = '(100, 1)
-- | United States dollar
type instance Scale' "USD" "USD" = '(100, 1)
type instance Scale' "USD" "dollar" = '(1, 1)
type instance Scale' "USD" "cent" = '(100, 1)
-- | United States dollar (next day)
type instance Scale' "USN" "USN" = '(100, 1)
-- | Uruguayan peso en unidades
type instance Scale' "UYI" "UYI" = '(100, 1)
-- | Uruguayan peso
type instance Scale' "UYU" "UYU" = '(100, 1)
type instance Scale' "UYU" "peso" = '(1, 1)
type instance Scale' "UYU" "centesimo" = '(100, 1)
-- | Uzbekistani som
type instance Scale' "UZS" "UZS" = '(100, 1)
type instance Scale' "UZS" "som" = '(1, 1)
type instance Scale' "UZS" "tiyin" = '(100, 1)
-- | Venezuelan bolivar
type instance Scale' "VEF" "VEF" = '(100, 1)
type instance Scale' "VEF" "bolivar" = '(1, 1)
type instance Scale' "VEF" "centimo" = '(100, 1)
-- | Vietnamese dong
type instance Scale' "VND" "VND" = '(10, 1)
type instance Scale' "VND" "dong" = '(1, 1)
type instance Scale' "VND" "hao" = '(10, 1)
-- | Vanuatu vatu
type instance Scale' "VUV" "VUV" = '(1, 1)
type instance Scale' "VUV" "vatu" = '(1, 1)
-- | Samoan tālā
type instance Scale' "WST" "WST" = '(100, 1)
type instance Scale' "WST" "tala" = '(1, 1)
type instance Scale' "WST" "sene" = '(100, 1)
-- | Central African CFA franc
type instance Scale' "XAF" "XAF" = '(100, 1)
type instance Scale' "XAF" "franc" = '(1, 1)
type instance Scale' "XAF" "centime" = '(100, 1)
-- | East Caribbean dollar
type instance Scale' "XCD" "XCD" = '(100, 1)
type instance Scale' "XCD" "dollar" = '(1, 1)
type instance Scale' "XCD" "cent" = '(100, 1)
-- | International Monetary Fund Special Drawing Right
type instance Scale' "XDR" "XDR" = '(100, 1)
-- | West African CFA franc
type instance Scale' "XOF" "XOF" = '(100, 1)
type instance Scale' "XOF" "franc" = '(1, 1)
type instance Scale' "XOF" "centime" = '(100, 1)
-- | CFP franc
type instance Scale' "XPF" "XPF" = '(100, 1)
type instance Scale' "XPF" "franc" = '(1, 1)
type instance Scale' "XPF" "centime" = '(100, 1)
-- | Sucre
type instance Scale' "XSU" "XSU" = '(100, 1)
-- | African Development Bank unit of account
type instance Scale' "XUA" "XUA" = '(100, 1)
-- | Yemeni rial
type instance Scale' "YER" "YER" = '(100, 1)
type instance Scale' "YER" "rial" = '(1, 1)
type instance Scale' "YER" "fils" = '(100, 1)
-- | South African rand
type instance Scale' "ZAR" "ZAR" = '(100, 1)
type instance Scale' "ZAR" "rand" = '(1, 1)
type instance Scale' "ZAR" "cent" = '(100, 1)
-- | Zambian kwacha
type instance Scale' "ZMW" "ZMW" = '(100, 1)
type instance Scale' "ZMW" "kwacha" = '(1, 1)
type instance Scale' "ZMW" "ngwee" = '(100, 1)
-- | Zimbawe dollar
type instance Scale' "ZWL" "ZWL" = '(100, 1)
type instance Scale' "ZWL" "dollar" = '(1, 1)
type instance Scale' "ZWL" "cent" = '(100, 1)

-- | Gold
type instance Scale' "XAU" "XAU" = ErrScaleNonCanonical "XAU"
type instance Scale' "XAU" "troy-ounce" = '(1, 1)
type instance Scale' "XAU" "grain" = '(480, 1)
type instance Scale' "XAU" "milligrain" = '(480000, 1)
type instance Scale' "XAU" "micrograin" = '(480000000, 1)
type instance Scale' "XAU" "kilogram" = '(31103477, 1000000000)
type instance Scale' "XAU" "gram" = '(31103477, 1000000)
type instance Scale' "XAU" "milligram" = '(31103477, 1000)
type instance Scale' "XAU" "microgram" = '(31103477, 1)

-- | Silver
type instance Scale' "XAG" "XAG" = ErrScaleNonCanonical "XAG"
type instance Scale' "XAG" "troy-ounce" = '(1, 1)
type instance Scale' "XAG" "grain" = '(480, 1)
type instance Scale' "XAG" "milligrain" = '(480000, 1)
type instance Scale' "XAG" "micrograin" = '(480000000, 1)
type instance Scale' "XAG" "kilogram" = '(31103477, 1000000000)
type instance Scale' "XAG" "gram" = '(31103477, 1000000)
type instance Scale' "XAG" "milligram" = '(31103477, 1000)
type instance Scale' "XAG" "microgram" = '(31103477, 1)

-- | Palladium
type instance Scale' "XPD" "XPD" = ErrScaleNonCanonical "XPD"
type instance Scale' "XPD" "troy-ounce" = '(1, 1)
type instance Scale' "XPD" "grain" = '(480, 1)
type instance Scale' "XPD" "milligrain" = '(480000, 1)
type instance Scale' "XPD" "micrograin" = '(480000000, 1)
type instance Scale' "XPD" "kilogram" = '(31103477, 1000000000)
type instance Scale' "XPD" "gram" = '(31103477, 1000000)
type instance Scale' "XPD" "milligram" = '(31103477, 1000)
type instance Scale' "XPD" "microgram" = '(31103477, 1)

-- | Platinum
type instance Scale' "XPT" "XPT" = ErrScaleNonCanonical "XPT"
type instance Scale' "XPT" "troy-ounce" = '(1, 1)
type instance Scale' "XPT" "grain" = '(480, 1)
type instance Scale' "XPT" "milligrain" = '(480000, 1)
type instance Scale' "XPT" "micrograin" = '(480000000, 1)
type instance Scale' "XPT" "kilogram" = '(31103477, 1000000000)
type instance Scale' "XPT" "gram" = '(31103477, 1000000)
type instance Scale' "XPT" "milligram" = '(31103477, 1000)
type instance Scale' "XPT" "microgram" = '(31103477, 1)

-- | Bitcoin
type instance Scale' "BTC" "BTC" = '(100000000, 1)
type instance Scale' "BTC" "bitcoin" = '(1, 1)
type instance Scale' "BTC" "satoshi" = '(100000000, 1)

-- | Bitcoin
type instance Scale' "XBT" "XBT" = '(100000000, 1)
type instance Scale' "XBT" "bitcoin" = '(1, 1)
type instance Scale' "XBT" "satoshi" = '(100000000, 1)

-- | Ether
type instance Scale' "ETH" "ETH" = '(1000000000000000000, 1)
type instance Scale' "ETH" "ether" = '(1, 1)
type instance Scale' "ETH" "kwei" = '(1000, 1)
type instance Scale' "ETH" "babbage" = '(1000, 1)
type instance Scale' "ETH" "mwei" = '(1000000, 1)
type instance Scale' "ETH" "lovelace" = '(1000000, 1)
type instance Scale' "ETH" "gwei" = '(1000000000, 1)
type instance Scale' "ETH" "shannon" = '(1000000000, 1)
type instance Scale' "ETH" "microether" = '(1000000000000, 1)
type instance Scale' "ETH" "szabo" = '(1000000000000, 1)
type instance Scale' "ETH" "finney" = '(1000000000000000, 1)
type instance Scale' "ETH" "milliether" = '(1000000000000000, 1)
type instance Scale' "ETH" "wei" = '(1000000000000000000, 1)

