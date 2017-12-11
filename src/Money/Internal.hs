{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

-- | This is an internal module. Import "Money" instead.
module Money.Internal
 ( -- * Dense monetary values
   Dense
 , dense
 , dense'
   -- * Discrete monetary values
 , Discrete
 , Discrete'
 , fromDiscrete
 , round
 , ceiling
 , floor
 , truncate
   -- * Currency scales
 , Scale
 , GoodScale
 , ErrScaleNonCanonical
 , scale
   -- * Currency exchange
 , ExchangeRate
 , exchangeRate
 , fromExchangeRate
 , flipExchangeRate
 , exchange
   -- * Serializable representations
 , SomeDense
 , toSomeDense
 , mkSomeDense
 , fromSomeDense
 , withSomeDense
 , someDenseCurrency
 , someDenseAmount
 , someDenseAmountNumerator
 , someDenseAmountDenominator
 , SomeDiscrete
 , toSomeDiscrete
 , mkSomeDiscrete
 , fromSomeDiscrete
 , withSomeDiscrete
 , someDiscreteCurrency
 , someDiscreteScale
 , someDiscreteScaleNumerator
 , someDiscreteScaleDenominator
 , someDiscreteAmount
 , SomeExchangeRate
 , toSomeExchangeRate
 , mkSomeExchangeRate
 , fromSomeExchangeRate
 , withSomeExchangeRate
 , someExchangeRateSrcCurrency
 , someExchangeRateDstCurrency
 , someExchangeRateRate
 , someExchangeRateRateNumerator
 , someExchangeRateRateDenominator
 ) where

import Control.Applicative (empty)
import Control.Monad ((<=<))
import Data.Constraint (Dict(Dict))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), numerator, denominator)
import qualified GHC.Generics as GHC
import GHC.Real (infinity, notANumber)
import GHC.TypeLits
  (Symbol, SomeSymbol(..), Nat, SomeNat(..), CmpNat, KnownSymbol, KnownNat,
   natVal, someNatVal, symbolVal, someSymbolVal)
import Prelude hiding (round, ceiling, floor, truncate)
import qualified Prelude
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read
import Unsafe.Coerce (unsafeCoerce)

#ifdef HAS_aeson
import qualified Data.Aeson as Ae
#endif

#ifdef HAS_binary
import qualified Data.Binary as Binary
#endif

#ifdef HAS_cereal
import qualified Data.Serialize as Cereal
#endif

#ifdef HAS_deepseq
import Control.DeepSeq (NFData)
#endif

#ifdef HAS_hashable
import Data.Hashable (Hashable)
#endif

#ifdef HAS_serialise
import qualified Codec.Serialise as Ser
#endif

#ifdef HAS_store
import qualified Data.Store as Store
#endif

#if MIN_VERSION_base(4,9,0)
import qualified GHC.TypeLits as GHC
#endif

--------------------------------------------------------------------------------
-- | 'Dense' represents a dense monetary value for @currency@ (usually a
-- ISO-4217 currency code, but not necessarily) as a rational number.
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
-- 'fromInteger' / 'fromIntegral' if that suffices.
--
-- /WARNING/ if you want to treat a dense monetary value as a /Real/ number (for
-- example, to take the square root of that monetary value), then you are on
-- your own. We can only guarantee lossless manipulation of rational values, so
-- you will need to convert back and forth betwen the 'Rational' representation
-- for 'Dense' and your (likely lossy) representation for /Real/ numbers.
newtype Dense (currency :: Symbol) = Dense Rational
  deriving (Eq, Ord, Num, Real, Fractional, GHC.Generic)

instance forall currency. KnownSymbol currency => Show (Dense currency) where
  showsPrec n = \(Dense r0) ->
    let c = symbolVal (Proxy :: Proxy currency)
    in showParen (n > 10) $
         showString "Dense " . showsPrec 0 c . showChar ' ' .
         showsPrec 0 (numerator r0) . showChar '%' .
         showsPrec 0 (denominator r0)

instance forall currency. KnownSymbol currency => Read (Dense currency) where
  readPrec = Read.parens $ do
    let c = symbolVal (Proxy :: Proxy currency)
    _ <- ReadPrec.lift (ReadP.string ("Dense " ++ show c ++ " "))
    maybe empty pure =<< fmap dense' Read.readPrec

-- | Build a 'Dense' monetary value from a 'Rational' value.
--
-- For example, if you want to represent @USD 12.52316@, then you can use:
--
-- @
-- 'dense' (125316 % 10000)
-- @
--
-- **WARNING** This function /crashes/ in case the denominator of the given
-- 'Rational' is zero, which is something very unlikely to happen unless you
-- called 'GHC.Real.infinity' or 'GHC.Real.notANumber' manually at some point.
-- If you care about that scenario (which you should, when dealing with
-- 'Rational' values from untrusted sources), then use 'dense'' instead.
dense :: Rational -> Dense currency
dense = \r0 ->
  if (denominator r0 == 0) then error "dense: denominator is zero"
  else Dense r0
{-# INLINABLE dense #-}

-- | Like 'dense', but returns 'Nothing' in case the denominator of the given
-- 'Rational' is zero.
dense' :: Rational -> Maybe (Dense currency)
dense' = \r0 ->
  if (denominator r0 == 0) then Nothing
  else Just (Dense r0)
{-# INLINABLE dense' #-}

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
-- /pennies/ equal 1 GBP (i.e., @'Scale' \"GBP\" ~ '(100, 1)@), then you can
-- use:
--
-- @
-- 'fromInteger' 2105 :: Discrete \"GBP\" \"penny\"
-- @
--
-- Because @2015 / 100 == 20.15@.
type Discrete (currency :: Symbol) (unit :: Symbol)
  = Discrete' currency (Scale currency unit)

-- | 'Discrete'' represents a discrete monetary value for a @currency@ expresed
-- as an amount of @scale@, which is a rational number expressed as @(numerator,
-- denominator)@.
--
-- You'll be using 'Discrete' instead of 'Discrete'' most of the time, which
-- mentions the unit name (such as /cent/ or /centavo/) instead of explicitely
-- mentioning the unit scale.
newtype Discrete' (currency :: Symbol) (scale :: (Nat, Nat))
  = Discrete Integer

deriving instance GoodScale scale => Eq (Discrete' currency scale)
deriving instance GoodScale scale => Ord (Discrete' currency scale)
deriving instance GoodScale scale => Enum (Discrete' currency scale)
deriving instance GoodScale scale => Num (Discrete' currency scale)
deriving instance GoodScale scale => Real (Discrete' currency scale)
deriving instance GoodScale scale => Integral (Discrete' currency scale)
deriving instance GoodScale scale => GHC.Generic (Discrete' currency scale)

instance forall currency scale.
  ( KnownSymbol currency, GoodScale scale
  ) => Show (Discrete' currency scale) where
  showsPrec n = \d0@(Discrete i0) ->
    let c = symbolVal (Proxy :: Proxy currency)
        s = scale d0
    in showParen (n > 10) $
         showString "Discrete " .  showsPrec 0 c . showChar ' ' .
         showsPrec 0 (numerator s) . showChar '%' .
         showsPrec 0 (denominator s) . showChar ' ' .
         showsPrec 0 i0

instance forall currency scale.
  ( KnownSymbol currency, GoodScale scale
  ) => Read (Discrete' currency scale) where
  readPrec = Read.parens $ do
    let c = symbolVal (Proxy :: Proxy currency)
        s = scale (Proxy :: Proxy scale)
    _ <- ReadPrec.lift (ReadP.string (concat
           [ "Discrete ", show c, " "
           , show (numerator s), "%"
           , show (denominator s), " "
           ]))
    Discrete <$> Read.readPrec

#if MIN_VERSION_base(4,9,0)
instance
  ( GHC.TypeError
      (('GHC.Text "The ") 'GHC.:<>:
       ('GHC.ShowType Discrete') 'GHC.:<>:
       ('GHC.Text " type is deliberately not a ") 'GHC.:<>:
       ('GHC.ShowType Fractional) 'GHC.:$$:
       ('GHC.Text "instance. Convert the ") 'GHC.:<>:
       ('GHC.ShowType Discrete') 'GHC.:<>:
       ('GHC.Text " value to a ") 'GHC.:<>:
       ('GHC.ShowType Dense) 'GHC.:$$:
       ('GHC.Text "value and use the ") 'GHC.:<>:
       ('GHC.ShowType Fractional) 'GHC.:<>:
       ('GHC.Text " features on it instead."))
  , GoodScale scale
  ) => Fractional (Discrete' currency scale) where
  fromRational = undefined
  recip = undefined
#endif

-- | Convert currency 'Discrete' monetary value into a 'Dense' monetary
-- value.
fromDiscrete
  :: GoodScale scale
  => Discrete' currency scale
  -> Dense currency -- ^
fromDiscrete = \c@(Discrete i) -> Dense (fromInteger i / scale c)
{-# INLINABLE fromDiscrete #-}

-- | Internal. Used to implement 'round', 'ceiling', 'floor' and 'truncate'.
roundf
  :: forall currency scale
  .  GoodScale scale
  => (Rational -> Integer) -- ^ 'Prelude.round', 'Prelude.ceiling' or similar.
  -> Dense currency
  -> (Discrete' currency scale, Maybe (Dense currency))
roundf f = \c0 ->
  let !r0 = toRational c0 :: Rational
      !r1 = scale (Proxy :: Proxy scale)
      !i2 = f (r0 * r1) :: Integer
      !r2 = fromInteger i2 / r1 :: Rational
      !ycrest | r0 == r2  = Nothing
              | otherwise = Just (Dense (r0 - r2))
      !d2 = Discrete i2
  in (d2, ycrest)
{-# INLINABLE roundf #-}

-- | Round a 'Dense' value @x@ to the nearest value fully representable in
-- its @currency@'s @unit@ 'Scale', which might be @x@ itself.
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale',
-- then the following holds:
--
-- @
-- 'round' x == (x, 'Nothing')
-- @
--
-- Otherwise, if the nearest value to @x@ that is fully representable in its
-- @currency@'s @unit@ 'Scale' is greater than @x@, then the following holds:
--
-- @
-- 'round' == 'ceiling'
-- @
--
-- Otherwise, the nearest value to @x@ that is fully representable in its
-- @currency@'s @unit@ 'Scale' is smaller than @x@, and the following holds:
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
  :: GoodScale scale
  => Dense currency
  -> (Discrete' currency scale, Maybe (Dense currency)) -- ^
round = roundf Prelude.round
{-# INLINABLE round #-}

-- | Round a 'Dense' value @x@ to the nearest value fully representable in
-- its @currency@'s @unit@ 'Scale' which is greater than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale',
-- then the following holds:
--
-- @
-- 'ceiling' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s @unit@ 'Scale',
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
  :: GoodScale scale
  => Dense currency
  -> (Discrete' currency scale, Maybe (Dense currency)) -- ^
ceiling = roundf Prelude.ceiling
{-# INLINABLE ceiling #-}

-- | Round a 'Dense' value @x@ to the nearest value fully representable in
-- its @currency@'s @unit@ 'Scale' which is smaller than @x@ or equal to @x@.
--
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale',
-- then the following holds:
--
-- @
-- 'floor' x == (x, 'Nothing')
-- @
--
-- Otherwise, if @x@ is not representable in its @currency@'s @unit@ 'Scale',
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
  :: GoodScale scale
  => Dense currency
  -> (Discrete' currency scale, Maybe (Dense currency)) -- ^
floor = roundf Prelude.floor
{-# INLINABLE floor #-}

-- | Round a 'Dense' value @x@ to the nearest value between zero and
-- @x@ (inclusive) which is fully representable in its @currency@'s @unit@
-- 'Scale'.
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale',
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
  :: GoodScale scale
  => Dense currency
  -> (Discrete' currency scale, Maybe (Dense currency)) -- ^
truncate = roundf Prelude.truncate
{-# INLINABLE truncate #-}

--------------------------------------------------------------------------------

-- | @'Scale' currency unit@ is a rational number (expressed as @'(numerator,
-- denominator)@) indicating how many pieces of @unit@ fit in @currency@.
--
-- @currency@ is usually a ISO-4217 currency code, but not necessarily.
--
-- The 'Scale' will determine how to convert a 'Dense' value into a
-- 'Discrete' value and vice-versa.
--
-- For example, there are 100 USD cents in 1 USD, so the scale for this
-- relationship is:
--
-- @
-- type instance 'Scale' \"USD\" \"cent\" = '(100, 1)
-- @
--
-- As another example, there is 1 dollar in USD, so the scale for this
-- relationship is:
--
-- @
-- type instance 'Scale' \"USD\" \"dollar\" = '(1, 1)
-- @
--
-- When using 'Discrete' values to represent money, it will be impossible to
-- represent an amount of @currency@ smaller than @unit@. So, if you decide to
-- use @Scale \"USD\" \"dollar\"@ as your scale, you will not be able to
-- represent values such as USD 3.50 or USD 21.87, since they are not exact
-- multiples of a dollar.
--
-- If there exists a canonical smallest @unit@ that can fully represent the
-- currency, then an instance @'Scale' currency currency@ exists.
--
-- @
-- type instance 'Scale' \"USD\" \"USD\" = Scale \"USD\" \"cent\"
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
-- type instance 'Scale' \"XAG\" \"milligrain\" = '(480000, 1)
-- @
--
-- You can use other units such as /milligrams/ for measuring XAU, for example.
-- However, since the amount of /milligrams/ in a /troy ounce/ (31103.477) is
-- not integral, we need to use rational number to express it.
--
-- @
-- type instance 'Scale' \"XAU\" \"milligram\" = '(31103477, 1000)
-- @
--
-- If you try to obtain the 'Scale of a @currency@ without an obvious smallest
-- representable @unit@, like XAU, you will get a compile error.
type family Scale (currency :: Symbol) (unit :: Symbol) :: (Nat, Nat)

#if MIN_VERSION_base(4,9,0)
-- | A friendly 'GHC.TypeError' to use for a @currency@ that doesn't have a
-- canonical small unit.
type family ErrScaleNonCanonical (currency :: Symbol) :: k where
  ErrScaleNonCanonical c = GHC.TypeError
    ( 'GHC.Text c 'GHC.:<>:
      'GHC.Text " is not a currency with a canonical smallest unit," 'GHC.:$$:
      'GHC.Text "be explicit about the currency unit you want to use." )
#else
-- | Forbid a @currency@ that doesn't have a canonical small unit.
--
-- In GHC versions before 8.0 we can't provide a nice error message here, so we
-- simply set this to a value that will fail to satisfy 'GoodScale'. As a
-- consequence, trying to use this 'Scale' will result in a cryptic error saying
-- /«@Couldn't match type ‘'EQ’ with ‘'LT’@»/.
type ErrScaleNonCanonical (currency :: Symbol) = '(0, 0)
#endif

-- | Constraints to a scale (like the one returned by @'Scale' currency unit@)
-- expected to always be satisfied. In particular, the scale is always
-- guaranteed to be a positive rational number ('infinity' and 'notANumber' are
-- forbidden by 'GoodScale').
type GoodScale (scale :: (Nat, Nat))
   = ( CmpNat 0 (Fst scale) ~ 'LT
     , CmpNat 0 (Snd scale) ~ 'LT
     , KnownNat (Fst scale)
     , KnownNat (Snd scale)
     )

-- | If the specified @num@ and @den@ satisfy the expectations of 'GoodScale' at
-- the type level, then construct a proof for 'GoodScale'.
mkGoodScale
  :: forall num den
  .  (KnownNat num, KnownNat den)
  => Maybe (Dict (GoodScale '(num, den)))
mkGoodScale =
  let n = natVal (Proxy :: Proxy num)
      d = natVal (Proxy :: Proxy den)
  in if (n > 0) && (d > 0)
     then Just (unsafeCoerce (Dict :: Dict ('LT ~ 'LT, 'LT ~ 'LT,
                                            KnownNat num, KnownNat den)))
     else Nothing
{-# INLINABLE mkGoodScale #-}

-- | Term-level representation for the @currency@'s @unit@ 'Scale'.
--
-- For example, the 'Scale' for @\"USD\"@ in @\"cent\"@s is @100/1@.
--
-- The returned 'Rational' is statically guaranteed to be a positive number, and
-- to be different from both 'notANumber' and 'infinity'.
scale :: forall proxy scale. GoodScale scale => proxy scale -> Rational -- ^
scale = \_ ->
   natVal (Proxy :: Proxy (Fst scale)) %
   natVal (Proxy :: Proxy (Snd scale))
{-# INLINABLE scale #-}

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
  deriving (Eq, Ord, GHC.Generic)

instance forall src dst.
  ( KnownSymbol src, KnownSymbol dst
  ) => Show (ExchangeRate src dst) where
  showsPrec n = \(ExchangeRate r0) ->
    let s = symbolVal (Proxy :: Proxy src)
        d = symbolVal (Proxy :: Proxy dst)
    in showParen (n > 10) $
         showString "ExchangeRate " . showsPrec 0 s . showChar ' ' .
         showsPrec 0 d . showChar ' ' .
         showsPrec 0 (numerator r0) . showChar '%' .
         showsPrec 0 (denominator r0)

instance forall src dst.
  ( KnownSymbol src, KnownSymbol dst
  ) => Read (ExchangeRate (src :: Symbol) (dst :: Symbol)) where
  readPrec = Read.parens $ do
    let s = symbolVal (Proxy :: Proxy src)
        d = symbolVal (Proxy :: Proxy dst)
    _ <- ReadPrec.lift (ReadP.string
            ("ExchangeRate " ++ show s ++ " " ++ show d ++ " "))
    maybe empty pure =<< fmap exchangeRate Read.readPrec

-- | Obtain a 'Rational' representation of the 'ExchangeRate'.
--
-- This 'Rational' is statically guaranteed to be greater than 0, different
-- from 'infinity' and different from 'notANumber'.
fromExchangeRate :: ExchangeRate src dst -> Rational
fromExchangeRate = \(ExchangeRate r0) -> r0
{-# INLINABLE fromExchangeRate #-}

-- | Safely construct an 'ExchangeRate' from a 'Rational' number.
--
-- For construction to succeed, this 'Rational' must be greater than 0,
-- different from 'infinity' and different from 'notANumber'.
exchangeRate :: Rational -> Maybe (ExchangeRate src dst)
exchangeRate = \r0 ->
  if (r0 <= 0 || infinity == r0 || notANumber == r0)
  then Nothing else Just (ExchangeRate r0)
{-# INLINABLE exchangeRate #-}

-- | Flip the direction of an 'ExchangeRate'.
--
-- Identity law:
--
-- @
-- 'flipExchangeRate' . 'flipExchangeRate'   ==  'id'
-- @
flipExchangeRate :: ExchangeRate a b -> ExchangeRate b a
flipExchangeRate = \(ExchangeRate x) -> ExchangeRate (1 / x)
{-# INLINABLE flipExchangeRate #-}

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
{-# INLINABLE exchange #-}

--------------------------------------------------------------------------------
-- SomeDense

-- | A monomorphic representation of 'Dense' that is easier to serialize and
-- deserialize than 'Dense' in case you don't know the type indexes involved.
--
-- If you are trying to construct a value of this type from some raw input, then
-- you will need to use the 'mkSomeDense' function.
--
-- In order to be able to effectively serialize a 'SomeDense' value, you
-- need to serialize the following three values (which are the eventual
-- arguments to 'mkSomeDense'):
--
-- * 'someDenseCurrency'
-- * 'someDenseAmountNumerator'
-- * 'someDenseAmountDenominator'
data SomeDense = SomeDense
  { _someDenseCurrency          :: !String
  , _someDenseAmountNumerator   :: !Integer
  , _someDenseAmountDenominator :: !Integer  -- ^ Positive, non-zero.
  } deriving (Eq, Show, GHC.Generic)

-- | WARNING: This instance does not compare monetary amounts, it just helps you
-- sort 'SomeDense' values in case you need to put them in a 'Data.Set.Set' or
-- similar.
deriving instance Ord SomeDense

-- | Currency name.
someDenseCurrency :: SomeDense -> String
someDenseCurrency = _someDenseCurrency
{-# INLINABLE someDenseCurrency #-}

-- | Currency unit amount.
someDenseAmount :: SomeDense -> Rational
someDenseAmount = \dr ->
  someDenseAmountNumerator dr % someDenseAmountDenominator dr
{-# INLINABLE someDenseAmount #-}

-- | Currency unit amount numerator.
someDenseAmountNumerator :: SomeDense -> Integer
someDenseAmountNumerator = _someDenseAmountNumerator
{-# INLINABLE someDenseAmountNumerator #-}

-- | Currency unit amount denominator. Positive, non-zero.
someDenseAmountDenominator :: SomeDense -> Integer
someDenseAmountDenominator = _someDenseAmountDenominator
{-# INLINABLE someDenseAmountDenominator #-}

-- | Build a 'SomeDense' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeDense' value to a 'Dense' value in order to do any arithmetic
-- operation on the monetary value.
mkSomeDense
  :: String -- ^ Currency. ('someDenseCurrency')
  -> Integer -- ^ Scale numerator. ('someDenseAmountNumerator')
  -> Integer -- ^ Scale denominator (positive, non zero). ('someDenseAmountDenominator')
  -> Maybe SomeDense
mkSomeDense = \c n d -> case d > 0 of
  False -> Nothing
  True -> Just (SomeDense c n d)
{-# INLINABLE mkSomeDense #-}

-- | Convert a 'Dense' to a 'SomeDense' for ease of serialization.
toSomeDense :: KnownSymbol currency => Dense currency -> SomeDense
toSomeDense = \(Dense r0 :: Dense currency) ->
  let c = symbolVal (Proxy :: Proxy currency)
  in SomeDense c (numerator r0) (denominator r0)
{-# INLINABLE toSomeDense #-}

-- | Attempt to convert a 'SomeDense' to a 'Dense', provided you know the target
-- @currency@.
fromSomeDense
  :: forall currency
  .  KnownSymbol currency
  => SomeDense
  -> Maybe (Dense currency)  -- ^
fromSomeDense = \dr ->
  case someDenseCurrency dr == symbolVal (Proxy :: Proxy currency) of
     False -> Nothing
     True -> Just (Dense (someDenseAmount dr))
{-# INLINABLE fromSomeDense #-}

-- | Convert a 'SomeDense' to a 'Dense' without knowing the target @currency@.
--
-- Notice that @currency@ here can't leave its intended scope unless you can
-- prove equality with some other type at the outer scope, but in that case you
-- would be better off using 'fromSomeDense' directly.
withSomeDense
  :: SomeDense
  -> (forall currency. KnownSymbol currency => Dense currency -> r)
  -> r  -- ^
withSomeDense dr = \f ->
   case someSymbolVal (someDenseCurrency dr) of
      SomeSymbol (Proxy :: Proxy currency) ->
         f (Dense (someDenseAmount dr) :: Dense currency)
{-# INLINABLE withSomeDense #-}

--------------------------------------------------------------------------------
-- SomeDiscrete

-- | A monomorphic representation of 'Discrete' that is easier to serialize and
-- deserialize than 'Discrete' in case you don't know the type indexes involved.
--
-- If you are trying to construct a value of this type from some raw input, then
-- you will need to use the 'mkSomeDiscrete' function.
--
-- In order to be able to effectively serialize a 'SomeDiscrete' value, you need
-- to serialize the following four values (which are the eventual arguments to
-- 'mkSomeDiscrete'):
--
-- * 'someDiscreteCurrency'
-- * 'someDiscreteScaleNumerator'
-- * 'someDiscreteScaleDenominator'
-- * 'someDiscreteAmount'
data SomeDiscrete = SomeDiscrete
  { _someDiscreteCurrency         :: !String   -- ^ Currency name.
  , _someDiscreteScaleNumerator   :: !Integer  -- ^ Positive, non-zero.
  , _someDiscreteScaleDenominator :: !Integer  -- ^ Positive, non-zero.
  , _someDiscreteAmount           :: !Integer  -- ^ Amount of unit.
  } deriving (Eq, Show, GHC.Generic)

-- | WARNING: This instance does not compare monetary amounts, it just helps you
-- sort 'SomeDiscrete' values in case you need to put them in a 'Data.Set.Set' or
-- similar.
deriving instance Ord SomeDiscrete

-- | Currency name.
someDiscreteCurrency :: SomeDiscrete -> String
someDiscreteCurrency = _someDiscreteCurrency
{-# INLINABLE someDiscreteCurrency #-}

-- | Positive, non-zero.
someDiscreteScaleNumerator :: SomeDiscrete -> Integer
someDiscreteScaleNumerator = _someDiscreteScaleNumerator
{-# INLINABLE someDiscreteScaleNumerator #-}

-- | Positive, non-zero.
someDiscreteScaleDenominator :: SomeDiscrete -> Integer
someDiscreteScaleDenominator = _someDiscreteScaleDenominator
{-# INLINABLE someDiscreteScaleDenominator #-}

-- | Amount of currency unit.
someDiscreteAmount :: SomeDiscrete -> Integer
someDiscreteAmount = _someDiscreteAmount
{-# INLINABLE someDiscreteAmount #-}

-- | Positive, non-zero.
someDiscreteScale :: SomeDiscrete -> Rational
someDiscreteScale = \dr ->
  someDiscreteScaleNumerator dr % someDiscreteScaleDenominator dr
{-# INLINABLE someDiscreteScale #-}


-- | Internal. Build a 'SomeDiscrete' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeDiscrete' value to a 'Discrete' vallue in order to do any arithmetic
-- operation on the monetary value.
mkSomeDiscrete
  :: String   -- ^ Currency name. ('someDiscreteCurrency')
  -> Integer  -- ^ Scale numerator. Positive, non-zero. ('someDiscreteScaleNumerator')
  -> Integer  -- ^ Scale denominator. Positive, non-zero. ('someDiscreteScaleDenominator')
  -> Integer  -- ^ Amount of unit. ('someDiscreteAmount')
  -> Maybe SomeDiscrete
mkSomeDiscrete = \c n d a -> case (n > 0) && (d > 0) of
  False -> Nothing
  True -> Just (SomeDiscrete c n d a)
{-# INLINABLE mkSomeDiscrete #-}

-- | Convert a 'Discrete' to a 'SomeDiscrete' for ease of serialization.
toSomeDiscrete
  :: (KnownSymbol currency, GoodScale scale)
  => Discrete' currency scale
  -> SomeDiscrete -- ^
toSomeDiscrete = \(Discrete i0 :: Discrete' currency scale) ->
  let c = symbolVal (Proxy :: Proxy currency)
      n = natVal (Proxy :: Proxy (Fst scale))
      d = natVal (Proxy :: Proxy (Snd scale))
  in SomeDiscrete c n d i0
{-# INLINABLE toSomeDiscrete #-}

-- | Attempt to convert a 'SomeDiscrete' to a 'Discrete', provided you know the
-- target @currency@ and @unit@.
fromSomeDiscrete
  :: forall currency scale
  .  (KnownSymbol currency, GoodScale scale)
  => SomeDiscrete
  -> Maybe (Discrete' currency scale)  -- ^
fromSomeDiscrete = \dr ->
   if (someDiscreteCurrency dr == symbolVal (Proxy :: Proxy currency)) &&
      (someDiscreteScaleNumerator dr == natVal (Proxy :: Proxy (Fst scale))) &&
      (someDiscreteScaleDenominator dr == natVal (Proxy :: Proxy (Snd scale)))
   then Just (Discrete (someDiscreteAmount dr))
   else Nothing
{-# INLINABLE fromSomeDiscrete #-}

-- | Convert a 'SomeDiscrete' to a 'Discrete' without knowing the target
-- @currency@ and @unit@.
--
-- Notice that @currency@ and @unit@ here can't leave its intended scope unless
-- you can prove equality with some other type at the outer scope, but in that
-- case you would be better off using 'fromSomeDiscrete' directly.
--
-- Notice that you may need to add an explicit type to the result of this
-- function in order to keep the compiler happy.
withSomeDiscrete
  :: forall r
  .  SomeDiscrete
  -> ( forall currency scale.
         ( KnownSymbol currency
         , GoodScale scale
         ) => Discrete' currency scale
           -> r )
  -> r  -- ^
withSomeDiscrete dr = \f ->
  case someSymbolVal (someDiscreteCurrency dr) of
    SomeSymbol (Proxy :: Proxy currency) ->
      case someNatVal (someDiscreteScaleNumerator dr) of
        Nothing -> error "withSomeDiscrete: impossible: numerator < 0"
        Just (SomeNat (Proxy :: Proxy num)) ->
          case someNatVal (someDiscreteScaleDenominator dr) of
            Nothing -> error "withSomeDiscrete: impossible: denominator < 0"
            Just (SomeNat (Proxy :: Proxy den)) ->
              case mkGoodScale of
                Nothing -> error "withSomeDiscrete: impossible: mkGoodScale"
                Just (Dict :: Dict (GoodScale '(num, den))) ->
                  f (Discrete (someDiscreteAmount dr)
                       :: Discrete' currency '(num, den))
{-# INLINABLE withSomeDiscrete #-}

--------------------------------------------------------------------------------
-- SomeExchangeRate

-- | A monomorphic representation of 'ExchangeRate' that is easier to serialize
-- and deserialize than 'ExchangeRate' in case you don't know the type indexes
-- involved.
--
-- If you are trying to construct a value of this type from some raw input, then
-- you will need to use the 'mkSomeExchangeRate' function.
--
-- In order to be able to effectively serialize an 'SomeExchangeRate' value, you
-- need to serialize the following four values (which are the eventual arguments
-- to 'mkSomeExchangeRate'):
--
-- * 'someExchangeRateSrcCurrency'
-- * 'someExchangeRateDstCurrency'
-- * 'someExchangeRateRateNumerator'
-- * 'someExchangeRateRateDenominator'
data SomeExchangeRate = SomeExchangeRate
  { _someExchangeRateSrcCurrency     :: !String
  , _someExchangeRateDstCurrency     :: !String
  , _someExchangeRateRateNumerator   :: !Integer  -- ^ Positive, non-zero.
  , _someExchangeRateRateDenominator :: !Integer  -- ^ Positive, non-zero.
  } deriving (Eq, Show, GHC.Generic)

-- | WARNING: This instance does not compare monetary amounts, it just helps you
-- sort 'SomeExchangeRate' values in case you need to put them in a
-- 'Data.Set.Set' or similar.
deriving instance Ord SomeExchangeRate

-- | Source currency name.
someExchangeRateSrcCurrency :: SomeExchangeRate -> String
someExchangeRateSrcCurrency = _someExchangeRateSrcCurrency
{-# INLINABLE someExchangeRateSrcCurrency #-}

-- | Destination currency name.
someExchangeRateDstCurrency :: SomeExchangeRate -> String
someExchangeRateDstCurrency = _someExchangeRateDstCurrency
{-# INLINABLE someExchangeRateDstCurrency #-}

-- | Exchange rate. Positive, non-zero.
someExchangeRateRate :: SomeExchangeRate -> Rational
someExchangeRateRate = \x ->
  someExchangeRateRateNumerator x % _someExchangeRateRateDenominator x
{-# INLINABLE someExchangeRateRate #-}

-- | Exchange rate numerator. Positive, non-zero.
someExchangeRateRateNumerator :: SomeExchangeRate -> Integer
someExchangeRateRateNumerator = _someExchangeRateRateNumerator
{-# INLINABLE someExchangeRateRateNumerator #-}

-- | Exchange rate denominator. Positive, non-zero.
someExchangeRateRateDenominator :: SomeExchangeRate -> Integer
someExchangeRateRateDenominator = _someExchangeRateRateDenominator
{-# INLINABLE someExchangeRateRateDenominator #-}

-- | Internal. Build a 'SomeExchangeRate' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeExchangeRate' value to a 'ExchangeRate' value in order to do any
-- arithmetic operation with the exchange rate.
mkSomeExchangeRate
  :: String   -- ^ Source currency name. ('someExchangeRateSrcCurrency')
  -> String   -- ^ Destination currency name. ('someExchangeRateDstCurrency')
  -> Integer  -- ^ Exchange rate numerator. Positive, non-zero. ('someExchangeRateRateNumerator')
  -> Integer  -- ^ Exchange rate denominator. Positive, non-zero. ('someExchangeRateRateDenominator')
  -> Maybe SomeExchangeRate
mkSomeExchangeRate = \src dst n d -> case (n > 0) && (d > 0) of
  False -> Nothing
  True -> Just (SomeExchangeRate src dst n d)
{-# INLINABLE mkSomeExchangeRate #-}

-- | Convert a 'ExchangeRate' to a 'SomeDiscrete' for ease of serialization.
toSomeExchangeRate
  :: (KnownSymbol src, KnownSymbol dst)
  => ExchangeRate src dst
  -> SomeExchangeRate -- ^
toSomeExchangeRate = \(ExchangeRate r0 :: ExchangeRate src dst) ->
  let src = symbolVal (Proxy :: Proxy src)
      dst = symbolVal (Proxy :: Proxy dst)
  in SomeExchangeRate src dst (numerator r0) (denominator r0)
{-# INLINABLE toSomeExchangeRate #-}

-- | Attempt to convert a 'SomeExchangeRate' to a 'ExchangeRate', provided you
-- know the target @src@ and @dst@ types.
fromSomeExchangeRate
  :: forall src dst
  .  (KnownSymbol src, KnownSymbol dst)
  => SomeExchangeRate
  -> Maybe (ExchangeRate src dst)  -- ^
fromSomeExchangeRate = \x ->
   if (someExchangeRateSrcCurrency x == symbolVal (Proxy :: Proxy src)) &&
      (someExchangeRateDstCurrency x == symbolVal (Proxy :: Proxy dst))
   then Just (ExchangeRate (someExchangeRateRate x))
   else Nothing
{-# INLINABLE fromSomeExchangeRate #-}

-- | Convert a 'SomeExchangeRate' to a 'ExchangeRate' without knowing the target
-- @currency@ and @unit@.
--
-- Notice that @src@ and @dst@ here can't leave its intended scope unless
-- you can prove equality with some other type at the outer scope, but in that
-- case you would be better off using 'fromSomeExchangeRate' directly.
withSomeExchangeRate
  :: SomeExchangeRate
  -> ( forall src dst.
         ( KnownSymbol src
         , KnownSymbol dst
         ) => ExchangeRate src dst
           -> r )
  -> r  -- ^
withSomeExchangeRate x = \f ->
  case someSymbolVal (someExchangeRateSrcCurrency x) of
    SomeSymbol (Proxy :: Proxy src) ->
      case someSymbolVal (someExchangeRateDstCurrency x) of
        SomeSymbol (Proxy :: Proxy dst) ->
          f (ExchangeRate (someExchangeRateRate x) :: ExchangeRate src dst)
{-# INLINABLE withSomeExchangeRate #-}

--------------------------------------------------------------------------------
-- Miscellaneous

type family Fst (ab :: (ka, kb)) :: ka where Fst '(a,b) = a
type family Snd (ab :: (ka, kb)) :: ka where Snd '(a,b) = b

--------------------------------------------------------------------------------
-- Extra instances: hashable
#ifdef HAS_hashable
instance Hashable (Dense currency)
instance Hashable SomeDense
instance GoodScale scale => Hashable (Discrete' currency scale)
instance Hashable SomeDiscrete
instance Hashable (ExchangeRate src dst)
instance Hashable SomeExchangeRate
#endif

--------------------------------------------------------------------------------
-- Extra instances: deepseq
#ifdef HAS_deepseq
instance NFData (Dense currency)
instance NFData SomeDense
instance GoodScale scale => NFData (Discrete' currency scale)
instance NFData SomeDiscrete
instance NFData (ExchangeRate src dst)
instance NFData SomeExchangeRate
#endif

--------------------------------------------------------------------------------
-- Extra instances: cereal
#ifdef HAS_cereal
-- | Compatible with 'SomeDense'.
instance (KnownSymbol currency) => Cereal.Serialize (Dense currency) where
  put = Cereal.put . toSomeDense
  get = maybe empty pure =<< fmap fromSomeDense Cereal.get
-- | Compatible with 'SomeDiscrete'.
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Cereal.Serialize (Discrete' currency scale) where
  put = Cereal.put . toSomeDiscrete
  get = maybe empty pure =<< fmap fromSomeDiscrete Cereal.get
-- | Compatible with 'SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Cereal.Serialize (ExchangeRate src dst) where
  put = Cereal.put . toSomeExchangeRate
  get = maybe empty pure =<< fmap fromSomeExchangeRate Cereal.get
-- | Compatible with 'Dense'.
instance Cereal.Serialize SomeDense where
  put = \(SomeDense c n d) -> Cereal.put c >> Cereal.put n >> Cereal.put d
  get = maybe empty pure =<< mkSomeDense
    <$> Cereal.get <*> Cereal.get <*> Cereal.get
-- | Compatible with 'Discrete'.
instance Cereal.Serialize SomeDiscrete where
  put = \(SomeDiscrete c n d a) ->
    Cereal.put c >> Cereal.put n >> Cereal.put d >> Cereal.put a
  get = maybe empty pure =<< mkSomeDiscrete
    <$> Cereal.get <*> Cereal.get <*> Cereal.get <*> Cereal.get
-- | Compatible with 'ExchangeRate'.
instance Cereal.Serialize SomeExchangeRate where
  put = \(SomeExchangeRate src dst n d) ->
    Cereal.put src >> Cereal.put dst >> Cereal.put n >> Cereal.put d
  get = maybe empty pure =<< mkSomeExchangeRate
    <$> Cereal.get <*> Cereal.get <*> Cereal.get <*> Cereal.get
#endif

--------------------------------------------------------------------------------
-- Extra instances: binary
#ifdef HAS_binary
-- | Compatible with 'SomeDense'.
instance (KnownSymbol currency) => Binary.Binary (Dense currency) where
  put = Binary.put . toSomeDense
  get = maybe empty pure =<< fmap fromSomeDense Binary.get
-- | Compatible with 'SomeDiscrete'.
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Binary.Binary (Discrete' currency scale) where
  put = Binary.put . toSomeDiscrete
  get = maybe empty pure =<< fmap fromSomeDiscrete Binary.get
-- | Compatible with 'SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Binary.Binary (ExchangeRate src dst) where
  put = Binary.put . toSomeExchangeRate
  get = maybe empty pure =<< fmap fromSomeExchangeRate Binary.get
-- | Compatible with 'Dense'.
instance Binary.Binary SomeDense where
  put = \(SomeDense c n d) -> Binary.put c >> Binary.put n >> Binary.put d
  get = maybe empty pure =<< mkSomeDense
    <$> Binary.get <*> Binary.get <*> Binary.get
-- | Compatible with 'Discrete'.
instance Binary.Binary SomeDiscrete where
  put = \(SomeDiscrete c n d a) ->
    Binary.put c >> Binary.put n >> Binary.put d >> Binary.put a
  get = maybe empty pure =<< mkSomeDiscrete
    <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
-- | Compatible with 'ExchangeRate'.
instance Binary.Binary SomeExchangeRate where
  put = \(SomeExchangeRate src dst n d) ->
    Binary.put src >> Binary.put dst >> Binary.put n >> Binary.put d
  get = maybe empty pure =<< mkSomeExchangeRate
    <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
#endif

--------------------------------------------------------------------------------
-- Extra instances: serialise
#ifdef HAS_serialise
-- | Compatible with 'SomeDense'.
instance (KnownSymbol currency) => Ser.Serialise (Dense currency) where
  encode = Ser.encode . toSomeDense
  decode = maybe (fail "Dense") pure =<< fmap fromSomeDense Ser.decode
-- | Compatible with 'SomeDiscrete'.
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Ser.Serialise (Discrete' currency scale) where
  encode = Ser.encode . toSomeDiscrete
  decode = maybe (fail "Discrete'") pure =<< fmap fromSomeDiscrete Ser.decode
-- | Compatible with 'SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ser.Serialise (ExchangeRate src dst) where
  encode = Ser.encode . toSomeExchangeRate
  decode = maybe (fail "ExchangeRate") pure
             =<< fmap fromSomeExchangeRate Ser.decode
-- | Compatible with 'Dense'.
instance Ser.Serialise SomeDense where
  encode = \(SomeDense c n d) -> Ser.encode c <> Ser.encode n <> Ser.encode d
  decode = maybe (fail "SomeDense") pure =<< mkSomeDense
    <$> Ser.decode <*> Ser.decode <*> Ser.decode
-- | Compatible with 'Discrete'.
instance Ser.Serialise SomeDiscrete where
  encode = \(SomeDiscrete c n d a) ->
    Ser.encode c <> Ser.encode n <> Ser.encode d <> Ser.encode a
  decode = maybe (fail "SomeDiscrete") pure =<< mkSomeDiscrete
    <$> Ser.decode <*> Ser.decode <*> Ser.decode <*> Ser.decode
-- | Compatible with 'ExchangeRate'.
instance Ser.Serialise SomeExchangeRate where
  encode = \(SomeExchangeRate src dst n d) ->
    Ser.encode src <> Ser.encode dst <> Ser.encode n <> Ser.encode d
  decode = maybe (fail "SomeExchangeRate") pure =<< mkSomeExchangeRate
    <$> Ser.decode <*> Ser.decode <*> Ser.decode <*> Ser.decode
#endif

--------------------------------------------------------------------------------
-- Extra instances: aeson
#ifdef HAS_aeson
-- | Compatible with 'SomeDense'
instance KnownSymbol currency => Ae.ToJSON (Dense currency) where
  toJSON = Ae.toJSON . toSomeDense
-- | Compatible with 'SomeDense'
instance KnownSymbol currency => Ae.FromJSON (Dense currency) where
  parseJSON = maybe empty pure <=< fmap fromSomeDense . Ae.parseJSON
-- | Compatible with 'Dense'
instance Ae.ToJSON SomeDense where
  toJSON = \(SomeDense c n d) -> Ae.toJSON ("Dense", c, n, d)
-- | Compatible with 'Dense'
instance Ae.FromJSON SomeDense where
  parseJSON = \v -> do
    ("Dense", c, n, d) <- Ae.parseJSON v
    maybe empty pure (mkSomeDense c n d)
-- | Compatible with 'SomeDiscrete'
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Ae.ToJSON (Discrete' currency scale) where
  toJSON = Ae.toJSON . toSomeDiscrete
-- | Compatible with 'SomeDiscrete'
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Ae.FromJSON (Discrete' currency scale) where
  parseJSON = maybe empty pure <=< fmap fromSomeDiscrete . Ae.parseJSON
-- | Compatible with 'Discrete''
instance Ae.ToJSON SomeDiscrete where
  toJSON = \(SomeDiscrete c n d a) -> Ae.toJSON ("Discrete", c, n, d, a)
-- | Compatible with 'Discrete''
instance Ae.FromJSON SomeDiscrete where
  parseJSON = \v -> do
    ("Discrete", c, n, d, a) <- Ae.parseJSON v
    maybe empty pure (mkSomeDiscrete c n d a)
-- | Compatible with 'SomeExchangeRate'
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ae.ToJSON (ExchangeRate src dst) where
  toJSON = Ae.toJSON . toSomeExchangeRate
-- | Compatible with 'SomeExchangeRate'
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ae.FromJSON (ExchangeRate src dst) where
  parseJSON = maybe empty pure <=< fmap fromSomeExchangeRate . Ae.parseJSON
-- | Compatible with 'ExchangeRate'
instance Ae.ToJSON SomeExchangeRate where
  toJSON = \(SomeExchangeRate src dst n d) ->
    Ae.toJSON ("ExchangeRate", src, dst, n, d)
-- | Compatible with 'ExchangeRate'
instance Ae.FromJSON SomeExchangeRate where
  parseJSON = \v -> do
    ("ExchangeRate", src, dst, n, d) <- Ae.parseJSON v
    maybe empty pure (mkSomeExchangeRate src dst n d)
#endif

--------------------------------------------------------------------------------
-- Extra instances: store
#ifdef HAS_store
-- | Compatible with 'SomeDense'.
instance (KnownSymbol currency) => Store.Store (Dense currency) where
  size = storeContramapSize toSomeDense Store.size
  poke = Store.poke . toSomeDense
  peek = maybe (fail "peek") pure =<< fmap fromSomeDense Store.peek
-- | Compatible with 'Dense'.
instance Store.Store SomeDense where
  poke = \(SomeDense c n d) -> Store.poke c >> Store.poke n >> Store.poke d
  peek = maybe (fail "peek") pure =<< do
    mkSomeDense <$> Store.peek <*> Store.peek <*> Store.peek

-- | Compatible with 'SomeDiscrete'.
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Store.Store (Discrete' currency scale) where
  size = storeContramapSize toSomeDiscrete Store.size
  poke = Store.poke . toSomeDiscrete
  peek = maybe (fail "peek") pure =<< fmap fromSomeDiscrete Store.peek
-- | Compatible with 'Discrete''.
instance Store.Store SomeDiscrete where
  poke = \(SomeDiscrete c n d a) ->
    Store.poke c >> Store.poke n >> Store.poke d >> Store.poke a
  peek = maybe (fail "peek") pure =<< do
    mkSomeDiscrete <$> Store.peek <*> Store.peek <*> Store.peek <*> Store.peek
-- | Compatible with 'SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Store.Store (ExchangeRate src dst) where
  size = storeContramapSize toSomeExchangeRate Store.size
  poke = Store.poke . toSomeExchangeRate
  peek = maybe (fail "peek") pure =<< fmap fromSomeExchangeRate Store.peek
-- | Compatible with 'ExchangeRate'.
instance Store.Store SomeExchangeRate where
  poke = \(SomeExchangeRate src dst n d) ->
    Store.poke src >> Store.poke dst >> Store.poke n >> Store.poke d
  peek = maybe (fail "peek") pure =<< mkSomeExchangeRate
    <$> Store.peek <*> Store.peek <*> Store.peek <*> Store.peek

storeContramapSize :: (a -> b) -> Store.Size b -> Store.Size a
storeContramapSize f = \case
  Store.VarSize g -> Store.VarSize (g . f)
  Store.ConstSize x -> Store.ConstSize x
{-# INLINABLE storeContramapSize #-}
#endif
