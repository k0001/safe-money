{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

-- | This is an internal module. Import "Money" instead.
module Money.Internal
 ( -- * Dense monetary values
   Dense
 , denseCurrency
 , denseFromRational
 , denseFromDiscrete
 , denseFromDecimal
 , denseToDecimal
   -- * Discrete monetary values
 , Discrete
 , Discrete'
 , discreteCurrency
 , discreteFromDense
 , discreteFromDecimal
   -- * Currency scales
 , Scale
 , GoodScale
 , ErrScaleNonCanonical
 , scale
   -- * Currency exchange
 , ExchangeRate
 , exchangeRate
 , exchangeRateToRational
 , exchangeRateRecip
 , exchange
   -- * Serializable representations
 , SomeDense
 , toSomeDense
 , mkSomeDense
 , fromSomeDense
 , withSomeDense
 , someDenseCurrency
 , someDenseAmount
 , SomeDiscrete
 , toSomeDiscrete
 , mkSomeDiscrete
 , fromSomeDiscrete
 , withSomeDiscrete
 , someDiscreteCurrency
 , someDiscreteScale
 , someDiscreteAmount
 , SomeExchangeRate
 , toSomeExchangeRate
 , mkSomeExchangeRate
 , fromSomeExchangeRate
 , withSomeExchangeRate
 , someExchangeRateSrcCurrency
 , someExchangeRateDstCurrency
 , someExchangeRateRate
 -- * Misc
 , Approximation(Round, Floor, Ceiling, Truncate)
 ) where

import Control.Applicative ((<|>), empty)
import Control.Category (Category((.), id))
import Control.Monad ((<=<), guard, when)
import qualified Data.Char as Char
import Data.Constraint (Dict(Dict))
import Data.Functor (($>))
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), numerator, denominator)
import Data.Word (Word8)
import GHC.Exts (Constraint, fromList)
import qualified GHC.Generics as GHC
import GHC.TypeLits
  (Symbol, SomeSymbol(..), Nat, SomeNat(..), CmpNat, KnownSymbol, KnownNat,
   natVal, someNatVal, symbolVal, someSymbolVal)
import Numeric.Natural (Natural)
import Prelude hiding ((.), id)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read
import Unsafe.Coerce (unsafeCoerce)

#ifdef HAS_aeson
import qualified Data.Aeson as Ae
import qualified Data.Text as T
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

#ifdef HAS_xmlbf
import qualified Xmlbf
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
-- Construct 'Dense' monetary values using denseFromRational, 'fromRational',
-- 'fromInteger' or 'fromIntegral'.
--
-- /WARNING/ if you want to treat a dense monetary value as a /Real/ number (for
-- example, to take the square root of that monetary value), then you are on
-- your own. We can only guarantee lossless manipulation of rational values, so
-- you will need to convert back and forth betwen the 'Rational' representation
-- for 'Dense' and your (likely lossy) representation for /Real/ numbers.
newtype Dense (currency :: Symbol) = Dense Rational
  deriving (Eq, Ord, Num, Real, GHC.Generic)

-- | /WARNING/ if there exists the possibility that the given 'Rational' has a
-- zero as a denominator, which although unlikely, is possible if the 'Rational'
-- is constructed unsafely using 'GHC.Real.infinity' or 'GHC.Real.notANumber'
-- for example, then use denseFromRational instead of 'fromRational'.
instance Fractional (Dense (currency :: Symbol)) where
  {-# INLINABLE recip #-}
  recip (Dense a) = Dense (recip a)
  {-# INLINABLE (/) #-}
  Dense a / Dense b = Dense (a / b)
  {-# INLINABLE fromRational #-}
  fromRational = \r -> case denominator r of
    0 -> error "fromRational :: Rational -> Dense currency: denominator is zero"
    _ -> Dense r

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
    maybe empty pure =<< fmap denseFromRational Read.readPrec

-- | Build a 'Dense' monetary value from a 'Rational' value.
--
-- For example, if you want to represent @USD 12.52316@, then you can use:
--
-- @
-- denseFromRational (125316 '%' 10000)
-- @
--
-- Returns 'Nothing' in case the denominator of the given 'Rational' is zero,
-- which although unlikely, is possible if the 'Rational' is constructed
-- unsafely using 'GHC.Real.infinity' or 'GHC.Real.notANumber', for example.
-- If you don't care about that scenario, you can use `fromRational` to build
-- the `Dense` value.
denseFromRational :: Rational -> Maybe (Dense currency)
denseFromRational = \r -> if denominator r == 0 then Nothing else Just (Dense r)
{-# INLINABLE denseFromRational #-}

-- | 'Dense' currency identifier.
--
-- @
-- > 'denseCurrency' (4 :: 'Dense' \"USD\")
-- \"USD\"
-- @
denseCurrency :: KnownSymbol currency => Dense currency -> String
denseCurrency = symbolVal
{-# INLINABLE denseCurrency #-}

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
denseFromDiscrete
  :: GoodScale scale
  => Discrete' currency scale
  -> Dense currency -- ^
denseFromDiscrete = \c@(Discrete i) -> Dense (fromInteger i / scale c)
{-# INLINABLE denseFromDiscrete #-}

-- | 'Discrete' currency identifier.
--
-- @
-- > 'discreteCurrency' (4 :: 'Discrete' \"USD\" \"cent\")
-- \"USD\"
-- @
discreteCurrency
  :: forall currency scale
  .  (KnownSymbol currency, GoodScale scale)
  => Discrete' currency scale
  -> String -- ^
discreteCurrency = \_ -> symbolVal (Proxy :: Proxy currency)
{-# INLINABLE discreteCurrency #-}

-- | What approach to use when approximating a fractional number to a whole
-- number.
--
-- See 'approximate'.
data Approximation
  = Round    -- ^ Approximate using 'round'
  | Floor    -- ^ Approximate using 'floor'
  | Ceiling  -- ^ Approximate using 'ceiling'
  | Truncate -- ^ Approximate using 'truncate'
  deriving (Eq, Show)

approximate :: Approximation -> Rational -> Integer
{-# INLINE approximate #-}
approximate Round = round
approximate Floor = floor
approximate Ceiling = ceiling
approximate Truncate = truncate

-- | Approximate a 'Dense' value @x@ to the nearest value fully representable in
-- its @currency@'s @unit@ 'Scale', which might be @x@ itself.
--
-- If @x@ is already fully representable in its @currency@'s @unit@ 'Scale',
-- then the following holds:
--
-- @
-- 'discreteFromDense' a x == ('denseFromDiscrete' x, 0)
-- @
--
-- Otherwise, if the nearest value to @x@ that is not representable in its
-- @currency@'s @unit@ 'Scale' is greater than @x@, then the returned remainder
-- 'Dense' will be non-zero.
--
-- Proof that 'discreteFromDense' doesn't lose money:
--
-- @
-- x == case 'round' a x of (y, z) -> 'denseFromDiscrete' y + z
-- @
discreteFromDense
  :: forall currency scale
  .  GoodScale scale
  => Approximation
  -- ^ Approximation to use if necesary in order to fit the 'Dense' amount in
  -- the requested @scale@.
  -> Dense currency
  -> (Discrete' currency scale, Dense currency)
discreteFromDense a = \c0 ->
  let !r0 = toRational c0 :: Rational
      !r1 = scale (Proxy :: Proxy scale)
      !i2 = approximate a (r0 * r1) :: Integer
      !r2 = fromInteger i2 / r1 :: Rational
      !d2 = Discrete i2
      !rest = Dense (r0 - r2)
  in (d2, rest)
{-# INLINABLE discreteFromDense #-}

--------------------------------------------------------------------------------

-- | @'Scale' currency unit@ is an irreducible rational number (expressed as
-- @'(numerator, denominator)@) indicating how many pieces of @unit@ fit in
-- @currency@.
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
-- guaranteed to be a positive rational number ('GHC.Real.infinity' and
-- 'GHC.Real.notANumber' are forbidden by 'GoodScale').
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
-- The returned 'Rational' is statically guaranteed to be a positive number with
-- a non-zero denominator.
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


-- | Composition of 'ExchangeRate's multiplies exchange rates together:
--
-- @
-- 'exchangeRateToRational' x * 'exchangeRateToRational' y  ==  'exchangeRateToRational' (x . y)
-- @
--
-- Identity:
--
-- @
-- x  ==  x . id  ==  id . x
-- @
--
-- Associativity:
--
-- @
-- x . y . z  ==  x . (y . z)  ==  (x . y) . z
-- @
--
-- Conmutativity (provided the types allow for composition):
--
-- @
-- x . y  ==  y . x
-- @
--
-- Multiplicative inverse:
--
-- @
-- 1  ==  'exchangeRateToRational' (x . 'exchangeRateRecip' x)
-- @
instance Category ExchangeRate where
  id = ExchangeRate 1
  {-# INLINE id #-}
  ExchangeRate a . ExchangeRate b = ExchangeRate (a * b)
  {-# INLINE (.) #-}

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
-- This 'Rational' is guaranteed to be greater than 0 and with a non-zero
-- denominator.
exchangeRateToRational :: ExchangeRate src dst -> Rational
exchangeRateToRational = \(ExchangeRate r0) -> r0
{-# INLINABLE exchangeRateToRational #-}

-- | Safely construct an 'ExchangeRate' from a 'Rational' number.
--
-- Notice that the absolute value of the given 'Rational' value is used, seeing
-- as there's no such thing as a negative exchange rate.
--
-- Returns 'Nothing' in case the denominator of the given 'Rational' is zero,
-- which although unlikely, is possible if the 'Rational' is constructed
-- unsafely using 'GHC.Real.infinity' or 'GHC.Real.notANumber', for example.
-- If you don't care about that scenario, you can use `fromRational` to build
-- the `Dense` value.
exchangeRate :: Rational -> Maybe (ExchangeRate src dst)
exchangeRate = \r ->
  if denominator r == 0 then Nothing else Just (ExchangeRate (abs r))
{-# INLINABLE exchangeRate #-}

-- | Reciprocal 'ExchangeRate'.
--
-- This function retuns the reciprocal or multiplicative inverse of the given
-- 'ExchangeRate', leading to the following identity law:
--
-- @
-- 'exchangeRateRecip' . 'exchangeRateRecip'   ==  'id'
-- @
--
-- Note: If 'ExchangeRate' had a 'Fractional' instance, then 'exchangeRateRecip'
-- would be the implementation of 'recip'.
exchangeRateRecip :: ExchangeRate a b -> ExchangeRate b a
exchangeRateRecip = \(ExchangeRate x) -> ExchangeRate (1 / x)
{-# INLINABLE exchangeRateRecip #-}

-- | Apply the 'ExchangeRate' to the given @'Dense' src@ monetary value.
--
-- Identity law:
--
-- @
-- 'exchange' ('exchangeRateRecip' x) . 'exchange' x  ==  'id'
-- @
--
-- Use the /Identity law/ for reasoning about going back and forth between @src@
-- and @dst@ in order to manage any leftovers that might not be representable as
-- a 'Discrete' monetary value of @src@.
exchange :: ExchangeRate src dst -> Dense src -> Dense dst
exchange (ExchangeRate r) = \(Dense s) -> Dense (r * s)
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
-- * 'someDenseAmount'
data SomeDense = SomeDense
  { _someDenseCurrency          :: !String
  , _someDenseAmount            :: !Rational
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
someDenseAmount = _someDenseAmount
{-# INLINABLE someDenseAmount #-}

-- | Build a 'SomeDense' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeDense' value to a 'Dense' value in order to do any arithmetic
-- operation on the monetary value.
mkSomeDense
  :: String   -- ^ Currency. ('someDenseCurrency')
  -> Rational -- ^ Scale. ('someDenseAmount')
  -> Maybe SomeDense
mkSomeDense = \c r -> do
  guard (denominator r /= 0)
  Just (SomeDense c r)
{-# INLINABLE mkSomeDense #-}

-- | Convert a 'Dense' to a 'SomeDense' for ease of serialization.
toSomeDense :: KnownSymbol currency => Dense currency -> SomeDense
toSomeDense = \(Dense r0 :: Dense currency) ->
  let c = symbolVal (Proxy :: Proxy currency)
  in SomeDense c r0
{-# INLINABLE toSomeDense #-}

-- | Attempt to convert a 'SomeDense' to a 'Dense', provided you know the target
-- @currency@.
fromSomeDense
  :: forall currency
  .  KnownSymbol currency
  => SomeDense
  -> Maybe (Dense currency)  -- ^
fromSomeDense = \dr -> do
  guard (someDenseCurrency dr == symbolVal (Proxy :: Proxy currency))
  Just (Dense (someDenseAmount dr))
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
-- * 'someDiscreteScale'
-- * 'someDiscreteAmount'
data SomeDiscrete = SomeDiscrete
  { _someDiscreteCurrency :: !String   -- ^ Currency name.
  , _someDiscreteScale    :: !Rational -- ^ Positive, non-zero.
  , _someDiscreteAmount   :: !Integer  -- ^ Amount of unit.
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
someDiscreteScale :: SomeDiscrete -> Rational
someDiscreteScale = _someDiscreteScale
{-# INLINABLE someDiscreteScale #-}

-- | Amount of currency unit.
someDiscreteAmount :: SomeDiscrete -> Integer
someDiscreteAmount = _someDiscreteAmount
{-# INLINABLE someDiscreteAmount #-}

-- | Internal. Build a 'SomeDiscrete' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeDiscrete' value to a 'Discrete' vallue in order to do any arithmetic
-- operation on the monetary value.
mkSomeDiscrete
  :: String   -- ^ Currency name. ('someDiscreteCurrency')
  -> Rational -- ^ Scale. Positive, non-zero. ('someDiscreteScale')
  -> Integer  -- ^ Amount of unit. ('someDiscreteAmount')
  -> Maybe SomeDiscrete
mkSomeDiscrete = \c r a -> do
  guard (denominator r /= 0)
  guard (r > 0)
  Just (SomeDiscrete c r a)
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
  in SomeDiscrete c (n % d) i0
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
      (someDiscreteScale dr == scale (Proxy :: Proxy scale))
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
      case someNatVal (numerator (someDiscreteScale dr)) of
        Nothing -> error "withSomeDiscrete: impossible: numerator < 0"
        Just (SomeNat (Proxy :: Proxy num)) ->
          case someNatVal (denominator (someDiscreteScale dr)) of
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
-- * 'someExchangeRateRate'
data SomeExchangeRate = SomeExchangeRate
  { _someExchangeRateSrcCurrency     :: !String
  , _someExchangeRateDstCurrency     :: !String
  , _someExchangeRateRate            :: !Rational -- ^ Positive, non-zero.
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
someExchangeRateRate = _someExchangeRateRate
{-# INLINABLE someExchangeRateRate #-}

-- | Internal. Build a 'SomeExchangeRate' from raw values.
--
-- This function is intended for deserialization purposes. You need to convert
-- this 'SomeExchangeRate' value to a 'ExchangeRate' value in order to do any
-- arithmetic operation with the exchange rate.
mkSomeExchangeRate
  :: String   -- ^ Source currency name. ('someExchangeRateSrcCurrency')
  -> String   -- ^ Destination currency name. ('someExchangeRateDstCurrency')
  -> Rational -- ^ Exchange rate . Positive, non-zero. ('someExchangeRateRate')
  -> Maybe SomeExchangeRate
mkSomeExchangeRate = \src dst r -> do
  guard (denominator r /= 0)
  guard (r > 0)
  Just (SomeExchangeRate src dst r)
{-# INLINABLE mkSomeExchangeRate #-}

-- | Convert a 'ExchangeRate' to a 'SomeDiscrete' for ease of serialization.
toSomeExchangeRate
  :: (KnownSymbol src, KnownSymbol dst)
  => ExchangeRate src dst
  -> SomeExchangeRate -- ^
toSomeExchangeRate = \(ExchangeRate r0 :: ExchangeRate src dst) ->
  let src = symbolVal (Proxy :: Proxy src)
      dst = symbolVal (Proxy :: Proxy dst)
  in SomeExchangeRate src dst r0
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
  put = \(SomeDense c r) -> do
    Cereal.put c
    Cereal.put (numerator r)
    Cereal.put (denominator r)
  get = maybe empty pure =<< do
    c :: String <- Cereal.get
    n :: Integer <- Cereal.get
    d :: Integer <- Cereal.get
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeDense c (n % d))

-- | Compatible with 'Discrete'.
instance Cereal.Serialize SomeDiscrete where
  put = \(SomeDiscrete c r a) -> do
    Cereal.put c
    Cereal.put (numerator r)
    Cereal.put (denominator r)
    Cereal.put a
  get = maybe empty pure =<< do
    c :: String <- Cereal.get
    n :: Integer <- Cereal.get
    d :: Integer <- Cereal.get
    when (d == 0) (fail "denominator is zero")
    a :: Integer <- Cereal.get
    pure (mkSomeDiscrete c (n % d) a)

-- | Compatible with 'ExchangeRate'.
instance Cereal.Serialize SomeExchangeRate where
  put = \(SomeExchangeRate src dst r) -> do
    Cereal.put src
    Cereal.put dst
    Cereal.put (numerator r)
    Cereal.put (denominator r)
  get = maybe empty pure =<< do
    src :: String <- Cereal.get
    dst :: String <- Cereal.get
    n :: Integer <- Cereal.get
    d :: Integer <- Cereal.get
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeExchangeRate src dst (n % d))
#endif

------------------------------------------------------------------------------
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
  put = \(SomeDense c r) ->
    Binary.put c >> Binary.put (numerator r) >> Binary.put (denominator r)
  get = maybe empty pure =<< do
    c :: String <- Binary.get
    n :: Integer <- Binary.get
    d :: Integer <- Binary.get
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeDense c (n % d))

-- | Compatible with 'Discrete'.
instance Binary.Binary SomeDiscrete where
  put = \(SomeDiscrete c r a) ->
    Binary.put c <>
    Binary.put (numerator r) <>
    Binary.put (denominator r) <>
    Binary.put a
  get = maybe empty pure =<< do
    c :: String <- Binary.get
    n :: Integer <- Binary.get
    d :: Integer <- Binary.get
    when (d == 0) (fail "denominator is zero")
    a :: Integer <- Binary.get
    pure (mkSomeDiscrete c (n % d) a)

-- | Compatible with 'ExchangeRate'.
instance Binary.Binary SomeExchangeRate where
  put = \(SomeExchangeRate src dst r) -> do
    Binary.put src
    Binary.put dst
    Binary.put (numerator r)
    Binary.put (denominator r)
  get = maybe empty pure =<< do
    src :: String <- Binary.get
    dst :: String <- Binary.get
    n :: Integer <- Binary.get
    d :: Integer <- Binary.get
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeExchangeRate src dst (n % d))
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
  encode = \(SomeDense c r) ->
    Ser.encode c <> Ser.encode (numerator r) <> Ser.encode (denominator r)
  decode = maybe (fail "SomeDense") pure =<< do
    c :: String <- Ser.decode
    n :: Integer <- Ser.decode
    d :: Integer <- Ser.decode
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeDense c (n % d))

-- | Compatible with 'Discrete'.
instance Ser.Serialise SomeDiscrete where
  encode = \(SomeDiscrete c r a) ->
    Ser.encode c <>
    Ser.encode (numerator r) <>
    Ser.encode (denominator r) <>
    Ser.encode a
  decode = maybe (fail "SomeDiscrete") pure =<< do
    c :: String <- Ser.decode
    n :: Integer <- Ser.decode
    d :: Integer <- Ser.decode
    when (d == 0) (fail "denominator is zero")
    a :: Integer <- Ser.decode
    pure (mkSomeDiscrete c (n % d) a)

-- | Compatible with 'ExchangeRate'.
instance Ser.Serialise SomeExchangeRate where
  encode = \(SomeExchangeRate src dst r) ->
    Ser.encode src <>
    Ser.encode dst <>
    Ser.encode (numerator r) <>
    Ser.encode (denominator r)
  decode = maybe (fail "SomeExchangeRate") pure =<< do
    src :: String <- Ser.decode
    dst :: String <- Ser.decode
    n :: Integer <- Ser.decode
    d :: Integer <- Ser.decode
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeExchangeRate src dst (n % d))
#endif

--------------------------------------------------------------------------------
-- Extra instances: aeson
#ifdef HAS_aeson
-- | Compatible with 'SomeDense'
--
-- Example rendering @'fromRational' (2 % 3) :: 'Dense' \"BTC\"@:
--
-- @
-- [\"BTC\", 2, 3]
-- @
--
-- Note: The JSON serialization changed in version 0.4 (the leading @"Dense"@
-- string was dropped from the rendered 'Ae.Array').
instance KnownSymbol currency => Ae.ToJSON (Dense currency) where
  toJSON = Ae.toJSON . toSomeDense

-- | Compatible with 'SomeDense'
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance KnownSymbol currency => Ae.FromJSON (Dense currency) where
  parseJSON = maybe empty pure <=< fmap fromSomeDense . Ae.parseJSON

-- | Compatible with 'Dense'
--
-- Note: The JSON serialization changed in version 0.4 (the leading @"Dense"@
-- string was dropped from the rendered 'Ae.Array').
instance Ae.ToJSON SomeDense where
  toJSON = \(SomeDense c r) ->
    Ae.toJSON (c, numerator r, denominator r)

-- | Compatible with 'Dense'.
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance Ae.FromJSON SomeDense where
  parseJSON = \v -> do
    (c, n, d) <- Ae.parseJSON v <|> do
       -- Pre 0.4 format.
       ("Dense" :: String, c, n, d) <- Ae.parseJSON v
       pure (c, n, d)
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (mkSomeDense c (n % d))

-- | Compatible with 'SomeDiscrete'
--
-- Example rendering @43 :: 'Discrete' \"BTC\" \"satoshi\"@:
--
-- @
-- [\"BTC\", 100000000, 1, 43]
-- @
--
-- Note: The JSON serialization changed in version 0.4 (the leading @"Discrete"@
-- string was dropped from the rendered 'Ae.Array').
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Ae.ToJSON (Discrete' currency scale) where
  toJSON = Ae.toJSON . toSomeDiscrete

-- | Compatible with 'SomeDiscrete'
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Ae.FromJSON (Discrete' currency scale) where
  parseJSON = maybe empty pure <=< fmap fromSomeDiscrete . Ae.parseJSON

-- | Compatible with 'Discrete''
--
-- Note: The JSON serialization changed in version 0.4 (the leading @"Discrete"@
-- string was dropped from the rendered 'Ae.Array').
instance Ae.ToJSON SomeDiscrete where
  toJSON = \(SomeDiscrete c r a) ->
    Ae.toJSON (c, numerator r, denominator r, a)

-- | Compatible with 'Discrete''
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance Ae.FromJSON SomeDiscrete where
  parseJSON = \v -> do
    (c, n, d, a) <- Ae.parseJSON v <|> do
       -- Pre 0.4 format.
       ("Discrete" :: String, c, n, d, a) <- Ae.parseJSON v
       pure (c, n, d, a)
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (mkSomeDiscrete c (n % d) a)

-- | Compatible with 'SomeExchangeRate'
--
-- Example rendering an 'ExchangeRate' constructed with
-- @'exchangeRate' (5 % 7) :: 'Maybe' ('ExchangeRate' \"USD\" \"JPY\")@
--
-- @
-- [\"USD\", \"JPY\", 5, 7]
-- @
--
-- Note: The JSON serialization changed in version 0.4 (the leading
-- @"ExchangeRate"@ string was dropped from the rendered 'Ae.Array').
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ae.ToJSON (ExchangeRate src dst) where
  toJSON = Ae.toJSON . toSomeExchangeRate

-- | Compatible with 'SomeExchangeRate'
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ae.FromJSON (ExchangeRate src dst) where
  parseJSON = maybe empty pure <=< fmap fromSomeExchangeRate . Ae.parseJSON

-- | Compatible with 'ExchangeRate'
--
-- Note: The JSON serialization changed in version 0.4 (the leading
-- @"ExchangeRate"@ string was dropped from the rendered 'Ae.Array').
instance Ae.ToJSON SomeExchangeRate where
  toJSON = \(SomeExchangeRate src dst r) ->
    Ae.toJSON (src, dst, numerator r, denominator r)

-- | Compatible with 'ExchangeRate'
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance Ae.FromJSON SomeExchangeRate where
  parseJSON = \v -> do
    (src, dst, n, d) <- Ae.parseJSON v <|> do
       -- Pre 0.4 format.
       ("ExchangeRate" :: String, src, dst, n, d) <- Ae.parseJSON v
       pure (src, dst, n, d)
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (mkSomeExchangeRate src dst (n % d))
#endif

--------------------------------------------------------------------------------
-- Extra instances: xmlbf
#ifdef HAS_xmlbf

-- | Compatible with 'SomeDense'
--
-- Example rendering @'fromRational' (2 % 3) :: 'Dense' \"BTC\"@:
--
-- @
-- \<money-dense c=\"BTC\" n=\"2\" d=\"3\"/>
-- @
instance KnownSymbol currency => Xmlbf.ToXml (Dense currency) where
  toXml = Xmlbf.toXml . toSomeDense

-- | Compatible with 'SomeDense'
instance KnownSymbol currency => Xmlbf.FromXml (Dense currency) where
  fromXml = maybe empty pure =<< fmap fromSomeDense Xmlbf.fromXml

-- | Compatible with 'Dense'
instance Xmlbf.ToXml SomeDense where
  toXml = \(SomeDense c r) ->
    let as = [ (T.pack "c", T.pack c)
             , (T.pack "n", T.pack (show (numerator r)))
             , (T.pack "d", T.pack (show (denominator r))) ]
        Right e = Xmlbf.element (T.pack "money-dense") (fromList as) []
    in [e]

-- | Compatible with 'Dense'.
instance Xmlbf.FromXml SomeDense where
  fromXml = Xmlbf.pElement (T.pack "money-dense") $ do
    c <- T.unpack <$> Xmlbf.pAttr "c"
    n <- Xmlbf.pRead =<< Xmlbf.pAttr "n"
    d <- Xmlbf.pRead =<< Xmlbf.pAttr "d"
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (mkSomeDense c (n % d))

-- | Compatible with 'SomeDiscrete'
--
-- Example rendering @43 :: 'Discrete' \"BTC\" \"satoshi\"@:
--
-- @
-- \<money-discrete c=\"BTC\" n=\"100000000\" d=\"1\" a=\"43\"/>
-- @
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Xmlbf.ToXml (Discrete' currency scale) where
  toXml = Xmlbf.toXml . toSomeDiscrete

-- | Compatible with 'SomeDiscrete'
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Xmlbf.FromXml (Discrete' currency scale) where
  fromXml = maybe empty pure =<< fmap fromSomeDiscrete Xmlbf.fromXml

-- | Compatible with 'Discrete''
instance Xmlbf.ToXml SomeDiscrete where
  toXml = \(SomeDiscrete c r a) ->
    let as = [ (T.pack "c", T.pack c)
             , (T.pack "n", T.pack (show (numerator r)))
             , (T.pack "d", T.pack (show (denominator r)))
             , (T.pack "a", T.pack (show a)) ]
        Right e = Xmlbf.element (T.pack "money-discrete") (fromList as) []
    in [e]

-- | Compatible with 'Discrete''
instance Xmlbf.FromXml SomeDiscrete where
  fromXml = Xmlbf.pElement (T.pack "money-discrete") $ do
    c <- T.unpack <$> Xmlbf.pAttr "c"
    n <- Xmlbf.pRead =<< Xmlbf.pAttr "n"
    d <- Xmlbf.pRead =<< Xmlbf.pAttr "d"
    when (d == 0) (fail "denominator is zero")
    a <- Xmlbf.pRead =<< Xmlbf.pAttr "a"
    maybe empty pure (mkSomeDiscrete c (n % d) a)

-- | Compatible with 'SomeExchangeRate'
--
-- Example rendering an 'ExchangeRate' constructed with
-- @'exchangeRate' (5 % 7) :: 'Maybe' ('ExchangeRate' \"USD\" \"JPY\")@
--
-- @
-- \<exchange-rate src=\"USD\" dst=\"JPY\" n=\"5\" d=\"7\"/>
-- @
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Xmlbf.ToXml (ExchangeRate src dst) where
  toXml = Xmlbf.toXml . toSomeExchangeRate

-- | Compatible with 'SomeExchangeRate'
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Xmlbf.FromXml (ExchangeRate src dst) where
  fromXml = maybe empty pure =<< fmap fromSomeExchangeRate Xmlbf.fromXml

-- | Compatible with 'ExchangeRate'
instance Xmlbf.ToXml SomeExchangeRate where
  toXml = \(SomeExchangeRate src dst r) ->
    let as = [ (T.pack "src", T.pack src)
             , (T.pack "dst", T.pack dst)
             , (T.pack "n", T.pack (show (numerator r)))
             , (T.pack "d", T.pack (show (denominator r))) ]
        Right e = Xmlbf.element (T.pack "exchange-rate") (fromList as) []
    in [e]

-- | Compatible with 'ExchangeRate'
instance Xmlbf.FromXml SomeExchangeRate where
  fromXml = Xmlbf.pElement (T.pack "exchange-rate") $ do
    src <- T.unpack <$> Xmlbf.pAttr "src"
    dst <- T.unpack <$> Xmlbf.pAttr "dst"
    n <- Xmlbf.pRead =<< Xmlbf.pAttr "n"
    d <- Xmlbf.pRead =<< Xmlbf.pAttr "d"
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (mkSomeExchangeRate src dst (n % d))
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
  poke = \(SomeDense c r) -> do
    Store.poke c
    Store.poke (numerator r)
    Store.poke (denominator r)
  peek = maybe (fail "peek") pure =<< do
    c :: String <- Store.peek
    n :: Integer <- Store.peek
    d :: Integer <- Store.peek
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeDense c (n % d))

-- | Compatible with 'SomeDiscrete'.
instance
  ( KnownSymbol currency, GoodScale scale
  ) => Store.Store (Discrete' currency scale) where
  size = storeContramapSize toSomeDiscrete Store.size
  poke = Store.poke . toSomeDiscrete
  peek = maybe (fail "peek") pure =<< fmap fromSomeDiscrete Store.peek
-- | Compatible with 'Discrete''.
instance Store.Store SomeDiscrete where
  poke = \(SomeDiscrete c r a) ->do
    Store.poke c
    Store.poke (numerator r)
    Store.poke (denominator r)
    Store.poke a
  peek = maybe (fail "peek") pure =<< do
    c :: String <- Store.peek
    n :: Integer <- Store.peek
    d :: Integer <- Store.peek
    when (d == 0) (fail "denominator is zero")
    a :: Integer <- Store.peek
    pure (mkSomeDiscrete c (n % d) a)
-- | Compatible with 'SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Store.Store (ExchangeRate src dst) where
  size = storeContramapSize toSomeExchangeRate Store.size
  poke = Store.poke . toSomeExchangeRate
  peek = maybe (fail "peek") pure =<< fmap fromSomeExchangeRate Store.peek
-- | Compatible with 'ExchangeRate'.
instance Store.Store SomeExchangeRate where
  poke = \(SomeExchangeRate src dst r) -> do
    Store.poke src
    Store.poke dst
    Store.poke (numerator r)
    Store.poke (denominator r)
  peek = maybe (fail "peek") pure =<< do
    src <- Store.peek
    dst <- Store.peek
    n <- Store.peek
    d <- Store.peek
    when (d == 0) (fail "denominator is zero")
    pure (mkSomeExchangeRate src dst (n % d))

storeContramapSize :: (a -> b) -> Store.Size b -> Store.Size a
storeContramapSize f = \case
  Store.VarSize g -> Store.VarSize (g . f)
  Store.ConstSize x -> Store.ConstSize x
{-# INLINABLE storeContramapSize #-}
#endif

--------------------------------------------------------------------------------
-- Decimal rendering

-- | Render a 'Dense' monetary amount as a decimal number in a potentially lossy
-- manner.
--
-- @
-- > 'denseToDecimal' 'Round' 'True' ('Just' \',\') \'.\' 2
--      ('Proxy' :: 'Proxy' ('Scale' \"USD\" \"dollar\"))
--      (123456 % 100 :: 'Dense' \"USD\")
-- \"+1,234.56\"
-- @
--
-- @
-- > 'denseToDecimal' 'Round' 'True' ('Just' \',\') \'.\' 2
--      ('Proxy' :: 'Proxy' ('Scale' \"USD\" \"cent\"))
--      (123456 % 100 :: 'Dense' \"USD\")
-- \"+123,456.00\"
-- @
denseToDecimal
  :: GoodScale scale
  => Approximation
  -- ^ Approximation to use if necesary in order to fit the 'Dense' amount in
  -- as many decimal numbers as requested.
  -> Bool
  -- ^ Whether to render a leading @\'+\'@ sign in case the amount is positive.
  -> Maybe Char
  -- ^ Thousands separator for the integer part, if any (i.e., the @\',\'@ in
  -- @1,234.56789@).
  -> Char
  -- ^ Decimal separator (i.e., the @\'.\'@ in @1,234.56789@)
  -> Word8
  -- ^ Number of decimal numbers to render, if any.
  -> Proxy scale
  -- ^ Scale used by the integer part of the decimal number. For example, a
  -- when rendering render @123 % 100 :: Dense "USD"@ as a decimal number with
  -- three decimal places, a scale of @1@ (i.e. @'Scale' \"USD\" \"dollar\"@)
  -- would render @1@ as the integer part and @230@ as the fractional part,
  -- whereas a scale of @100@ (i.e., @'Scale' \"USD\" \"cent\"@) would render
  -- @123@ as the integer part and @000@ as the fractional part.
  -> Dense currency
  -- ^ The dense monetary amount to render.
  -> String
{-# INLINE denseToDecimal #-}
denseToDecimal a plus ytsep dsep fdigs0 ps = \(Dense r0) ->
  let r1 :: Rational = r0 * scale ps
      parts :: Integer = approximate a (r1 * (10 ^ fdigs0))
      ipart :: Natural = fromInteger (abs parts) `div` (10 ^ fdigs0)
      ftext :: String = drop (length (show ipart)) (show (abs parts))
      itext :: String = maybe (show ipart) (renderThousands ipart) ytsep
      fpad0 :: String = List.replicate (fromIntegral fdigs0 - length ftext) '0'
  in mconcat
       [ if | parts < 0 -> "-"
            | plus && parts > 0 -> "+"
            | otherwise -> ""
       , itext
       , if | fdigs0 > 0 -> dsep : ftext <> fpad0
            | otherwise -> ""
       ]


-- | Render a 'Natural' number with thousand markers.
--
-- @
-- > 'renderThousands' 12045 \',\'
-- \"12,045\"
-- @
renderThousands :: Natural -> Char -> String
renderThousands n sep
  | n < 1000 = show n
  | otherwise
      = List.foldl' (flip mappend) mempty
      $ List.intersperse [sep]
      $ List.unfoldr (\x ->
          case divMod x 1000 of
             (0, 0) -> Nothing
             (0, z) -> Just (show z, 0)
             (y, z) | z <  10   -> Just ('0':'0':show z, y)
                    | z < 100   -> Just (    '0':show z, y)
                    | otherwise -> Just (        show z, y))
      $ n

--------------------------------------------------------------------------------
-- Decimal parsing

-- | Parses a decimal representation of a 'Dense'.
--
-- Leading @\'-\'@ and @\'+\'@ characters are considered.
denseFromDecimal
  :: Maybe Char
  -- ^ Thousands separator for the integer part, if any (i.e., the @\',\'@ in
  -- @-1,234.56789@).
  -> Char
  -- ^ Decimal separator (i.e., the @\'.\'@ in @-1,234.56789@)
  -> String
  -- ^ The raw string containing the decimal representation (e.g.,
  -- @"-1,234.56789"@).
  -> Maybe (Dense currency)
denseFromDecimal yst sf = fmap Dense . rationalFromDecimal yst sf

-- | Parses a decimal representation of a 'Discrete'.
--
-- Leading @\'-\'@ and @\'+\'@ characters are considered.
--
-- Notice that parsing will fail unless the entire precision of the decimal
-- number can be represented in the desired @scale@.
discreteFromDecimal
  :: GoodScale scale
  => Maybe Char
  -- ^ Thousands separator for the integer part, if any (i.e., the @\',\'@ in
  -- @-1,234.56789@).
  -> Char
  -- ^ Decimal separator (i.e., the @\'.\'@ in @-1,234.56789@)
  -> String
  -- ^ The raw string containing the decimal representation (e.g.,
  -- @"-1,234.56789"@).
  -> Maybe (Discrete' currency scale)
discreteFromDecimal yst sf = \s -> do
  dns <- denseFromDecimal yst sf s
  case discreteFromDense Truncate dns of
    (x, 0) -> Just x
    _ -> Nothing -- We fail for decimals that don't fit exactly in our scale.

rationalFromDecimal
  :: Maybe Char
  -- ^ Thousands separator for the integer part, if any (i.e., the @\',\'@ in
  -- @-1,234.56789@).
  -> Char
  -- ^ Decimal separator (i.e., the @\'.\'@ in @-1,234.56789@)
  -> String
  -- ^ The raw string containing the decimal representation (e.g.,
  -- @"-1,234.56789"@).
  -> Maybe Rational
rationalFromDecimal yst sf = \s ->
  case ReadP.readP_to_S (rationalFromDecimalP yst sf) s of
    [(x,"")] -> Just x
    _ -> Nothing

-- TODO limit number of digits parsed to prevent DoS
rationalFromDecimalP
  :: Maybe Char
  -- ^ Thousands separator for the integer part, if any (i.e., the @\',\'@ in
  -- @-1,234.56789@).
  --
  -- The separator can't be a digit or control character. If it is, then parsing
  -- will always fail.
  -> Char
  -- ^ Decimal separator (i.e., the @\'.\'@ in @-1,234.56789@).
  --
  -- The separator can't be a digit or control character. If it is, then parsing
  -- will always fail.
  -> ReadP.ReadP Rational
rationalFromDecimalP yst sf = do
   guard (not (Char.isDigit sf || maybe False Char.isDigit yst))
   sig :: Rational -> Rational <-
     (ReadP.char '-' $> negate) <|>
     (ReadP.char '+' $> id) <|>
     (pure id)
   ipart :: String <- case yst of
     Nothing -> ReadP.munch1 Char.isDigit
     Just st -> do
       ihead <- (ReadP.count 3 (ReadP.satisfy Char.isDigit) <|>
                 ReadP.count 2 (ReadP.satisfy Char.isDigit) <|>
                 ReadP.count 1 (ReadP.satisfy Char.isDigit))
       itail <- concat <$> ReadP.sepBy
                   (ReadP.count 3 (ReadP.satisfy Char.isDigit))
                   (ReadP.char st)
       pure (ihead <> itail)
   yfpart :: Maybe String <-
     (ReadP.char sf *> fmap Just (ReadP.munch1 Char.isDigit) <* ReadP.eof) <|>
     (ReadP.eof $> Nothing)
   pure $! sig $ case yfpart of
     Nothing -> fromInteger (read ipart)
     Just fpart -> read (ipart <> fpart) % (10 ^ length fpart)
