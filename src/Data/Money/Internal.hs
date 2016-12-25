{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This is an internal module. Import "Data.Money" instead.
module Data.Money.Internal
 ( -- * Dense monetary values
   Dense
 , dense
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
 , DenseRep
 , denseRepCurrency
 , denseRepAmount
 , denseRepAmountNumerator
 , denseRepAmountDenominator
 , toDenseRep
 , mkDenseRep
 , fromDenseRep
 , withDenseRep
 , DiscreteRep
 , discreteRepCurrency
 , discreteRepScale
 , discreteRepScaleNumerator
 , discreteRepScaleDenominator
 , discreteRepAmount
 , toDiscreteRep
 , mkDiscreteRep
 , fromDiscreteRep
 , withDiscreteRep
 , ExchangeRateRep
 , exchangeRateRepSrcCurrency
 , exchangeRateRepDstCurrency
 , exchangeRateRepRate
 , exchangeRateRepRateNumerator
 , exchangeRateRepRateDenominator
 , toExchangeRateRep
 , mkExchangeRateRep
 , fromExchangeRateRep
 , withExchangeRateRep
 ) where

import Control.Applicative (empty)
import Data.Constraint (Dict(Dict))
import Data.Proxy (Proxy(..))
import Data.Ratio ((%), numerator, denominator)
import GHC.Real (infinity, notANumber)
import GHC.TypeLits
  (Symbol, SomeSymbol(..), Nat, SomeNat(..), CmpNat, KnownSymbol, KnownNat,
   natVal, someNatVal, symbolVal, someSymbolVal)
import qualified GHC.TypeLits as GHC
import Prelude hiding (round, ceiling, floor, truncate)
import qualified Prelude
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readPrec)
import Unsafe.Coerce (unsafeCoerce)

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
  deriving (Eq, Ord, Num, Real, Fractional)

instance forall currency. KnownSymbol currency => Show (Dense currency) where
  show = \(Dense r0) ->
    let c = symbolVal (Proxy :: Proxy currency)
    in concat [ "Dense ", show c, " (", show r0, ")" ]

instance forall currency. KnownSymbol currency => Read (Dense currency) where
  readPrec = do
    let c = symbolVal (Proxy :: Proxy currency)
    _ <- ReadPrec.lift (ReadP.string ("Dense " ++ show c ++ " "))
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
-- /pennies/ equal 1 GBP (i.e., @'Scale' \"GBP\" ~ '(100, 1)@), then you can
-- use:
--
-- @
-- 'fromInteger' 2105 :: Discrete "GBP" "penny"
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
  deriving (Eq, Ord, Enum, Num, Real, Integral)

instance forall currency scale.
  ( KnownSymbol currency, GoodScale scale
  ) => Show (Discrete' currency scale) where
  show = \d0@(Discrete i0) ->
    let c = symbolVal (Proxy :: Proxy currency)
        s = scale d0
    in concat [ "Discrete ", show c, " (", show s, ") ", show i0 ]

instance forall currency scale.
  ( KnownSymbol currency, GoodScale scale
  ) => Read (Discrete' currency scale) where
  readPrec = do
    let c = symbolVal (Proxy :: Proxy currency)
        s = scale (Proxy :: Proxy scale)
    _ <- ReadPrec.lift (ReadP.string
           ("Discrete " ++ show c ++ " (" ++ show s ++ ") ") )
    Discrete <$> readPrec

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
       ('GHC.Text " features on it instead.")) )
  => Fractional (Discrete' currency scale) where
  fromRational = undefined
  recip = undefined

-- | Convert currency 'Discrete' monetary value into a 'Dense' monetary
-- value.
fromDiscrete
  :: GoodScale scale
  => Discrete' currency scale
  -> Dense currency -- ^
fromDiscrete = \c@(Discrete i) -> Dense (fromInteger i / scale c)
{-# INLINE fromDiscrete #-}

-- | Internal. Used to implement 'round', 'ceiling', 'floor' and 'truncate'.
roundf
  :: GoodScale scale
  => (Rational -> Integer) -- ^ 'Prelude.round', 'Prelude.ceiling' or similar.
  -> Dense currency
  -> (Discrete' currency scale, Maybe (Dense currency))
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
{-# INLINE round #-}

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
{-# INLINE ceiling #-}

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
{-# INLINE floor #-}

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
{-# INLINE truncate #-}

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
-- If there exists a cannonical smallest @unit@ that can fully represent the
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

-- | A friendly 'GHC.TypeError' to use for a @currency@ that doesn't have a
-- cannonical small unit.
type family ErrScaleNonCanonical (currency :: Symbol) :: k where
  ErrScaleNonCanonical c = GHC.TypeError
    ( 'GHC.Text c 'GHC.:<>:
      'GHC.Text " is not a currency with a canonical smallest unit," 'GHC.:$$:
      'GHC.Text "be explicit about the currency unit you want to use." )

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
     then Just (unsafeCoerce (Dict :: Dict ()))
     else Nothing
{-# INLINE mkGoodScale #-}

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
{-# INLINE scale #-}

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
  deriving (Eq, Ord)

instance forall src dst.
  ( KnownSymbol src, KnownSymbol dst
  ) => Show (ExchangeRate src dst) where
  show = \(ExchangeRate r0) ->
    let s = symbolVal (Proxy :: Proxy src)
        d = symbolVal (Proxy :: Proxy dst)
    in concat [ "ExchangeRate ", show s, " ", show d, " (", show r0, ")" ]

instance forall src dst.
  ( KnownSymbol src, KnownSymbol dst
  ) => Read (ExchangeRate (src :: Symbol) (dst :: Symbol)) where
  readPrec = do
    let s = symbolVal (Proxy :: Proxy src)
        d = symbolVal (Proxy :: Proxy dst)
    _ <- ReadPrec.lift (ReadP.string
            ("ExchangeRate " ++ show s ++ " " ++ show d ++ " "))
    maybe empty pure =<< fmap exchangeRate readPrec

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
-- DenseRep

-- | A monomorphic representation of 'Dense' that is easier to serialize and
-- deserialize than 'Dense' in case you don't know the type indexes involved.
data DenseRep = DenseRep
  { _denseRepCurrency          :: !String
  , _denseRepAmountNumerator   :: !Integer
  , _denseRepAmountDenominator :: !Integer  -- ^ Positive, non-zero.
  } deriving (Eq, Show, Read)

-- | WARNING: This instance does not compare monetary amounts, it just helps you
-- sort 'DenseRep' values in case you need to put them in a 'Data.Set.Set' or
-- similar.
deriving instance Ord DenseRep

-- | Currency name.
denseRepCurrency :: DenseRep -> String
denseRepCurrency = _denseRepCurrency
{-# INLINE denseRepCurrency #-}

-- | Currency unit amount.
denseRepAmount :: DenseRep -> Rational
denseRepAmount = \dr ->
  denseRepAmountNumerator dr % denseRepAmountDenominator dr
{-# INLINE denseRepAmount #-}

-- | Currency unit amount numerator.
denseRepAmountNumerator :: DenseRep -> Integer
denseRepAmountNumerator = _denseRepAmountNumerator
{-# INLINE denseRepAmountNumerator #-}

-- | Currency unit amount denominator. Positive, non-zero.
denseRepAmountDenominator :: DenseRep -> Integer
denseRepAmountDenominator = _denseRepAmountDenominator
{-# INLINE denseRepAmountDenominator #-}

-- | Internal. Build a 'DenseRep' from raw values.
mkDenseRep
  :: String -- ^ Currency.
  -> Integer -- ^ Scale nominator.
  -> Integer -- ^ Scale denominator (positive, non zero)
  -> Maybe DenseRep
mkDenseRep = \c n d -> case d > 0 of
  False -> Nothing
  True -> Just (DenseRep c n d)
{-# INLINE mkDenseRep #-}

-- | Convert a 'Dense' to a 'DenseRep' for ease of serialization.
toDenseRep :: KnownSymbol currency => Dense currency -> DenseRep
toDenseRep = \(Dense r0 :: Dense currency) ->
  let c = symbolVal (Proxy :: Proxy currency)
  in DenseRep c (numerator r0) (denominator r0)
{-# INLINE toDenseRep #-}

-- | Attempt to convert a 'DenseRep' to a 'Dense', provided you know the target
-- @currency@.
fromDenseRep
  :: forall currency
  .  KnownSymbol currency
  => DenseRep
  -> Maybe (Dense currency)  -- ^
fromDenseRep = \dr ->
  case denseRepCurrency dr == symbolVal (Proxy :: Proxy currency) of
     False -> Nothing
     True -> Just (Dense (denseRepAmount dr))
{-# INLINE fromDenseRep #-}

-- | Convert a 'DenseRep' to a 'Dense' without knowing the target @currency@.
--
-- Notice that @currency@ here can't leave its intended scope unless you can
-- prove equality with some other type at the outer scope, but in that case you
-- would be better off using 'fromDenseRep' directly.
withDenseRep
  :: DenseRep
  -> (forall currency. KnownSymbol currency => Dense currency -> r)
  -> r  -- ^
withDenseRep dr = \f ->
   case someSymbolVal (denseRepCurrency dr) of
      SomeSymbol (Proxy :: Proxy currency) ->
         f (Dense (denseRepAmount dr) :: Dense currency)
{-# INLINE withDenseRep #-}

--------------------------------------------------------------------------------
-- DiscreteRep

-- | A monomorphic representation of 'Discrete' that is easier to serialize and
-- deserialize than 'Discrete' in case you don't know the type indexes involved.
data DiscreteRep = DiscreteRep
  { _discreteRepCurrency         :: !String   -- ^ Currency name.
  , _discreteRepScaleNumerator   :: !Integer  -- ^ Positive, non-zero.
  , _discreteRepScaleDenominator :: !Integer  -- ^ Positive, non-zero.
  , _discreteRepAmount           :: !Integer  -- ^ Amount of unit.
  } deriving (Eq, Show, Read)

-- | WARNING: This instance does not compare monetary amounts, it just helps you
-- sort 'DiscreteRep' values in case you need to put them in a 'Data.Set.Set' or
-- similar.
deriving instance Ord DiscreteRep

-- | Currency name.
discreteRepCurrency :: DiscreteRep -> String
discreteRepCurrency = _discreteRepCurrency
{-# INLINE discreteRepCurrency #-}

-- | Positive, non-zero.
discreteRepScale :: DiscreteRep -> Rational
discreteRepScale = \dr ->
  discreteRepScaleNumerator dr % discreteRepScaleDenominator dr
{-# INLINE discreteRepScale #-}

-- | Positive, non-zero.
discreteRepScaleNumerator :: DiscreteRep -> Integer
discreteRepScaleNumerator = _discreteRepScaleNumerator
{-# INLINE discreteRepScaleNumerator #-}

-- | Positive, non-zero.
discreteRepScaleDenominator :: DiscreteRep -> Integer
discreteRepScaleDenominator = _discreteRepScaleDenominator
{-# INLINE discreteRepScaleDenominator #-}

-- | Amount of currency unit.
discreteRepAmount :: DiscreteRep -> Integer
discreteRepAmount = _discreteRepAmount
{-# INLINE discreteRepAmount #-}

-- | Internal. Build a 'DiscreteRep' from raw values.
mkDiscreteRep
  :: String   -- ^ Currency name.
  -> Integer  -- ^ Scale numerator. Positive, non-zero.
  -> Integer  -- ^ Scale denominator. Positive, non-zero.
  -> Integer  -- ^ Amount of unit.
  -> Maybe DiscreteRep
mkDiscreteRep = \c n d a -> case (n > 0) && (d > 0) of
  False -> Nothing
  True -> Just (DiscreteRep c n d a)
{-# INLINE mkDiscreteRep #-}

-- | Convert a 'Discrete' to a 'DiscreteRep' for ease of serialization.
toDiscreteRep
  :: (KnownSymbol currency, GoodScale scale)
  => Discrete' currency scale
  -> DiscreteRep -- ^
toDiscreteRep = \(Discrete i0 :: Discrete' currency scale) ->
  let c = symbolVal (Proxy :: Proxy currency)
      n = natVal (Proxy :: Proxy (Fst scale))
      d = natVal (Proxy :: Proxy (Snd scale))
  in DiscreteRep c n d i0
{-# INLINE toDiscreteRep #-}

-- | Attempt to convert a 'DiscreteRep' to a 'Discrete', provided you know the
-- target @currency@ and @unit@.
fromDiscreteRep
  :: forall currency scale
  .  (KnownSymbol currency, GoodScale scale)
  => DiscreteRep
  -> Maybe (Discrete' currency scale)  -- ^
fromDiscreteRep = \dr ->
   if (discreteRepCurrency dr == symbolVal (Proxy :: Proxy currency)) &&
      (discreteRepScaleNumerator dr == natVal (Proxy :: Proxy (Fst scale))) &&
      (discreteRepScaleDenominator dr == natVal (Proxy :: Proxy (Snd scale)))
   then Just (Discrete (discreteRepAmount dr))
   else Nothing
{-# INLINE fromDiscreteRep #-}

-- | Convert a 'DiscreteRep' to a 'Discrete' without knowing the target
-- @currency@ and @unit@.
--
-- Notice that @currency@ and @unit@ here can't leave its intended scope unless
-- you can prove equality with some other type at the outer scope, but in that
-- case you would be better off using 'fromDiscreteRep' directly.
withDiscreteRep
  :: DiscreteRep
  -> ( forall currency scale.
         ( KnownSymbol currency
         , GoodScale scale
         ) => Discrete' currency scale
           -> r )
  -> r  -- ^
withDiscreteRep dr = \f ->
  case someSymbolVal (discreteRepCurrency dr) of
    SomeSymbol (Proxy :: Proxy currency) ->
      case someNatVal (discreteRepScaleNumerator dr) of
        Nothing -> error "withDiscreteRep: impossible: numerator < 0"
        Just (SomeNat (Proxy :: Proxy num)) ->
          case someNatVal (discreteRepScaleDenominator dr) of
            Nothing -> error "withDiscreteRep: impossible: denominator < 0"
            Just (SomeNat (Proxy :: Proxy den)) ->
              case mkGoodScale of
                Nothing -> error "withDiscreteRep: impossible: mkGoodScale"
                Just (Dict :: Dict (GoodScale '(num, den))) ->
                  f (Discrete (discreteRepAmount dr)
                       :: Discrete' currency '(num, den))
{-# INLINABLE withDiscreteRep #-}

--------------------------------------------------------------------------------
-- ExchangeRateRep

-- | A monomorphic representation of 'ExchangeRate' that is easier to serialize
-- and deserialize than 'ExchangeRate' in case you don't know the type indexes
-- involved.
data ExchangeRateRep = ExchangeRateRep
  { _exchangeRateRepSrcCurrency     :: !String
  , _exchangeRateRepDstCurrency     :: !String
  , _exchangeRateRepRateNumerator   :: !Integer  -- ^ Positive, non-zero.
  , _exchangeRateRepRateDenominator :: !Integer  -- ^ Positive, non-zero.
  } deriving (Eq, Show)

-- | WARNING: This instance does not compare monetary amounts, it just helps you
-- sort 'ExchangeRateRep' values in case you need to put them in a
-- 'Data.Set.Set' or similar.
deriving instance Ord ExchangeRateRep

-- | Source currency name.
exchangeRateRepSrcCurrency :: ExchangeRateRep -> String
exchangeRateRepSrcCurrency = _exchangeRateRepSrcCurrency
{-# INLINE exchangeRateRepSrcCurrency #-}

-- | Destination currency name.
exchangeRateRepDstCurrency :: ExchangeRateRep -> String
exchangeRateRepDstCurrency = _exchangeRateRepDstCurrency
{-# INLINE exchangeRateRepDstCurrency #-}

-- | Exchange rate. Positive, non-zero.
exchangeRateRepRate :: ExchangeRateRep -> Rational
exchangeRateRepRate = \x ->
  exchangeRateRepRateNumerator x % _exchangeRateRepRateDenominator x
{-# INLINE exchangeRateRepRate #-}

-- | Exchange rate numerator. Positive, non-zero.
exchangeRateRepRateNumerator :: ExchangeRateRep -> Integer
exchangeRateRepRateNumerator = _exchangeRateRepRateNumerator
{-# INLINE exchangeRateRepRateNumerator #-}

-- | Exchange rate denominator. Positive, non-zero.
exchangeRateRepRateDenominator :: ExchangeRateRep -> Integer
exchangeRateRepRateDenominator = _exchangeRateRepRateDenominator
{-# INLINE exchangeRateRepRateDenominator #-}

-- | Internal. Build a 'ExchangeRateRep' from raw values.
mkExchangeRateRep
  :: String   -- ^ Source currency name.
  -> String   -- ^ Destination currency name.
  -> Integer  -- ^ Exchange rate numerator. Positive, non-zero.
  -> Integer  -- ^ Exchange rate denominator. Positive, non-zero.
  -> Maybe ExchangeRateRep
mkExchangeRateRep = \src dst n d -> case (n > 0) && (d > 0) of
  False -> Nothing
  True -> Just (ExchangeRateRep src dst n d)
{-# INLINE mkExchangeRateRep #-}

-- | Convert a 'ExchangeRate' to a 'DiscreteRep' for ease of serialization.
toExchangeRateRep
  :: (KnownSymbol src, KnownSymbol dst)
  => ExchangeRate src dst
  -> ExchangeRateRep -- ^
toExchangeRateRep = \(ExchangeRate r0 :: ExchangeRate src dst) ->
  let src = symbolVal (Proxy :: Proxy src)
      dst = symbolVal (Proxy :: Proxy dst)
  in ExchangeRateRep src dst (numerator r0) (denominator r0)
{-# INLINE toExchangeRateRep #-}

-- | Attempt to convert a 'ExchangeRateRep' to a 'ExchangeRate', provided you
-- know the target @src@ and @dst@ types.
fromExchangeRateRep
  :: forall src dst
  .  (KnownSymbol src, KnownSymbol dst)
  => ExchangeRateRep
  -> Maybe (ExchangeRate src dst)  -- ^
fromExchangeRateRep = \x ->
   if (exchangeRateRepSrcCurrency x == symbolVal (Proxy :: Proxy src)) &&
      (exchangeRateRepDstCurrency x == symbolVal (Proxy :: Proxy dst))
   then Just (ExchangeRate (exchangeRateRepRate x))
   else Nothing
{-# INLINE fromExchangeRateRep #-}

-- | Convert a 'ExchangeRateRep' to a 'ExchangeRate' without knowing the target
-- @currency@ and @unit@.
--
-- Notice that @src@ and @dst@ here can't leave its intended scope unless
-- you can prove equality with some other type at the outer scope, but in that
-- case you would be better off using 'fromExchangeRateRep' directly.
withExchangeRateRep
  :: ExchangeRateRep
  -> ( forall src dst.
         ( KnownSymbol src
         , KnownSymbol dst
         ) => ExchangeRate src dst
           -> r )
  -> r  -- ^
withExchangeRateRep x = \f ->
  case someSymbolVal (exchangeRateRepSrcCurrency x) of
    SomeSymbol (Proxy :: Proxy src) ->
      case someSymbolVal (exchangeRateRepDstCurrency x) of
        SomeSymbol (Proxy :: Proxy dst) ->
          f (ExchangeRate (exchangeRateRepRate x) :: ExchangeRate src dst)
{-# INLINABLE withExchangeRateRep #-}

--------------------------------------------------------------------------------
-- Miscellaneous

type family Fst (ab :: (ka, kb)) :: ka where Fst '(a,b) = a
type family Snd (ab :: (ka, kb)) :: ka where Snd '(a,b) = b

