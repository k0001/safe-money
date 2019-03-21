{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module only exports orphan 'Ae.FromJSON' and 'Ae.ToJSON' instances.
-- Import as:
--
-- @
-- import "Money.Aeson" ()
-- @
module Money.Aeson () where

import Control.Applicative ((<|>), empty)
import Control.Monad ((<=<), when)
import qualified Data.Aeson as Ae
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol)
import qualified Money
import qualified Money.Internal as MoneyI

--------------------------------------------------------------------------------
-- | Compatible with 'Money.SomeDense'
--
-- Example rendering @'Money.dense'' (2 '%' 3) :: 'Money.Dense' \"BTC\"@:
--
-- @
-- [\"BTC\", 2, 3]
-- @
--
-- Note: The JSON serialization changed in version 0.4 (the leading @"Dense"@
-- string was dropped from the rendered 'Ae.Array').
instance KnownSymbol currency => Ae.ToJSON (Money.Dense currency) where
  toJSON = Ae.toJSON . Money.toSomeDense

-- | Compatible with 'Money.SomeDense'
--
-- Note: The JSON serialization changed in @safe-money@ version 0.4. However,
-- this instance is still able to cope with the previous format.
instance KnownSymbol currency => Ae.FromJSON (Money.Dense currency) where
  parseJSON = maybe empty pure <=< fmap Money.fromSomeDense . Ae.parseJSON

-- | Compatible with 'Money.Dense'
--
-- Note: The JSON serialization changed in @safe-money@ version 0.4 (the leading
-- @"Dense"@ string was dropped from the rendered 'Ae.Array').
instance Ae.ToJSON Money.SomeDense where
  toJSON = \sd ->
    let r = Money.someDenseAmount sd
    in Ae.toJSON (MoneyI.someDenseCurrency' sd, numerator r, denominator r)

-- | Compatible with 'Money.Dense'.
--
-- Note: The JSON serialization changed in @safe-money@ version 0.4. However,
-- this instance is still able to cope with the previous format.
instance Ae.FromJSON Money.SomeDense where
  parseJSON = \v -> do
    (c, n, d) <- Ae.parseJSON v <|> do
       -- Pre 0.4 format.
       ("Dense" :: String, c, n, d) <- Ae.parseJSON v
       pure (c, n, d)
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (MoneyI.mkSomeDense' c (n % d))

-- | Compatible with 'Money.SomeDiscrete'
--
-- Example rendering @'Money.discrete' 43 :: 'Money.Discrete' \"BTC\" \"satoshi\"@:
--
-- @
-- [\"BTC\", 100000000, 1, 43]
-- @
--
-- Note: The JSON serialization changed in @safe-money@ version 0.4 (the leading
-- @"Discrete"@ string was dropped from the rendered 'Ae.Array').
instance
  ( KnownSymbol currency, Money.GoodScale scale
  ) => Ae.ToJSON (Money.Discrete' currency scale) where
  toJSON = Ae.toJSON . Money.toSomeDiscrete

-- | Compatible with 'Money.SomeDiscrete'
--
-- Note: The JSON serialization changed in @safe-money@ version 0.4. However,
-- this instance is still able to cope with the previous format.
instance
  ( KnownSymbol currency, Money.GoodScale scale
  ) => Ae.FromJSON (Money.Discrete' currency scale) where
  parseJSON = maybe empty pure <=< fmap Money.fromSomeDiscrete . Ae.parseJSON

-- | Compatible with 'Money.Discrete''
--
-- Note: The JSON serialization changed in version 0.4 (the leading @"Discrete"@
-- string was dropped from the rendered 'Ae.Array').
instance Ae.ToJSON Money.SomeDiscrete where
  toJSON = \sd ->
    let rs = Money.scaleToRational (Money.someDiscreteScale sd)
    in Ae.toJSON (MoneyI.someDiscreteCurrency' sd,
                  numerator rs, denominator rs,
                  Money.someDiscreteAmount sd)

-- | Compatible with 'Money.Discrete''
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance Ae.FromJSON Money.SomeDiscrete where
  parseJSON = \v -> do
    (c, n, d, a) <- Ae.parseJSON v <|> do
       -- Pre 0.4 format.
       ("Discrete" :: T.Text, c, n, d, a) <- Ae.parseJSON v
       pure (c, n, d, a)
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (MoneyI.mkSomeDiscrete' c
                        <$> Money.scaleFromRational (n % d)
                        <*> pure a)

-- | Compatible with 'Money.SomeExchangeRate'
--
-- Example rendering an 'Money.ExchangeRate' constructed with
-- @'Money.exchangeRate' (5 '%' 7) :: 'Money.ExchangeRate' \"USD\" \"JPY\"@
--
-- @
-- [\"USD\", \"JPY\", 5, 7]
-- @
--
-- Note: The JSON serialization changed in version 0.4 (the leading
-- @"ExchangeRate"@ string was dropped from the rendered 'Ae.Array').
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ae.ToJSON (Money.ExchangeRate src dst) where
  toJSON = Ae.toJSON . Money.toSomeExchangeRate

-- | Compatible with 'Money.SomeExchangeRate'
--
-- Note: The JSON serialization changed in version 0.4. However, this instance
-- is still able to cope with the previous format.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ae.FromJSON (Money.ExchangeRate src dst) where
  parseJSON =
    maybe empty pure <=< fmap Money.fromSomeExchangeRate . Ae.parseJSON

-- | Compatible with 'Money.ExchangeRate'
--
-- Note: The JSON serialization changed in @safe-money@ version 0.4 (the leading
-- @"ExchangeRate"@ string was dropped from the rendered 'Ae.Array').
instance Ae.ToJSON Money.SomeExchangeRate where
  toJSON = \ser ->
    let r = Money.someExchangeRateRate ser
    in Ae.toJSON (MoneyI.someExchangeRateSrcCurrency' ser,
                  MoneyI.someExchangeRateDstCurrency' ser,
                  numerator r, denominator r)

-- | Compatible with 'Money.ExchangeRate'
--
-- Note: The JSON serialization changed in @safe-money@ version 0.4. However,
-- this instance is still able to cope with the previous format.
instance Ae.FromJSON Money.SomeExchangeRate where
  parseJSON = \v -> do
    (src, dst, n, d) <- Ae.parseJSON v <|> do
       -- Pre 0.4 format.
       ("ExchangeRate" :: T.Text, src, dst, n, d) <- Ae.parseJSON v
       pure (src, dst, n, d)
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (MoneyI.mkSomeExchangeRate' src dst (n % d))

