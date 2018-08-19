{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module only exports orphan 'Ser.Serialise' instances. Import as:
--
-- @
-- import "Money.Serialise" ()
-- @
module Money.Serialise () where

import qualified Codec.Serialise as Ser
import Control.Monad (when)
import Data.Ratio ((%), numerator, denominator)
import GHC.TypeLits (KnownSymbol)
import qualified Money
import qualified Money.Internal as MoneyI

--------------------------------------------------------------------------------
-- | Compatible with 'Money.SomeDense'.
instance (KnownSymbol currency) => Ser.Serialise (Money.Dense currency) where
  encode = Ser.encode . Money.toSomeDense
  decode = maybe (fail "Dense") pure =<< fmap Money.fromSomeDense Ser.decode

-- | Compatible with 'Money.SomeDiscrete'.
instance
  ( KnownSymbol currency, Money.GoodScale scale
  ) => Ser.Serialise (Money.Discrete' currency scale) where
  encode = Ser.encode . Money.toSomeDiscrete
  decode = maybe (fail "Discrete'") pure
             =<< fmap Money.fromSomeDiscrete Ser.decode

-- | Compatible with 'Money.SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Ser.Serialise (Money.ExchangeRate src dst) where
  encode = Ser.encode . Money.toSomeExchangeRate
  decode = maybe (fail "ExchangeRate") pure
             =<< fmap Money.fromSomeExchangeRate Ser.decode

-- | Compatible with 'Money.Dense'.
instance Ser.Serialise Money.SomeDense where
  encode = \sd ->
    let r = Money.someDenseAmount sd
    in Ser.encode (MoneyI.someDenseCurrency' sd) <>
       Ser.encode (numerator r) <>
       Ser.encode (denominator r)
  decode = maybe (fail "SomeDense") pure =<< do
    c :: String <- Ser.decode
    n :: Integer <- Ser.decode
    d :: Integer <- Ser.decode
    when (d == 0) (fail "denominator is zero")
    pure (MoneyI.mkSomeDense' c (n % d))

-- | Compatible with 'Money.Discrete'.
instance Ser.Serialise Money.SomeDiscrete where
  encode = \sd ->
    let r = Money.someDiscreteScale sd
    in Ser.encode (MoneyI.someDiscreteCurrency' sd) <>
       Ser.encode (numerator r) <>
       Ser.encode (denominator r) <>
       Ser.encode (Money.someDiscreteAmount sd)
  decode = maybe (fail "SomeDiscrete") pure =<< do
    c :: String <- Ser.decode
    n :: Integer <- Ser.decode
    d :: Integer <- Ser.decode
    when (d == 0) (fail "denominator is zero")
    a :: Integer <- Ser.decode
    pure (MoneyI.mkSomeDiscrete' c (n % d) a)

-- | Compatible with 'Money.ExchangeRate'.
instance Ser.Serialise Money.SomeExchangeRate where
  encode = \ser ->
    let r = Money.someExchangeRateRate ser
    in Ser.encode (MoneyI.someExchangeRateSrcCurrency' ser) <>
       Ser.encode (MoneyI.someExchangeRateDstCurrency' ser) <>
       Ser.encode (numerator r) <>
       Ser.encode (denominator r)
  decode = maybe (fail "SomeExchangeRate") pure =<< do
    src :: String <- Ser.decode
    dst :: String <- Ser.decode
    n :: Integer <- Ser.decode
    d :: Integer <- Ser.decode
    when (d == 0) (fail "denominator is zero")
    pure (MoneyI.mkSomeExchangeRate' src dst (n % d))

