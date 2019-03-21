{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module only exports orphan 'Cereal.Serialize' instances. Import as:
--
-- @
-- import "Money.Cereal" ()
-- @
module Money.Cereal () where

import Control.Applicative (empty)
import Control.Monad (when)
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Serialize as Cereal
import GHC.TypeLits (KnownSymbol)
import qualified Money
import qualified Money.Internal as MoneyI

--------------------------------------------------------------------------------

-- | Compatible with 'Money.SomeDense'.
instance (KnownSymbol currency) => Cereal.Serialize (Money.Dense currency) where
  put = Cereal.put . Money.toSomeDense
  get = maybe empty pure =<< fmap Money.fromSomeDense Cereal.get

-- | Compatible with 'Money.SomeDiscrete'.
instance
  ( KnownSymbol currency, Money.GoodScale scale
  ) => Cereal.Serialize (Money.Discrete' currency scale) where
  put = Cereal.put . Money.toSomeDiscrete
  get = maybe empty pure =<< fmap Money.fromSomeDiscrete Cereal.get

-- | Compatible with 'Money.SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Cereal.Serialize (Money.ExchangeRate src dst) where
  put = Cereal.put . Money.toSomeExchangeRate
  get = maybe empty pure =<< fmap Money.fromSomeExchangeRate Cereal.get

-- | Compatible with 'Money.Dense'.
instance Cereal.Serialize Money.SomeDense where
  put = \sd -> do
    Cereal.put (MoneyI.someDenseCurrency' sd)
    let r = Money.someDenseAmount sd
    Cereal.put (numerator r)
    Cereal.put (denominator r)
  get = maybe empty pure =<< do
    c :: String <- Cereal.get
    n :: Integer <- Cereal.get
    d :: Integer <- Cereal.get
    when (d == 0) (fail "denominator is zero")
    pure (MoneyI.mkSomeDense' c (n % d))

instance Cereal.Serialize Money.Scale where
  put = \s -> do
    let r = Money.scaleToRational s
    Cereal.put (numerator r)
    Cereal.put (denominator r)
  get = maybe empty pure =<< do
    -- We go through String for backwards compatibility.
    n :: Integer <- Cereal.get
    d :: Integer <- Cereal.get
    when (d == 0) (fail "denominator is zero")
    pure (Money.scaleFromRational (n % d))

-- | Compatible with 'Money.Discrete'.
instance Cereal.Serialize Money.SomeDiscrete where
  put = \sd -> do
    -- We go through String for backwards compatibility.
    Cereal.put (MoneyI.someDiscreteCurrency' sd)
    Cereal.put (Money.someDiscreteScale sd)
    Cereal.put (Money.someDiscreteAmount sd)
  get = do
    -- We go through String for backwards compatibility.
    c :: String <- Cereal.get
    s :: Money.Scale <- Cereal.get
    a :: Integer <- Cereal.get
    pure (MoneyI.mkSomeDiscrete' c s a)

-- | Compatible with 'Money.ExchangeRate'.
instance Cereal.Serialize Money.SomeExchangeRate where
  put = \ser -> do
    -- We go through String for backwards compatibility.
    Cereal.put (MoneyI.someExchangeRateSrcCurrency' ser)
    Cereal.put (MoneyI.someExchangeRateDstCurrency' ser)
    let r = Money.someExchangeRateRate ser
    Cereal.put (numerator r)
    Cereal.put (denominator r)
  get = maybe empty pure =<< do
    -- We go through String for backwards compatibility.
    src :: String <- Cereal.get
    dst :: String <- Cereal.get
    n :: Integer <- Cereal.get
    d :: Integer <- Cereal.get
    when (d == 0) (fail "denominator is zero")
    pure (MoneyI.mkSomeExchangeRate' src dst (n % d))

