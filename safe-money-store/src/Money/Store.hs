{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module only exports orphan 'Store.Store' instances. Import as:
--
-- @
-- import "Money.Store" ()
-- @
module Money.Store () where

import Control.Monad (when)
import Data.Ratio ((%), numerator, denominator)
import GHC.TypeLits (KnownSymbol)
import qualified Data.Store as Store
import qualified Money
import qualified Money.Internal as MoneyI

--------------------------------------------------------------------------------

-- | Compatible with 'Money.SomeDense'.
instance (KnownSymbol currency) => Store.Store (Money.Dense currency) where
  size = storeContramapSize Money.toSomeDense Store.size
  poke = Store.poke . Money.toSomeDense
  peek = maybe (fail "peek") pure =<< fmap Money.fromSomeDense Store.peek

-- | Compatible with 'Money.Dense'.
instance Store.Store Money.SomeDense where
  poke = \sd -> do
    Store.poke (MoneyI.someDenseCurrency' sd)
    let r = Money.someDenseAmount sd
    Store.poke (numerator r)
    Store.poke (denominator r)
  peek = maybe (fail "peek") pure =<< do
    c :: String <- Store.peek
    n :: Integer <- Store.peek
    d :: Integer <- Store.peek
    when (d == 0) (fail "denominator is zero")
    pure (MoneyI.mkSomeDense' c (n % d))

-- | Compatible with 'Money.SomeDiscrete'.
instance
  ( KnownSymbol currency, Money.GoodScale scale
  ) => Store.Store (Money.Discrete' currency scale) where
  size = storeContramapSize Money.toSomeDiscrete Store.size
  poke = Store.poke . Money.toSomeDiscrete
  peek = maybe (fail "peek") pure =<< fmap Money.fromSomeDiscrete Store.peek

instance Store.Store Money.Scale where
  poke = \s -> do
    let r = Money.scaleToRational s
    Store.poke (numerator r)
    Store.poke (denominator r)
  peek = maybe (fail "peek") pure =<< do
    n :: Integer <- Store.peek
    d :: Integer <- Store.peek
    when (d == 0) (fail "denominator is zero")
    pure (Money.scaleFromRational (n % d))


-- | Compatible with 'Money.Discrete''.
instance Store.Store Money.SomeDiscrete where
  poke = \sd -> do
    Store.poke (MoneyI.someDiscreteCurrency' sd)
    Store.poke (Money.someDiscreteScale sd)
    Store.poke (Money.someDiscreteAmount sd)
  peek = do
    -- We go through String for backwards compatibility.
    c :: String <- Store.peek
    s :: Money.Scale <- Store.peek
    a :: Integer <- Store.peek
    pure (MoneyI.mkSomeDiscrete' c s a)

-- | Compatible with 'Money.SomeExchangeRate'.
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Store.Store (Money.ExchangeRate src dst) where
  size = storeContramapSize Money.toSomeExchangeRate Store.size
  poke = Store.poke . Money.toSomeExchangeRate
  peek = maybe (fail "peek") pure =<< fmap Money.fromSomeExchangeRate Store.peek

-- | Compatible with 'ExchangeRate'.
instance Store.Store Money.SomeExchangeRate where
  poke = \ser -> do
    Store.poke (MoneyI.someExchangeRateSrcCurrency' ser)
    Store.poke (MoneyI.someExchangeRateDstCurrency' ser)
    let r = Money.someExchangeRateRate ser
    Store.poke (numerator r)
    Store.poke (denominator r)
  peek = maybe (fail "peek") pure =<< do
    src :: String <- Store.peek
    dst :: String <- Store.peek
    n :: Integer <- Store.peek
    d :: Integer <- Store.peek
    when (d == 0) (fail "denominator is zero")
    pure (MoneyI.mkSomeExchangeRate' src dst (n % d))

storeContramapSize :: (a -> b) -> Store.Size b -> Store.Size a
storeContramapSize f = \case
  Store.VarSize g -> Store.VarSize (g . f)
  Store.ConstSize x -> Store.ConstSize x
{-# INLINABLE storeContramapSize #-}

