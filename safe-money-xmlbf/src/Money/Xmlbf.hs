{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module only exports orphan 'Ae.FromXml' and 'Ae.ToXml' instances.
-- Import as:
--
-- @
-- import "Money.Xmlbf" ()
-- @
module Money.Xmlbf () where

import Control.Applicative (empty)
import Control.Monad (when)
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Text as T
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol)
import qualified Money
import qualified Xmlbf
import qualified Data.Text.Read as TR

pRead :: Integral a => TR.Reader a -> T.Text -> Xmlbf.Parser a
pRead parser txt = case parser txt of
  Left err -> fail err
  Right ( n, "" ) -> return n
  Right _ -> fail "Parser did not match fully."

--------------------------------------------------------------------------------

-- | Compatible with 'Money.SomeDense'
--
-- Example rendering @'Money.dense' (2 '%' 3) :: 'Money.Dense' \"BTC\"@:
--
-- @
-- \<money-dense c=\"BTC\" n=\"2\" d=\"3\"/>
-- @
instance KnownSymbol currency => Xmlbf.ToXml (Money.Dense currency) where
  toXml = Xmlbf.toXml . Money.toSomeDense

-- | Compatible with 'Money.SomeDense'
instance KnownSymbol currency => Xmlbf.FromXml (Money.Dense currency) where
  fromXml = maybe empty pure =<< fmap Money.fromSomeDense Xmlbf.fromXml

-- | Compatible with 'Money.Dense'
instance Xmlbf.ToXml Money.SomeDense where
  toXml = \sd ->
    let r = Money.someDenseAmount sd
        as = [ (T.pack "c", Money.someDenseCurrency sd)
             , (T.pack "n", T.pack (show (numerator r)))
             , (T.pack "d", T.pack (show (denominator r))) ]
    in [ either error id $ Xmlbf.element' (T.pack "money-dense") (fromList as) [] ]

-- | Compatible with 'Money.Dense'.
instance Xmlbf.FromXml Money.SomeDense where
  fromXml = Xmlbf.pElement (T.pack "money-dense") $ do
    c <- Xmlbf.pAttr "c"
    n <- pRead (TR.signed TR.decimal) =<< Xmlbf.pAttr "n"
    d <- pRead TR.decimal =<< Xmlbf.pAttr "d"
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (Money.mkSomeDense c (n % d))

-- | Compatible with 'Money.SomeDiscrete'
--
-- Example rendering @'Money.discrete' 43 :: 'Money.Discrete' \"BTC\" \"satoshi\"@:
--
-- @
-- \<money-discrete c=\"BTC\" n=\"100000000\" d=\"1\" a=\"43\"/>
-- @
instance
  ( KnownSymbol currency, Money.GoodScale scale
  ) => Xmlbf.ToXml (Money.Discrete' currency scale) where
  toXml = Xmlbf.toXml . Money.toSomeDiscrete

-- | Compatible with 'Money.SomeDiscrete'
instance
  ( KnownSymbol currency, Money.GoodScale scale
  ) => Xmlbf.FromXml (Money.Discrete' currency scale) where
  fromXml = maybe empty pure =<< fmap Money.fromSomeDiscrete Xmlbf.fromXml

-- | Compatible with 'Money.Discrete''
instance Xmlbf.ToXml Money.SomeDiscrete where
  toXml = \sd ->
    let r = Money.scaleToRational (Money.someDiscreteScale sd)
        as = [ ("c", Money.someDiscreteCurrency sd)
             , ("n", T.pack (show (numerator r)))
             , ("d", T.pack (show (denominator r)))
             , ("a", T.pack (show (Money.someDiscreteAmount sd))) ]
    in [ either error id $ Xmlbf.element' (T.pack "money-discrete") (fromList as) [] ]

-- | Compatible with 'Money.Discrete''
instance Xmlbf.FromXml Money.SomeDiscrete where
  fromXml = Xmlbf.pElement (T.pack "money-discrete") $ do
    c <- Xmlbf.pAttr "c"
    n <- pRead TR.decimal =<< Xmlbf.pAttr "n"
    d <- pRead TR.decimal =<< Xmlbf.pAttr "d"
    when (d == 0) (fail "denominator is zero")
    a <- pRead (TR.signed TR.decimal) =<< Xmlbf.pAttr "a"
    maybe empty pure (Money.mkSomeDiscrete c <$> Money.scaleFromRational (n % d)
                                             <*> pure a)

-- | Compatible with 'Money..SomeExchangeRate'
--
-- Example rendering an 'Money.ExchangeRate' constructed with
-- @'Money.exchangeRate' (5 '%' 7) :: 'Money.ExchangeRate' \"USD\" \"JPY\"@
--
-- @
-- \<exchange-rate src=\"USD\" dst=\"JPY\" n=\"5\" d=\"7\"/>
-- @
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Xmlbf.ToXml (Money.ExchangeRate src dst) where
  toXml = Xmlbf.toXml . Money.toSomeExchangeRate

-- | Compatible with 'Money.SomeExchangeRate'
instance
  ( KnownSymbol src, KnownSymbol dst
  ) => Xmlbf.FromXml (Money.ExchangeRate src dst) where
  fromXml = maybe empty pure =<< fmap Money.fromSomeExchangeRate Xmlbf.fromXml

-- | Compatible with 'Money.ExchangeRate'
instance Xmlbf.ToXml Money.SomeExchangeRate where
  toXml = \ser ->
    let r = Money.someExchangeRateRate ser
        as = [ ("src", Money.someExchangeRateSrcCurrency ser)
             , ("dst", Money.someExchangeRateDstCurrency ser)
             , ("n", T.pack (show (numerator r)))
             , ("d", T.pack (show (denominator r))) ]
    in [ either error id $ Xmlbf.element' (T.pack "exchange-rate") (fromList as) [] ]

-- | Compatible with 'Money.ExchangeRate'
instance Xmlbf.FromXml Money.SomeExchangeRate where
  fromXml = Xmlbf.pElement (T.pack "exchange-rate") $ do
    src <- Xmlbf.pAttr "src"
    dst <- Xmlbf.pAttr "dst"
    n <- pRead TR.decimal =<< Xmlbf.pAttr "n"
    d <- pRead TR.decimal =<< Xmlbf.pAttr "d"
    when (d == 0) (fail "denominator is zero")
    maybe empty pure (Money.mkSomeExchangeRate src dst (n % d))

