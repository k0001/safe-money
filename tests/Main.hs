{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC

import GHC.TypeLits (CmpNat, KnownNat, Symbol, KnownSymbol, symbolVal)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific, scientific)

import qualified Data.Money as Money

--------------------------------------------------------------------------------

instance QC.Arbitrary (Money.Discrete currency) where
  arbitrary = fmap fromInteger QC.arbitrary
  shrink = fmap fromInteger . QC.shrink . toInteger

instance
  ( KnownNat (Money.Scale currency)
  , CmpNat 0 (Money.Scale currency) ~ 'LT
  ) => QC.Arbitrary (Money.Continuous currency) where
  arbitrary = maybe QC.arbitrary pure . Money.continuous =<< QC.arbitrary
  shrink = catMaybes . fmap Money.continuous . QC.shrink . toRational

--------------------------------------------------------------------------------

main :: IO ()
main =  Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tests


tests :: Tasty.TestTree
tests = Tasty.testGroup "root"
  [ testCurrency (Proxy :: Proxy "USD")
  , testCurrency (Proxy :: Proxy "USD/cent")
  , testCurrency (Proxy :: Proxy "USD/dollar")
  , testCurrency (Proxy :: Proxy "BTC")
  , testCurrency (Proxy :: Proxy "BTC/satoshi")
  , testCurrency (Proxy :: Proxy "BTC/bitcoin")
  , testCurrency (Proxy :: Proxy "BTC/bitcoin")
  , testCurrency (Proxy :: Proxy "XAU")
  , testCurrency (Proxy :: Proxy "XAU/micrograin")
  , testCurrency (Proxy :: Proxy "XAU/milligrain")
  , testCurrency (Proxy :: Proxy "XAU/grain")
  ]

testCurrency
  :: forall (currency :: Symbol)
  . ( KnownSymbol currency
    , KnownNat (Money.Scale currency)
    , CmpNat 0 (Money.Scale currency) ~ 'LT )
  => Proxy currency
  -> Tasty.TestTree
testCurrency pc = Tasty.testGroup ("Currency " ++ symbolVal pc)
  [ testShowRead pc
  , testRounding pc
  ]

testShowRead
  :: forall (currency :: Symbol)
  . ( KnownSymbol currency
    , KnownNat (Money.Scale currency)
    , CmpNat 0 (Money.Scale currency) ~ 'LT )
  => Proxy currency
  -> Tasty.TestTree
testShowRead _ = Tasty.testGroup "read . show == id"
  [ QC.testProperty "Continuous" $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x == read (show x)
  , QC.testProperty "Discrete" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency) ->
         x == read (show x)
  ]

testRounding
  :: forall (currency :: Symbol)
  . ( KnownSymbol currency
    , KnownNat (Money.Scale currency)
    , CmpNat 0 (Money.Scale currency) ~ 'LT )
  => Proxy currency
  -> Tasty.TestTree
testRounding _ = Tasty.testGroup "Rounding"
  [ QC.testProperty "floor" $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.floor x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  , QC.testProperty "ceiling" $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.ceiling x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  , QC.testProperty "round" $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.round x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  , QC.testProperty "truncate" $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.truncate x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  ]
