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

instance QC.Arbitrary (Money.Discrete currency unit) where
  arbitrary = fmap fromInteger QC.arbitrary
  shrink = fmap fromInteger . QC.shrink . toInteger

instance QC.Arbitrary (Money.Continuous currency) where
  arbitrary = maybe QC.arbitrary pure . Money.continuous =<< QC.arbitrary
  shrink = catMaybes . fmap Money.continuous . QC.shrink . toRational

--------------------------------------------------------------------------------

main :: IO ()
main =  Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "root"
  [ testCurrency     (Proxy :: Proxy "USD")
  , testCurrencyUnit (Proxy :: Proxy "USD") (Proxy :: Proxy "USD")
  , testCurrencyUnit (Proxy :: Proxy "USD") (Proxy :: Proxy "cent")
  , testCurrencyUnit (Proxy :: Proxy "USD") (Proxy :: Proxy "dollar")
  , testCurrency     (Proxy :: Proxy "BTC")
  , testCurrencyUnit (Proxy :: Proxy "BTC") (Proxy :: Proxy "BTC")
  , testCurrencyUnit (Proxy :: Proxy "BTC") (Proxy :: Proxy "satoshi")
  , testCurrencyUnit (Proxy :: Proxy "BTC") (Proxy :: Proxy "bitcoin")
  , testCurrency     (Proxy :: Proxy "XAU")
  , testCurrencyUnit (Proxy :: Proxy "XAU") (Proxy :: Proxy "micrograin")
  , testCurrencyUnit (Proxy :: Proxy "XAU") (Proxy :: Proxy "milligrain")
  , testCurrencyUnit (Proxy :: Proxy "XAU") (Proxy :: Proxy "grain")
  ]

testCurrency
  :: KnownSymbol currency
  => Proxy currency
  -> Tasty.TestTree
testCurrency pc =
  Tasty.testGroup ("Currency " ++ symbolVal pc)
  [ testShowReadContinuous pc
  ]

testCurrencyUnit
  :: forall (currency :: Symbol) (unit :: Symbol)
  . ( KnownSymbol currency
    , KnownSymbol unit
    , KnownNat (Money.Scale' currency unit)
    , CmpNat 0 (Money.Scale' currency unit) ~ 'LT )
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testCurrencyUnit pc pu =
  Tasty.testGroup ("Currency unit " ++ symbolVal pc ++ " " ++ symbolVal pu)
  [ testShowReadDiscrete pc pu
  , testRounding pc pu
  ]

testShowReadContinuous
  :: forall (currency :: Symbol)
  .  KnownSymbol currency
  => Proxy currency
  -> Tasty.TestTree
testShowReadContinuous _ =
  Tasty.testGroup "read . show == id"
  [ QC.testProperty "Continuous" $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x == read (show x)
  ]

testShowReadDiscrete
  :: forall (currency :: Symbol) (unit :: Symbol)
  . ( KnownSymbol currency
    , KnownNat (Money.Scale' currency unit)
    , CmpNat 0 (Money.Scale' currency unit) ~ 'LT )
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testShowReadDiscrete _ _ =
  Tasty.testGroup "read . show == id"
  [ QC.testProperty "Discrete" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         x == read (show x)
  ]

testRounding
  :: forall (currency :: Symbol) (unit :: Symbol)
  . ( KnownSymbol currency
    , KnownNat (Money.Scale' currency unit)
    , CmpNat 0 (Money.Scale' currency unit) ~ 'LT )
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testRounding _ _ =
    Tasty.testGroup "Rounding"
    [ QC.testProperty "floor"    $ QC.forAll QC.arbitrary (g Money.floor)
    , QC.testProperty "ceiling"  $ QC.forAll QC.arbitrary (g Money.ceiling)
    , QC.testProperty "round"    $ QC.forAll QC.arbitrary (g Money.round)
    , QC.testProperty "truncate" $ QC.forAll QC.arbitrary (g Money.truncate)
    ]
  where
    g f = \(x :: Money.Continuous currency) -> x === case f x of
      (y, Nothing) -> Money.fromDiscrete (y :: Money.Discrete currency unit)
      (y, Just z)  -> Money.fromDiscrete y + z
