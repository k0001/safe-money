{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===), (==>))
import qualified Test.Tasty.QuickCheck as QC

import GHC.TypeLits (Nat, Symbol, KnownSymbol, symbolVal)
import Data.Maybe (catMaybes, isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific, scientific)

import qualified Data.Money as Money

--------------------------------------------------------------------------------

instance QC.Arbitrary (Money.Discrete currency unit) where
  arbitrary = fmap fromInteger QC.arbitrary
  shrink = fmap fromInteger . QC.shrink . toInteger

instance QC.Arbitrary (Money.Dense currency) where
  arbitrary = do
    Just x <- QC.suchThat (fmap Money.dense QC.arbitrary) isJust
    pure x
  shrink = catMaybes . fmap Money.dense . QC.shrink . toRational

instance QC.Arbitrary (Money.ExchangeRate src dst) where
  arbitrary = do
    Just x <- QC.suchThat (fmap Money.exchangeRate QC.arbitrary) isJust
    pure x
  shrink =
    catMaybes . fmap Money.exchangeRate . QC.shrink . Money.fromExchangeRate

--------------------------------------------------------------------------------

main :: IO ()
main =  Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "root"
  [ testCurrencies
  , testCurrencyUnits
  , testExchange
  ]

testCurrencies :: Tasty.TestTree
testCurrencies =
  Tasty.testGroup "Currency"
  [ testCurrency (Proxy :: Proxy "BTC")
  , testCurrency (Proxy :: Proxy "USD")
  , testCurrency (Proxy :: Proxy "VUV")
  , testCurrency (Proxy :: Proxy "XAU")
  ]

testCurrencyUnits :: Tasty.TestTree
testCurrencyUnits =
  Tasty.testGroup "Currency units"
  [ testCurrencyUnit (Proxy :: Proxy "BTC") (Proxy :: Proxy "BTC")
  , testCurrencyUnit (Proxy :: Proxy "BTC") (Proxy :: Proxy "satoshi")
  , testCurrencyUnit (Proxy :: Proxy "BTC") (Proxy :: Proxy "bitcoin")
  , testCurrencyUnit (Proxy :: Proxy "USD") (Proxy :: Proxy "USD")
  , testCurrencyUnit (Proxy :: Proxy "USD") (Proxy :: Proxy "cent")
  , testCurrencyUnit (Proxy :: Proxy "USD") (Proxy :: Proxy "dollar")
  , testCurrencyUnit (Proxy :: Proxy "VUV") (Proxy :: Proxy "vatu")
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
  [ testShowReadDense pc
  ]

testExchange :: Tasty.TestTree
testExchange =
  Tasty.testGroup "Exchange"
  [ testExchangeRate (Proxy :: Proxy "BTC") (Proxy :: Proxy "BTC")
  , testExchangeRate (Proxy :: Proxy "BTC") (Proxy :: Proxy "USD")
  , testExchangeRate (Proxy :: Proxy "BTC") (Proxy :: Proxy "VUV")
  , testExchangeRate (Proxy :: Proxy "BTC") (Proxy :: Proxy "XAU")
  , testExchangeRate (Proxy :: Proxy "USD") (Proxy :: Proxy "BTC")
  , testExchangeRate (Proxy :: Proxy "USD") (Proxy :: Proxy "USD")
  , testExchangeRate (Proxy :: Proxy "USD") (Proxy :: Proxy "VUV")
  , testExchangeRate (Proxy :: Proxy "USD") (Proxy :: Proxy "XAU")
  , testExchangeRate (Proxy :: Proxy "VUV") (Proxy :: Proxy "BTC")
  , testExchangeRate (Proxy :: Proxy "VUV") (Proxy :: Proxy "USD")
  , testExchangeRate (Proxy :: Proxy "VUV") (Proxy :: Proxy "VUV")
  , testExchangeRate (Proxy :: Proxy "VUV") (Proxy :: Proxy "XAU")
  , testExchangeRate (Proxy :: Proxy "XAU") (Proxy :: Proxy "BTC")
  , testExchangeRate (Proxy :: Proxy "XAU") (Proxy :: Proxy "USD")
  , testExchangeRate (Proxy :: Proxy "XAU") (Proxy :: Proxy "VUV")
  , testExchangeRate (Proxy :: Proxy "XAU") (Proxy :: Proxy "XAU")
  ]

testCurrencyUnit
  :: forall (currency :: Symbol) (unit :: Symbol)
  .  (Money.GoodScale currency unit, KnownSymbol currency, KnownSymbol unit)
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testCurrencyUnit pc pu =
  Tasty.testGroup ("Currency unit " ++ show (symbolVal pc) ++ " "
                                    ++ show (symbolVal pu))
  [ testShowReadDiscrete pc pu
  , testRounding pc pu
  ]

testShowReadDense
  :: forall (currency :: Symbol)
  .  KnownSymbol currency
  => Proxy currency
  -> Tasty.TestTree
testShowReadDense _ =
  Tasty.testGroup "Dense"
  [ QC.testProperty "read . show == id" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         x === read (show x)
  ]

testShowReadDiscrete
  :: forall (currency :: Symbol) (unit :: Symbol)
  .  Money.GoodScale currency unit
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testShowReadDiscrete _ _ =
  Tasty.testGroup "Discrete"
  [ QC.testProperty "read . show == id" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         x === read (show x)
  , QC.testProperty "coerceUnit == id" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         x == Money.coerceUnit x
  ]

testExchangeRate
  :: forall (src :: Symbol) (dst :: Symbol)
  .  (KnownSymbol src, KnownSymbol dst)
  => Proxy src
  -> Proxy dst
  -> Tasty.TestTree
testExchangeRate ps pd =
  Tasty.testGroup ("ExchangeRate " ++ show (symbolVal ps) ++ " "
                                   ++ show (symbolVal pd))
  -- TODO: these diverge.
  [ QC.testProperty "flipExchangeRate . flipExchangeRate == id" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         let xr' = Money.flipExchangeRate xr
         in (Money.fromExchangeRate xr /= Money.fromExchangeRate xr')
               ==> (xr === Money.flipExchangeRate xr')
  , QC.testProperty "exchange (flipExchangeRate x) . exchange x == id" $
      QC.forAll QC.arbitrary $
         \( c0 :: Money.Dense src
          , xr :: Money.ExchangeRate src dst
          ) -> c0 === Money.exchange (Money.flipExchangeRate xr)
                                     (Money.exchange xr c0)
  , QC.testProperty "x == 1 ===> exchange x == id" $
      QC.forAll QC.arbitrary $
         \( c0 :: Money.Dense src
          ) -> let Just xr = Money.exchangeRate 1
               in toRational c0 === toRational (Money.exchange xr c0)
  , QC.testProperty "x /= 1 ===> exchange x /= id" $
      QC.forAll QC.arbitrary $
         \( c0 :: Money.Dense src
          , xr :: Money.ExchangeRate src dst
          ) -> (Money.fromExchangeRate xr /= 1)
                  ==> (toRational c0 /= toRational (Money.exchange xr c0))
  ]

testRounding
  :: forall (currency :: Symbol) (unit :: Symbol)
  .  Money.GoodScale currency unit
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
    g f = \(x :: Money.Dense currency) -> x === case f x of
      (y, Nothing) -> Money.fromDiscrete (y :: Money.Discrete currency unit)
      (y, Just z)  -> Money.fromDiscrete y + z
