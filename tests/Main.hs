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
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Proxy (Proxy(Proxy))

import qualified Data.Money as Money
import qualified Data.Money.Internal as Money

--------------------------------------------------------------------------------

instance QC.Arbitrary (Money.Discrete' currency scale) where
  arbitrary = fmap fromInteger QC.arbitrary
  shrink = fmap fromInteger . QC.shrink . toInteger

instance QC.Arbitrary Money.DiscreteRep where
  arbitrary = do
    let md = Money.mkDiscreteRep <$> QC.arbitrary <*> QC.arbitrary
                                 <*> QC.arbitrary <*> QC.arbitrary
    Just x <- QC.suchThat md isJust
    pure x
  shrink = \x -> Money.withDiscreteRep x (map Money.toDiscreteRep . QC.shrink)

instance QC.Arbitrary (Money.Dense currency) where
  arbitrary = do
    Just x <- QC.suchThat (Money.dense <$> QC.arbitrary) isJust
    pure x
  shrink = catMaybes . fmap Money.dense . QC.shrink . toRational

instance QC.Arbitrary Money.DenseRep where
  arbitrary = do
    let md = Money.mkDenseRep <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    Just x <- QC.suchThat md isJust
    pure x
  shrink = \x -> Money.withDenseRep x (map Money.toDenseRep . QC.shrink)

instance QC.Arbitrary (Money.ExchangeRate src dst) where
  arbitrary = do
    Just x <- QC.suchThat (fmap Money.exchangeRate QC.arbitrary) isJust
    pure x
  shrink =
    catMaybes . fmap Money.exchangeRate . QC.shrink . Money.fromExchangeRate

instance QC.Arbitrary Money.ExchangeRateRep where
  arbitrary = do
    let md = Money.mkExchangeRateRep <$> QC.arbitrary <*> QC.arbitrary
                                     <*> QC.arbitrary <*> QC.arbitrary
    Just x <- QC.suchThat md isJust
    pure x
  shrink = \x ->
    Money.withExchangeRateRep x (map Money.toExchangeRateRep . QC.shrink)

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
  [ testDense (Proxy :: Proxy "BTC")  -- A cryptocurrency.
  , testDense (Proxy :: Proxy "USD")  -- A fiat currency with decimal fractions.
  , testDense (Proxy :: Proxy "VUV")  -- A fiat currency with non-decimal fractions.
  , testDense (Proxy :: Proxy "XAU")  -- A precious metal.
  ]

testCurrencyUnits :: Tasty.TestTree
testCurrencyUnits =
  Tasty.testGroup "Currency units"
  [ testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "BTC")
  , testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "satoshi")
  , testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "bitcoin")
  , testDiscrete (Proxy :: Proxy "USD") (Proxy :: Proxy "USD")
  , testDiscrete (Proxy :: Proxy "USD") (Proxy :: Proxy "cent")
  , testDiscrete (Proxy :: Proxy "USD") (Proxy :: Proxy "dollar")
  , testDiscrete (Proxy :: Proxy "VUV") (Proxy :: Proxy "vatu")
  , testDiscrete (Proxy :: Proxy "XAU") (Proxy :: Proxy "micrograin")
  , testDiscrete (Proxy :: Proxy "XAU") (Proxy :: Proxy "milligrain")
  , testDiscrete (Proxy :: Proxy "XAU") (Proxy :: Proxy "grain")
  ]

testDense
  :: forall currency
  .  KnownSymbol currency
  => Proxy currency
  -> Tasty.TestTree
testDense pc =
  Tasty.testGroup ("DenseRep " ++ show (symbolVal pc))
  [ QC.testProperty "read . show == id" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         x === read (show x)
  , QC.testProperty "fromDenseRep . toDenseRep == Just" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just x === Money.fromDenseRep (Money.toDenseRep x)
  , QC.testProperty "fromDenseRep works only for same currency" $
      QC.forAll QC.arbitrary $ \(dr :: Money.DenseRep) ->
        (Money.denseRepCurrency dr /= symbolVal pc)
           ==> isNothing (Money.fromDenseRep dr :: Maybe (Money.Dense currency))
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

testDiscrete
  :: forall (currency :: Symbol) (unit :: Symbol)
  .  ( Money.GoodScale (Money.Scale currency unit)
     , KnownSymbol currency
     , KnownSymbol unit )
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testDiscrete pc pu =
  Tasty.testGroup ("Discrete " ++ show (symbolVal pc) ++ " "
                               ++ show (symbolVal pu))
  [ testRounding pc pu
  , QC.testProperty "read . show == id" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         x === read (show x)
  , QC.testProperty "fromDiscreteRep . toDiscreteRep == Just" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just x === Money.fromDiscreteRep (Money.toDiscreteRep x)
  , QC.testProperty "fromDiscreteRep works only for same currency and scale" $
      QC.forAll QC.arbitrary $ \(dr :: Money.DiscreteRep) ->
        ((Money.discreteRepCurrency dr /= symbolVal pc) &&
         (Money.discreteRepScale dr /=
             Money.scale (Proxy :: Proxy (Money.Scale currency unit)))
        ) ==> isNothing (Money.fromDiscreteRep dr
                          :: Maybe (Money.Discrete currency unit))
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
  [ QC.testProperty "read . show == id" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         xr === read (show xr)
  , QC.testProperty "flipExchangeRate . flipExchangeRate == id" $
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
  , QC.testProperty "fromExchangeRateRep . toExchangeRateRep == Just" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just x === Money.fromExchangeRateRep (Money.toExchangeRateRep x)
  , QC.testProperty "fromDiscreteRep works only for same currencies" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRateRep) ->
        ((Money.exchangeRateRepSrcCurrency x /= symbolVal ps) &&
         (Money.exchangeRateRepDstCurrency x /= symbolVal pd))
            ==> isNothing (Money.fromExchangeRateRep x
                            :: Maybe (Money.ExchangeRate src dst))
  ]

testRounding
  :: forall (currency :: Symbol) (unit :: Symbol)
  .  (Money.GoodScale (Money.Scale currency unit), KnownSymbol currency)
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
