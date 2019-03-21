{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Codec.Serialise as Ser
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Proxy (Proxy(Proxy))
import Data.Ratio ((%), numerator, denominator)
import GHC.TypeLits (Nat, Symbol, KnownSymbol, symbolVal)
import qualified Money
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=), (@=?))
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===), (==>), (.&&.))
import qualified Test.Tasty.QuickCheck as QC

import Money.Serialise()

--------------------------------------------------------------------------------

main :: IO ()
main =  Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] (Tasty.localOption (QC.QuickCheckTests 100) tests)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "root"
  [ testCurrencies
  , testCurrencyUnits
  , testExchange
  , testRawSerializations
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
  [ testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "satoshi")
  , testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "bitcoin")
  , testDiscrete (Proxy :: Proxy "USD") (Proxy :: Proxy "cent")
  , testDiscrete (Proxy :: Proxy "USD") (Proxy :: Proxy "dollar")
  , testDiscrete (Proxy :: Proxy "VUV") (Proxy :: Proxy "vatu")
  , testDiscrete (Proxy :: Proxy "XAU") (Proxy :: Proxy "gram")
  , testDiscrete (Proxy :: Proxy "XAU") (Proxy :: Proxy "grain")
  ]

testDense
  :: forall currency
  .  KnownSymbol currency
  => Proxy currency
  -> Tasty.TestTree
testDense pc =
  Tasty.testGroup ("Dense " ++ show (symbolVal pc))
  [ QC.testProperty "Serialise encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just x === hush (Ser.deserialiseOrFail (Ser.serialise x))
  , QC.testProperty "Serialise encoding roundtrip (SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
         in Just x' === hush (Ser.deserialiseOrFail (Ser.serialise x'))
  , QC.testProperty "Serialise encoding roundtrip (Dense through SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just x === hush (Ser.deserialiseOrFail (Ser.serialise (Money.toSomeDense x)))
  , QC.testProperty "Serialise encoding roundtrip (SomeDense through Dense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just (Money.toSomeDense x) === hush (Ser.deserialiseOrFail (Ser.serialise x))
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
  .  ( Money.GoodScale (Money.UnitScale currency unit)
     , KnownSymbol currency
     , KnownSymbol unit )
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testDiscrete pc pu =
  Tasty.testGroup ("Discrete " ++ show (symbolVal pc) ++ " "
                               ++ show (symbolVal pu))
  [ QC.testProperty "Serialise encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just x === hush (Ser.deserialiseOrFail (Ser.serialise x))
  , QC.testProperty "Serialise encoding roundtrip (SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
         in Just x' === hush (Ser.deserialiseOrFail (Ser.serialise x'))
  , QC.testProperty "Serialise encoding roundtrip (Discrete through SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just x === hush (Ser.deserialiseOrFail (Ser.serialise (Money.toSomeDiscrete x)))
  , QC.testProperty "Serialise encoding roundtrip (SomeDiscrete through Discrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just (Money.toSomeDiscrete x) === hush (Ser.deserialiseOrFail (Ser.serialise x))
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
  [ QC.testProperty "Serialise encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just x === hush (Ser.deserialiseOrFail (Ser.serialise x))
  , QC.testProperty "Serialise encoding roundtrip (SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
         in Just x' === hush (Ser.deserialiseOrFail (Ser.serialise x'))
  , QC.testProperty "Serialise encoding roundtrip (ExchangeRate through SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just x === hush (Ser.deserialiseOrFail (Ser.serialise (Money.toSomeExchangeRate x)))
  , QC.testProperty "Serialise encoding roundtrip (SomeExchangeRate through ExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just (Money.toSomeExchangeRate x) === hush (Ser.deserialiseOrFail (Ser.serialise x))
  ]

--------------------------------------------------------------------------------
-- Raw parsing "golden tests"

testRawSerializations :: Tasty.TestTree
testRawSerializations =
  Tasty.testGroup "Raw serializations"
  [ Tasty.testGroup "serialise"
    [ Tasty.testGroup "decode"
      [ HU.testCase "Dense" $ do
          Just rawDns0 @=? hush (Ser.deserialiseOrFail rawDns0_serialise)
      , HU.testCase "Discrete" $ do
          Just rawDis0 @=? hush (Ser.deserialiseOrFail rawDis0_serialise)
      , HU.testCase "ExchangeRate" $ do
          Just rawXr0 @=? hush (Ser.deserialiseOrFail rawXr0_serialise)
      ]
    , Tasty.testGroup "encode"
      [ HU.testCase "Dense" $ rawDns0_serialise @=? Ser.serialise rawDns0
      , HU.testCase "Discrete" $ rawDis0_serialise @=? Ser.serialise rawDis0
      , HU.testCase "ExchangeRate" $ rawXr0_serialise @=? Ser.serialise rawXr0
      ]
    ]
  ]

rawDns0 :: Money.Dense "USD"
rawDns0 = Money.dense' (26%1)

rawDis0 :: Money.Discrete "USD" "cent"
rawDis0 = Money.discrete 4

rawXr0 :: Money.ExchangeRate "USD" "BTC"
Just rawXr0 = Money.exchangeRate (3%2)

rawDns0_serialise :: BL.ByteString
rawDns0_serialise = "cUSD\CAN\SUB\SOH"
rawDis0_serialise :: BL.ByteString
rawDis0_serialise = "cUSD\CANd\SOH\EOT"
rawXr0_serialise :: BL.ByteString
rawXr0_serialise = "cUSDcBTC\ETX\STX"

--------------------------------------------------------------------------------
-- Misc

hush :: Either a b -> Maybe b
hush (Left _ ) = Nothing
hush (Right b) = Just b

