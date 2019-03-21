{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aeson as Ae
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Data.Proxy (Proxy(Proxy))
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Exts (fromList)
import GHC.TypeLits (Nat, Symbol, KnownSymbol, symbolVal)
import qualified Money
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=), (@=?))
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===), (==>), (.&&.))
import qualified Test.Tasty.QuickCheck as QC

import qualified Money.Aeson

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
  [ QC.testProperty "Aeson encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just x === Ae.decode (Ae.encode x)
  , QC.testProperty "Aeson encoding roundtrip (SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
         in Just x' === Ae.decode (Ae.encode x')
  , QC.testProperty "Aeson encoding roundtrip (Dense through SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just x === Ae.decode (Ae.encode (Money.toSomeDense x))
  , QC.testProperty "Aeson encoding roundtrip (SomeDense through Dense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just (Money.toSomeDense x) === Ae.decode (Ae.encode x)
  , QC.testProperty "Aeson decoding of pre-0.4 format (Dense, SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
        let sx = Money.toSomeDense x
            c = T.unpack (Money.someDenseCurrency sx)
            r = Money.someDenseAmount sx
            bs = Ae.encode ("Dense" :: String, c, numerator r, denominator r)
        in (Just  x === Ae.decode bs) .&&.
           (Just sx === Ae.decode bs)
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
  [ QC.testProperty "Aeson encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just x === Ae.decode (Ae.encode x)
  , QC.testProperty "Aeson encoding roundtrip (SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
         in Just x' === Ae.decode (Ae.encode x')
  , QC.testProperty "Aeson encoding roundtrip (Discrete through SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just x === Ae.decode (Ae.encode (Money.toSomeDiscrete x))
  , QC.testProperty "Aeson encoding roundtrip (SomeDiscrete through Discrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just (Money.toSomeDiscrete x) === Ae.decode (Ae.encode x)
  , QC.testProperty "Aeson decoding of pre-0.4 format (Discrete, SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
        let sx = Money.toSomeDiscrete x
            c = T.unpack (Money.someDiscreteCurrency sx)
            rs = Money.scaleToRational (Money.someDiscreteScale sx)
            a = Money.someDiscreteAmount sx
            bs = Ae.encode ("Discrete" :: String, c, numerator rs,
                            denominator rs, a)
        in (Just  x === Ae.decode bs) .&&.
           (Just sx === Ae.decode bs)
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
  [ QC.testProperty "Aeson encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just x === Ae.decode (Ae.encode x)
  , QC.testProperty "Aeson encoding roundtrip (SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
         in Just x' === Ae.decode (Ae.encode x')
  , QC.testProperty "Aeson encoding roundtrip (ExchangeRate through SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just x === Ae.decode (Ae.encode (Money.toSomeExchangeRate x))
  , QC.testProperty "Aeson encoding roundtrip (SomeExchangeRate through ExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just (Money.toSomeExchangeRate x) === Ae.decode (Ae.encode x)
  , QC.testProperty "Aeson decoding of pre-0.4 format (ExchangeRate, SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
        let sx = Money.toSomeExchangeRate x
            src = T.unpack (Money.someExchangeRateSrcCurrency sx)
            dst = T.unpack (Money.someExchangeRateDstCurrency sx)
            r = Money.someExchangeRateRate sx
            bs = Ae.encode ("ExchangeRate" :: String, src, dst, numerator r, denominator r)
        in (Just  x === Ae.decode bs) .&&.
           (Just sx === Ae.decode bs)
  ]

--------------------------------------------------------------------------------
-- Raw parsing "golden tests"

testRawSerializations :: Tasty.TestTree
testRawSerializations =
  Tasty.testGroup "Raw serializations"
  [ Tasty.testGroup "aeson"
    [ Tasty.testGroup "decode"
      [ HU.testCase "Dense" $ Just rawDns0 @=? Ae.decode rawDns0_aeson
      , HU.testCase "Discrete" $ Just rawDis0 @=? Ae.decode rawDis0_aeson
      , HU.testCase "ExchangeRate" $ Just rawXr0 @=? Ae.decode rawXr0_aeson
      ]
    , Tasty.testGroup "decode (pre-0.4)"
      [ HU.testCase "Dense" $ Just rawDns0 @=? Ae.decode rawDns0_aeson_pre04
      , HU.testCase "Discrete" $ Just rawDis0 @=? Ae.decode rawDis0_aeson_pre04
      , HU.testCase "ExchangeRate" $ Just rawXr0 @=? Ae.decode rawXr0_aeson_pre04
      ]
    , Tasty.testGroup "encode"
      [ HU.testCase "Dense" $ rawDns0_aeson @=? Ae.encode rawDns0
      , HU.testCase "Discrete" $ rawDis0_aeson @=? Ae.encode rawDis0
      , HU.testCase "ExchangeRate" $ rawXr0_aeson @=? Ae.encode rawXr0
      ]
    ]
  ]

rawDns0 :: Money.Dense "USD"
rawDns0 = Money.dense' (26%1)

rawDis0 :: Money.Discrete "USD" "cent"
rawDis0 = Money.discrete 4

rawXr0 :: Money.ExchangeRate "USD" "BTC"
Just rawXr0 = Money.exchangeRate (3%2)

rawDns0_aeson :: BL.ByteString
rawDns0_aeson = "[\"USD\",26,1]"
rawDis0_aeson :: BL.ByteString
rawDis0_aeson = "[\"USD\",100,1,4]"
rawXr0_aeson :: BL.ByteString
rawXr0_aeson = "[\"USD\",\"BTC\",3,2]"

-- pre safe-money 0.4
rawDns0_aeson_pre04 :: BL.ByteString
rawDns0_aeson_pre04 = "[\"Dense\",\"USD\",26,1]"
rawDis0_aeson_pre04 :: BL.ByteString
rawDis0_aeson_pre04 = "[\"Discrete\",\"USD\",100,1,4]"
rawXr0_aeson_pre04 :: BL.ByteString
rawXr0_aeson_pre04 = "[\"ExchangeRate\",\"USD\",\"BTC\",3,2]"

