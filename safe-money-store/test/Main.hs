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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Data.Proxy (Proxy(Proxy))
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Store as Store
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

import Money.Store ()

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
  [ testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "BTC")
  , testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "satoshi")
  , testDiscrete (Proxy :: Proxy "BTC") (Proxy :: Proxy "bitcoin")
  , testDiscrete (Proxy :: Proxy "USD") (Proxy :: Proxy "USD")
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
  [ QC.testProperty "Store encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right x === Store.decode (Store.encode x)
  , QC.testProperty "Store encoding roundtrip (SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
         in Right x' === Store.decode (Store.encode x')
  , QC.testProperty "Store encoding roundtrip (Dense through SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right x === Store.decode (Store.encode (Money.toSomeDense x))
  , QC.testProperty "Store encoding roundtrip (SomeDense through Dense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right (Money.toSomeDense x) === Store.decode (Store.encode x)
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
  [ QC.testProperty "Store encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right x === Store.decode (Store.encode x)
  , QC.testProperty "Store encoding roundtrip (SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
         in Right x' === Store.decode (Store.encode x')
  , QC.testProperty "Store encoding roundtrip (Discrete through SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right x === Store.decode (Store.encode (Money.toSomeDiscrete x))
  , QC.testProperty "Store encoding roundtrip (SomeDiscrete through Discrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right (Money.toSomeDiscrete x) === Store.decode (Store.encode x)
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
  [ QC.testProperty "Store encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right x === Store.decode (Store.encode x)
  , QC.testProperty "Store encoding roundtrip (SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
         in Right x' === Store.decode (Store.encode x')
  , QC.testProperty "Store encoding roundtrip (ExchangeRate through SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right x === Store.decode (Store.encode (Money.toSomeExchangeRate x))
  , QC.testProperty "Store encoding roundtrip (SomeExchangeRate through ExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right (Money.toSomeExchangeRate x) === Store.decode (Store.encode x)
  ]

--------------------------------------------------------------------------------
-- Raw parsing "golden tests"

testRawSerializations :: Tasty.TestTree
testRawSerializations =
  Tasty.testGroup "Raw serializations"
  [ Tasty.testGroup "store"
    [ Tasty.testGroup "decode"
      [ HU.testCase "Dense" $ do
          Right rawDns0 @=? Store.decode rawDns0_store
      , HU.testCase "Discrete" $ do
          Right rawDis0 @=? Store.decode rawDis0_store
      , HU.testCase "ExchangeRate" $ do
          Right rawXr0 @=? Store.decode rawXr0_store
      ]
    , Tasty.testGroup "encode"
      [ HU.testCase "Dense" $ rawDns0_store @=? Store.encode rawDns0
      , HU.testCase "Discrete" $ rawDis0_store @=? Store.encode rawDis0
      , HU.testCase "ExchangeRate" $ rawXr0_store @=? Store.encode rawXr0
      ]
    ]
  ]

rawDns0 :: Money.Dense "USD"
rawDns0 = Money.dense' (26%1)

rawDis0 :: Money.Discrete "USD" "cent"
rawDis0 = Money.discrete 4

rawXr0 :: Money.ExchangeRate "USD" "BTC"
Just rawXr0 = Money.exchangeRate (3%2)


-- Such a waste of space these many bytes! Can we shrink this and maintain
-- backwards compatibility?
rawDns0_store :: B.ByteString
rawDns0_store = "\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NULU\NUL\NUL\NULS\NUL\NUL\NULD\NUL\NUL\NUL\NUL\SUB\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
rawDis0_store :: B.ByteString
rawDis0_store = "\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NULU\NUL\NUL\NULS\NUL\NUL\NULD\NUL\NUL\NUL\NULd\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
rawXr0_store :: B.ByteString
rawXr0_store = "\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NULU\NUL\NUL\NULS\NUL\NUL\NULD\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NULB\NUL\NUL\NULT\NUL\NUL\NULC\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
