{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Proxy (Proxy(Proxy))
import Data.Ratio (numerator, denominator)
import GHC.TypeLits (Nat, Symbol, KnownSymbol, symbolVal)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===), (==>), (.&&.))
import qualified Test.Tasty.QuickCheck as QC

#ifdef HAS_aeson
import qualified Data.Aeson as Ae
#endif

#ifdef HAS_binary
import qualified Data.Binary as Binary
#endif

#ifdef HAS_cereal
import qualified Data.Serialize as Cereal
#endif

#ifdef HAS_serialise
import qualified Codec.Serialise as Ser
#endif

#ifdef HAS_store
import qualified Data.Store as Store
#endif

#ifdef HAS_xmlbf
import qualified Xmlbf
import qualified Data.Text as Text
#endif

import qualified Money

--------------------------------------------------------------------------------

instance
  ( Money.GoodScale scale
  ) => QC.Arbitrary (Money.Discrete' currency scale) where
  arbitrary = fmap fromInteger QC.arbitrary
  shrink = fmap fromInteger . QC.shrink . toInteger

instance QC.Arbitrary Money.SomeDiscrete where
  arbitrary = do
    let md = Money.mkSomeDiscrete
               <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    Just x <- QC.suchThat md isJust
    pure x
  shrink = \x -> Money.withSomeDiscrete x (map Money.toSomeDiscrete . QC.shrink)

instance QC.Arbitrary (Money.Dense currency) where
  arbitrary = do
    Just x <- QC.suchThat (Money.dense <$> QC.arbitrary) isJust
    pure x
  shrink = catMaybes . fmap Money.dense . QC.shrink . toRational

instance QC.Arbitrary Money.SomeDense where
  arbitrary = do
    let md = Money.mkSomeDense <$> QC.arbitrary <*> QC.arbitrary
    Just x <- QC.suchThat md isJust
    pure x
  shrink = \x -> Money.withSomeDense x (map Money.toSomeDense . QC.shrink)

instance QC.Arbitrary (Money.ExchangeRate src dst) where
  arbitrary = do
    Just x <- QC.suchThat (fmap Money.exchangeRate QC.arbitrary) isJust
    pure x
  shrink =
    catMaybes . fmap Money.exchangeRate . QC.shrink . Money.fromExchangeRate

instance QC.Arbitrary Money.SomeExchangeRate where
  arbitrary = do
    let md = Money.mkSomeExchangeRate
               <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    Just x <- QC.suchThat md isJust
    pure x
  shrink = \x ->
    Money.withSomeExchangeRate x (map Money.toSomeExchangeRate . QC.shrink)

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
  Tasty.testGroup ("Dense " ++ show (symbolVal pc))
  [ QC.testProperty "read . show == id" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         x === read (show x)
  , QC.testProperty "read . show . Just == Just " $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just x === read (show (Just x))
  , QC.testProperty "fromSomeDense . someDense == Just" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Just x === Money.fromSomeDense (Money.toSomeDense x)
  , QC.testProperty "fromSomeDense works only for same currency" $
      QC.forAll QC.arbitrary $ \(dr :: Money.SomeDense) ->
        (Money.someDenseCurrency dr /= symbolVal pc)
           ==> isNothing (Money.fromSomeDense dr :: Maybe (Money.Dense currency))
  , QC.testProperty "withSomeDense" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
        let dr = Money.toSomeDense x
        in Money.withSomeDense dr $ \x' ->
             (show x, dr, Money.toSomeDense (x + 1))
                === (show x', Money.toSomeDense x', Money.toSomeDense (x' + 1))

#ifdef HAS_aeson
  , QC.testProperty "Aeson encoding roundtrip" $
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
            c = Money.someDenseCurrency sx
            r = Money.someDenseAmount sx
            bs = Ae.encode ("Dense", c, numerator r, denominator r)
        in (Just  x === Ae.decode bs) .&&.
           (Just sx === Ae.decode bs)
#endif

#ifdef HAS_binary
  , QC.testProperty "Binary encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let Right (_,_,y) = Binary.decodeOrFail (Binary.encode x)
         in x === y
  , QC.testProperty "Binary encoding roundtrip (SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
             bs = Binary.encode x'
         in Right (mempty, BSL.length bs, x') === Binary.decodeOrFail bs
  , QC.testProperty "Binary encoding roundtrip (Dense through SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
             bs = Binary.encode x'
         in Right (mempty, BSL.length bs, x) === Binary.decodeOrFail bs
  , QC.testProperty "Binary encoding roundtrip (SomeDense through Dense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
             bs = Binary.encode x
         in Right (mempty, BSL.length bs, x') === Binary.decodeOrFail bs
#endif

#ifdef HAS_cereal
  , QC.testProperty "Cereal encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right x === Cereal.decode (Cereal.encode x)
  , QC.testProperty "Cereal encoding roundtrip (SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
         in Right x' === Cereal.decode (Cereal.encode x')
  , QC.testProperty "Cereal encoding roundtrip (Dense through SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right x === Cereal.decode (Cereal.encode (Money.toSomeDense x))
  , QC.testProperty "Cereal encoding roundtrip (SomeDense through Dense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right (Money.toSomeDense x) === Cereal.decode (Cereal.encode x)
#endif

#ifdef HAS_serialise
  , QC.testProperty "Serialise encoding roundtrip" $
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
#endif

#ifdef HAS_store
  , QC.testProperty "Store encoding roundtrip" $
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
#endif

#ifdef HAS_xmlbf
  , QC.testProperty "Xmlbf encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right x === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x)
  , QC.testProperty "Xmlbf encoding roundtrip (SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         let x' = Money.toSomeDense x
         in Right x' === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x')
  , QC.testProperty "Xmlbf encoding roundtrip (Dense through SomeDense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right x === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml (Money.toSomeDense x))
  , QC.testProperty "Xmlbf encoding roundtrip (SomeDense through Dense)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
         Right (Money.toSomeDense x) === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x)
#endif
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
  , QC.testProperty "read . show . Just == Just" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just x === read (show (Just x))
  , QC.testProperty "fromSomeDiscrete . someDiscrete == Just" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Just x === Money.fromSomeDiscrete (Money.toSomeDiscrete x)
  , QC.testProperty "fromSomeDiscrete works only for same currency and scale" $
      QC.forAll QC.arbitrary $ \(dr :: Money.SomeDiscrete) ->
        ((Money.someDiscreteCurrency dr /= symbolVal pc) &&
         (Money.someDiscreteScale dr /=
             Money.scale (Proxy :: Proxy (Money.Scale currency unit)))
        ) ==> isNothing (Money.fromSomeDiscrete dr
                          :: Maybe (Money.Discrete currency unit))
  , QC.testProperty "withSomeDiscrete" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
        let dr = Money.toSomeDiscrete x
        in ( Money.withSomeDiscrete dr $ \x' ->
                (show x, dr, Money.toSomeDiscrete (x + 1))
                   === (show x', Money.toSomeDiscrete x', Money.toSomeDiscrete (x' + 1))
           ) :: QC.Property

#ifdef HAS_aeson
  , QC.testProperty "Aeson encoding roundtrip" $
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
            c = Money.someDiscreteCurrency sx
            r = Money.someDiscreteScale sx
            a = Money.someDiscreteAmount sx
            bs = Ae.encode ("Discrete", c, numerator r, denominator r, a)
        in (Just  x === Ae.decode bs) .&&.
           (Just sx === Ae.decode bs)
#endif

#ifdef HAS_binary
  , QC.testProperty "Binary encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let Right (_,_,y) = Binary.decodeOrFail (Binary.encode x)
         in x === y
  , QC.testProperty "Binary encoding roundtrip (SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
             bs = Binary.encode x'
         in Right (mempty, BSL.length bs, x') === Binary.decodeOrFail bs
  , QC.testProperty "Binary encoding roundtrip (Discrete through SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
             bs = Binary.encode x'
         in Right (mempty, BSL.length bs, x) === Binary.decodeOrFail bs
  , QC.testProperty "Binary encoding roundtrip (SomeDiscrete through Discrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
             bs = Binary.encode x
         in Right (mempty, BSL.length bs, x') === Binary.decodeOrFail bs
#endif

#ifdef HAS_cereal
  , QC.testProperty "Cereal encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right x === Cereal.decode (Cereal.encode x)
  , QC.testProperty "Cereal encoding roundtrip (SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
         in Right x' === Cereal.decode (Cereal.encode x')
  , QC.testProperty "Cereal encoding roundtrip (Discrete through SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right x === Cereal.decode (Cereal.encode (Money.toSomeDiscrete x))
  , QC.testProperty "Cereal encoding roundtrip (SomeDiscrete through Discrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right (Money.toSomeDiscrete x) === Cereal.decode (Cereal.encode x)
#endif

#ifdef HAS_serialise
  , QC.testProperty "Serialise encoding roundtrip" $
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
#endif

#ifdef HAS_store
  , QC.testProperty "Store encoding roundtrip" $
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
#endif

#ifdef HAS_xmlbf
  , QC.testProperty "Xmlbf encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right x === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x)
  , QC.testProperty "Xmlbf encoding roundtrip (SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         let x' = Money.toSomeDiscrete x
         in Right x' === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x')
  , QC.testProperty "Xmlbf encoding roundtrip (Discrete through SomeDiscrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right x === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml (Money.toSomeDiscrete x))
  , QC.testProperty "Xmlbf encoding roundtrip (SomeDiscrete through Discrete)" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
         Right (Money.toSomeDiscrete x) === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x)
#endif
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
  , QC.testProperty "read . show . Just == Just" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         Just xr === read (show (Just xr))
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
  , QC.testProperty "fromSomeExchangeRate . someExchangeRate == Just" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Just x === Money.fromSomeExchangeRate (Money.toSomeExchangeRate x)
  , QC.testProperty "fromSomeExchangeRate works only for same currencies" $
      QC.forAll QC.arbitrary $ \(x :: Money.SomeExchangeRate) ->
        ((Money.someExchangeRateSrcCurrency x /= symbolVal ps) &&
         (Money.someExchangeRateDstCurrency x /= symbolVal pd))
            ==> isNothing (Money.fromSomeExchangeRate x
                            :: Maybe (Money.ExchangeRate src dst))
  , QC.testProperty "withSomeExchangeRate" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
        let dr = Money.toSomeExchangeRate x
        in Money.withSomeExchangeRate dr $ \x' ->
             (show x, dr) === (show x', Money.toSomeExchangeRate x')

#ifdef HAS_aeson
  , QC.testProperty "Aeson encoding roundtrip" $
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
            src = Money.someExchangeRateSrcCurrency sx
            dst = Money.someExchangeRateDstCurrency sx
            r = Money.someExchangeRateRate sx
            bs = Ae.encode ("ExchangeRate", src, dst, numerator r, denominator r)
        in (Just  x === Ae.decode bs) .&&.
           (Just sx === Ae.decode bs)
#endif

#ifdef HAS_binary
  , QC.testProperty "Binary encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let Right (_,_,y) = Binary.decodeOrFail (Binary.encode x)
         in x === y
  , QC.testProperty "Binary encoding roundtrip (SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
             bs = Binary.encode x'
         in Right (mempty, BSL.length bs, x') === Binary.decodeOrFail bs
  , QC.testProperty "Binary encoding roundtrip (ExchangeRate through SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
             bs = Binary.encode x'
         in Right (mempty, BSL.length bs, x) === Binary.decodeOrFail bs
  , QC.testProperty "Binary encoding roundtrip (SomeExchangeRate through ExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
             bs = Binary.encode x
         in Right (mempty, BSL.length bs, x') === Binary.decodeOrFail bs
#endif

#ifdef HAS_cereal
  , QC.testProperty "Cereal encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right x === Cereal.decode (Cereal.encode x)
  , QC.testProperty "Cereal encoding roundtrip (SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
         in Right x' === Cereal.decode (Cereal.encode x')
  , QC.testProperty "Cereal encoding roundtrip (ExchangeRate through SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right x === Cereal.decode (Cereal.encode (Money.toSomeExchangeRate x))
  , QC.testProperty "Cereal encoding roundtrip (SomeExchangeRate through ExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right (Money.toSomeExchangeRate x) === Cereal.decode (Cereal.encode x)
#endif

#ifdef HAS_serialise
  , QC.testProperty "Serialise encoding roundtrip" $
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
#endif

#ifdef HAS_store
  , QC.testProperty "Store encoding roundtrip" $
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
#endif

#ifdef HAS_xmlbf
  , QC.testProperty "Xmlbf encoding roundtrip" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right x === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x)
  , QC.testProperty "Xmlbf encoding roundtrip (SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         let x' = Money.toSomeExchangeRate x
         in Right x' === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x')
  , QC.testProperty "Xmlbf encoding roundtrip (ExchangeRate through SomeExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right x === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml (Money.toSomeExchangeRate x))
  , QC.testProperty "Xmlbf encoding roundtrip (SomeExchangeRate through ExchangeRate)" $
      QC.forAll QC.arbitrary $ \(x :: Money.ExchangeRate src dst) ->
         Right (Money.toSomeExchangeRate x) === Xmlbf.runParser Xmlbf.fromXml (Xmlbf.toXml x)
#endif
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
    , QC.testProperty "floor no reminder"    $ QC.forAll QC.arbitrary (h Money.floor)
    , QC.testProperty "ceiling no reminder"  $ QC.forAll QC.arbitrary (h Money.ceiling)
    , QC.testProperty "round no reminder"    $ QC.forAll QC.arbitrary (h Money.round)
    , QC.testProperty "truncate no reminder" $ QC.forAll QC.arbitrary (h Money.truncate)
    ]
  where
    g :: (Money.Dense currency -> (Money.Discrete' currency (Money.Scale currency unit), Money.Dense currency))
      -> Money.Dense currency
      -> QC.Property
    g f = \x -> x === case f x of (y, z) -> Money.fromDiscrete y + z

    h :: (Money.Dense currency -> (Money.Discrete' currency (Money.Scale currency unit), Money.Dense currency))
      -> Money.Discrete currency unit
      -> QC.Property
    h f = \x -> (Money.fromDiscrete x) === case f (Money.fromDiscrete x) of
      (y, 0) -> Money.fromDiscrete y
      (_, _) -> error "testRounding.h: unexpected"

hush :: Either a b -> Maybe b
hush (Left _ ) = Nothing
hush (Right b) = Just b

