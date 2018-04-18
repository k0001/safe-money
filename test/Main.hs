{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Category (Category((.), id))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import Data.Maybe (catMaybes, isJust, isNothing, fromJust)
import Data.Proxy (Proxy(Proxy))
import Data.Ratio ((%), numerator, denominator)
import Data.Word (Word8)
import GHC.TypeLits (Nat, Symbol, KnownSymbol, symbolVal)
import Prelude hiding ((.), id)
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HU
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

import Money ()
import qualified Money.Internal as Money

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
    fromJust <$> QC.suchThat md isJust
  shrink = \x -> Money.withSomeDiscrete x (map Money.toSomeDiscrete . QC.shrink)

instance QC.Arbitrary (Money.Dense currency) where
  arbitrary = do
     let myd = fmap Money.dense QC.arbitrary
     fromJust <$> QC.suchThat myd isJust
  shrink = catMaybes . map Money.dense . QC.shrink . toRational

instance QC.Arbitrary Money.SomeDense where
  arbitrary = do
    let md = Money.mkSomeDense <$> QC.arbitrary <*> QC.arbitrary
    fromJust <$> QC.suchThat md isJust
  shrink = \x -> Money.withSomeDense x (map Money.toSomeDense . QC.shrink)

instance QC.Arbitrary (Money.ExchangeRate src dst) where
  arbitrary = do
    let myxr = fmap Money.exchangeRate QC.arbitrary
    fromJust <$> QC.suchThat myxr isJust
  shrink = catMaybes . map Money.exchangeRate
         . QC.shrink . Money.exchangeRateToRational

instance QC.Arbitrary Money.SomeExchangeRate where
  arbitrary = do
    let md = Money.mkSomeExchangeRate
               <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    fromJust <$> QC.suchThat md isJust
  shrink = \x ->
    Money.withSomeExchangeRate x (map Money.toSomeExchangeRate . QC.shrink)

instance QC.Arbitrary Money.Approximation where
  arbitrary = QC.oneof [ pure Money.Round
                       , pure Money.Floor
                       , pure Money.Ceiling
                       , pure Money.Truncate ]

-- | Generates a valid 'Money.rationalToDecimal' result. Returns the thousand
-- and decimal separators as welland decimal separators as well.
genDecimal :: QC.Gen (String, Maybe Char, Char)
genDecimal = do
  aprox :: Money.Approximation <- QC.arbitrary
  plus :: Bool <- QC.arbitrary
  digs :: Word8 <- QC.arbitrary
  r :: Rational <- (%) <$> QC.arbitrary <*> QC.suchThat QC.arbitrary (/= 0)
  (yts, ds) <- genDecimalSeps
  Just dec <- pure (Money.rationalToDecimal aprox plus yts ds digs r)
  pure (dec, yts, ds)

-- | Generates valid separators for decimal representations (see genDecimal).
genDecimalSeps :: QC.Gen (Maybe Char, Char)
genDecimalSeps = do
  let msep = QC.suchThat QC.arbitrary (not . Char.isDigit)
  ds :: Char <- msep
  yts :: Maybe Char <- genMaybe (QC.suchThat msep (/= ds))
  pure (yts, ds)


genMaybe :: QC.Gen a -> QC.Gen (Maybe a)
genMaybe m = QC.oneof [pure Nothing, fmap Just m]

--------------------------------------------------------------------------------

main :: IO ()
main =  Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] (Tasty.localOption (QC.QuickCheckTests 500) tests)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "root"
  [ testCurrencies
  , testCurrencyUnits
  , testExchange
  , testRationalToDecimal
  , testRationalFromDecimal
  , testDiscreteFromDecimal
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


testRationalToDecimal :: Tasty.TestTree
testRationalToDecimal =
  Tasty.testGroup "rationalToDecimal"
  [ HU.testCase "Round: r1" $ do
       render Money.Round r1 @?=
         [ "1023004567.90"        --  0
         , "1,023,004,567.90"     --  1
         , "+1023004567.90"       --  2
         , "+1,023,004,567.90"    --  3
         , "1023004568"           --  8
         , "1,023,004,568"        --  9
         , "+1023004568"          -- 10
         , "+1,023,004,568"       -- 11
         ]
  , HU.testCase "Round: negate r1" $ do
       render Money.Round (negate r1) @?=
         [ "-1023004567.90"       --  0
         , "-1,023,004,567.90"    --  1
         , "-1023004567.90"       --  2
         , "-1,023,004,567.90"    --  3
         , "-1023004568"          --  8
         , "-1,023,004,568"       --  9
         , "-1023004568"          -- 10
         , "-1,023,004,568"       -- 11
         ]
  , HU.testCase "Round: r2" $ do
       render Money.Round r2 @?=
         [ "1.23"    --  0
         , "1.23"    --  1
         , "+1.23"   --  2
         , "+1.23"   --  3
         , "1"       --  8
         , "1"       --  9
         , "+1"      -- 10
         , "+1"      -- 11
         ]
  , HU.testCase "Round: negate r2" $ do
       render Money.Round (negate r2) @?=
         [ "-1.23"    --  0
         , "-1.23"    --  1
         , "-1.23"    --  2
         , "-1.23"    --  3
         , "-1"       --  8
         , "-1"       --  9
         , "-1"       -- 10
         , "-1"       -- 11
         ]
  , HU.testCase "Round: r3" $ do
       render Money.Round r3 @?=
         [ "0.34"   --  0
         , "0.34"   --  1
         , "+0.34"  --  2
         , "+0.34"  --  3
         , "0"      --  8
         , "0"      --  9
         , "0"      -- 10
         , "0"      -- 11
         ]
  , HU.testCase "Round: negate r3" $ do
       render Money.Round (negate r3) @?=
         [ "-0.34"   --  0
         , "-0.34"   --  1
         , "-0.34"   --  2
         , "-0.34"   --  3
         , "0"       --  8
         , "0"       --  9
         , "0"       -- 10
         , "0"       -- 11
         ]
  , HU.testCase "Floor: r1" $ do
       render Money.Floor r1 @?=
         [ "1023004567.89"        --  0
         , "1,023,004,567.89"     --  1
         , "+1023004567.89"       --  2
         , "+1,023,004,567.89"    --  3
         , "1023004567"           --  8
         , "1,023,004,567"        --  9
         , "+1023004567"          -- 10
         , "+1,023,004,567"       -- 11
         ]
  , HU.testCase "Floor: negate r1" $ do
       render Money.Floor (negate r1) @?=
         [ "-1023004567.90"       --  0
         , "-1,023,004,567.90"    --  1
         , "-1023004567.90"       --  2
         , "-1,023,004,567.90"    --  3
         , "-1023004568"          --  8
         , "-1,023,004,568"       --  9
         , "-1023004568"          -- 10
         , "-1,023,004,568"       -- 11
         ]
  , HU.testCase "Floor: r2" $ do
       render Money.Floor r2 @?=
         [ "1.23"    --  0
         , "1.23"    --  1
         , "+1.23"   --  2
         , "+1.23"   --  3
         , "1"       --  8
         , "1"       --  9
         , "+1"      -- 10
         , "+1"      -- 11
         ]
  , HU.testCase "Floor: negate r2" $ do
       render Money.Floor (negate r2) @?=
         [ "-1.23"    --  0
         , "-1.23"    --  1
         , "-1.23"    --  2
         , "-1.23"    --  3
         , "-2"       --  8
         , "-2"       --  9
         , "-2"       -- 10
         , "-2"       -- 11
         ]
  , HU.testCase "Floor: r3" $ do
       render Money.Floor r3 @?=
         [ "0.34"   --  0
         , "0.34"   --  1
         , "+0.34"  --  2
         , "+0.34"  --  3
         , "0"      --  8
         , "0"      --  9
         , "0"      -- 10
         , "0"      -- 11
         ]
  , HU.testCase "Floor: negate r3" $ do
       render Money.Floor (negate r3) @?=
         [ "-0.35"   --  0
         , "-0.35"   --  1
         , "-0.35"   --  2
         , "-0.35"   --  3
         , "-1"      --  8
         , "-1"      --  9
         , "-1"      -- 10
         , "-1"      -- 11
         ]
  , HU.testCase "Ceiling: r1" $ do
       render Money.Ceiling r1 @?=
         [ "1023004567.90"        --  0
         , "1,023,004,567.90"     --  1
         , "+1023004567.90"       --  2
         , "+1,023,004,567.90"    --  3
         , "1023004568"           --  8
         , "1,023,004,568"        --  9
         , "+1023004568"          -- 10
         , "+1,023,004,568"       -- 11
         ]
  , HU.testCase "Ceiling: negate r1" $ do
       render Money.Ceiling (negate r1) @?=
         [ "-1023004567.89"       --  0
         , "-1,023,004,567.89"    --  1
         , "-1023004567.89"       --  2
         , "-1,023,004,567.89"    --  3
         , "-1023004567"          --  8
         , "-1,023,004,567"       --  9
         , "-1023004567"          -- 10
         , "-1,023,004,567"       -- 11
         ]
  , HU.testCase "Ceiling: r2" $ do
       render Money.Ceiling r2 @?=
         [ "1.23"    --  0
         , "1.23"    --  1
         , "+1.23"   --  2
         , "+1.23"   --  3
         , "2"       --  8
         , "2"       --  9
         , "+2"      -- 10
         , "+2"      -- 11
         ]
  , HU.testCase "Ceiling: negate r2" $ do
       render Money.Ceiling (negate r2) @?=
         [ "-1.23"    --  0
         , "-1.23"    --  1
         , "-1.23"    --  2
         , "-1.23"    --  3
         , "-1"       --  8
         , "-1"       --  9
         , "-1"       -- 10
         , "-1"       -- 11
         ]
  , HU.testCase "Ceiling: r3" $ do
       render Money.Ceiling r3 @?=
         [ "0.35"   --  0
         , "0.35"   --  1
         , "+0.35"  --  2
         , "+0.35"  --  3
         , "1"      --  8
         , "1"      --  9
         , "+1"     -- 10
         , "+1"     -- 11
         ]
  , HU.testCase "Ceiling: negate r3" $ do
       render Money.Ceiling (negate r3) @?=
         [ "-0.34"   --  0
         , "-0.34"   --  1
         , "-0.34"   --  2
         , "-0.34"   --  3
         , "0"       --  8
         , "0"       --  9
         , "0"       -- 10
         , "0"       -- 11
         ]

  , HU.testCase "Truncate: r1" $ do
      render Money.Truncate r1 @?= render Money.Floor r1

  , HU.testCase "Truncate: negate r1" $ do
      render Money.Truncate (negate r1) @?= render Money.Ceiling (negate r1)

  , HU.testCase "Truncate: r2" $ do
      render Money.Truncate r2 @?= render Money.Floor r2

  , HU.testCase "Truncate: negate r2" $ do
      render Money.Truncate (negate r2) @?= render Money.Ceiling (negate r2)

  , HU.testCase "Truncate: r3" $ do
      render Money.Truncate r3 @?= render Money.Floor r3

  , HU.testCase "Truncate: negate r3" $ do
      render Money.Truncate (negate r3) @?= render Money.Ceiling (negate r3)
  ]
  where
    r1 :: Rational = 1023004567895 % 1000
    r2 :: Rational = 123 % 100
    r3 :: Rational = 345 % 1000

    render :: Money.Approximation -> Rational -> [String]
    render a r =
      [ fromJust $ Money.rationalToDecimal a False Nothing    '.' 2 r  --  0
      , fromJust $ Money.rationalToDecimal a False (Just ',') '.' 2 r  --  1
      , fromJust $ Money.rationalToDecimal a True  Nothing    '.' 2 r  --  2
      , fromJust $ Money.rationalToDecimal a True  (Just ',') '.' 2 r  --  3
      , fromJust $ Money.rationalToDecimal a False Nothing    '.' 0 r  --  8
      , fromJust $ Money.rationalToDecimal a False (Just ',') '.' 0 r  --  9
      , fromJust $ Money.rationalToDecimal a True  Nothing    '.' 0 r  -- 10
      , fromJust $ Money.rationalToDecimal a True  (Just ',') '.' 0 r  -- 11
      ]

testRationalFromDecimal :: Tasty.TestTree
testRationalFromDecimal =
  Tasty.testGroup "rationalFromDecimal"
  [ QC.testProperty "Unsupported separators" $
      let mbadsep :: QC.Gen Char = QC.suchThat QC.arbitrary Char.isDigit
          mgoodsep :: QC.Gen Char = QC.suchThat QC.arbitrary (not . Char.isDigit)
      in QC.forAll ((,,) <$> mbadsep <*> mbadsep <*> mgoodsep) $
           \(s1 :: Char, s2 :: Char, s3 :: Char) ->
              let f = Money.rationalFromDecimal
              in (f Nothing s1 (error "untouched") === Nothing) .&&.
                 (f (Just s1) s2 (error "untouched") === Nothing) .&&.
                 (f (Just s1) s1 (error "untouched") === Nothing) .&&.
                 (f (Just s3) s3 (error "untouched") === Nothing)

  , QC.testProperty "Lossy roundtrip" $
      -- We check that the roundtrip results in a close amount with a fractional
      -- difference of up to one.
      let gen = (,) <$> genDecimalSeps <*> QC.arbitrary
      in QC.forAll gen $ \( (yst :: Maybe Char, sd :: Char)
                       , (r :: Rational, plus :: Bool, digs :: Word8,
                          aprox :: Money.Approximation) ) ->
           let Just dec = Money.rationalToDecimal aprox plus yst sd digs r
               Just r' = Money.rationalFromDecimal yst sd dec
           in 1 > abs (abs r - abs r')
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

  , QC.testProperty "denseCurrency" $
      QC.forAll QC.arbitrary $ \(x :: Money.Dense currency) ->
        Money.denseCurrency x === symbolVal pc

  , QC.testProperty "denseToDecimal: Same as rationalToDecimal" $
      let gen = (,) <$> genDecimalSeps <*> QC.arbitrary
      in QC.forAll gen $ \( (yst :: Maybe Char, sd :: Char)
                          , (dns :: Money.Dense currency, plus :: Bool,
                             digs :: Word8, aprox :: Money.Approximation) ) ->
            let ydnsd1 = Money.denseToDecimal aprox plus yst sd digs (Proxy :: Proxy '(1,1)) dns
                ydnsd100 = Money.denseToDecimal aprox plus yst sd digs (Proxy :: Proxy '(100,1)) dns
                yrd1 = Money.rationalToDecimal aprox plus yst sd digs (toRational dns)
                yrd100 = Money.rationalToDecimal aprox plus yst sd digs (toRational dns * 100)
            in (ydnsd1 === yrd1) .&&. (ydnsd100 === yrd100)

  , QC.testProperty "denseFromDecimal: Same as rationalFromDecimal" $
      QC.forAll genDecimal $ \(dec :: String, yts :: Maybe Char, ds :: Char) ->
         let Just r = Money.rationalFromDecimal yts ds dec
             Just dns = Money.denseFromDecimal yts ds dec
         in r === toRational (dns :: Money.Dense currency)

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

  , QC.testProperty "discreteCurrency" $
      QC.forAll QC.arbitrary $ \(x :: Money.Discrete currency unit) ->
        Money.discreteCurrency x === symbolVal pc

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
  [ QC.testProperty "Category: left identity" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         xr === id . xr
  , QC.testProperty "Category: right identity" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         xr === xr . id
  , QC.testProperty "Category: composition with inverse" $
      QC.forAll QC.arbitrary $ \(xr1 :: Money.ExchangeRate src dst) ->
         (1 === Money.exchangeRateToRational (xr1 . Money.exchangeRateRecip xr1)) .&&.
         (1 === Money.exchangeRateToRational (Money.exchangeRateRecip xr1 . xr1))
  , QC.testProperty "Category: composition with other" $
      QC.forAll QC.arbitrary $ \(xr1 :: Money.ExchangeRate src dst,
                                 xr2 :: Money.ExchangeRate dst src) ->
         let a = Money.exchangeRateToRational xr1 * Money.exchangeRateToRational xr2
         in (a === Money.exchangeRateToRational (xr1 . xr2)) .&&.
            (a === Money.exchangeRateToRational (xr2 . xr1))

  , QC.testProperty "read . show == id" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         xr === read (show xr)
  , QC.testProperty "read . show . Just == Just" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         Just xr === read (show (Just xr))
  , QC.testProperty "flipExchangeRate . flipExchangeRate == id" $
      QC.forAll QC.arbitrary $ \(xr :: Money.ExchangeRate src dst) ->
         let xr' = Money.exchangeRateRecip xr
         in (Money.exchangeRateToRational xr /= Money.exchangeRateToRational xr')
               ==> (xr === Money.exchangeRateRecip xr')
  , QC.testProperty "exchange (flipExchangeRate x) . exchange x == id" $
      QC.forAll QC.arbitrary $
         \( c0 :: Money.Dense src
          , xr :: Money.ExchangeRate src dst
          ) -> c0 === Money.exchange (Money.exchangeRateRecip xr)
                                     (Money.exchange xr c0)
  , QC.testProperty "x == 1 ==> exchange x == id" $
      QC.forAll QC.arbitrary $
         \( c0 :: Money.Dense src
          ) -> let Just xr = Money.exchangeRate 1
               in toRational c0 === toRational (Money.exchange xr c0)
  , QC.testProperty "x /= 1 ==> exchange x /= id" $
      QC.forAll QC.arbitrary $
         \( c0 :: Money.Dense src
          , xr :: Money.ExchangeRate src dst
          ) -> (Money.exchangeRateToRational xr /= 1 && toRational c0 /= 0)
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

  , QC.testProperty "exchangeRateToDecimal: Same as rationalToDecimal" $
      let gen = (,) <$> genDecimalSeps <*> QC.arbitrary
      in QC.forAll gen $ \( (yst :: Maybe Char, sd :: Char)
                          , (xr :: Money.ExchangeRate src dst, digs :: Word8,
                             aprox :: Money.Approximation ) ) ->
           let xrd = Money.exchangeRateToDecimal aprox yst sd digs xr
               rd = Money.rationalToDecimal aprox False yst sd digs
                       (Money.exchangeRateToRational xr)
           in xrd === rd

  , QC.testProperty "exchangeRateFromDecimal: Same as rationalFromDecimal" $
      QC.forAll genDecimal $ \(dec :: String, yts :: Maybe Char, ds :: Char) ->
         let Just r = Money.rationalFromDecimal yts ds dec
             yxr = Money.exchangeRateFromDecimal yts ds dec
                      :: Maybe (Money.ExchangeRate src dst)
         in (r > 0) ==> (Just r === fmap Money.exchangeRateToRational yxr)

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


testDiscreteFromDecimal :: Tasty.TestTree
testDiscreteFromDecimal =
  Tasty.testGroup "discreteFromDecimal"
  [ HU.testCase "Too large" $ do
      Money.discreteFromDecimal Nothing '.' "0.053"
        @?= (Nothing :: Maybe (Money.Discrete "USD" "cent"))
      Money.discreteFromDecimal (Just ',') '.' "0.253"
        @?= (Nothing :: Maybe (Money.Discrete "USD" "cent"))

  , HU.testCase "USD cent, small, zero" $ do
      let dis = 0 :: Money.Discrete "USD" "cent"
          f = Money.discreteFromDecimal
      f Nothing '.' "0" @?= Just dis
      f Nothing '.' "+0" @?= Just dis
      f Nothing '.' "-0" @?= Just dis
      f (Just ',') '.' "0" @?= Just dis
      f (Just ',') '.' "+0" @?= Just dis
      f (Just ',') '.' "-0" @?= Just dis

  , HU.testCase "USD cent, small, positive" $ do
      let dis = 25 :: Money.Discrete "USD" "cent"
          f = Money.discreteFromDecimal
      f Nothing '.' "0.25" @?= Just dis
      f Nothing '.' "+0.25" @?= Just dis
      f (Just ',') '.' "0.25" @?= Just dis
      f (Just ',') '.' "+0.25" @?= Just dis

  , HU.testCase "USD cent, small, negative" $ do
      let dis = -25 :: Money.Discrete "USD" "cent"
          f = Money.discreteFromDecimal
      f Nothing '.' "-0.25" @?= Just dis
      f Nothing '.' "-0.25" @?= Just dis
      f (Just ',') '.' "-0.25" @?= Just dis
      f (Just ',') '.' "-0.25" @?= Just dis

  , HU.testCase "USD cent, big, positive" $ do
      let dis = 102300456789 :: Money.Discrete "USD" "cent"
          f = Money.discreteFromDecimal
      f Nothing '.' "1023004567.89" @?= Just dis
      f Nothing '.' "+1023004567.89" @?= Just dis
      f (Just ',') '.' "1,023,004,567.89" @?= Just dis
      f (Just ',') '.' "+1,023,004,567.89" @?= Just dis

  , HU.testCase "USD cent, big, negative" $ do
      let dis = -102300456789 :: Money.Discrete "USD" "cent"
          f = Money.discreteFromDecimal
      f Nothing '.' "-1023004567.89" @?= Just dis
      f Nothing '.' "-1023004567.89" @?= Just dis
      f (Just ',') '.' "-1,023,004,567.89" @?= Just dis
      f (Just ',') '.' "-1,023,004,567.89" @?= Just dis
  ]

testRounding
  :: forall (currency :: Symbol) (unit :: Symbol)
  .  (Money.GoodScale (Money.Scale currency unit), KnownSymbol currency)
  => Proxy currency
  -> Proxy unit
  -> Tasty.TestTree
testRounding _ _ =
    Tasty.testGroup "Rounding"
    [ QC.testProperty "floor"    $ QC.forAll QC.arbitrary (g (Money.discreteFromDense Money.Floor))
    , QC.testProperty "ceiling"  $ QC.forAll QC.arbitrary (g (Money.discreteFromDense Money.Ceiling))
    , QC.testProperty "round"    $ QC.forAll QC.arbitrary (g (Money.discreteFromDense Money.Round))
    , QC.testProperty "truncate" $ QC.forAll QC.arbitrary (g (Money.discreteFromDense Money.Truncate))
    , QC.testProperty "floor no reminder"    $ QC.forAll QC.arbitrary (h (Money.discreteFromDense Money.Floor))
    , QC.testProperty "ceiling no reminder"  $ QC.forAll QC.arbitrary (h (Money.discreteFromDense Money.Ceiling))
    , QC.testProperty "round no reminder"    $ QC.forAll QC.arbitrary (h (Money.discreteFromDense Money.Round))
    , QC.testProperty "truncate no reminder" $ QC.forAll QC.arbitrary (h (Money.discreteFromDense Money.Truncate))
    ]
  where
    g :: (Money.Dense currency -> (Money.Discrete' currency (Money.Scale currency unit), Money.Dense currency))
      -> Money.Dense currency
      -> QC.Property
    g f = \x -> x === case f x of (y, z) -> Money.denseFromDiscrete y + z

    h :: (Money.Dense currency -> (Money.Discrete' currency (Money.Scale currency unit), Money.Dense currency))
      -> Money.Discrete currency unit
      -> QC.Property
    h f = \x -> (Money.denseFromDiscrete x) === case f (Money.denseFromDiscrete x) of
      (y, 0) -> Money.denseFromDiscrete y
      (_, _) -> error "testRounding.h: unexpected"

hush :: Either a b -> Maybe b
hush (Left _ ) = Nothing
hush (Right b) = Just b

