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
  [ testRounding (Proxy :: Proxy "USD")
  , testRounding (Proxy :: Proxy "USD/cent")
  , testRounding (Proxy :: Proxy "USD/dollar")
  , testRounding (Proxy :: Proxy "BTC")
  , testRounding (Proxy :: Proxy "BTC/satoshi")
  , testRounding (Proxy :: Proxy "BTC/bitcoin")
  , testRounding (Proxy :: Proxy "BTC/bitcoin")
  , testRounding (Proxy :: Proxy "XAU")
  , testRounding (Proxy :: Proxy "XAU/micrograin")
  , testRounding (Proxy :: Proxy "XAU/milligrain")
  , testRounding (Proxy :: Proxy "XAU/grain")
  ]

testRounding
  :: forall (currency :: Symbol)
  . ( KnownSymbol currency
    , KnownNat (Money.Scale currency)
    , CmpNat 0 (Money.Scale currency) ~ 'LT )
  => Proxy currency
  -> Tasty.TestTree
testRounding pc = Tasty.testGroup "Rounding"
  [ QC.testProperty ("floor - " ++ symbolVal pc) $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.floor x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  , QC.testProperty ("ceiling - " ++ symbolVal pc) $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.ceiling x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  , QC.testProperty ("round - " ++ symbolVal pc) $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.round x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  , QC.testProperty ("truncate - " ++ symbolVal pc) $
      QC.forAll QC.arbitrary $ \(x :: Money.Continuous currency) ->
         x === case Money.truncate x of
                  (y, Nothing) -> Money.fromDiscrete y
                  (y, Just z)  -> Money.fromDiscrete y + z
  ]
