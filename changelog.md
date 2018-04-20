# Version 0.6

* _COMPILER ASSISTED BREAKING CHANGE_. `denseToDecimal` now takes a positive
  `Rational` rather than a `Proxy scale`.

* _COMPILER ASSISTED BREAKING CHANGE_. `denseFromDecimal` and
  `discreteFromDecimal` now take a positive `Rational` scale, like their
  `xxxToDecimal` counterparts.

* Introduced a new function `discreteToDecimal`.


# Version 0.5

* _COMPILER ASSISTED BREAKING CHANGE_. The `round`, `floor`, `ceiling` and
  `truncate` functions were replaced by a single `discreteFromDense` function
  taking an argument of type `Approximation` (`Round`, `Floor`, `Ceiling` or
  `Truncate`) as an argument.

* _COMPILER ASSISTED BREAKING CHANGE_. The `fromDiscrete` function was
  renamed to `denseFromDiscrete`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `fromExchangeRate` function was
  renamed to `exchangeRateToRational`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `flipExchangeRate` function was
  renamed to `exchangeRateRecip`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `Dense` is not an instance of
  `Fractional` anymore because `recip` and `/` could potentially crash.

* Introduced new functions for rendering and parsing decimal reperesentations
  of monetary amounts: `denseCurrency`, `discreteCurrency`, `denseFromDecimal`,
  `denseToDecimal`, `discreteFromDecimal`, `exchangeRateFromDecimal`,
  `exchangeRateToDecimal`.

* Introduced optional `AdditiveGroup` and `VectorSpace` group instances for
  `Dense` and `Discrete`. These type-classes come from the `vector-space`
  library and they can be enabled or disabled via the `vector-space` Cabal flag,
  which is enabled by default.

* Introduced `discrete` constructor which behaves just like `fromInteger`.

* Introduced the `dense'` constructor as an unsafe but convenient version of
  `dense`.

* The `ErrScaleNonCanonical` type is now exported.

* The `Money.Internal` module is now exposed, but hidden from the Haddock
  documentation.

* New scale: `"BTC" "millibitcoin"`.

* Added many tests.


# Version 0.4.1

* `ExchangeRate` is now a `Category`.


# Version 0.4

* **BREAKING CHANGE REQUIRING HUMAN INTERVENTION**. The JSON serializations for
  all of `Dense`, `SomeDense`, `Discrete`, `SomeDiscrete`, `ExchangeRate` and
  `SomeExchangeRate` changed. The `FromJSON` instances are backwards compatible
  with the old serializations, but the `ToJSON` instances will only generate the
  new format, which is the same as the old format except the leading strings
  `"Dense"`, `"Discrete"` and `"ExchangeRate"`, respectively, are not present in
  the rendered JSON array anymore. So, if you were manually relying on the
  `ToJSON` instance, please update your code.

* _COMPILER ASSISTED BREAKING CHANGE_. Changed the `Rep` suffix for a `Some`
  prefix Everywhere.  For example, `DenseRep` was renamed to `SomeDense`.

* _COMPILER ASSISTED BREAKING CHANGE_. Replaced the
  `someDenseAmountNumerator` and `someDenseAmountDenominator` `Integers` with a
  single `someDenseAmount` `Rational` number. Similarly for `someDiscreteScale`
  and `someExchangeRateRate`. The `mkSomeDense`, `someDiscreteScale` and
  `mkSomeDense` also take a `Rational` now.

* _COMPILER ASSISTED BREAKING CHANGE_. The `truncate`, `floor`, `celing` and
  `round` functions now return just `0` as a reminder if there's no significant
  reminder, instead of `Nothing`.

* Added instances for `serialise`.

* Added instances for `xmlbf`.

* Fixed `Show` instances so that surrounding parentheses are included when
  necessary.

* New currencies: Ripple, Litecoin, Ada, Monero.


# Version 0.3

* _COMPILER ASSISTED BREAKING CHANGE_. The `Data.Money` module was renamed to
  `Money`.

* _COMPILER ASSISTED BREAKING CHANGE_. The `Data.Money.Internal` module is not
  exposed anymore. All of its contents are now exported from the `Money` module.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed `discreteRep` to
  `toDiscreteRep`.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed `denseRep` to `toDenseRep`.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed `exchangeRateRep` to
  `toExchangeRateRep`.

* _COMPILER ASSISTED BREAKING CHANGE_. Renamed Iceleandic currency `"eyir"` to
  `"eyrir"`

* Remove upper bound constraints from all dependencies except `base`.

* Made dependency on `store` optional for the test suite, so that it can run on
  GHCJS.


# Version 0.2

* Cabal flags are now manual (`aeson`, `binary`, `bytes`, `cereal`, `deepseq`,
  `hashable`).

* Backwards compatibility with GHC 7.10.

* Fix `Store` instances and test them.


# Version 0.1

* Initial release.
