# Version HEAD

* BREAKING CHANGE: The `Data.Money` module was renamed to `Money`.

* BREAKING CHANGE: The `Data.Money.Internal` module is not exposed anymore. All
  of its contents are now exported from the `Money` module (with some renaming
  like `mkDiscreteRep` being now called `fromRawDiscreteRep`, etc.)

* BREAKING CHANGE: Renamed `discreteRep` to `toDiscreteRep`.

* BREAKING CHANGE: Renamed `denseRep` to `toDenseRep`.

* BREAKING CHANGE: Renamed `exchangeRateRep` to `toExchangeRateRep`.

* BREAKING CHANGE: Renamed Iceleandic currency `"eyir"` to `"eyrir"`

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
