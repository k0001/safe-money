{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Import this module qualified as follows:
--
-- @
-- import qualified Money
-- @
--
-- Note: This module exports many well-known currencies out-of-the-box, but
-- you are not limited to the currencies mentioned here. You can simply create
-- a new 'I.Scale' instance, and /voilà/. If you want to add a new currency to the
-- out-of-the-box offer, please request so in
-- https://github.com/k0001/safe-money/issues and the authors will see to it.
--
-- This module offers plenty of documentation, but for a deep explanation of
-- how all of the pieces fit together, please read
-- https://ren.zone/articles/safe-money
module Money
 ( -- * Dense monetary values
   I.Dense
 , I.dense
   -- * Discrete monetary values
 , I.Discrete
 , I.Discrete'
 , I.fromDiscrete
 , I.round
 , I.ceiling
 , I.floor
 , I.truncate
   -- * Currency scales
 , I.Scale
 , I.GoodScale
 , I.scale
   -- * Currency exchange
 , I.ExchangeRate
 , I.exchangeRate
 , I.fromExchangeRate
 , I.flipExchangeRate
 , I.exchange
   -- * Serializable representations
 , I.SomeDense
 , I.toSomeDense
 , I.mkSomeDense
 , I.fromSomeDense
 , I.withSomeDense
 , I.someDenseCurrency
 , I.someDenseAmount
 , I.SomeDiscrete
 , I.toSomeDiscrete
 , I.mkSomeDiscrete
 , I.fromSomeDiscrete
 , I.withSomeDiscrete
 , I.someDiscreteCurrency
 , I.someDiscreteScale
 , I.someDiscreteAmount
 , I.SomeExchangeRate
 , I.toSomeExchangeRate
 , I.mkSomeExchangeRate
 , I.fromSomeExchangeRate
 , I.withSomeExchangeRate
 , I.someExchangeRateSrcCurrency
 , I.someExchangeRateDstCurrency
 , I.someExchangeRateRate
 -- * Textual rendering
 , I.renderCurrency
 , I.renderDiscreteDecimal
 , I.renderRationalDecimal
 , I.renderThousands
 ) where

import qualified Money.Internal as I

--------------------------------------------------------------------------------
-- Currency scales

-- | United Arab Emirates dirham
type instance I.Scale "AED" "AED" = '(100, 1)
type instance I.Scale "AED" "dirham" = '(1, 1)
type instance I.Scale "AED" "fils" = '(100, 1)
-- | Afghan afghani
type instance I.Scale "AFN" "AFN" = '(100, 1)
type instance I.Scale "AFN" "afghani" = '(1, 1)
type instance I.Scale "AFN" "pul" = '(100, 1)
-- | Albanian lek
type instance I.Scale "ALL" "ALL" = '(100, 1)
type instance I.Scale "ALL" "lek" = '(1, 1)
type instance I.Scale "ALL" "qindarke" = '(100, 1)
-- | Armenian dram
type instance I.Scale "AMD" "AMD" = '(100, 1)
type instance I.Scale "AMD" "dram" = '(1, 1)
type instance I.Scale "AMD" "luma" = '(100, 1)
-- | Netherlands Antillean guilder
type instance I.Scale "ANG" "ANG" = '(100, 1)
type instance I.Scale "ANG" "guilder" = '(1, 1)
type instance I.Scale "ANG" "cent" = '(100, 1)
-- | Angolan kwanza
type instance I.Scale "AOA" "AOA" = '(100, 1)
type instance I.Scale "AOA" "kwanza" = '(1, 1)
type instance I.Scale "AOA" "centimo" = '(100, 1)
-- | Argentine peso
type instance I.Scale "ARS" "ARS" = '(100, 1)
type instance I.Scale "ARS" "peso" = '(1, 1)
type instance I.Scale "ARS" "centavo" = '(100, 1)
-- | Australian dollar
type instance I.Scale "AUD" "AUD" = '(100, 1)
type instance I.Scale "AUD" "dollar" = '(1, 1)
type instance I.Scale "AUD" "cent" = '(100, 1)
-- | Aruban florin
type instance I.Scale "AWG" "AWG" = '(100, 1)
type instance I.Scale "AWG" "florin" = '(1, 1)
type instance I.Scale "AWG" "cent" = '(100, 1)
-- | Azerbaijani manat
type instance I.Scale "AZN" "AZN" = '(100, 1)
type instance I.Scale "AZN" "manat" = '(1, 1)
type instance I.Scale "AZN" "qapik" = '(100, 1)
-- | Bosnia and Herzegovina convertible mark
type instance I.Scale "BAM" "BAM" = '(100, 1)
type instance I.Scale "BAM" "mark" = '(1, 1)
type instance I.Scale "BAM" "fening" = '(100, 1)
-- | Barbadian dollar
type instance I.Scale "BBD" "BBD" = '(100, 1)
type instance I.Scale "BBD" "dollar" = '(1, 1)
type instance I.Scale "BBD" "cent" = '(100, 1)
-- | Bangladeshi taka
type instance I.Scale "BDT" "BDT" = '(100, 1)
type instance I.Scale "BDT" "taka" = '(1, 1)
type instance I.Scale "BDT" "paisa" = '(100, 1)
-- | Bulgarian lev
type instance I.Scale "BGN" "BGN" = '(100, 1)
type instance I.Scale "BGN" "lev" = '(1, 1)
type instance I.Scale "BGN" "stotinka" = '(100, 1)
-- | Bahraini dinar
type instance I.Scale "BHD" "BHD" = '(1000, 1)
type instance I.Scale "BHD" "dinar" = '(1, 1)
type instance I.Scale "BHD" "fils" = '(1000, 1)
-- | Burundi franc
type instance I.Scale "BIF" "BIF" = '(100, 1)
type instance I.Scale "BIF" "franc" = '(1, 1)
type instance I.Scale "BIF" "centime" = '(100, 1)
-- | Bermudian dollar
type instance I.Scale "BMD" "BMD" = '(100, 1)
type instance I.Scale "BMD" "dollar" = '(1, 1)
type instance I.Scale "BMD" "cent" = '(100, 1)
-- | Brunei dollar
type instance I.Scale "BND" "BND" = '(100, 1)
type instance I.Scale "BND" "dollar" = '(1, 1)
type instance I.Scale "BND" "sen" = '(100, 1)
-- | Bolivian boliviano
type instance I.Scale "BOB" "BOB" = '(100, 1)
type instance I.Scale "BOB" "boliviano" = '(1, 1)
type instance I.Scale "BOB" "centavo" = '(100, 1)
-- | Bolivian Mvdol
type instance I.Scale "BOV" "BOV" = '(100, 1)
-- | Brazilian real
type instance I.Scale "BRL" "BRL" = '(100, 1)
type instance I.Scale "BRL" "real" = '(1, 1)
type instance I.Scale "BRL" "centavo" = '(100, 1)
-- | Bahamian dollar
type instance I.Scale "BSD" "BSD" = '(100, 1)
type instance I.Scale "BSD" "dollar" = '(1, 1)
type instance I.Scale "BSD" "cent" = '(1, 1)
-- | Bhutanese ngultrum
type instance I.Scale "BTN" "BTN" = '(100, 1)
type instance I.Scale "BTN" "ngultrum" = '(1, 1)
type instance I.Scale "BTN" "chetrum" = '(100, 1)
-- | Botswana pula
type instance I.Scale "BWP" "BWP" = '(100, 1)
type instance I.Scale "BWP" "pula" = '(1, 1)
type instance I.Scale "BWP" "thebe" = '(100, 1)
-- | Belarusian ruble
type instance I.Scale "BYN" "BYN" = '(100, 1)
-- | Belarusian ruble
type instance I.Scale "BYR" "BYR" = '(100, 1)
type instance I.Scale "BYR" "ruble" = '(1, 1)
type instance I.Scale "BYR" "kapyeyka" = '(100, 1)
-- | Belize dollar
type instance I.Scale "BZD" "BZD" = '(100, 1)
type instance I.Scale "BZD" "dollar" = '(1, 1)
type instance I.Scale "BZD" "cent" = '(100, 1)
-- | Canadian dollar
type instance I.Scale "CAD" "CAD" = '(100, 1)
type instance I.Scale "CAD" "dollar" = '(1, 1)
type instance I.Scale "CAD" "cent" = '(100, 1)
-- | Congolese franc
type instance I.Scale "CDF" "CDF" = '(100, 1)
type instance I.Scale "CDF" "franc" = '(1, 1)
type instance I.Scale "CDF" "centime" = '(100, 1)
-- | WIR euro
type instance I.Scale "CHE" "CHE" = '(100, 1)
-- | Swiss franc
type instance I.Scale "CHF" "CHF" = '(100, 1)
type instance I.Scale "CHF" "franc" = '(1, 1)
type instance I.Scale "CHF" "rappen" = '(100, 1)
-- | WIR franc
type instance I.Scale "CHW" "CHW" = '(100, 1)
-- | Chilean unidad de fomento
type instance I.Scale "CLF" "CLF" = '(100, 1)
-- | Chilean peso
type instance I.Scale "CLP" "CLP" = '(100, 1)
type instance I.Scale "CLP" "peso" = '(1, 1)
type instance I.Scale "CLP" "centavo" = '(100, 1)
-- | Chinese Renminbi
type instance I.Scale "CNY" "CNY" = '(100, 1)
type instance I.Scale "CNY" "yuan" = '(1, 1)
type instance I.Scale "CNY" "fen" = '(100, 1)
-- | Colombian peso
type instance I.Scale "COP" "COP" = '(100, 1)
type instance I.Scale "COP" "peso" = '(1, 1)
type instance I.Scale "COP" "centavo" = '(100, 1)
-- | Colombian unidad de valor real
type instance I.Scale "COU" "COU" = '(100, 1)
-- | Costa Rican colon
type instance I.Scale "CRC" "CRC" = '(100, 1)
type instance I.Scale "CRC" "colon" = '(1, 1)
type instance I.Scale "CRC" "centimo" = '(100, 1)
-- | Cuban peso convertible
type instance I.Scale "CUC" "CUC" = '(100, 1)
type instance I.Scale "CUC" "peso" = '(1, 1)
type instance I.Scale "CUC" "centavo" = '(100, 1)
-- | Cuban peso
type instance I.Scale "CUP" "CUP" = '(100, 1)
type instance I.Scale "CUP" "peso" = '(1, 1)
type instance I.Scale "CUP" "centavo" = '(100, 1)
-- | Cape Verdean escudo
type instance I.Scale "CVE" "CVE" = '(100, 1)
type instance I.Scale "CVE" "escudo" = '(1, 1)
type instance I.Scale "CVE" "centavo" = '(100, 1)
-- | Czech koruna
type instance I.Scale "CZK" "CZK" = '(100, 1)
type instance I.Scale "CZK" "koruna" = '(1, 1)
type instance I.Scale "CZK" "haler" = '(100, 1)
-- | Djiboutian franc
type instance I.Scale "DJF" "DJF" = '(100, 1)
type instance I.Scale "DJF" "franc" = '(1, 1)
type instance I.Scale "DJF" "centime" = '(100, 1)
-- | Danish krone
type instance I.Scale "DKK" "DKK" = '(100, 1)
type instance I.Scale "DKK" "krone" = '(1, 1)
type instance I.Scale "DKK" "ore" = '(100, 1)
-- | Dominican peso
type instance I.Scale "DOP" "DOP" = '(100, 1)
type instance I.Scale "DOP" "peso" = '(1, 1)
type instance I.Scale "DOP" "centavo" = '(100, 1)
-- | Algerian dinar
type instance I.Scale "DZD" "DZD" = '(100, 1)
type instance I.Scale "DZD" "dinar" = '(1, 1)
type instance I.Scale "DZD" "santeem" = '(100, 1)
-- | Egyptian pound
type instance I.Scale "EGP" "EGP" = '(100, 1)
type instance I.Scale "EGP" "pound" = '(1, 1)
type instance I.Scale "EGP" "piastre" = '(100, 1)
-- | Eritrean nakfa
type instance I.Scale "ERN" "ERN" = '(100, 1)
type instance I.Scale "ERN" "nafka" = '(1, 1)
type instance I.Scale "ERN" "cent" = '(100, 1)
-- | Ethiopian birr
type instance I.Scale "ETB" "ETB" = '(100, 1)
type instance I.Scale "ETB" "birr" = '(1, 1)
type instance I.Scale "ETB" "santim" = '(100, 1)
-- | European euro
type instance I.Scale "EUR" "EUR" = '(100, 1)
type instance I.Scale "EUR" "euro" = '(1, 1)
type instance I.Scale "EUR" "cent" = '(100, 1)
-- | Fijian dollar
type instance I.Scale "FJD" "FJD" = '(100, 1)
-- | Falkland Islands pound
type instance I.Scale "FKP" "FKP" = '(100, 1)
type instance I.Scale "FKP" "pound" = '(1, 1)
type instance I.Scale "FKP" "penny" = '(100, 1)
-- | Pound sterling
type instance I.Scale "GBP" "GBP" = '(100, 1)
type instance I.Scale "GBP" "pound" = '(1, 1)
type instance I.Scale "GBP" "penny" = '(100, 1)
-- | Georgian lari
type instance I.Scale "GEL" "GEL" = '(100, 1)
type instance I.Scale "GEL" "lari" = '(1, 1)
type instance I.Scale "GEL" "tetri" = '(100, 1)
-- | Ghanaian cedi
type instance I.Scale "GHS" "GHS" = '(100, 1)
type instance I.Scale "GHS" "cedi" = '(1, 1)
type instance I.Scale "GHS" "pesewa" = '(100, 1)
-- | Gibraltar pound
type instance I.Scale "GIP" "GIP" = '(100, 1)
type instance I.Scale "GIP" "pound" = '(1, 1)
type instance I.Scale "GIP" "penny" = '(100, 1)
-- | Gambian dalasi
type instance I.Scale "GMD" "GMD" = '(100, 1)
type instance I.Scale "GMD" "dalasi" = '(1, 1)
type instance I.Scale "GMD" "butut" = '(100, 1)
-- | Guinean franc
type instance I.Scale "GNF" "GNF" = '(100, 1)
type instance I.Scale "GNF" "franc" = '(1, 1)
type instance I.Scale "GNF" "centime" = '(100, 1)
-- | Guatemalan quetzal
type instance I.Scale "GTQ" "GTQ" = '(100, 1)
type instance I.Scale "GTQ" "quetzal" = '(1, 1)
type instance I.Scale "GTQ" "centavo" = '(100, 1)
-- | Guyanese dollar
type instance I.Scale "GYD" "GYD" = '(100, 1)
type instance I.Scale "GYD" "dollar" = '(1, 1)
type instance I.Scale "GYD" "cent" = '(100, 1)
-- | Hong Kong dollar
type instance I.Scale "HKD" "HKD" = '(100, 1)
type instance I.Scale "HKD" "dollar" = '(1, 1)
type instance I.Scale "HKD" "cent" = '(100, 1)
-- | Honduran lempira
type instance I.Scale "HNL" "HNL" = '(100, 1)
type instance I.Scale "HNL" "lempira" = '(1, 1)
type instance I.Scale "HNL" "centavo" = '(100, 1)
-- | Croatian kuna
type instance I.Scale "HRK" "HRK" = '(100, 1)
type instance I.Scale "HRK" "kuna" = '(1, 1)
type instance I.Scale "HRK" "lipa" = '(100, 1)
-- | Haitian gourde
type instance I.Scale "HTG" "HTG" = '(100, 1)
type instance I.Scale "HTG" "gourde" = '(1, 1)
type instance I.Scale "HTG" "centime" = '(1, 1)
-- | Hungarian forint
type instance I.Scale "HUF" "HUF" = '(100, 1)
type instance I.Scale "HUF" "forint" = '(1, 1)
type instance I.Scale "HUF" "filler" = '(100, 1)
-- | Indonesian rupiah
type instance I.Scale "IDR" "IDR" = '(100, 1)
type instance I.Scale "IDR" "rupiah" = '(1, 1)
type instance I.Scale "IDR" "sen" = '(100, 1)
-- | Israeli new shekel
type instance I.Scale "ILS" "ILS" = '(100, 1)
type instance I.Scale "ILS" "shekel" = '(1, 1)
type instance I.Scale "ILS" "agora" = '(100, 1)
-- | Indian rupee
type instance I.Scale "INR" "INR" = '(100, 1)
type instance I.Scale "INR" "rupee" = '(1, 1)
type instance I.Scale "INR" "paisa" = '(100, 1)
-- | Iraqi dinar
type instance I.Scale "IQD" "IQD" = '(1000, 1)
type instance I.Scale "IQD" "dinar" = '(1, 1)
type instance I.Scale "IQD" "fils" = '(1000, 1)
-- | Iranian rial
type instance I.Scale "IRR" "IRR" = '(100, 1)
type instance I.Scale "IRR" "rial" = '(1, 1)
type instance I.Scale "IRR" "dinar" = '(100, 1)
-- | Icelandic króna
type instance I.Scale "ISK" "ISK" = '(100, 1)
type instance I.Scale "ISK" "krona" = '(1, 1)
type instance I.Scale "ISK" "eyrir" = '(100, 1)
-- | Jamaican dollar
type instance I.Scale "JMD" "JMD" = '(100, 1)
type instance I.Scale "JMD" "dollar" = '(1, 1)
type instance I.Scale "JMD" "cent" = '(100, 1)
-- | Jordanian dinar
type instance I.Scale "JOD" "JOD" = '(100, 1)
type instance I.Scale "JOD" "dinar" = '(1, 1)
type instance I.Scale "JOD" "piastre" = '(100, 1)
-- | Japanese yen
type instance I.Scale "JPY" "JPY" = '(100, 1)
type instance I.Scale "JPY" "yen" = '(1, 1)
type instance I.Scale "JPY" "sen" = '(100, 1)
-- | Kenyan shilling
type instance I.Scale "KES" "KES" = '(100, 1)
type instance I.Scale "KES" "shilling" = '(1, 1)
type instance I.Scale "KES" "cent" = '(100, 1)
-- | Kyrgyzstani som
type instance I.Scale "KGS" "KGS" = '(100, 1)
type instance I.Scale "KGS" "som" = '(1, 1)
type instance I.Scale "KGS" "tyiyn" = '(100, 1)
-- | Cambodian riel
type instance I.Scale "KHR" "KHR" = '(100, 1)
type instance I.Scale "KHR" "riel" = '(1, 1)
type instance I.Scale "KHR" "sen" = '(100, 1)
-- | Comorian franc
type instance I.Scale "KMF" "KMF" = '(100, 1)
type instance I.Scale "KMF" "franc" = '(1, 1)
type instance I.Scale "KMF" "centime" = '(100, 1)
-- | North Korean won
type instance I.Scale "KPW" "KPW" = '(100, 1)
type instance I.Scale "KPW" "won" = '(1, 1)
type instance I.Scale "KPW" "chon" = '(100, 1)
-- | South Korean won
type instance I.Scale "KRW" "KRW" = '(100, 1)
type instance I.Scale "KRW" "won" = '(1, 1)
type instance I.Scale "KRW" "jeon" = '(100, 1)
-- | Kuwaiti dinar
type instance I.Scale "KWD" "KWD" = '(1000, 1)
type instance I.Scale "KWD" "dinar" = '(1, 1)
type instance I.Scale "KWD" "fils" = '(1000, 1)
-- | Cayman Islands dollar
type instance I.Scale "KYD" "KYD" = '(100, 1)
type instance I.Scale "KYD" "dollar" = '(1, 1)
type instance I.Scale "KYD" "cent" = '(100, 1)
-- | Kazakhstani tenge
type instance I.Scale "KZT" "KZT" = '(100, 1)
type instance I.Scale "KZT" "tenge" = '(1, 1)
type instance I.Scale "KZT" "tiyin" = '(100, 1)
-- | Lao kip
type instance I.Scale "LAK" "LAK" = '(100, 1)
type instance I.Scale "LAK" "kip" = '(1, 1)
type instance I.Scale "LAK" "att" = '(100, 1)
-- | Lebanese pound
type instance I.Scale "LBP" "LBP" = '(100, 1)
type instance I.Scale "LBP" "pound" = '(1, 1)
type instance I.Scale "LBP" "piastre" = '(100, 1)
-- | Sri Lankan rupee
type instance I.Scale "LKR" "LKR" = '(100, 1)
type instance I.Scale "LKR" "rupee" = '(1, 1)
type instance I.Scale "LKR" "cent" = '(100, 1)
-- | Liberian dollar
type instance I.Scale "LRD" "LRD" = '(100, 1)
type instance I.Scale "LRD" "dollar" = '(1, 1)
type instance I.Scale "LRD" "cent" = '(100, 1)
-- | Lesotho loti
type instance I.Scale "LSL" "LSL" = '(100, 1)
type instance I.Scale "LSL" "loti" = '(1, 1)
type instance I.Scale "LSL" "sente" = '(100, 1)
-- | Libyan dinar
type instance I.Scale "LYD" "LYD" = '(100, 1)
type instance I.Scale "LYD" "dinar" = '(1, 1)
type instance I.Scale "LYD" "dirham" = '(1000, 1)
-- | Moroccan dirham
type instance I.Scale "MAD" "MAD" = '(100, 1)
type instance I.Scale "MAD" "dirham" = '(1, 1)
type instance I.Scale "MAD" "centime" = '(100, 1)
-- | Moldovan leu
type instance I.Scale "MDL" "MDL" = '(100, 1)
type instance I.Scale "MDL" "leu" = '(100, 1)
type instance I.Scale "MDL" "ban" = '(100, 1)
-- | Malagasy ariary
type instance I.Scale "MGA" "MGA" = '(5, 1)
type instance I.Scale "MGA" "ariary" = '(1, 1)
type instance I.Scale "MGA" "iraimbilanja" = '(5, 1)
-- | Macedonian denar
type instance I.Scale "MKD" "MKD" = '(100, 1)
type instance I.Scale "MKD" "denar" = '(1, 1)
type instance I.Scale "MKD" "deni" = '(100, 1)
-- | Myanmar kyat
type instance I.Scale "MMK" "MMK" = '(100, 1)
type instance I.Scale "MMK" "kyat" = '(1, 1)
type instance I.Scale "MMK" "pya" = '(100, 1)
-- | Mongolian tugrik
type instance I.Scale "MNT" "MNT" = '(100, 1)
type instance I.Scale "MNT" "tugrik" = '(1, 1)
type instance I.Scale "MNT" "mongo" = '(100, 1)
-- | Macanese pataca
type instance I.Scale "MOP" "MOP" = '(100, 1)
type instance I.Scale "MOP" "pataca" = '(1, 1)
type instance I.Scale "MOP" "avo" = '(100, 1)
-- | Mauritanian ouguiya
type instance I.Scale "MRO" "MRO" = '(5, 1)
type instance I.Scale "MRO" "ouguiya" = '(1, 1)
type instance I.Scale "MRO" "khoums" = '(5, 1)
-- | Mauritian rupee
type instance I.Scale "MUR" "MUR" = '(100, 1)
type instance I.Scale "MUR" "rupee" = '(1, 1)
type instance I.Scale "MUR" "cent" = '(100, 1)
-- | Maldivian rufiyaa
type instance I.Scale "MVR" "MVR" = '(100, 1)
type instance I.Scale "MVR" "rufiyaa" = '(1, 1)
type instance I.Scale "MVR" "laari" = '(100, 1)
-- | Malawian kwacha
type instance I.Scale "MWK" "MWK" = '(100, 1)
type instance I.Scale "MWK" "kwacha" = '(1, 1)
type instance I.Scale "MWK" "tambala" = '(100, 1)
-- | Mexican peso
type instance I.Scale "MXN" "MXN" = '(100, 1)
type instance I.Scale "MXN" "peso" = '(1, 1)
type instance I.Scale "MXN" "centavo" = '(100, 1)
-- | Mexican unidad de inversion
type instance I.Scale "MXV" "MXV" = '(100, 1)
-- | Malaysian ringgit
type instance I.Scale "MYR" "MYR" = '(100, 1)
type instance I.Scale "MYR" "ringgit" = '(1, 1)
type instance I.Scale "MYR" "sen" = '(100, 1)
-- | Mozambican metical
type instance I.Scale "MZN" "MZN" = '(100, 1)
type instance I.Scale "MZN" "metical" = '(1, 1)
type instance I.Scale "MZN" "centavo" = '(100, 1)
-- | Namibian dollar
type instance I.Scale "NAD" "NAD" = '(100, 1)
type instance I.Scale "NAD" "dollar" = '(1, 1)
type instance I.Scale "NAD" "cent" = '(100, 1)
-- | Nigerian naira
type instance I.Scale "NGN" "NGN" = '(100, 1)
type instance I.Scale "NGN" "naira" = '(1, 1)
type instance I.Scale "NGN" "kobo" = '(100, 1)
-- | Nicaraguan cordoba
type instance I.Scale "NIO" "NIO" = '(100, 1)
type instance I.Scale "NIO" "cordoba" = '(1, 1)
type instance I.Scale "NIO" "centavo" = '(100, 1)
-- | Norwegian krone
type instance I.Scale "NOK" "NOK" = '(100, 1)
type instance I.Scale "NOK" "krone" = '(1, 1)
type instance I.Scale "NOK" "ore" = '(100, 1)
-- | Nepalese rupee
type instance I.Scale "NPR" "NPR" = '(100, 1)
type instance I.Scale "NPR" "rupee" = '(1, 1)
type instance I.Scale "NPR" "paisa" = '(100, 1)
-- | New Zealand dollar
type instance I.Scale "NZD" "NZD" = '(100, 1)
type instance I.Scale "NZD" "dollar" = '(1, 1)
type instance I.Scale "NZD" "cent" = '(100, 1)
-- | Omani rial
type instance I.Scale "OMR" "OMR" = '(1000, 1)
type instance I.Scale "OMR" "rial" = '(1, 1)
type instance I.Scale "OMR" "baisa" = '(1000, 1)
-- | Panamenian balboa
type instance I.Scale "PAB" "PAB" = '(100, 1)
type instance I.Scale "PAB" "balboa" = '(1, 1)
type instance I.Scale "PAB" "centesimo" = '(100, 1)
-- | Peruvian sol
type instance I.Scale "PEN" "PEN" = '(100, 1)
type instance I.Scale "PEN" "sol" = '(1, 1)
type instance I.Scale "PEN" "centimo" = '(100, 1)
-- | Papua New Guinean kina
type instance I.Scale "PGK" "PGK" = '(100, 1)
type instance I.Scale "PGK" "kina" = '(1, 1)
type instance I.Scale "PGK" "toea" = '(100, 1)
-- | Philippine peso
type instance I.Scale "PHP" "PHP" = '(100, 1)
type instance I.Scale "PHP" "peso" = '(1, 1)
type instance I.Scale "PHP" "centavo" = '(100, 1)
-- | Pakistani rupee
type instance I.Scale "PKR" "PKR" = '(100, 1)
type instance I.Scale "PKR" "rupee" = '(1, 1)
type instance I.Scale "PKR" "paisa" = '(100, 1)
-- | Polish zloty
type instance I.Scale "PLN" "PLN" = '(100, 1)
type instance I.Scale "PLN" "zloty" = '(1, 1)
type instance I.Scale "PLN" "grosz" = '(100, 1)
-- | Paraguayan guarani
type instance I.Scale "PYG" "PYG" = '(100, 1)
type instance I.Scale "PYG" "guarani" = '(1, 1)
type instance I.Scale "PYG" "centimo" = '(100, 1)
-- | Qatari riyal
type instance I.Scale "QAR" "QAR" = '(100, 1)
type instance I.Scale "QAR" "riyal" = '(1, 1)
type instance I.Scale "QAR" "dirham" = '(100, 1)
-- | Romanian leu
type instance I.Scale "RON" "RON" = '(100, 1)
type instance I.Scale "RON" "leu" = '(1, 1)
type instance I.Scale "RON" "ban" = '(100, 1)
-- | Serbian dinar
type instance I.Scale "RSD" "RSD" = '(100, 1)
type instance I.Scale "RSD" "dinar" = '(1, 1)
type instance I.Scale "RSD" "para" = '(100, 1)
-- | Russian ruble
type instance I.Scale "RUB" "RUB" = '(100, 1)
type instance I.Scale "RUB" "ruble" = '(1, 1)
type instance I.Scale "RUB" "kopek" = '(100, 1)
-- | Rwandan franc
type instance I.Scale "RWF" "RWF" = '(100, 1)
type instance I.Scale "RWF" "franc" = '(1, 1)
type instance I.Scale "RWF" "centime" = '(100, 1)
-- | Saudi Arabian riyal
type instance I.Scale "SAR" "SAR" = '(100, 1)
type instance I.Scale "SAR" "riyal" = '(1, 1)
type instance I.Scale "SAR" "halala" = '(100, 1)
-- | Solomon Islands dollar
type instance I.Scale "SBD" "SBD" = '(100, 1)
type instance I.Scale "SBD" "dollar" = '(100, 1)
type instance I.Scale "SBD" "cent" = '(100, 1)
-- | Seychellois rupee
type instance I.Scale "SCR" "SCR" = '(100, 1)
type instance I.Scale "SCR" "rupee" = '(1, 1)
type instance I.Scale "SCR" "cent" = '(100, 1)
-- | Sudanese pound
type instance I.Scale "SDG" "SDG" = '(100, 1)
type instance I.Scale "SDG" "pound" = '(1, 1)
type instance I.Scale "SDG" "piastre" = '(100, 1)
-- | Swedish krona
type instance I.Scale "SEK" "SEK" = '(100, 1)
type instance I.Scale "SEK" "krona" = '(1, 1)
type instance I.Scale "SEK" "ore" = '(100, 1)
-- | Singapore dollar
type instance I.Scale "SGD" "SGD" = '(100, 1)
type instance I.Scale "SGD" "dollar" = '(1, 1)
type instance I.Scale "SGD" "cent" = '(100, 1)
-- | Saint Helena pound
type instance I.Scale "SHP" "SHP" = '(100, 1)
type instance I.Scale "SHP" "pound" = '(1, 1)
type instance I.Scale "SHP" "penny" = '(100, 1)
-- | Sierra Leonean leone
type instance I.Scale "SLL" "SLL" = '(100, 1)
type instance I.Scale "SLL" "leone" = '(1, 1)
type instance I.Scale "SLL" "cent" = '(100, 1)
-- | Somali shilling
type instance I.Scale "SOS" "SOS" = '(100, 1)
type instance I.Scale "SOS" "shilling" = '(1, 1)
type instance I.Scale "SOS" "cent" = '(100, 1)
-- | Surinamese dollar
type instance I.Scale "SRD" "SRD" = '(100, 1)
type instance I.Scale "SRD" "dollar" = '(1, 1)
type instance I.Scale "SRD" "cent" = '(100, 1)
-- | South Sudanese pound
type instance I.Scale "SSP" "SSP" = '(100, 1)
type instance I.Scale "SSP" "pound" = '(1, 1)
type instance I.Scale "SSP" "piastre" = '(100, 1)
-- | Sao Tome and Principe dobra
type instance I.Scale "STD" "STD" = '(100, 1)
type instance I.Scale "STD" "dobra" = '(1, 1)
type instance I.Scale "STD" "centimo" = '(100, 1)
-- | Salvadoran colon
type instance I.Scale "SVC" "SVC" = '(100, 1)
type instance I.Scale "SVC" "colon" = '(1, 1)
type instance I.Scale "SVC" "centavo" = '(100, 1)
-- | Syrian pound
type instance I.Scale "SYP" "SYP" = '(100, 1)
type instance I.Scale "SYP" "pound" = '(1, 1)
type instance I.Scale "SYP" "piastre" = '(100, 1)
-- | Swazi lilangeni
type instance I.Scale "SZL" "SZL" = '(100, 1)
type instance I.Scale "SZL" "lilangeni" = '(1, 1)
type instance I.Scale "SZL" "cent" = '(100, 1)
-- | Thai baht
type instance I.Scale "THB" "THB" = '(100, 1)
type instance I.Scale "THB" "baht" = '(1, 1)
type instance I.Scale "THB" "satang" = '(100, 1)
-- | Tajikistani somoni
type instance I.Scale "TJS" "TJS" = '(100, 1)
type instance I.Scale "TJS" "somoni" = '(1, 1)
type instance I.Scale "TJS" "diram" = '(100, 1)
-- | Turkmen manat
type instance I.Scale "TMT" "TMT" = '(100, 1)
type instance I.Scale "TMT" "manat" = '(1, 1)
type instance I.Scale "TMT" "tennesi" = '(100, 1)
-- | Tunisian dinar
type instance I.Scale "TND" "TND" = '(1000, 1)
type instance I.Scale "TND" "dinar" = '(1, 1)
type instance I.Scale "TND" "millime" = '(1000, 1)
-- | Tongan pa’anga
type instance I.Scale "TOP" "TOP" = '(100, 1)
type instance I.Scale "TOP" "pa'anga" = '(1, 1)
type instance I.Scale "TOP" "seniti" = '(100, 1)
-- | Turkish lira
type instance I.Scale "TRY" "TRY" = '(100, 1)
type instance I.Scale "TRY" "lira" = '(1, 1)
type instance I.Scale "TRY" "kurus" = '(100, 1)
-- | Tobago Trinidad and Tobago dollar
type instance I.Scale "TTD" "TTD" = '(100, 1)
type instance I.Scale "TTD" "dollar" = '(1, 1)
type instance I.Scale "TTD" "cent" = '(100, 1)
-- | New Taiwan dollar
type instance I.Scale "TWD" "TWD" = '(100, 1)
type instance I.Scale "TWD" "dollar" = '(1, 1)
type instance I.Scale "TWD" "cent" = '(100, 1)
-- | Tanzanian shilling
type instance I.Scale "TZS" "TZS" = '(100, 1)
type instance I.Scale "TZS" "shilling" = '(1, 1)
type instance I.Scale "TZS" "cent" = '(100, 1)
-- | Ukrainian hryvnia
type instance I.Scale "UAH" "UAH" = '(100, 1)
type instance I.Scale "UAH" "hryvnia" = '(1, 1)
type instance I.Scale "UAH" "kopiyka" = '(100, 1)
-- | Ugandan shilling
type instance I.Scale "UGX" "UGX" = '(100, 1)
type instance I.Scale "UGX" "shilling" = '(1, 1)
type instance I.Scale "UGX" "cent" = '(100, 1)
-- | United States dollar
type instance I.Scale "USD" "USD" = '(100, 1)
type instance I.Scale "USD" "dollar" = '(1, 1)
type instance I.Scale "USD" "cent" = '(100, 1)
-- | United States dollar (next day)
type instance I.Scale "USN" "USN" = '(100, 1)
-- | Uruguayan peso en unidades
type instance I.Scale "UYI" "UYI" = '(100, 1)
-- | Uruguayan peso
type instance I.Scale "UYU" "UYU" = '(100, 1)
type instance I.Scale "UYU" "peso" = '(1, 1)
type instance I.Scale "UYU" "centesimo" = '(100, 1)
-- | Uzbekistani som
type instance I.Scale "UZS" "UZS" = '(100, 1)
type instance I.Scale "UZS" "som" = '(1, 1)
type instance I.Scale "UZS" "tiyin" = '(100, 1)
-- | Venezuelan bolivar
type instance I.Scale "VEF" "VEF" = '(100, 1)
type instance I.Scale "VEF" "bolivar" = '(1, 1)
type instance I.Scale "VEF" "centimo" = '(100, 1)
-- | Vietnamese dong
type instance I.Scale "VND" "VND" = '(10, 1)
type instance I.Scale "VND" "dong" = '(1, 1)
type instance I.Scale "VND" "hao" = '(10, 1)
-- | Vanuatu vatu
type instance I.Scale "VUV" "VUV" = '(1, 1)
type instance I.Scale "VUV" "vatu" = '(1, 1)
-- | Samoan tālā
type instance I.Scale "WST" "WST" = '(100, 1)
type instance I.Scale "WST" "tala" = '(1, 1)
type instance I.Scale "WST" "sene" = '(100, 1)
-- | Central African CFA franc
type instance I.Scale "XAF" "XAF" = '(100, 1)
type instance I.Scale "XAF" "franc" = '(1, 1)
type instance I.Scale "XAF" "centime" = '(100, 1)
-- | East Caribbean dollar
type instance I.Scale "XCD" "XCD" = '(100, 1)
type instance I.Scale "XCD" "dollar" = '(1, 1)
type instance I.Scale "XCD" "cent" = '(100, 1)
-- | International Monetary Fund Special Drawing Right
type instance I.Scale "XDR" "XDR" = '(100, 1)
-- | West African CFA franc
type instance I.Scale "XOF" "XOF" = '(100, 1)
type instance I.Scale "XOF" "franc" = '(1, 1)
type instance I.Scale "XOF" "centime" = '(100, 1)
-- | CFP franc
type instance I.Scale "XPF" "XPF" = '(100, 1)
type instance I.Scale "XPF" "franc" = '(1, 1)
type instance I.Scale "XPF" "centime" = '(100, 1)
-- | Sucre
type instance I.Scale "XSU" "XSU" = '(100, 1)
-- | African Development Bank unit of account
type instance I.Scale "XUA" "XUA" = '(100, 1)
-- | Yemeni rial
type instance I.Scale "YER" "YER" = '(100, 1)
type instance I.Scale "YER" "rial" = '(1, 1)
type instance I.Scale "YER" "fils" = '(100, 1)
-- | South African rand
type instance I.Scale "ZAR" "ZAR" = '(100, 1)
type instance I.Scale "ZAR" "rand" = '(1, 1)
type instance I.Scale "ZAR" "cent" = '(100, 1)
-- | Zambian kwacha
type instance I.Scale "ZMW" "ZMW" = '(100, 1)
type instance I.Scale "ZMW" "kwacha" = '(1, 1)
type instance I.Scale "ZMW" "ngwee" = '(100, 1)
-- | Zimbawe dollar
type instance I.Scale "ZWL" "ZWL" = '(100, 1)
type instance I.Scale "ZWL" "dollar" = '(1, 1)
type instance I.Scale "ZWL" "cent" = '(100, 1)

-- | Gold. No canonical smallest unit. Unusable instance.
type instance I.Scale "XAU" "XAU" = I.ErrScaleNonCanonical "XAU"
type instance I.Scale "XAU" "troy-ounce" = '(1, 1)
type instance I.Scale "XAU" "grain" = '(480, 1)
type instance I.Scale "XAU" "milligrain" = '(480000, 1)
type instance I.Scale "XAU" "micrograin" = '(480000000, 1)
type instance I.Scale "XAU" "kilogram" = '(31103477, 1000000000)
type instance I.Scale "XAU" "gram" = '(31103477, 1000000)
type instance I.Scale "XAU" "milligram" = '(31103477, 1000)
type instance I.Scale "XAU" "microgram" = '(31103477, 1)

-- | Silver. No canonical smallest unit. Unusable instance.
type instance I.Scale "XAG" "XAG" = I.ErrScaleNonCanonical "XAG"
type instance I.Scale "XAG" "troy-ounce" = '(1, 1)
type instance I.Scale "XAG" "grain" = '(480, 1)
type instance I.Scale "XAG" "milligrain" = '(480000, 1)
type instance I.Scale "XAG" "micrograin" = '(480000000, 1)
type instance I.Scale "XAG" "kilogram" = '(31103477, 1000000000)
type instance I.Scale "XAG" "gram" = '(31103477, 1000000)
type instance I.Scale "XAG" "milligram" = '(31103477, 1000)
type instance I.Scale "XAG" "microgram" = '(31103477, 1)

-- | Palladium. No canonical smallest unit. Unusable instance.
type instance I.Scale "XPD" "XPD" = I.ErrScaleNonCanonical "XPD"
type instance I.Scale "XPD" "troy-ounce" = '(1, 1)
type instance I.Scale "XPD" "grain" = '(480, 1)
type instance I.Scale "XPD" "milligrain" = '(480000, 1)
type instance I.Scale "XPD" "micrograin" = '(480000000, 1)
type instance I.Scale "XPD" "kilogram" = '(31103477, 1000000000)
type instance I.Scale "XPD" "gram" = '(31103477, 1000000)
type instance I.Scale "XPD" "milligram" = '(31103477, 1000)
type instance I.Scale "XPD" "microgram" = '(31103477, 1)

-- | Platinum. No canonical smallest unit. Unusable instance.
type instance I.Scale "XPT" "XPT" = I.ErrScaleNonCanonical "XPT"
type instance I.Scale "XPT" "troy-ounce" = '(1, 1)
type instance I.Scale "XPT" "grain" = '(480, 1)
type instance I.Scale "XPT" "milligrain" = '(480000, 1)
type instance I.Scale "XPT" "micrograin" = '(480000000, 1)
type instance I.Scale "XPT" "kilogram" = '(31103477, 1000000000)
type instance I.Scale "XPT" "gram" = '(31103477, 1000000)
type instance I.Scale "XPT" "milligram" = '(31103477, 1000)
type instance I.Scale "XPT" "microgram" = '(31103477, 1)

-- | Bitcoin
type instance I.Scale "BTC" "BTC" = '(100000000, 1)
type instance I.Scale "BTC" "bitcoin" = '(1, 1)
type instance I.Scale "BTC" "satoshi" = '(100000000, 1)

-- | Bitcoin
type instance I.Scale "XBT" "XBT" = '(100000000, 1)
type instance I.Scale "XBT" "bitcoin" = '(1, 1)
type instance I.Scale "XBT" "satoshi" = '(100000000, 1)

-- | Ether
type instance I.Scale "ETH" "ETH" = '(1000000000000000000, 1)
type instance I.Scale "ETH" "ether" = '(1, 1)
type instance I.Scale "ETH" "kwei" = '(1000, 1)
type instance I.Scale "ETH" "babbage" = '(1000, 1)
type instance I.Scale "ETH" "mwei" = '(1000000, 1)
type instance I.Scale "ETH" "lovelace" = '(1000000, 1)
type instance I.Scale "ETH" "gwei" = '(1000000000, 1)
type instance I.Scale "ETH" "shannon" = '(1000000000, 1)
type instance I.Scale "ETH" "microether" = '(1000000000000, 1)
type instance I.Scale "ETH" "szabo" = '(1000000000000, 1)
type instance I.Scale "ETH" "finney" = '(1000000000000000, 1)
type instance I.Scale "ETH" "milliether" = '(1000000000000000, 1)
type instance I.Scale "ETH" "wei" = '(1000000000000000000, 1)

-- | Ada
type instance I.Scale "ADA" "ADA" = '(1000000, 1)
type instance I.Scale "ADA" "ada" = '(1, 1)
type instance I.Scale "ADA" "lovelace" = '(1000000, 1)

-- | Litecoin
type instance I.Scale "LTC" "LTC" = '(100000000, 1)
type instance I.Scale "LTC" "litecoin" = '(1, 1)
type instance I.Scale "LTC" "lite" = '(1000, 1)
type instance I.Scale "LTC" "photon" = '(100000000, 1)

-- | Ripple
type instance I.Scale "XRP" "XRP" = '(1000000, 1)
type instance I.Scale "XRP" "ripple" = '(1, 1)
type instance I.Scale "XRP" "drop" = '(1000000, 1)

-- | Monero
type instance I.Scale "XMR" "XMR" = '(1000000000000, 1)
type instance I.Scale "XMR" "monero" = '(1, 1)
type instance I.Scale "XMR" "decinero" = '(10, 1)
type instance I.Scale "XMR" "centinero" = '(100, 1)
type instance I.Scale "XMR" "millinero" = '(1000, 1)
type instance I.Scale "XMR" "micronero" = '(1000000, 1)
type instance I.Scale "XMR" "nanonero" = '(1000000000, 1)
type instance I.Scale "XMR" "piconero" = '(1000000000000, 1)

