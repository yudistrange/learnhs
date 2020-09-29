module Caesar (encode)  where

import Data.Char

encode s = map caesarCipher s
  where caesarCipher x = chr (ord x + 3)
