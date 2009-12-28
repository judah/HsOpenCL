{-# LANGUAGE TemplateHaskell, MagicHash #-}
module MultiLine where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

clProg :: QuasiQuoter
clProg = QuasiQuoter (\s -> appE (varE 'B.pack) (litE $ stringL s))
            (litP . stringL)
