{-# LANGUAGE TemplateHaskell, MagicHash #-}
module MultiLine where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import Data.Char
import System.IO.Unsafe
import OpenCL.Simple

clProg :: QuasiQuoter
clProg = QuasiQuoter (\s -> appE (varE 'B.pack) (litE $ stringL s))
            (litP . stringL)

clKern :: QuasiQuoter
clKern = QuasiQuoter (\s -> (litE $ stringL s))
            (litP . stringL)

-- TODO: We can go even further:
-- parse the __kernel line, and turn it into a type.
-- Parsing can easily give arity
-- though we'd probably want a type class for carrays/buffers so it
-- could be reused.
--
-- eventually, just use top-level syntax
--
-- > declareKernels [$clKern|...|]
-- and also
-- > declareKernelsFromFile "foo.cl"
-- which parses the string from either the quasiquote or the file,
-- finds the __kernel functions within, and turns each one
-- into a typed Haskell function.  If it worked, it'd be pretty cool...
--

makeKernel :: KernelFunc f => String -> String -> f
makeKernel name body = unsafePerformIO $ do
            prog <- newSimpleProgram DeviceTypeGPU [B.pack body]
            getKernelFunc prog name

declareKernel :: String -> TypeQ -> String -> Q [Dec]
declareKernel name_str ty text = sequence
    [sigD name ty
    , funD name [clause [] (normalB [|makeKernel $(stringE name_str)
                                        $(stringE text)|]) []]
    ]
  where
    name = mkName name_str

{-
Spec:
Use Parsec.
parse through for __kernel
(alt: build, get names of kernels, then parse through for them
if we find /*, skip until the next */.
parse the arguments of the __kernel fnc and turn into something.

-}
