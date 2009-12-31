{-# LANGUAGE TemplateHaskell #-}
module Parse where

import Text.Parsec
import Text.Parsec.String
import Language.Haskell.TH
import OpenCL
import OpenCL.Simple
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as B
import System.IO.Unsafe

{-
Alternate, safe way:
declare datatype Prog_= as record
and have a function buildProg which builds the file and fills
the records.
-}

-- Bool, others; should they get extra instances?
-- i.e. instance KernelArg (Scalar Bool) where...
-- Alt, just do a whitelist of allowed args...
-- yeah, that's prob. better.
-- (then don't want Storable, rather something else?)
declareKernelsFromFile :: String -> FilePath -> Q [Dec]
declareKernelsFromFile progStr file = do
    res <- runIO $ parseFromFile clFile file
    case res of
        Left err -> do
            report True $ "Error parsing OpenCL file " ++ show file
                                ++ ":\n" ++ show err
            return []
        Right fs -> do
                contents <- runIO $ readFile file
                let progName = mkName progStr
                sequence $ concat [
                            map kernelSig fs
                            , progDef progName contents
                            , map (kernelDef progName . fst) fs
                            ]

kernelSig :: (String, [Type]) -> Q Dec
kernelSig (nameStr,ty) = sigD (mkName nameStr) (kerFuncType ty)

kerFuncType :: [Type] -> TypeQ
kerFuncType types = do
    let funcTy = loop types 
    [t|NDRange d => d -> Maybe d -> $(return funcTy)|]
  where
    loop [] = ConT ''Command
    loop (t:ts) = AppT (AppT ArrowT t) $ loop ts


progDef :: Name -> String -> [Q Dec]
progDef p text = [valD (varP p) (normalB body) []]
  where
    body = [|unsafePerformIO $ newSimpleProgram DeviceTypeGPU
                                [B.pack $(stringE text)]|]

kernelDef :: Name -> String -> Q Dec
kernelDef p kName = valD (varP (mkName kName)) (normalB body) []
  where
    body = [|unsafePerformIO $ do
                k <- createKernel (simpleProgram $(varE p)) $(stringE kName)
                return $ runKernel k |]




{-- OK, the grammar:

based on ISO/IEC 9899:1999

__kernel or kernel: declares function which can be called by the host.
arguments which are pointers must be declared with __global, __constant or __local.

Hmm, what's __constant? variables which are accessible only as read-only
vars.  (For now, ignore it I guess.)

-}

-- TODO: fill out, and decide whether I need a new typeclass to get
-- better marshalling.
-- esp. Int vs uint etc...
scalarTypes :: [(String,Type)]
scalarTypes = [ ("float",ConT ''Float)
              , ("int",ConT ''Int)]

-- Search for:
-- __kernel (or kernel) followed by void followed by name then "("
-- followed by a comma-separated list of params, followed by ")"

-- param:
-- __global (type) * name   => Buffer type
-- __local (type) * name    => Local type
-- type name                => type

testParse :: FilePath -> IO ()
testParse file = do
    res <- parseFromFile clFile file
    print $ fmap (map showFunc) res
  where
    showFunc (s,ts) = show (s,map ppr ts)

clFile :: Parser [(String,[Type])]
clFile = do
            many otherStuff
            fs <- sepEndBy kernelDeclaration (many otherStuff)
            eof
            return fs
  where
    otherStuff = aWhiteSpace <|> nonKernelChar <|> ok_
    nonKernelChar :: Parser ()
    nonKernelChar = ignore (noneOf "_")
    ok_ :: Parser ()
    ok_ = ignore $ try $ char '_' >> dontDo (try (string "_kernel"))

dontDo :: Parser a -> Parser ()
dontDo f = notFollowedBy (f>>return ' ')

kernelDeclaration :: Parser (String,[Type])
kernelDeclaration = do
    word "__kernel"
    word "void"
    liftM2 (,) ident paramList



paramList :: Parser [Type]
paramList = do
    word "("
    ts <- sepBy kernelParam (word ",")
    word ")"
    return ts


kernelParam :: Parser Type
kernelParam = globalParam <|> localParam <|> simpleParam
  where
    globalParam = do
        keyword "__global"
        AppT (ConT ''Buffer) <$> ptrParam
    localParam = do
        keyword "__local"
        AppT (ConT ''Local) <$> ptrParam
    simpleParam = do
        t <- typeIdent
        var <- ident
        return t
    ptrParam = do
        optional $ keyword "const"
        t <- typeIdent
        asterisk
        v <- ident
        return t

tok :: Parser a -> Parser a
tok m = do {x <- m; whiteSpace ; return x}

ignore :: Monad m => m a -> m ()
ignore = (>> return ())

-- TODO: many and manyTill build lists of stuff we throw away.
-- should we be more efficient?
whiteSpace :: Parser ()
whiteSpace = ignore $ many aWhiteSpace

aWhiteSpace :: Parser ()
aWhiteSpace = ignore space <|> ignore singleLineComment
                            <|> ignore multiLineComment
  where
    singleLineComment = do
                    try (string "//")
                    manyTill anyChar $ newline
    multiLineComment = do
                    try (string "/*")
                    manyTill anyChar $ try (string "*/")

word :: String -> Parser String
word = try . tok . string

keyword :: String -> Parser ()
keyword s = try $ tok $ string s >> notFollowedBy alphaNum

asterisk = tok (char '*')

ident = tok $ liftM2 (:) identNonDigit (many identChar)
  where
    identNonDigit = letter <|> char '_'
    identChar = alphaNum <|> char '_'


typeIdent :: Parser Type
typeIdent = do
    n <- ident
    case lookup n scalarTypes of
        Nothing -> fail $ "bad type: " ++ show n
        Just t -> return t
    

