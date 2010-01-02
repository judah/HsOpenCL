{-# LANGUAGE TemplateHaskell #-}
module Parse(declareKernelsFromFile
            , declareKernels
            , clProg
            ) where

import Text.Parsec
import Text.Parsec.ByteString
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import OpenCL
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Char8 as B
import System.IO.Unsafe

{-
This module declares Template Haskell commands for automatically
importing an OpenCL file into source code, with type-safety.

For example, say file.cl contains two kernels:

__kernel void add(__global float *a, __global float* b, __global float* c)
__kernel void scale(float x, __global float *y)

Then calling $(declareKernelsFromFile "ProgAdd" "path/to/file.cl") outputs
two declarations:

 data ProgAdd = ProgAdd {add :: Buffer Float -> Buffer Float
                                -> Bufer Float -> Command
                        , scale :: Float -> Buffer Float -> Command
                        }

buildProgAdd :: MonadQueue m => m ProgAdd
buildProgAdd = ...
-}

declareKernelsFromFile :: String -> FilePath -> Q [Dec]
declareKernelsFromFile progStr file = do
    contents <- runIO $ B.readFile file
    declareKernels' file progStr contents

declareKernels :: String -> B.ByteString -> Q [Dec]
declareKernels progStr contents = do
    loc <- location
    declareKernels' (loc_filename loc ++ ": "
                        ++ show (loc_start loc, loc_end loc))
        progStr contents

clProg :: QuasiQuoter
clProg = QuasiQuoter (\s -> appE (varE 'B.pack) (litE $ stringL s))
            (litP . stringL)


declareKernels' :: String -> String -> B.ByteString -> Q [Dec]
declareKernels' source progStr contents
    = case runParser clFile () source contents of
            Left err -> do
                report True $ "Error parsing OpenCL file: " ++ show err
                return []
            Right fs -> let
                progName = mkName progStr
                buildName = mkName ("build" ++ progStr)
                in sequence [ declareProgData progName fs
                            , declareBuildSig buildName progName
                            , declareBuildDef buildName progName fs
                                      (B.unpack contents)
                            ] 

declareProgData :: Name -> [(String,[Type])] -> Q Dec
declareProgData prog fields = do
    dataD (return []) prog []
            [recC prog (map mkField fields)] []
  where
    mkField (name,argTypes) = do
        t <- kerFuncType argTypes
        return (mkName name,NotStrict,t)


kerFuncType :: [Type] -> TypeQ
kerFuncType types = do
    let funcTy = loop types 
    [t|NDRange d => d -> Maybe d -> $(return funcTy)|]
  where
    loop [] = ConT ''Command
    loop (t:ts) = AppT (AppT ArrowT t) $ loop ts

declareBuildSig :: Name -> Name -> Q Dec
declareBuildSig buildName progName = sigD buildName
            [t|MonadQueue m => m $(conT progName)|]

-- OK, now the build def:
-- buildProg = do
--   p <- buildProgramFromSource "" [contents]
--   ker1_ <- createKernel p kerNameStr
--   ...
--   return Prog {ker1_=ker1, ,,,}
--
declareBuildDef :: Name -> Name -> [(String,[Type])] -> String -> Q Dec
declareBuildDef buildName progName fs contents
    = do
        p <- newName "p"
        ks <- replicateM (length fs) (newName "k")
        let kernels = zip ks (map fst fs)
        let builder = bindS (varP p) [|buildProgramFromSource "" [B.pack $(stringE contents)]|]
        let bindKernel (k, kerStr) = bindS (varP k)
                                    [|createKernel $(varE p) $(stringE kerStr)|]
        let setField (k, kerStr) = return (mkName kerStr,AppE (VarE 'runKernel) (VarE k))
        let constructor = recConE progName $ map setField kernels
        valD (varP buildName)
            (normalB $ doE $ concat [[builder]
                                , map bindKernel kernels
                                , [noBindS [|return $constructor|] ] ])
            []
            
    




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
    
