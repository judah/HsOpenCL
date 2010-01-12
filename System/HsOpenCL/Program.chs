module System.HsOpenCL.Program(Program,
                    createProgramWithSource,
                    createProgramWithBinary,
                    buildProgram,
                    buildProgramForDevices,
                    printBuildErrors,
                    unloadCompiler,
                    buildProgramFromSource,
                    -- Queries
                    programContext,
                    programDevices,
                    programSource,
                    getProgramBinaries,
                    -- Build info
                    BuildStatus(..),
                    getBuildStatus,
                    getBuildOptions,
                    getBuildLog,
                ) where

#include <OpenCL/OpenCL.h>
import System.HsOpenCL.Internal.Types
import System.HsOpenCL.Internal.C2HS
import System.HsOpenCL.Error
import System.HsOpenCL.Platform() -- for Show instance of DeviceID
import System.HsOpenCL.MonadQueue
import System.HsOpenCL.CommandQueue

import Control.Exception
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Internal (fromForeignPtr, mallocByteString)

{#fun clCreateProgramWithSource as clCreateProgramWithSource
  { withContext* `Context'
  , `Int'
  , castPtr `Ptr CString'
  , id `Ptr CULong'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `Program' newProgram*
#}

withByteStringPtrs :: [ByteString]
    -> (Ptr CString -> Ptr CULong -> IO a) -> IO a
withByteStringPtrs bs f = withMany unsafeUseAsCStringLen bs $ \cs -> do
    let (cstrs, strLens) = unzip cs
    withArray cstrs $ \cstrP -> do
    withArray (map toEnum strLens) $ \lensP -> do
    f (castPtr cstrP) lensP

createN :: [Int] -> (Ptr CString -> IO ()) -> IO [ByteString]
createN sizes f = do
    fps <- mapM mallocByteString sizes
    withMany withForeignPtr fps $ \cstrs -> withArray cstrs (f . castPtr)
    return $ zipWith (flip fromForeignPtr 0) fps sizes

createProgramWithSource :: Context -> [ByteString] -> IO Program
createProgramWithSource context bs = withByteStringPtrs bs $ \cs lenP ->
    clCreateProgramWithSource context (length bs) cs lenP

{#fun clCreateProgramWithBinary
  { withContext* `Context'
  , withDeviceIDs* `[DeviceID]'&
  , id `Ptr CULong'
  , castPtr `Ptr CString'
  , id `Ptr CInt' -- binary statuses
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `Program' newProgram*
#}

-- TODO: 
-- Handle if InvalidBinary is thrown, and if so do something?
-- use error?  Other thing?
-- maybe add another constructor to CLError?
createProgramWithBinary :: Context -> [(DeviceID,ByteString)] -> IO Program
createProgramWithBinary cxt devBinaries = do
    let numDevices = length devBinaries
    let (devs,binaries) = unzip devBinaries
    withByteStringPtrs binaries $ \binariesP lengthsP -> do
    allocaArray numDevices $ \statuses -> do
    clCreateProgramWithBinary cxt devs lengthsP
                    binariesP statuses

-- | Convenience function; prints an error message when fails.
buildProgramFromSource :: MonadQueue m => String -> [ByteString] -> m Program
buildProgramFromSource options sources = getContext >>= \cxt -> liftIO $ do
    prog <- createProgramWithSource cxt sources
    printBuildErrors prog $ buildProgram prog options
    return prog


{#fun clBuildProgram as clBuildProgram
  { withProgram* `Program'
  , id `CUInt'
  , id `Ptr (Ptr ())'
  , `String'
  , castFunPtr `FunPtr ()' -- notification
  , id `Ptr ()'
  } -> `Int' checkSuccess*-
#}

-- TODO: options should be String or ByteString?
buildProgram :: Program -> String -> IO ()
buildProgram prog options = clBuildProgram prog 0 nullPtr options nullFunPtr nullPtr

buildProgramForDevices :: Program -> [DeviceID] -> String -> IO ()
buildProgramForDevices prog devs options = withDeviceIDs devs $ \(n,ds) ->
    clBuildProgram prog n ds options nullFunPtr nullPtr

printBuildErrors :: Program -> IO a -> IO a
printBuildErrors prog = handle printErr
  where
    printErr :: CLError -> IO a
    printErr e = do
        putStrLn $ "Error building program: " ++ show e
        forM_ (programDevices prog) $ \dev -> do
            stat <- getBuildStatus prog dev
            when (stat == BuildError) $ do
                log <- getBuildLog prog dev
                putStrLn (show dev ++ ":")
                B.putStrLn log
        throw e

{#fun clUnloadCompiler as unloadCompiler
  { } -> `Int' checkSuccess*-
#}

#c
enum CLProgramInfo {
    CLProgramContext = CL_PROGRAM_CONTEXT,
    CLProgramNumDevices = CL_PROGRAM_NUM_DEVICES,
    CLProgramDevices = CL_PROGRAM_DEVICES,
    CLProgramSource = CL_PROGRAM_SOURCE,
    CLProgramBinarySizes = CL_PROGRAM_BINARY_SIZES,
    CLProgramBinaries = CL_PROGRAM_BINARIES
};
#endc
{#enum CLProgramInfo {} #}

{#fun clGetProgramInfo as getInfo
  { withProgram* `Program'
  , cEnum `CLProgramInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

programContext :: Program -> Context
programContext p@(Program fp)
    = unsafePerformIO $ withForeignPtr fp $ \_ ->
            getProp (getInfo p CLProgramContext)
                >>= retainedCLContext

programDevices :: Program -> [DeviceID]
programDevices p = map DeviceID $ unsafePerformIO
        $ getArrayN (getPureProp (getInfo p CLProgramNumDevices))
                (getInfo p CLProgramDevices)

-- Note the spec ensures that the returned char[] is null-terminated,
-- so it's OK to use ByteString's Property instance.
programSource :: Program -> ByteString
programSource p = getPureProp $ getInfo p CLProgramSource

-- NB: one for each device associated with the program.
getProgramBinaries :: Program -> IO [ByteString]
getProgramBinaries prog = do
    numDevs <- getProp (getInfo prog CLProgramNumDevices)
    sizes :: [CSize] <- getArrayN numDevs (getInfo prog CLProgramBinarySizes)
    createN (map fromEnum sizes) $ \cstrs -> do
        let infoSize = sizeOf (undefined :: CString) * numDevs
        r <- getInfo prog CLProgramBinaries infoSize (castPtr cstrs)
        when (infoSize /= r) $ error "getProgramBinaries: bad result size"


#c
enum CLProgramBuildInfo {
    CLProgramBuildStatus = CL_PROGRAM_BUILD_STATUS,
    CLProgramBuildOptions = CL_PROGRAM_BUILD_OPTIONS,
    CLProgramBuildLog = CL_PROGRAM_BUILD_LOG
};
#endc
{#enum CLProgramBuildInfo {}#}

#c
enum BuildStatus {
    BuildNone = CL_BUILD_NONE,
    BuildError = CL_BUILD_ERROR,
    BuildSuccess = CL_BUILD_SUCCESS,
    BuildInProgress = CL_BUILD_IN_PROGRESS
};
#endc
{#enum BuildStatus {} deriving (Show,Eq)#}
    

{#fun clGetProgramBuildInfo as getBuildInfo
  { withProgram* `Program'
  , deviceIDPtr `DeviceID'
  , cEnum `CLProgramBuildInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

getBuildStatus :: Program -> DeviceID -> IO BuildStatus
getBuildStatus prog device = toEnum <$> (getProp $
        getBuildInfo prog device CLProgramBuildStatus)

getBuildOptions :: Program -> DeviceID -> IO String
getBuildOptions prog device = getProp $
        getBuildInfo prog device CLProgramBuildOptions

getBuildLog :: Program -> DeviceID -> IO ByteString
getBuildLog prog device = getProp $
        getBuildInfo prog device CLProgramBuildLog
