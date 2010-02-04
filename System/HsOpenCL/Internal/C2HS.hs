--  C->Haskell Compiler: Marshalling library
--
--  Copyright (c) [1999...2005] Manuel M T Chakravarty
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
-- 
--  1. Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer. 
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution. 
--  3. The name of the author may not be used to endorse or promote products
--     derived from this software without specific prior written permission. 
--
--  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
--  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98
--
--  This module provides the marshaling routines for Haskell files produced by 
--  C->Haskell for binding to C library interfaces.  It exports all of the
--  low-level FFI (language-independent plus the C-specific parts) together
--  with the C->HS-specific higher-level marshalling routines.
--

module System.HsOpenCL.Internal.C2HS (

  -- * Re-export the language-independent component of the FFI 
  module Foreign,

  -- * Re-export the C language component of the FFI
  module Foreign.C,

  -- * Composite marshalling functions
  -- withCStringLenIntConv, peekCStringLenIntConv, withIntConv, withFloatConv,
  withIntConv, withFloatConv,
  peekIntConv, peekFloatConv, withBool, peekBool, withEnum, peekEnum,

  -- * Conditional results using 'Maybe'
  nothingIf, nothingIfNull,

  -- * Bit masks
  combineBitMasks, containsBitMask, extractBitMasks,

  -- * Conversion between C and Haskell types
  cIntConv, cFloatConv, cToBool, cFromBool, cToEnum, cFromEnum
) where 


import Foreign
       hiding       (Word)
		    -- Should also hide the Foreign.Marshal.Pool exports in
		    -- compilers that export them
import Foreign.C

import Control.Monad        (liftM)


-- Composite marshalling functions
-- -------------------------------

-- Strings with explicit length
--
-- withCStringLenIntConv s f    = withCStringLen s $ \(p, n) -> f (p, cIntConv n)
-- peekCStringLenIntConv (s, n) = peekCStringLen (s, cIntConv n)

-- Marshalling of numerals
--

withIntConv   :: (Storable b, Integral a, Integral b) 
	      => a -> (Ptr b -> IO c) -> IO c
withIntConv    = with . cIntConv

withFloatConv :: (Storable b, RealFloat a, RealFloat b) 
	      => a -> (Ptr b -> IO c) -> IO c
withFloatConv  = with . cFloatConv

peekIntConv   :: (Storable a, Integral a, Integral b) 
	      => Ptr a -> IO b
peekIntConv    = liftM cIntConv . peek

peekFloatConv :: (Storable a, RealFloat a, RealFloat b) 
	      => Ptr a -> IO b
peekFloatConv  = liftM cFloatConv . peek

-- Passing Booleans by reference
--

withBool :: (Integral a, Storable a) => Bool -> (Ptr a -> IO b) -> IO b
withBool  = with . fromBool

peekBool :: (Integral a, Storable a) => Ptr a -> IO Bool
peekBool  = liftM toBool . peek


-- Passing enums by reference
--

withEnum :: (Enum a, Integral b, Storable b) => a -> (Ptr b -> IO c) -> IO c
withEnum  = with . cFromEnum

peekEnum :: (Enum a, Integral b, Storable b) => Ptr b -> IO a
peekEnum  = liftM cToEnum . peek


{- Disabled b/c of orphan warning:
-- Storing of 'Maybe' values
-- -------------------------

instance Storable a => Storable (Maybe a) where
  sizeOf    _ = sizeOf    (undefined :: Ptr ())
  alignment _ = alignment (undefined :: Ptr ())

  peek p = do
	     ptr <- peek (castPtr p)
	     if ptr == nullPtr
	       then return Nothing
	       else liftM Just $ peek ptr

  poke p v = do
	       ptr <- case v of
		        Nothing -> return nullPtr
			Just v' -> new v'
               poke (castPtr p) ptr

-}

-- Conditional results using 'Maybe'
-- ---------------------------------

-- Wrap the result into a 'Maybe' type.
--
-- * the predicate determines when the result is considered to be non-existing,
--   ie, it is represented by `Nothing'
--
-- * the second argument allows to map a result wrapped into `Just' to some
--   other domain
--
nothingIf       :: (a -> Bool) -> (a -> b) -> a -> Maybe b
nothingIf p f x  = if p x then Nothing else Just $ f x

-- |Instance for special casing null pointers.
--
nothingIfNull :: (Ptr a -> b) -> Ptr a -> Maybe b
nothingIfNull  = nothingIf (== nullPtr)


-- Support for bit masks
-- ---------------------

-- Given a list of enumeration values that represent bit masks, combine these
-- masks using bitwise disjunction.
--
combineBitMasks :: (Enum a, Bits b) => [a] -> b
combineBitMasks = foldl (.|.) 0 . map (fromIntegral . fromEnum)

-- Tests whether the given bit mask is contained in the given bit pattern
-- (i.e., all bits set in the mask are also set in the pattern).
--
containsBitMask :: (Bits a, Enum b) => a -> b -> Bool
bits `containsBitMask` bm = let bm' = fromIntegral . fromEnum $ bm
			    in
			    bm' .&. bits == bm'

-- |Given a bit pattern, yield all bit masks that it contains.
--
-- * This does *not* attempt to compute a minimal set of bit masks that when
--   combined yield the bit pattern, instead all contained bit masks are
--   produced.
--
extractBitMasks :: (Bits a, Enum b, Bounded b) => a -> [b]
extractBitMasks bits = 
  [bm | bm <- [minBound..maxBound], bits `containsBitMask` bm]


-- Conversion routines
-- -------------------

-- |Integral conversion
--
cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

-- |Floating conversion
--
cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac
-- As this conversion by default goes via `Rational', it can be very slow...
{-# RULES 
  "cFloatConv/Float->Float"   forall (x::Float).  cFloatConv x = x;
  "cFloatConv/Double->Double" forall (x::Double). cFloatConv x = x
 #-}

-- |Obtain C value from Haskell 'Bool'.
--
cFromBool :: Num a => Bool -> a
cFromBool  = fromBool

-- |Obtain Haskell 'Bool' from C value.
--
cToBool :: Num a => a -> Bool
cToBool  = toBool

-- |Convert a C enumeration to Haskell.
--
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

-- |Convert a Haskell enumeration to C.
--
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum
