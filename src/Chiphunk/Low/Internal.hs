module Chiphunk.Low.Internal where

import Foreign
import Foreign.C.Types

-- | Internal helper function to convert list of storable elements to tuple of length and elements stored in memory
-- and executes IO actions, cleaning up afterwards.
withList :: Storable a => [a] -> ((CInt, Ptr a) -> IO b) -> IO b
withList xs inner = withArray xs $ \p -> inner (fromIntegral $ length xs, castPtr p)
