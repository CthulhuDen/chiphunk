module Chiphunk.Low.Internal where

import Foreign
import Foreign.C.Types

withList :: Storable a => [a] -> ((CInt, Ptr a) -> IO b) -> IO b
withList xs inner = withArray xs $ \p -> inner (fromIntegral $ length xs, castPtr p)
