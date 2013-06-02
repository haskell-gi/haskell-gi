
module GI.Internal.CallableInfo
    ( callableInfoReturnType
    , callableInfoCallerOwns
    , callableInfoMayReturnNull
    , callableInfoReturnAttributes
    , callableInfoSkipReturn
    , callableInfoArgs
    ) where

import Control.Applicative ((<$>))
import Foreign.Safe
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import GI.Internal.ArgInfo
import GI.Util (getList)

{# import GI.Internal.Types #}

#include <girepository.h>

{# context prefix="g_callable_info" #}

stupidCast :: CallableInfoClass call => call -> Ptr ()
stupidCast call = castPtr p
    where (CallableInfo p) = callableInfo call

callableInfoReturnType :: CallableInfoClass call => call -> TypeInfo
callableInfoReturnType ci = unsafePerformIO $ TypeInfo <$> castPtr <$>
    {# call get_return_type #} (stupidCast ci)

callableInfoCallerOwns :: CallableInfoClass call => call -> Transfer
callableInfoCallerOwns ci = unsafePerformIO $ toEnum <$> fromIntegral <$>
    {# call get_caller_owns #} (stupidCast ci)

callableInfoMayReturnNull :: CallableInfoClass call => call -> Bool
callableInfoMayReturnNull ci = unsafePerformIO $ (/= 0) <$>
    {# call may_return_null #} (stupidCast ci)

callableInfoSkipReturn :: CallableInfoClass call => call -> Bool
callableInfoSkipReturn ci = unsafePerformIO $ (/= 0) <$>
    {# call skip_return #} (stupidCast ci)

callableInfoReturnAttributes :: CallableInfoClass call => call -> [(String, String)]
callableInfoReturnAttributes ci = unsafePerformIO $ do
    allocaBytes {# sizeof GIAttributeIter #} $ \iter -> do
        zero {# sizeof GIAttributeIter #} (castPtr iter)
        loop (castPtr iter) []

    where -- XXX: There's probably a simpler way to do this.
          zero :: Int -> Ptr Word8 -> IO ()
          zero 0 _ = return ()
          zero n p = poke p 0 >> zero (n - 1) (plusPtr p 1)

          loop :: Ptr a -> [(String, String)] -> IO [(String, String)]
          loop iter acc = do
              name <- new nullPtr
              value <- new nullPtr
              let iter' = AttributeIter (castPtr iter)
              ok <- {# call iterate_return_attributes #}
                  (stupidCast ci) iter' name value
              if ok == 0 then return acc
                  else do
                      name' <- peekCString =<< peek name
                      value' <- peekCString =<< peek value
                      loop iter ((name', value') : acc)

callableInfoArgs :: CallableInfoClass call => call -> [ArgInfo]
callableInfoArgs ci = unsafePerformIO $ map (ArgInfo <$> castPtr) <$>
    getList {# call get_n_args #} {# call get_arg #} (stupidCast ci)

