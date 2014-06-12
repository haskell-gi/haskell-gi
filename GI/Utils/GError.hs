{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

-- GError handling. We mostly follow the naming conventions of gtk2hs,
-- with the difference that gErrorDomain is a String, rather than the
-- GQuark itself. This is because libgirepository gives us the String,
-- not the accessor function.
--
-- Some of the code here comes straight from gtk2hs, but we have
-- removed the deprecated methods.

module GI.Utils.GError
    (
    -- * Data types
    --
      GError(..)
    , GErrorDomain
    , GErrorCode
    , GErrorMessage

    -- * Catching GError exceptions
    -- | To catch GError exceptions thrown by Gtk2Hs functions use the
    -- catchGError* or handleGError* functions. They work in a similar way to
    -- the standard 'Control.Exception.catch' and 'Control.Exception.handle'
    -- functions.
    --
    -- 'catchGError' \/ 'handleGError' catches all GError exceptions,
    -- you provide a handler function that gets given the GError if an
    -- exception was thrown.  This is the most general but is probably
    -- not what you want most of the time. It just gives you the raw
    -- error code rather than a Haskell enumeration of the error
    -- codes. Most of the time you will only want to catch a specific
    -- error or any error from a specific error domain. To catch just
    -- a single specific error use 'catchGErrorJust' \/
    -- 'handleGErrorJust'. To catch any error in a particular error
    -- domain use 'catchGErrorJustDomain' \/ 'handleGErrorJustDomain'
    --
    -- For convenience, generated code also includes specialized
    -- variants of 'catchGErrorJust' \/ 'handleGErrorJust' for each
    -- error type, of the form 'catchPixbufError' \/
    -- 'handlePixbufError'.
    , catchGErrorJust
    , catchGErrorJustDomain

    , handleGErrorJust
    , handleGErrorJustDomain

    , GErrorClass(..)

    , propagateGError
    , checkGError

    ) where

import Foreign.Safe
import Foreign.C
import Control.Exception
import Data.Typeable

-- | A GError consists of a domain, code and a human readable message.
data GError = GError !GErrorDomain !GErrorCode !GErrorMessage
  deriving Typeable

instance Show GError where
  show (GError _ _ msg) = msg

instance Exception GError

type GQuark = Word32

-- | A code used to identify the \'namespace\' of the error. Within each error
--   domain all the error codes are defined in an enumeration. Each gtk\/gnome
--   module that uses GErrors has its own error domain. The rationale behind
--   using error domains is so that each module can organise its own error codes
--   without having to coordinate on a global error code list.
type GErrorDomain  = GQuark

-- | A code to identify a specific error within a given 'GErrorDomain'. Most of
--   time you will not need to deal with this raw code since there is an
--   enumeration type for each error domain. Of course which enumeraton to use
--   depends on the error domain, but if you use 'catchGErrorJustDomain' or
--   'handleGErrorJustDomain', this is worked out for you automatically.
type GErrorCode = CInt

-- | A human readable error message.
type GErrorMessage = String

-- | Each error domain's error enumeration type should be an instance of this
--   class. This class helps to hide the raw error and domain codes from the
--   user.
--
-- Example for 'Graphics.UI.Gtk.Gdk.Pixbuf.PixbufError':
--
-- > instance GErrorClass PixbufError where
-- >   gerrorDomain _ = "gdk-pixbuf-error-quark"
--
class Enum err => GErrorClass err where
  gerrorDomain :: err -> String -- ^ This must not use the value of its
                                -- parameter so that it is safe to pass
				-- 'undefined'.

foreign import ccall unsafe "g_quark_try_string" g_quark_try_string ::
    CString -> IO GQuark

-- | Given the string representation of an error domain returns the
--   corresponding error quark.
gErrorQuarkFromDomain :: String -> IO GQuark
gErrorQuarkFromDomain domain = withCString domain $ \cstring ->
                               g_quark_try_string cstring

-- | This will catch just a specific GError exception. If you need to catch a
--   range of related errors, 'catchGErrorJustDomain' is probably more
--   appropriate. Example:
--
-- > do image <- catchGErrorJust PixbufErrorCorruptImage
-- >               loadImage
-- >               (\errorMessage -> do log errorMessage
-- >                                    return mssingImagePlaceholder)
--
catchGErrorJust :: GErrorClass err => err  -- ^ The error to catch
                -> IO a                    -- ^ The computation to run
                -> (GErrorMessage -> IO a) -- ^ Handler to invoke if an exception is raised
                -> IO a
catchGErrorJust code action handler = do
  errorQuark <- gErrorQuarkFromDomain $ gerrorDomain code
  catch action (handler' errorQuark)
  where handler' quark gerror@(GError domain code' msg)
          | domain == quark
           && code' == (fromIntegral . fromEnum) code
                                       = handler msg
          | otherwise                  = throw gerror

-- | Catch all GErrors from a particular error domain. The handler function
--   should just deal with one error enumeration type. If you need to catch
--   errors from more than one error domain, use this function twice with an
--   appropriate handler functions for each.
--
-- > catchGErrorJustDomain
-- >   loadImage
-- >   (\err message -> case err of
-- >       PixbufErrorCorruptImage -> ...
-- >       PixbufErrorInsufficientMemory -> ...
-- >       PixbufErrorUnknownType -> ...
-- >       _ -> ...)
--
catchGErrorJustDomain :: forall err a. GErrorClass err =>
                         IO a        -- ^ The computation to run
                      -> (err -> GErrorMessage -> IO a) -- ^ Handler to invoke if an exception is raised
                      -> IO a
catchGErrorJustDomain action handler = do
  errorQuark <- gErrorQuarkFromDomain $ gerrorDomain (undefined::err)
  catch action (handler' errorQuark)
  where handler' quark gerror@(GError domain code msg)
          | domain == quark = handler (toEnum $ fromIntegral code) msg
          | otherwise       = throw gerror

-- | A verson of 'handleGErrorJust' with the arguments swapped around.
handleGErrorJust :: GErrorClass err => err -> (GErrorMessage -> IO a) -> IO a -> IO a
handleGErrorJust code = flip (catchGErrorJust code)

-- | A verson of 'catchGErrorJustDomain' with the arguments swapped around.
handleGErrorJustDomain :: GErrorClass err => (err -> GErrorMessage -> IO a) -> IO a -> IO a
handleGErrorJustDomain = flip catchGErrorJustDomain

foreign import ccall "g_error_free" g_error_free ::
    Ptr () -> IO ()

-- | Run the given function catching possible GErrors in its
-- execution. If a GError is emitted this throws the corresponding
-- exception.
propagateGError :: (Ptr (Ptr ()) -> IO a) -> IO a
propagateGError f = checkGError f throw

-- | Like propagateGError, but allows to specify a custom handler
-- instead of just throwing the exception.
checkGError :: (Ptr (Ptr ()) -> IO a) -> (GError -> IO a) -> IO a
checkGError f handler = do
  gerrorPtr <- malloc :: IO (Ptr (Ptr ()))
  poke gerrorPtr nullPtr
  result <- f gerrorPtr
  gerror <- peek gerrorPtr
  if gerror /= nullPtr
  then do domain <- peek (castPtr gerror) :: IO GErrorDomain
          code <- peek (gerror `plusPtr` sizeOf domain) :: IO GErrorCode
          c_msg <- peek (gerror `plusPtr` sizeOf domain `plusPtr` sizeOf code) :: IO CString
          msg <- peekCString c_msg
          g_error_free gerror
          handler $ GError domain code msg
  else return result
