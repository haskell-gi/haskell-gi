{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

This is an __experimental__ module that introduces support for dynamic
values: these are functions from a record @model@ to some type @a@
which keep track of which selectors of @model@ does the result depend
on. For example, for a record of the form

> data Example = Example {
>     first  :: Int,
>     second :: Bool,
>     third  :: Float
> }

a `DynVal Example String` could be constructed, assuming that you are
given a @record@ `DynVal` representing the full record, using:

> let format = \f s -> "First is " <> f <> " and second is " <> s
>     formatted = format <$> record.first <*> record.second :: DynVal Example String

Here we are showcasing two properties of `DynVal`s: they can be
conveniently constructed using @OverloadedRecordDot@, and they provide
an `Applicative` instance. The resulting @formatted@ `DynVal` keeps
track of the fact that it depends on the @first@ and @second@ record
selectors.

-}

module Data.GI.Base.DynVal
  ( DynVal(..), DVKey(..), ModelProxy(..), dvKeys, dvRead,
    modelProxyCurrentValue, modelProxyRegisterHandler, modelProxyUpdate) where

import GHC.Records (HasField(..))
import qualified GHC.TypeLits as TL

import Data.Proxy (Proxy(..))
import qualified Data.Set as S
import Data.String (IsString(..))
import qualified Data.Text as T

data DVKey = DVKeyDirect [T.Text]
             -- ^ Direct access to subfields: for example writing
             -- @record.field.subfield@ (using the `HasField`
             -- instance) would lead to @`DVKeyDirect` ["field",
             -- "subfield"]@
           | DVKeyDerived (S.Set [T.Text])
             -- ^ Value derived from a direct key, by acting with the
             -- functor or applicative instances.
  deriving (Eq, Ord, Show)

-- | A `DynVal` is a way of extracting values of type @a@ from
-- @model@, which keeps track of which fields (parameterised by
-- `dvKeys`) in @model@ are needed for computing the `DynVal`.
data DynVal model a = DynVal DVKey (model -> a)

-- | Keys to fields in the model that this `DynVal` depends on.
dvKeys :: DynVal model a -> DVKey
dvKeys (DynVal s _) = s

-- | Compute the actual value given a model.
dvRead :: DynVal model a -> model -> a
dvRead (DynVal _ r) = r

-- | Turn a key into a derived one.
toDerived :: DVKey -> DVKey
toDerived (DVKeyDirect d) = DVKeyDerived (S.singleton d)
toDerived derived = derived

-- | Joining of keys always produces derived ones.
instance Semigroup DVKey where
  DVKeyDirect a <> DVKeyDirect b = DVKeyDerived $ S.fromList [a,b]
  (DVKeyDirect a) <> (DVKeyDerived b) =
    DVKeyDerived $ S.insert a b
  (DVKeyDerived a) <> (DVKeyDirect b) =
    DVKeyDerived $ S.insert b a
  (DVKeyDerived a) <> (DVKeyDerived b) =
    DVKeyDerived $ S.union a b

instance Functor (DynVal model) where
  fmap f dv = DynVal (toDerived $ dvKeys dv) (f . dvRead dv)

instance Applicative (DynVal model) where
  pure x = DynVal (DVKeyDerived S.empty) (const x)
  dF <*> dA = DynVal (dvKeys dF <> dvKeys dA)
                     (\m -> let f = dvRead dF m
                            in f (dvRead dA m))

instance IsString (DynVal model T.Text) where
  fromString s = pure (T.pack s)

{-
-- If we make dvKeys :: model -> S.Set DVKey we can also produce a
-- Monad instance, but the set of resulting keys might depend on the
-- specific model passed, which could lead to subtle bugs.

instance Monad (DynVal model) where
  dv >>= gen = let runGen = \m -> gen (dvRead dv m)
               in DynVal {dvKeys = \m -> S.union (dvKeys dv m)
                                         (dvKeys (runGen m) m)
                         , dvRead = \m -> dvRead (runGen m) m
                         }
-}

-- | A `ModelProxy` is a way of obtaining records of type `model`,
-- which allows for registering for notifications whenever certain
-- keys (typically associated to record fields) get modified, and
-- allows to modify fields of the model.
data ModelProxy model = ModelProxy (IO model) (DVKey -> (model -> IO ()) -> IO ()) ([T.Text] -> (model -> Maybe model) -> IO ())

-- The following would be most naturally field accessors, but because
-- we introduce HasField instances for proxies we need to make these
-- ordinary functions instead.

-- | Obtain the current value of the model.
modelProxyCurrentValue :: ModelProxy model -> IO model
modelProxyCurrentValue (ModelProxy m _ _) = m

-- | Register a handler that will be executed whenever any of the
-- fields in the model pointed to by the keys is modified.
modelProxyRegisterHandler :: ModelProxy model -> DVKey -> (model -> IO ()) -> IO ()
modelProxyRegisterHandler (ModelProxy _ r _) = r

-- | Modify the given keys in the proxy, using the given update
-- function, of type (model -> Maybe model). If this function returns
-- Nothing no modification will be performed, otherwise the modified
-- model will be stored in the ModelProxy, and any listeners will be
-- notified of a change.
modelProxyUpdate :: ModelProxy model -> [T.Text] -> (model -> Maybe model)
                 -> IO ()
modelProxyUpdate (ModelProxy _ _ u) = u

instance (HasField fieldName field a,
          TL.KnownSymbol fieldName) =>
  HasField fieldName (DynVal model field) (DynVal model a) where
  getField dv = let fn = T.pack . TL.symbolVal $ (Proxy :: Proxy fieldName)
                    key = case dvKeys dv of
                      derived@(DVKeyDerived _) -> derived
                      DVKeyDirect direct -> DVKeyDirect (direct <> [fn])
                in DynVal key (getField @fieldName . dvRead dv)
