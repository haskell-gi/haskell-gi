-- | Basic types used in GIR parsing.
module Data.GI.GIR.BasicTypes
    ( Name(..)
    , Transfer(..)
    , Alias(..)
    , Type(..)
    , BasicType(..)
    ) where

import Data.Text (Text)

-- | Name for a symbol in the GIR file.
data Name = Name { namespace :: Text, name :: Text }
    deriving (Eq, Ord, Show)

-- | Transfer mode for an argument or property.
data Transfer = TransferNothing
              | TransferContainer
              | TransferEverything
                deriving (Show, Eq, Ord)

-- | An alias, which is simply (Namespace, name).
newtype Alias = Alias Name deriving (Ord, Eq, Show)

-- | Basic types. These are generally trivial to marshal, and the GIR
-- assumes that they are defined.
data BasicType = TBoolean         -- ^ gboolean
               | TInt             -- ^ gint
               | TUInt            -- ^ guint
               | TLong            -- ^ glong
               | TULong           -- ^ gulong
               | TInt8            -- ^ gint8
               | TUInt8           -- ^ guint8
               | TInt16           -- ^ gint16
               | TUInt16          -- ^ guint16
               | TInt32           -- ^ gint32
               | TUInt32          -- ^ guint32
               | TInt64           -- ^ gint64
               | TUInt64          -- ^ guint64
               | TFloat           -- ^ gfloat
               | TDouble          -- ^ gdouble
               | TUniChar         -- ^ gunichar
               | TGType           -- ^ GType
               | TUTF8            -- ^ gchar*, encoded as UTF-8
               | TFileName        -- ^ gchar*, encoding a filename
               | TPtr             -- ^ gpointer
               | TIntPtr          -- ^ gintptr
               | TUIntPtr         -- ^ guintptr
                 deriving (Eq, Show, Ord)

-- | This type represents the types found in GObject Introspection
-- interfaces: the types of constants, arguments, etc.
data Type
    = TBasicType BasicType
    | TError           -- ^ GError
    | TVariant         -- ^ GVariant
    | TGValue          -- ^ GValue
    | TParamSpec       -- ^ GParamSpec
    | TCArray Bool Int Int Type  -- ^ Zero terminated, Array Fixed
                                 -- Size, Array Length, Element Type
    | TGArray Type     -- ^ GArray
    | TPtrArray Type   -- ^ GPtrArray
    | TByteArray       -- ^ GByteArray
    | TGList Type      -- ^ GList
    | TGSList Type     -- ^ GSList
    | TGHash Type Type -- ^ GHashTable
    | TGClosure (Maybe Type) -- ^ GClosure containing the given API (if known)
    | TInterface Name  -- ^ A reference to some API in the GIR
      deriving (Eq, Show, Ord)
