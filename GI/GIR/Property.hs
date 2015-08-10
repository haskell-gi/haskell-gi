module GI.GIR.Property
    ( Property(..)
    ) where

import GI.Type (Type)
import GI.Internal.ArgInfo (Transfer)
import GI.Internal.PropertyInfo (ParamFlag)
import GI.GIR.Deprecation (DeprecationInfo)

data Property = Property {
        propName :: String,
        propType :: Type,
        propFlags :: [ParamFlag],
        propTransfer :: Transfer,
        propDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)
