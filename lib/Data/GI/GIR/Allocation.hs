-- | Information on explicit allocation/deallocation for foreign pointers.
module Data.GI.GIR.Allocation
    ( AllocationInfo(..)
    , AllocationOp(..)
    , unknownAllocationInfo
    ) where

import Data.Text (Text)

-- | Allocation/deallocation information for a given foreign pointer.
data AllocationInfo = AllocationInfo {
      allocCalloc :: AllocationOp
    , allocCopy   :: AllocationOp
    , allocFree   :: AllocationOp
    } deriving (Show)

-- | Information about a given allocation operation. It is either disallowed,
-- allowed via the given function, or it is unknown at the current
-- stage how to perform the operation.
data AllocationOp = AllocationOpUnknown
                  | AllocationOp Text
                    deriving (Show, Eq)

-- | A convenience function, filling in all the allocation info to unknown.
unknownAllocationInfo :: AllocationInfo
unknownAllocationInfo = AllocationInfo {
                          allocCalloc = AllocationOpUnknown
                        , allocCopy = AllocationOpUnknown
                        , allocFree = AllocationOpUnknown

                        }
