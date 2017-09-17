module Stack (
    Stack,
    newStack,
    push,
    pop,
    toList,
    isEmpty,
) where

{-|
  Stack datatype
-}
newtype Stack a = StackImpl [a] deriving Show

-- | the 'newStack' function creates a new empty Stack
newStack :: Stack a
newStack = StackImpl []

-- | Add the element to the stack
push :: Stack a -> a -> Stack a
push (StackImpl elems) el = StackImpl (el:elems)

pop :: Stack a -> (Maybe a, Stack a)
pop (StackImpl []) = (Nothing, StackImpl [])
pop (StackImpl (el:els)) = (Just el, StackImpl els)

toList :: Stack a -> [a]
toList (StackImpl elems) = reverse elems

isEmpty :: Stack a -> Bool
isEmpty (StackImpl []) = True
isEmpty _ = False
