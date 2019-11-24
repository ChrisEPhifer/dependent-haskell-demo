-- Load the DataKinds extension, which allows us to promote data/value
-- constructors to be type constructors for types of a new kind created when a
-- datatype is defined, in a separate namespace.
{-# LANGUAGE DataKinds #-}

-- Load the GADTs extension, which allows finer control over the definitions of
-- datatypes, in particular allowing us to do things such as return different
-- types depending on which constructor was used.
{-# LANGUAGE GADTs #-}

-- Load the KindSignatures extension, which allows us to annotate the kind of a
-- type / type constructor. This will mostly be helpful for readability and
-- understanding.
{-# LANGUAGE KindSignatures #-}

-- Load the TypeFamilies extension, which allows us to define type-level
-- functions. We'll be restricting our usage to 'closed' type families, where
-- all of the cases are defined at once.
{-# LANGUAGE TypeFamilies #-}

-- Load the TypeOperators extension, which allows us to use symbolic
-- constructors at the type-level.
{-# LANGUAGE TypeOperators #-}

module Lib where

-- Here, we define a new datatype to represent natural numbers. Because we have
-- the DataKinds extension loaded, however, this also defines:
-- * A kind Nat
-- * A type constant 'Zero
-- * A type constructor 'Succ which accepts a type of kind Nat
-- We'll see how to use these new features when we implement the Vector type.
-- NOTE: There are no values of types of kind Nat! Only types of kind * have
-- values.
data Nat = Zero | Succ Nat

-- Now we define a type Vector, which will use our lifted Nat kind and the type
-- constructors 'Zero and 'Succ to add value information to the value
-- constructors, in service of embedding at the type level information about
-- the length of the vector.
--
-- Notice: We annotate n to have kind Nat, a to have kind *, and explicitly
-- annotate the types of our two value constructors using the lifted type
-- constructors 'Zero and 'Succ to capture the information about the length
-- of the resultant vectors. Haskell can actually infer all of this information
-- for us, so we could have written this type as:
--
-- data Vector n a where
--     VNil :: Vector Zero a
--     VCons :: a -> Vector n a -> Vector (Succ n) a
--
-- where we left off the kind annotations and the explicit usage of the type
-- constructor versions of Zero and Succ. Since this is all new and it helps to
-- see the separation between value- and type-level constructs, we've left the
-- explicitly annotated version as the implementation given to GHC.
data Vector (n :: Nat) (a :: *) where
    VNil  :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

-- So we can do useful things in the repl, we'll write a Show instance for this
-- type.
instance Show a => Show (Vector n a) where
    show VNil         = "VNil"
    show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

-- We now wish to implement the function append. To start, we expect this to
-- have a type like:
--
-- append :: Vector n a -> Vector m a -> Vector ??? a
--
-- The question is what would we like to put in place of "???"? As you might
-- expect, the answer is something like (n + m); this would mean, at compile
-- time, we can verify that the implementation of append we provide satisfies
-- the property length (append v1 v2) == (length v1) + (length v2). But, how do
-- we put functions at the type level? This requires the TypeFamilies extension
-- and looks something like this:
type family Add n m where
    Add 'Zero n     = n
    Add ('Succ n) m = 'Succ (Add n m)

-- As it turns out, this particular implementation is very important; if we were
-- instead to use:
--
-- Add 'Zero n     = n
-- Add ('Succ n) m = Add n ('Succ m)
--
-- We would run into some gnarly type errors due to structural inequality.
-- Since getting into this requires breaking down some nasty Haskell type
-- errors, I recommend on your own changing to this implementation to see what
-- happens.

-- Without further ado, we implement the append function:
append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs         = xs
append (VCons x xs) ys = VCons x (append xs ys)

-------------------------------------------------------------------------------

-- Now, as an exercise, implement the type HList xs, which will represent
-- a heterogeneous list (so like a tuple, but inductively defined.) Here is a
-- skeleton you can use to get started:
--
-- data HList xs where
--     HNil  :: ...
--     (:::) :: ...
-- infixr 6 :::
--
-- Try to fill in the elipsis with an appropriate type. HINT: You'll need to
-- use normal Haskell lists at the type level!



















































-- SOLUTION:
data HList xs where
    HNil  :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)
infixr 6 :::
