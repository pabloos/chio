

{- 
    Types.hs defines the types implemented by the language:
        - integers
        - booleans
        - strings
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

data TypeValue = IntType | BoolType | StringType deriving (Eq, Show)

-- Sized typeclass defines the number of machine words every type uses in each platform
class Sized where
    size :: TypeValue -> Int

-- Typed typeclass defines the common interface to get a type from a generic construction
class Typed a where
    typeOf :: a -> TypeValue