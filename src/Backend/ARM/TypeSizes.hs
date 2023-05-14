

{-
    TypeSizes.hs declares the different sizes every type has in ARM
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.ARM.TypeSizes where

import Types

instance Sized where
    size IntType = 4
    size BoolType = 4
    size StringType = 1