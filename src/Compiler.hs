

{-  
    Compiler.hs defines a common interface to implement by
    every stage of the compiler
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler where

class Compiler source target where
    compile :: source -> target