

{-
    TextSection.hs describes the text section of the ARM assembly
    and how is string-encoded
-}

{-# LANGUAGE InstanceSigs #-}

module Backend.ARM.Spec.TextSection where

import qualified Backend.ARM.Spec.Instructions as ARM

newtype TextSection = Insts [ARM.Instruction] deriving (Eq)

instance Show TextSection where
    show :: TextSection -> String
    show (Insts insts) = ".section .text\n.global _start\n\n" ++ concatMap show insts