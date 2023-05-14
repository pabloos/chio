

{-
    Label.hs defines the label's format
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.Label where
  
import Data.String (IsString)

newtype Label = Label String deriving (Eq, IsString)

prefix :: String
prefix = ".LBB"

newLabel :: Int -> Int -> Label
newLabel fi li = Label $ prefix <> show fi <> "_" <> show li

nolabel :: Label
nolabel = Label ""

instance Show Label where
    show (Label s) = case s of
      "" -> "\t"
      s -> s ++ ": "