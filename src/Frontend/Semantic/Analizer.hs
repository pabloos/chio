

{-
    Analizer.hs defines the Analizer datatype as a ExceptT Monad
-}

module Frontend.Semantic.Analizer where

import Control.Monad.Except

import Frontend.Semantic.State (AnalysisState)
import Frontend.Semantic.Error.Report

type Analizer = ExceptT SemanticReport AnalysisState