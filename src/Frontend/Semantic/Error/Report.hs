

{-
    Report.hs defines the error repoting for the semantics stage.
    It uses the same megaparsec reporting that we used in the parser stage
-}

module Frontend.Semantic.Error.Report where

import           Data.List.NonEmpty as NonEmpty hiding (insert)
import qualified Data.Set           as Set

import Text.Megaparsec

import Frontend.Semantic.Error.Errors (Error)

type SemanticReport = ParseErrorBundle String Error

instance ShowErrorComponent Error where
    showErrorComponent e = show e

new :: String -> Int -> Error -> SemanticReport
new src offset e = do
    let initialState = PosState
            { pstateInput = src
            , pstateOffset = 0
            , pstateSourcePos = initialPos ""
            , pstateTabWidth = defaultTabWidth
            , pstateLinePrefix = ""
            }

    ParseErrorBundle
          { bundleErrors = NonEmpty.fromList [FancyError offset (Set.singleton $ ErrorCustom e) ]  --[TrivialError offset Nothing Set.empty] -- A collection of 'ParseError's that is sorted by parse error offsets
          , bundlePosState = initialState -- State that is used for line\/column calculation
          }