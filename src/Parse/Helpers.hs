module Parse.Helpers
  ( parsePretty
  )
where

import           Control.Lens                   ( _Left
                                                , over
                                                )
import           Text.Megaparsec                ( errorBundlePretty
                                                , parse
                                                )

parsePretty p = over _Left errorBundlePretty . parse p ""

