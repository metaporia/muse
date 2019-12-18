module Parse.Helpers (parsePretty) where

import Control.Lens (over, _Left)
import Text.Megaparsec (parse, errorBundlePretty)

parsePretty p = over _Left errorBundlePretty . parse p ""

