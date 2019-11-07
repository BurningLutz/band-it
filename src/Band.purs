module Band where

import Prelude

import Data.Array ((..))
import Data.Int (toNumber)
import Effect (Effect, foreachE)
import HSL (hue)
import Web.DOM (Element)
import Web.DOM.Document (createElement)
import Web.DOM.Element (setAttribute, setClassName, toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- | create a hue band
createHueBand :: Effect Element
createHueBand = do
  hdoc <- window >>= document

  let
    doc = toDocument hdoc
    angles = 0 .. 359

  band <- createElement "div" doc
  setClassName "band" band

  let
    p = toNode band

  angles
    # flip foreachE \angle -> do
        foreachE [0.0, 0.5] \parts -> do
          let
            hsl = hue (toNumber angle + parts)

          elem <- createElement "div" doc

          setClassName "col" elem
          setAttribute "style" ("background: " <> show hsl) elem

          void $ appendChild (toNode elem) p

  pure band
