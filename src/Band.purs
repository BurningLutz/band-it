module Band where

import Prelude

import Data.Array ((..))
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
    angles = 0 .. 360

  band <- createElement "div" doc
  setClassName "band" band

  let
    p = toNode band

  angles
    # flip foreachE \angle -> do
        let
          hsl = hue angle

        elem <- createElement "div" doc

        setClassName "col" elem
        setAttribute "style" ("background: " <> show hsl) elem

        void $ appendChild (toNode elem) p

  pure band
