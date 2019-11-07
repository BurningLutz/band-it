module Main where

import Prelude

import Band (createHueBand)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM (Element)
import Web.DOM.Document (createElement)
import Web.DOM.Element (setClassName, toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (alert, document)

-- | init the root container
initContainer :: Element -> Effect Unit
initContainer = setClassName "container"

-- | add a hue band
addHueBand :: Element -> Effect Unit
addHueBand bo = do
  hueBand <- createHueBand

  let
    p = toNode bo
    c = toNode hueBand
  void $ appendChild c p

-- | add a standard hue band
addStdHueBand :: Element -> Effect Unit
addStdHueBand bo = do
  doc <- window >>= document <#> toDocument
  hueBand <- createElement "div" doc

  setClassName "std-band" hueBand

  let
    p = toNode bo
    c = toNode hueBand
  void $ appendChild c p

main :: Effect Unit
main = do
  w <- window
  mbody <- w # document >>= body

  case mbody of
    Nothing -> alert "Oooops! Your browser seems don't support it!" w
    Just bo -> do
      let
        bo' = toElement bo

      -- init body
      initContainer bo'

      -- draw a hue band
      addHueBand bo'

      -- draw a standard hue band
      addStdHueBand bo'
