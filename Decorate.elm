module Decorate where

import Graphics.Element as Element exposing (Element)

import Html
import Html.Attributes exposing (attribute)

{- Add an HTML attribute to an element. -}
decorate : (String, String) -> Element -> Element
decorate (attrName, attrValue) elt =
  let html = Html.fromElement elt
      width = Element.widthOf elt
      height = Element.heightOf elt
      htmlWithStyle = Html.div [attribute attrName attrValue] [html]
  in
    Html.toElement width height htmlWithStyle