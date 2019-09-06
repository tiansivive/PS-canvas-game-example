module Render where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (Context2D, arc, fillPath, fillRect, getCanvasElementById, getContext2D, setFillStyle)
import Math (pi)
import Prelude (Unit, bind, discard, show, (<>), ($), (*))
import Utils (Color)




toStringCSS :: Color -> String
toStringCSS c = "rgb(" <> show c.r <> "," <> show c.g <> "," <> show c.b <> ")"


handleCanvas :: (Context2D -> Effect Unit) -> Effect Unit
handleCanvas fn = do
    maybe <- getCanvasElementById "canvas"
    case maybe of
        Nothing -> log "Could not find canvas"
        Just canvas -> do 
            ctx <- getContext2D canvas
            fn ctx


rectangle :: Number -> Number -> Number -> Number -> Color -> Effect Unit
rectangle x y width height color = 
    handleCanvas \ctx -> do
        setFillStyle ctx $ toStringCSS color
        fillRect ctx  
            { x
            , y
            , width
            , height
            }



circle :: Number -> Number -> Number -> Color -> Effect Unit
circle x y radius color =
    handleCanvas \ctx -> do
        setFillStyle ctx $ toStringCSS color
        fillPath ctx $ arc ctx 
            { x
            , y
            , radius
            , start: 0.0
            , end: 2.0 * pi
            }