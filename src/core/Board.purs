module Board where

import Data.Array ((..))
import Data.Int (toNumber)
import Data.Traversable (for, for_)
import Render as Render
import Effect (Effect)
import Effect.Random (randomRange)
import Prelude (Unit, bind, pure, (-), (*))
import Utils (Color)



type Coords = { x :: Int, y :: Int}
type Cell = { coords :: Coords, color :: Color }

type Row = Array Cell
type Board  = Array Row



size :: { width :: Int, height :: Int, side :: Int }
size =
    { width: 600
    , height: 600
    , side: 10
    }


createCell :: Int -> Int -> Effect Cell
createCell x y = do
    r <- randomRange 0.0 50.0
    g <- randomRange 200.0 255.0
    b <- randomRange 0.0 50.0

    pure { coords: {x: x * size.side, y: y * size.side }, color: { r, g, b, a: 1.0 } }


createRow :: Int -> Effect Row
createRow y = 
    for cols \x -> createCell x y
        where cols = 0..(size.width - size.side)

createBoard :: Effect Board
createBoard =
    for row createRow
        where row = 0..(size.height - size.side)
        



draw :: Board -> Effect Unit
draw board =
    for_ board \row ->
        for_ row  \cell ->
            Render.rectangle 
                (toNumber cell.coords.x)
                (toNumber cell.coords.y)
                (toNumber size.side)
                (toNumber size.side)
                cell.color