module Character where

import Board as Board
import Control.Monad.State.Class (class MonadState, modify, get, put)
import Data.Int (toNumber)
import Render as Render
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Prelude (Unit, bind, pure, (+), (-))
import Utils (Color)

type Position = { x :: Number, y :: Number }

type Character = 
    { pos :: Position
    , color :: Color 
    }

data Direction = Up | Down | Left | Right | Unknown

create :: Effect Character
create = do
    x <- randomRange 0.0 (toNumber Board.size.width)
    y <- randomRange 0.0 (toNumber Board.size.height)

    pure { pos: {x, y}, color }
    where color = { r: 255.0, g: 255.0, b: 255.0, a: 0.0 }


set :: forall m. MonadState Character m => MonadEffect m => Character -> m Unit
set = put

query ::  forall m. MonadState Character m => MonadEffect m => m Character
query = get

move :: forall m. MonadState Character m => MonadEffect m => Direction -> m Character
move d = do
    case d of
        Up -> modify (\s -> s { pos = { x: s.pos.x, y: s.pos.y + 10.0 } })
        Down -> modify \s -> s { pos = { x: s.pos.x, y: s.pos.y - 10.0 } }
        Left -> modify \s -> s { pos = { x: s.pos.x + 10.0, y: s.pos.y } }
        Right -> modify \s -> s { pos = { x: s.pos.x - 10.0, y: s.pos.y } }
        Unknown -> get


draw :: Character -> Effect Unit
draw c = do
    _ <- log "drawing Character"
    Render.circle c.pos.x c.pos.y 10.0 c.color