module Effect.Events where

import Effect
import Effect.Class (class MonadEffect)
import Prelude (Unit)

data Key = Up | Down | Left | Right

foreign import onKeyDown :: forall m. MonadEffect m => String -> m Unit -> Effect Unit