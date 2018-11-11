module Example.Effects.Eff.Random.Component where

import Prelude
import Effect.Aff (Aff)
import Effect.Random (random)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { num :: Maybe Number }

data Action = Regenerate

ui :: forall q i o. H.Component HH.HTML q i o Aff
ui =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { num: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    value = maybe "No number generated yet" show state.num
  in
    HH.div_ $
      [ HH.h1_ [ HH.text "Random number" ]
      , HH.p_ [ HH.text ("Current value: " <> value) ]
      , HH.button
      [ HE.onClick \_ -> Just Regenerate ]
      [ HH.text "Generate new number" ]
      ]

handleAction ∷ forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.put { num: Just newNumber }
