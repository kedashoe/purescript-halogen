module Example.Components.Inputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Example.Components.Inputs.Display as Display
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { num :: Int }

data Action
  = Increment
  | Decrement

type ChildSlots =
  ( display :: Display.Slot Int
  )

_display = SProxy :: SProxy "display"

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { num: 1 }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.ul_
        [ HH.slot _display 1 Display.component state.num absurd
        , HH.slot _display 2 Display.component (state.num * 2) absurd
        , HH.slot _display 3 Display.component (state.num * 3) absurd
        , HH.slot _display 4 Display.component (state.num * 10) absurd
        , HH.slot _display 5 Display.component (state.num * state.num) absurd
        ]
    , HH.button
        [ HE.onClick \_ -> Just Increment ]
        [ HH.text "+1"]
    , HH.button
        [ HE.onClick \_ -> Just Decrement ]
        [ HH.text "-1"]
    ]

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Increment -> do
    H.modify_ \st -> st { num = st.num + 1 }
  Decrement -> do
    H.modify_ \st -> st { num = st.num - 1 }
