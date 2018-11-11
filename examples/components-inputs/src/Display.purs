module Example.Components.Inputs.Display where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH

type State = { num :: Int }

data Action = HandleInput Int

type Slot = H.Slot (Const Void) Void

type Input = Int

component :: forall q o m. H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = receive
      }
    }

initialState :: forall i. i -> State
initialState _ = { num: 1 }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.text "My input value is: "
    , HH.strong_ [ HH.text (show state) ]
    ]

receive :: Input -> Maybe Action
receive i = Just (HandleInput i)

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput n -> do
    oldSt <- H.get
    when (oldSt.num /= n) $ H.put { num: n }
