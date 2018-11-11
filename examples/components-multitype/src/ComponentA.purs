module Example.Components.Multitype.ComponentA where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { on :: Boolean }

data Query a = GetState (Boolean -> a)

data Action = ToggleState

type Slot = H.Slot Query Void

component :: forall i o m. H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ = { on: false }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "Toggle me!" ]
    , HH.button
        [ HE.onClick \_ -> Just ToggleState ]
        [ HH.text (if state.on then "On" else "Off") ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  ToggleState -> do
    H.modify_ \st -> st { on = not st.on }

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  GetState k -> do
    st <- H.get
    pure (Just (k st.on))
