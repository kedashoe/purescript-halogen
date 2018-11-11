module Example.Components.Multitype.ComponentC where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { val :: String }

data Query a = GetValue (String -> a)

data Action = HandleInput String

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
initialState _ = { val: "" }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.label_
    [ HH.p_ [ HH.text "What do you have to say?" ]
    , HH.input
        [ HP.value state.val
        , HE.onValueInput \i -> Just (HandleInput i)
        ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput val -> do
    H.put { val }

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  GetValue k -> do
    st <- H.get
    pure (Just (k st.val))
