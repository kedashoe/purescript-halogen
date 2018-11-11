module Example.Components.Multitype.ComponentB where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { n :: Int }

data Query a = GetCount (Int -> a)

data Action = Increment

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
initialState _ = { n: 1 }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.p_
        [ HH.text "Current value:"
        , HH.strong_ [ HH.text (show state) ]
        ]
    , HH.button
        [ HE.onClick \_ -> Just Increment ]
        [ HH.text ("Increment") ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment -> do
    H.modify_ \st -> st { n = st.n + 1 }

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  GetCount k -> do
    st <- H.get
    pure (Just (k st.n))
