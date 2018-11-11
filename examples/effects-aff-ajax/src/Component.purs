module Example.Effects.Aff.Ajax.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Action
  = SetUsername String
  | MakeRequest

ui :: forall q i o. H.Component HH.HTML q i o Aff
ui =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { loading: false, username: "", result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form_ $
    [ HH.h1_ [ HH.text "Lookup GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput (\x -> Just (SetUsername x))
            ]
        ]
    , HH.div_ [ HH.text $ "user=" <> st.username ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonButton
        , HE.onClick (\_ -> Just MakeRequest)
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text (if st.loading then "Working..." else "") ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    ]

handleAction ∷ forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ (_ { username = username, result = Nothing :: Maybe String })
  MakeRequest -> do
    username <- H.gets _.username
    H.modify_ (_ { loading = true })
    response <- H.liftAff $ AX.get AXResponse.string ("https://api.github.com/users/" <> username)
    H.modify_ (_ { loading = false, result = Just response.response })
