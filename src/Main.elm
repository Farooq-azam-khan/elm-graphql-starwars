module Main exposing(main)
import Browser
import Graphql.Operation exposing (RootQuery)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Graphql.Document as GqlDoc 
import Html 
import Html.Attributes as HA
import RemoteData exposing (RemoteData)

import StarWars.Object 

import StarWars.Object.Human as Human
import StarWars.Object.Droid as Droid 

import StarWars.Query as Query 
import StarWars.Scalar exposing (Id(..))


type alias Response = {human1: Maybe HumanData, human2: Maybe HumanData, human3: Maybe HumanData, human4: Maybe HumanData, droid1:Maybe DroidData} 

query : SelectionSet Response RootQuery
query = SelectionSet.map5 Response 
                (Query.human {id = Id "1001"} humanSelection) 
                (Query.human {id = Id "1002"} humanSelection) 
                (Query.human {id = Id "1003"} humanSelection) 
                (Query.human {id = Id "1004"} humanSelection) 
                (Query.droid {id = Id "2000"} droidSelection)

-- Map Graphql Human type to elm `HumanData` type
type alias HumanData = {name: String, homePlanet: Maybe String, avatarUrl: String}
humanSelection : SelectionSet HumanData StarWars.Object.Human 
humanSelection = SelectionSet.map3 HumanData Human.name Human.homePlanet Human.avatarUrl

type alias DroidData = {name:String, primaryFunction: Maybe String, avatarUrl:String}
droidSelection : SelectionSet DroidData StarWars.Object.Droid 
droidSelection = SelectionSet.map3 DroidData Droid.name Droid.primaryFunction Droid.avatarUrl

request_api : String 
request_api = "https://graphqelm.herokuapp.com/api"

makeRequest : Cmd Msg 
makeRequest = 
    query |> Graphql.Http.queryRequest request_api
          |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type Msg = GotResponse Model 
type alias Model = RemoteData (Graphql.Http.Error Response) Response

type alias Flags = {}
init : Flags -> (Model, Cmd Msg)
init _ = (RemoteData.Loading, makeRequest)

update :  Msg -> Model -> (Model, Cmd Msg)
update  msg model =
    case msg of 
        GotResponse response -> 
            (response, Cmd.none)


display_droid : DroidData -> Html.Html Msg 
display_droid droid = 
    Html.div 
        [] 
        [ Html.h2 [] [Html.text droid.name]
        , Html.img [HA.src droid.avatarUrl, HA.alt droid.name] []
        , Html.p [] [case droid.primaryFunction of 
                        Just pf -> Html.text <| "Primary Function:" ++ pf 
                        Nothing -> Html.text "No Primary Function"]
        ]

display_human : HumanData -> Html.Html Msg 
display_human human =
    Html.div 
        []
        [ Html.h2 [] [Html.text human.name]
        , Html.img [HA.src human.avatarUrl, HA.alt human.name] []
        , Html.p 
            [] 
            [ case human.homePlanet of 
                Just hp -> Html.text <| "Home Planet:" ++ hp
                Nothing -> Html.text "Home planet not known"
            ]

        ]
view : Model -> Html.Html Msg 
view model = 
    Html.div 
        [] 
        [ Html.h1  []  [Html.text "Generate Query"]
        , Html.pre [] [Html.text <| GqlDoc.serializeQuery query]
        , Html.h1 [] [Html.text "Response"]
        , Html.text <| Debug.toString model
        , case model of 
            RemoteData.Success resp ->
                Html.div 
                [] 
                [ case resp.droid1 of 
                        Just droid -> 
                            display_droid droid 

                        _ -> Html.text ""
                , case resp.human1 of 
                    Just human -> 
                        display_human human
                    _ -> Html.text ""
                , case resp.human2 of 
                    Just human -> 
                        display_human human
                    _ -> Html.text ""
                , case resp.human3 of 
                    Just human -> 
                        display_human human
                    _ -> Html.text ""
                , case resp.human4 of 
                    Just human -> 
                        display_human human
                    _ -> Html.text ""
                ]
            _ -> Html.text ""   
        ]
    
main : Program Flags Model Msg 
main = 
    Browser.element 
        { init = init
        , view = view 
        , update = update 
        , subscriptions = sub
        }

sub : Model -> Sub Msg 
sub _ = Sub.none 