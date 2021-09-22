-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module StarWars.Query exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import StarWars.Enum.Episode
import StarWars.InputObject
import StarWars.Interface
import StarWars.Object
import StarWars.Scalar
import StarWars.ScalarCodecs
import StarWars.Union


type alias DroidRequiredArguments =
    { id : StarWars.ScalarCodecs.Id }


{-|

  - id - ID of the droid.

-}
droid :
    DroidRequiredArguments
    -> SelectionSet decodesTo StarWars.Object.Droid
    -> SelectionSet (Maybe decodesTo) RootQuery
droid requiredArgs____ object____ =
    Object.selectionForCompositeField "droid" [ Argument.required "id" requiredArgs____.id (StarWars.ScalarCodecs.codecs |> StarWars.Scalar.unwrapEncoder .codecId) ] object____ (Basics.identity >> Decode.nullable)


{-| Getting this field will result in an error.
-}
forcedError : SelectionSet (Maybe String) RootQuery
forcedError =
    Object.selectionForField "(Maybe String)" "forcedError" [] (Decode.string |> Decode.nullable)


type alias GreetRequiredArguments =
    { input : StarWars.InputObject.Greeting }


greet :
    GreetRequiredArguments
    -> SelectionSet String RootQuery
greet requiredArgs____ =
    Object.selectionForField "String" "greet" [ Argument.required "input" requiredArgs____.input StarWars.InputObject.encodeGreeting ] Decode.string


hello : SelectionSet String RootQuery
hello =
    Object.selectionForField "String" "hello" [] Decode.string


type alias HeroOptionalArguments =
    { episode : OptionalArgument StarWars.Enum.Episode.Episode }


{-|

  - episode - If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.

-}
hero :
    (HeroOptionalArguments -> HeroOptionalArguments)
    -> SelectionSet decodesTo StarWars.Interface.Character
    -> SelectionSet decodesTo RootQuery
hero fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { episode = Absent }

        optionalArgs____ =
            [ Argument.optional "episode" filledInOptionals____.episode (Encode.enum StarWars.Enum.Episode.toString) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "hero" optionalArgs____ object____ Basics.identity


type alias HeroUnionOptionalArguments =
    { episode : OptionalArgument StarWars.Enum.Episode.Episode }


{-|

  - episode - If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.

-}
heroUnion :
    (HeroUnionOptionalArguments -> HeroUnionOptionalArguments)
    -> SelectionSet decodesTo StarWars.Union.CharacterUnion
    -> SelectionSet decodesTo RootQuery
heroUnion fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { episode = Absent }

        optionalArgs____ =
            [ Argument.optional "episode" filledInOptionals____.episode (Encode.enum StarWars.Enum.Episode.toString) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "heroUnion" optionalArgs____ object____ Basics.identity


type alias HumanRequiredArguments =
    { id : StarWars.ScalarCodecs.Id }


{-|

  - id - ID of the human.

-}
human :
    HumanRequiredArguments
    -> SelectionSet decodesTo StarWars.Object.Human
    -> SelectionSet (Maybe decodesTo) RootQuery
human requiredArgs____ object____ =
    Object.selectionForCompositeField "human" [ Argument.required "id" requiredArgs____.id (StarWars.ScalarCodecs.codecs |> StarWars.Scalar.unwrapEncoder .codecId) ] object____ (Basics.identity >> Decode.nullable)


now : SelectionSet StarWars.ScalarCodecs.PosixTime RootQuery
now =
    Object.selectionForField "ScalarCodecs.PosixTime" "now" [] (StarWars.ScalarCodecs.codecs |> StarWars.Scalar.unwrapCodecs |> .codecPosixTime |> .decoder)


today : SelectionSet String RootQuery
today =
    Object.selectionForField "String" "today" [] Decode.string
