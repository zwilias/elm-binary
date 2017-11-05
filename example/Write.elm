module Write exposing (..)

import Binary exposing (Tape)
import Binary.Parser as P exposing ((|=), Parser)
import Binary.Writer as W exposing ((<>), Writer)
import Html exposing (Html, text)


type alias Model =
    { foo : String
    , bar : Int
    , baz : Maybe String
    , union : MyUnion
    }


type MyUnion
    = Empty
    | One String
    | Two Int String


myUnionWriter : MyUnion -> Writer
myUnionWriter myUnion =
    W.union <|
        case myUnion of
            Empty ->
                ( 0, W.empty )

            One stringVal ->
                ( 1, W.string stringVal )

            Two intVal stringVal ->
                ( 2, W.int intVal <> W.string stringVal )


modelWriter : Model -> Writer
modelWriter model =
    W.string model.foo
        <> W.int model.bar
        <> W.maybe W.string model.baz
        <> myUnionWriter model.union


exampleModel : Model
exampleModel =
    { foo = "hello", bar = 12, baz = Just "foo", union = Two 2 "two" }


tape : Tape
tape =
    W.write (modelWriter exampleModel)


parser : Parser Model
parser =
    P.succeed Model
        |= P.string
        |= P.int
        |= P.maybe P.string
        |= P.union
            [ P.succeed Empty
            , P.succeed One |= P.string
            , P.succeed Two |= P.int |= P.string
            ]


decoded : Result String Model
decoded =
    P.parse parser tape


main : Html msg
main =
    Html.ul []
        [ Html.li [] [ Html.pre [] [ text <| toString exampleModel ] ]
        , Html.li [] [ Html.pre [] [ text <| toString tape ] ]
        , Html.li [] [ Html.pre [] [ text <| toString decoded ] ]
        ]
