module Binary.Parser exposing (..)

import Binary exposing (Tape)
import Bitwise
import String.UTF8


type Parser a
    = Parser (Tape -> Result String ( a, Tape ))


parse : Parser a -> Tape -> Result String a
parse (Parser parserFn) tape =
    parserFn tape |> Result.map Tuple.first


intLength : Int
intLength =
    8


readByte : Parser Int
readByte =
    Parser <|
        \tape ->
            case tape of
                byte :: rest ->
                    Ok ( byte, rest )

                _ ->
                    Err "Tried to read a byte after the tape ended"


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen toParserB (Parser parserAFn) =
    Parser <|
        \tape ->
            case parserAFn tape of
                Ok ( value, tape_ ) ->
                    let
                        (Parser parserBFn) =
                            toParserB value
                    in
                    parserBFn tape_

                Err err ->
                    Err err


succeed : a -> Parser a
succeed val =
    Parser <|
        \tape ->
            Ok ( val, tape )


fail : String -> Parser a
fail error =
    Parser <| \_ -> Err error


map : (a -> b) -> Parser a -> Parser b
map f parser =
    parser |> andThen (\v -> succeed (f v))


apply : Parser (a -> b) -> Parser a -> Parser b
apply fnParser aParser =
    fnParser |> andThen (\fn -> map fn aParser)


(|=) : Parser (a -> b) -> Parser a -> Parser b
(|=) =
    apply


(|.) : Parser keep -> Parser ignore -> Parser keep
(|.) keep ignore =
    succeed always
        |= keep
        |= ignore


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f parserA parserB =
    succeed f
        |= parserA
        |= parserB


sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    List.foldr (map2 (::)) (succeed []) parsers


readBytes : Int -> Parser (List Int)
readBytes count =
    sequence <| List.repeat count readByte


int : Parser Int
int =
    readBytes intLength
        |> map
            (List.foldl
                (\value acc ->
                    Bitwise.shiftLeftBy 4 acc + value
                )
                0
            )


string : Parser String
string =
    int
        |> andThen readBytes
        |> andThen
            (\bytes ->
                case String.UTF8.toString bytes of
                    Ok s ->
                        succeed s

                    Err e ->
                        fail e
            )


union : List (Parser a) -> Parser a
union options =
    readByte
        |> andThen
            (\unionCode ->
                List.foldl
                    (\parser ( idx, result ) ->
                        if idx == unionCode then
                            ( idx + 1, parser )
                        else
                            ( idx + 1, result )
                    )
                    ( 0, fail "Failed to decode union" )
                    options
                    |> Tuple.second
            )


bool : Parser Bool
bool =
    union [ succeed False, succeed True ]


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    union [ succeed Nothing, map Just parser ]


end : Parser ()
end =
    Parser <|
        \tape ->
            case tape of
                [] ->
                    Ok ( (), tape )

                _ ->
                    Err "Expected end of tape"
