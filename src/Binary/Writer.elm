module Binary.Writer exposing (..)

import Binary exposing (Tape)
import Bitwise
import String.UTF8


intLength : Int
intLength =
    8


type Writer
    = Writer (Tape -> Tape)


write : Writer -> Tape
write (Writer writerFn) =
    writerFn [] |> List.reverse


byte : Int -> Writer
byte x =
    Writer <| \tape -> Bitwise.and 0xFF x :: tape


bytes : List Int -> Writer
bytes xs =
    List.foldl (byte >> compose) empty xs


int : Int -> Writer
int input =
    let
        intHelper : Int -> Int -> List Int -> List Int
        intHelper val count acc =
            if count == 0 then
                acc
            else
                intHelper
                    (Bitwise.shiftLeftBy 4 val)
                    (count - 1)
                    (Bitwise.shiftRightZfBy
                        (4 * 7)
                        (Bitwise.and 0xF0000000 val)
                        :: acc
                    )
    in
    Writer <|
        \tape ->
            intHelper input intLength tape


string : String -> Writer
string input =
    int (String.length input)
        <> (Writer <|
                \tape -> String.UTF8.foldl (::) tape input
           )


bool : Bool -> Writer
bool bool =
    Writer <|
        \tape ->
            if bool then
                1 :: tape
            else
                0 :: tape


maybe : (a -> Writer) -> Maybe a -> Writer
maybe toWriter =
    Maybe.map (\x -> ( 1, toWriter x ))
        >> Maybe.withDefault ( 0, empty )
        >> union


empty : Writer
empty =
    Writer identity


union : ( Int, Writer ) -> Writer
union ( idx, writer ) =
    byte idx <> writer


compose : Writer -> Writer -> Writer
compose (Writer rightFn) (Writer leftFn) =
    Writer <| leftFn >> rightFn


(<>) : Writer -> Writer -> Writer
(<>) =
    flip compose
