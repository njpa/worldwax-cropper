module Exif exposing (Orientation(..), orientation)

import Bytes
import Bytes.Decode as Decode exposing (Decoder)


type Orientation
    = Normal
    | MirrorHorizontal
    | Rotate180
    | MirrorVertical
    | MirrorHorizontalRotate270CW
    | Rotate90CW
    | MirrorHorizontalRotate90CW
    | Rotate270CW


orientation : Decoder Orientation
orientation =
    Decode.succeed identity
        |> skip startOfImage
        |> andMap marker
        |> Decode.andThen handleMarker


handleMarker : Int -> Decoder Orientation
handleMarker m =
    if m == 0xE1 then
        Decode.succeed identity
            |> skip (Decode.unsignedInt16 Bytes.BE)
            |> smatch "Exif" (Decode.string 6)
            --|> smatch "Exif\u{0000}\u{0000}" (Decode.string 6)
            |>
                andMap (Decode.string 2)
            |> Decode.andThen toEndianness
            |> Decode.andThen decodeExif
    else
        Decode.succeed identity
            |> skip block
            |> andMap marker
            |> Decode.andThen handleMarker


decodeExif : Bytes.Endianness -> Decoder Orientation
decodeExif bo =
    Decode.succeed identity
        |> smatch 0x2A (Decode.unsignedInt16 bo)
        |> andMap (Decode.unsignedInt32 bo)
        |> Decode.map (\x -> x - 8)
        |> Decode.andThen skipBytes
        |> Decode.andThen
            (\_ ->
                Decode.unsignedInt16 bo
                    |> Decode.andThen (\x -> Decode.loop x (decodeDirEntry bo))
            )


decodeDirEntry : Bytes.Endianness -> Int -> Decoder (Decode.Step Int Orientation)
decodeDirEntry bo entries =
    if entries <= 0 then
        Decode.fail
    else
        Decode.unsignedInt16 bo
            |> Decode.andThen
                (\tag ->
                    if tag == 0x0112 then
                        Decode.succeed Decode.Done
                            |> skip (Decode.unsignedInt16 bo)
                            |> skip (Decode.unsignedInt32 bo)
                            |> andMap (orientationFromUi16 bo)
                    else
                        skipBytes 10
                            |> Decode.map (\_ -> Decode.Loop (entries - 1))
                )


orientationFromUi16 : Bytes.Endianness -> Decoder Orientation
orientationFromUi16 bo =
    Decode.unsignedInt16 bo
        |> Decode.andThen
            (\v ->
                case v of
                    1 ->
                        Decode.succeed Normal

                    2 ->
                        Decode.succeed MirrorHorizontal

                    3 ->
                        Decode.succeed Rotate180

                    4 ->
                        Decode.succeed MirrorVertical

                    5 ->
                        Decode.succeed MirrorHorizontalRotate270CW

                    6 ->
                        Decode.succeed Rotate90CW

                    7 ->
                        Decode.succeed MirrorHorizontalRotate90CW

                    8 ->
                        Decode.succeed Rotate270CW

                    _ ->
                        Decode.fail
            )


toEndianness : String -> Decoder Bytes.Endianness
toEndianness mark =
    if mark == "II" then
        Decode.succeed Bytes.LE
    else if mark == "MM" then
        Decode.succeed Bytes.BE
    else
        Decode.fail


block : Decoder ()
block =
    Decode.unsignedInt16 Bytes.BE
        |> Decode.map (\x -> x - 2)
        |> Decode.andThen skipBytes


skipBytes : Int -> Decoder ()
skipBytes x =
    Decode.loop x skipBytesStep


skipBytesStep : Int -> Decoder (Decode.Step Int ())
skipBytesStep n =
    if n <= 0 then
        Decode.succeed (Decode.Done ())
    else
        Decode.map (always (Decode.Loop (n - 1))) ui8


startOfImage : Decoder ()
startOfImage =
    Decode.succeed ()
        |> smatch 0xD8 marker


marker : Decoder Int
marker =
    Decode.succeed identity
        |> smatch 0xFF ui8
        |> andMap ui8


smatch : v -> Decoder v -> Decoder a -> Decoder a
smatch v dec =
    skip (match v dec)


match : v -> Decoder v -> Decoder ()
match v dec =
    Decode.andThen
        (\d ->
            if d == v then
                Decode.succeed ()
            else
                Decode.fail
        )
        dec


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap val pipe =
    Decode.map2 (<|) pipe val


skip : Decoder a -> Decoder b -> Decoder b
skip s k =
    Decode.map2 always k s


ui8 : Decoder Int
ui8 =
    Decode.unsignedInt8
