port module Main exposing (..)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Cropper
import Task
import File exposing (File)
import File.Select as Select
import Json.Decode as D


-- PORTS


port testtest : (String -> msg) -> Sub msg


port cropperData : Cropper.CropData -> Cmd msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { state : State
    , hoverUpload : Bool
    , uploadedImage : String
    , cropper : Cropper.Model
    , croppedImage : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = NotAsked
      , hoverUpload = False
      , uploadedImage = ""
      , cropper =
            Cropper.init
                { url = ""
                , crop = { width = 500, height = 500 }
                }
      , croppedImage = ""
      }
    , Cmd.none
    )



-- TYPES


type State
    = NotAsked
    | Asked
    | FileSelected
    | FileReceived
    | ImageLoadedInCropper
    | ImageCropped
    | Confirmed



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ToCropper (Cropper.subscriptions model.cropper)
        , testtest GotCroppedImage
        ]



-- MESSAGES


type Msg
    = Reset
    | Pick
    | DragEnter
    | DragLeave
    | GotFile File (List File)
    | GotPreview String
    | Clear
    | ToCropper Cropper.Msg
    | Zoom String
    | ExportImage
    | GotCroppedImage String
    | ConfirmedCrop



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | state = NotAsked }
            , Cmd.none
            )

        Pick ->
            ( { model | state = Asked }
            , Select.files [ "image/*" ] GotFile
            )

        DragEnter ->
            ( { model | hoverUpload = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hoverUpload = False }
            , Cmd.none
            )

        GotFile file _ ->
            ( { model | state = FileSelected, hoverUpload = False }
            , Task.perform GotPreview (File.toUrl file)
            )

        GotPreview preview ->
            ( { model
                | state = FileReceived
                , uploadedImage = preview
                , cropper =
                    Cropper.init
                        { url =
                            preview
                            --Image.image
                        , crop = { width = 500, height = 500 }
                        }
              }
            , Cmd.none
            )

        Clear ->
            ( { model | uploadedImage = "", state = ImageLoadedInCropper }
            , Cmd.none
            )

        ToCropper subMsg ->
            let
                ( updatedSubModel, subCmd ) =
                    Cropper.update subMsg model.cropper
            in
                ( { model
                    | state = ImageLoadedInCropper
                    , cropper = updatedSubModel
                  }
                , Cmd.map ToCropper subCmd
                )

        Zoom zoom ->
            ( { model
                | cropper = Cropper.zoom model.cropper (Maybe.withDefault 0 (String.toFloat zoom))
              }
            , Cmd.none
            )

        ExportImage ->
            ( { model | state = ImageCropped }
            , cropperData (Cropper.cropData model.cropper)
            )

        GotCroppedImage croppedImage ->
            ( { model | croppedImage = croppedImage }
            , Cmd.none
            )

        ConfirmedCrop ->
            ( { model | state = Confirmed }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ case model.state of
            NotAsked ->
                viewPick model

            Asked ->
                div [] [ text "asked" ]

            FileSelected ->
                div [] [ text "file selected" ]

            FileReceived ->
                viewCropper model

            ImageLoadedInCropper ->
                div []
                    [ viewCropper model
                    , button [ onClick Reset ] [ text "Another image" ]
                    ]

            ImageCropped ->
                div []
                    [ img [ src model.croppedImage ] []
                    , button [ onClick Clear ] [ text "Clear" ]
                    , button [ onClick ConfirmedCrop ] [ text "Submit" ]
                    ]

            Confirmed ->
                div []
                    [ img [ src model.croppedImage ] []
                    ]
        ]


viewAskUpload : Html Msg
viewAskUpload =
    button [ onClick Pick ] [ text "Upload Image" ]


viewPick : Model -> Html Msg
viewPick model =
    div
        [ style "border"
            (if model.hoverUpload then
                "6px dashed purple"
             else
                "6px dashed #ccc"
            )
        , style "border-radius" "20px"
        , style "width" "480px"
        , style "margin" "100px auto"
        , style "padding" "40px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragover" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ button [ onClick Pick ] [ text "Upload Image" ]
        ]


viewCropper : Model -> Html Msg
viewCropper model =
    div []
        [ Cropper.view model.cropper |> Html.map ToCropper
        , cropInfoItems model.cropper
        , div []
            [ p []
                [ label [] [ text "Z" ]
                , input [ onInput Zoom, type_ "range", Html.Attributes.min "0", Html.Attributes.max "1", Html.Attributes.step "0.0001", value (String.fromFloat model.cropper.zoom) ] []
                , span [] [ text <| showRound 4 model.cropper.zoom ]
                ]
            , button [ onClick <| ExportImage ] [ text "Crop" ]
            ]
        ]



-- ELM/CROPPER UTILITIES


showRound : Int -> Float -> String
showRound d value =
    let
        f =
            modBy (10 ^ d) (round (value * toFloat (10 ^ d)))
    in
        String.fromInt (floor value) ++ "." ++ (String.padLeft d '0' <| String.fromInt f)


cropInfoItems : Cropper.Model -> Html Msg
cropInfoItems model =
    div [ style "max-width" (String.fromInt model.crop.width ++ "px") ]
        [ span [] [ "W: " ++ showRound 2 (Cropper.imageSize model).x |> text ]
        , span [] [ "H: " ++ showRound 2 (Cropper.imageSize model).y |> text ]
        , span [] [ "X: " ++ String.fromInt (floor (Cropper.cropOrigin model).x) |> text ]
        , span [] [ "Y: " ++ String.fromInt (floor (Cropper.cropOrigin model).y) |> text ]
        ]



-- ELM/FILE UTILITIES


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFile File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
