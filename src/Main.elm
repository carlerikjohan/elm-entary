module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Random



-- Main


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Quote =
    { text : String
    , author : String
    }


type State
    = Intro
    | HttpFailure String
    | Loading
    | Success


type alias Model =
    { state : State
    , quotes : List Quote
    , correctAnswer : Quote
    , score : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Intro [] { author = "", text = "" } 0, Cmd.none )



-- Update


getQuoteAtIndex : List Quote -> Int -> Maybe Quote
getQuoteAtIndex quotes index =
    Array.get index (Array.fromList quotes)


getAnswer : Maybe Quote -> Quote
getAnswer maybeQuote =
    case maybeQuote of
        Nothing ->
            { author = "", text = "" }

        Just quote ->
            quote


scoreIfRightAnswer : Int -> Quote -> Quote -> Int
scoreIfRightAnswer score correctAnswer answer =
    case correctAnswer == answer of
        True ->
            score + 1

        False ->
            score


type Msg
    = GetQuote
    | GotQuote (Result Http.Error (List Quote))
    | SetCorrectAnswer Int
    | Answer Quote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( { model | state = Loading }, getRandomQuotes )

        GotQuote result ->
            case result of
                Ok response ->
                    ( { model | quotes = response }, Random.generate SetCorrectAnswer (Random.int 0 1) )

                Err error ->
                    ( { model | state = HttpFailure (Debug.toString error) }, Cmd.none )

        SetCorrectAnswer index ->
            ( { model | state = Success, correctAnswer = getAnswer (getQuoteAtIndex model.quotes index) }, Cmd.none )

        Answer answer ->
            ( { model | state = Loading, score = scoreIfRightAnswer model.score model.correctAnswer answer }, getRandomQuotes )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


renderError : String -> Html Msg
renderError error =
    div [ class "failure" ]
        [ text error
        , button [ onClick GetQuote ] [ text "Try again!" ]
        ]


renderQuote : Quote -> Html Msg
renderQuote quote =
    text quote.text


renderAnswerButton : Quote -> Html Msg
renderAnswerButton quote =
    button [ onClick (Answer quote), class "button" ] [ text quote.author ]


view : Model -> Html Msg
view model =
    case model.state of
        Intro ->
            div []
                [ h1 [] []
                , button [ onClick GetQuote ] [ text "Start!" ]
                ]

        HttpFailure error ->
            renderError error

        Loading ->
            text "Loading..."

        Success ->
            div [ class "success" ]
                [ div [ class "quote" ] [ renderQuote model.correctAnswer ]
                , h2 [ class "who" ] [ text (String.fromInt model.score) ]
                , div [ class "buttons" ] (List.map renderAnswerButton model.quotes)
                ]



-- HTTP


getRandomQuotes : Cmd Msg
getRandomQuotes =
    Http.get
        { url = "https://goquotes-api.herokuapp.com/api/v1/random?count=2"

        --url = "akj"
        , expect = Http.expectJson GotQuote responseDecoder
        }



-- Decoders


quoteDecoder : Decoder Quote
quoteDecoder =
    Json.Decode.map2 Quote
        (field "text" string)
        (field "author" string)


quotesDecoder : Decoder (List Quote)
quotesDecoder =
    Json.Decode.list quoteDecoder


responseDecoder : Decoder (List Quote)
responseDecoder =
    field "quotes" quotesDecoder
