module UiExplorer exposing
    ( application, defaultConfig, ApplicationConfig, Model, Msg, PageMsg
    , firstPage, nextPage, static, Page, PageSize, PageBuilder
    )

{-| Create an app that lets you browse and interact with UI you've created.

![example-image](example-image.png)
In the example above, the panel to the left is called the sidebar and the page selected in it is shown in the remaining space to the right.

Note that this package is built primarily for UI created with [`mdgriffith/elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/).
You can still use `elm/html` with [`Element.html`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#html) though.


# Application

@docs application, defaultConfig, ApplicationConfig, Model, Msg, PageMsg


# Pages

A "page" is something you can select in the sidebar to display when the app is running.
Pages can contain a single widgets, tables showing every variation of your button components, or an entire login page. It's up to you!

@docs firstPage, nextPage, static, Page, PageSize, PageBuilder

-}

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input
import Element.Region
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Task
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>))


{-| The first page in your UI explorer. This the default page if the user doesn't specify a url path.

    import Element
    import UiExplorer

    pages =
        UiExplorer.firstPage "My first page!" (UiExplorer.static (\_ _ -> Element.text "Howdy!"))

-}
firstPage : String -> Page model msg flags -> PageBuilder ( (), model ) (PageMsg () msg) flags
firstPage id config =
    PageBuilder
        { init = always ( (), Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , view =
            \_ _ _ ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    , Element.Font.size 28
                    ]
                    (Element.text "Page not found")
        , subscriptions = \_ -> Sub.none
        , ids = []
        }
        |> nextPage id config


{-| The size of the page your UI gets placed in.
This is not the same as `Browser.Events.resize` since the UI explorer displays a sidebar that can take up some of the window space.

You'll need [`ianmackenzie/elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/) in order to use `Quantity Int Pixels`.

    import Pixels

    getWidth : PageSize -> Int
    getWidth pageSize =
        Pixels.inPixels pageSize.width

-}
type alias PageSize =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


{-| All the functions you need for wiring together an interactive page. It's basically just `Browser.element`.

    import MyCoolUi
    import UiExplorer

    loginPage =
        { init = MyCoolUi.loginInit
        , update = MyCoolUi.loginUpdate
        , view = \pageSize model -> MyCoolUi.loginView model
        , subscriptions = always Sub.none
        }

    pages =
        UiExplorer.firstPage "Login Form" loginPage

-}
type alias Page model msg flags =
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : PageSize -> model -> Element msg
    , subscriptions : model -> Sub msg
    }


{-| -}
type PageBuilder model msg flags
    = PageBuilder
        { init : flags -> ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : String -> PageSize -> model -> Element msg
        , subscriptions : model -> Sub msg
        , ids : List String
        }


{-| A page that doesn't change or react to user input. It's just a view function.
-}
static : (PageSize -> flags -> Element msg) -> Page flags msg flags
static pageView =
    { init = \flags -> ( flags, Cmd.none )
    , update = \_ m -> ( m, Cmd.none )
    , view = pageView
    , subscriptions = \_ -> Sub.none
    }


{-| -}
type PageMsg previous current
    = Previous previous
    | Current current


{-| Additional pages in your UI explorer.
You have to start with `firstPage` before chaining the result to `nextPage`s.
Each page must also have a unique name.

    import Element
    import UiExplorer

    pages =
        UiExplorer.firstPage "My first page!" (UiExplorer.static (\_ _ -> Element.text "Howdy!"))
            |> UiExplorer.nextPage "My second page" (UiExplorer.static (\_ _ -> Element.none))

-}
nextPage :
    String
    -> Page model msg flags
    -> PageBuilder modelPrevious msgPrevious flags
    -> PageBuilder ( modelPrevious, model ) (PageMsg msgPrevious msg) flags
nextPage id config (PageBuilder previous) =
    let
        init_ : flags -> ( ( modelPrevious, model ), Cmd (PageMsg msgPrevious msg) )
        init_ flags =
            let
                ( previousModel, previousCmds ) =
                    previous.init flags

                ( model, cmds ) =
                    config.init flags
            in
            ( ( previousModel, model ), Cmd.batch [ Cmd.map Previous previousCmds, Cmd.map Current cmds ] )

        update_ :
            PageMsg msgPrevious msg
            -> ( modelPrevious, model )
            -> ( ( modelPrevious, model ), Cmd (PageMsg msgPrevious msg) )
        update_ msg ( previousModel, model ) =
            case msg of
                Previous previousMsg ->
                    let
                        ( newPreviousModel, previousCmds ) =
                            previous.update previousMsg previousModel
                    in
                    ( ( newPreviousModel, model ), Cmd.map Previous previousCmds )

                Current currentMsg ->
                    let
                        ( newModel, cmds ) =
                            config.update currentMsg model
                    in
                    ( ( previousModel, newModel ), Cmd.map Current cmds )

        view_ : String -> PageSize -> ( modelPrevious, model ) -> Element (PageMsg msgPrevious msg)
        view_ pageId windowSize ( previousModel, model ) =
            if id == pageId then
                config.view windowSize model |> Element.map Current

            else
                previous.view pageId windowSize previousModel |> Element.map Previous

        subscriptions_ ( previousModel, model ) =
            Sub.batch
                [ Sub.map Current (config.subscriptions model)
                , Sub.map Previous (previous.subscriptions previousModel)
                ]
    in
    PageBuilder
        { init = init_
        , update = update_
        , view = view_
        , subscriptions = subscriptions_
        , ids = id :: previous.ids
        }


{-| -}
type Msg pageMsg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | WindowResized PageSize
    | UserPressedMinimizeSidebar
    | NoOp
    | PageMsg pageMsg
    | PressedChangePageHotkey String


{-| -}
type Model pageModel flags
    = FlagsParsed (SuccessModel pageModel flags)
    | FlagsDidNotParse { errorMessage : String }


type alias SuccessModel pageModel flags =
    { page : String
    , key : Browser.Navigation.Key
    , windowSize : PageSize
    , minimizeSidebar : Bool
    , pageModel : pageModel
    , flags : flags
    }


urlParser : Url.Parser.Parser (Maybe String -> b) b
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Nothing (Url.Parser.s "ui")
        , Url.Parser.map Just (Url.Parser.s "ui" </> Url.Parser.string)
        ]


pageFromUrl : PageBuilder pageModel pageMsg flags -> Browser.Navigation.Key -> Url -> ( String, Cmd (Msg pageMsg) )
pageFromUrl (PageBuilder pages) key url =
    case Url.Parser.parse urlParser url of
        Just Nothing ->
            case pages.ids |> List.reverse |> List.head of
                Just firstPage_ ->
                    ( "", Browser.Navigation.replaceUrl key (uiUrl firstPage_) )

                Nothing ->
                    ( "", Cmd.none )

        Just (Just page) ->
            case Url.percentDecode page of
                Just decodedPage ->
                    ( decodedPage, Cmd.none )

                Nothing ->
                    ( "", Cmd.none )

        Nothing ->
            ( "", Cmd.none )


uiUrl : String -> String
uiUrl pageId =
    Url.Builder.absolute [ "ui", Url.percentEncode pageId ] []


init :
    Decode.Decoder flags
    -> PageBuilder pageModel pageMsg flags
    -> Decode.Value
    -> Url
    -> Browser.Navigation.Key
    -> ( Model pageModel flags, Cmd (Msg pageMsg) )
init flagsDecoder (PageBuilder pages) flagsJson url key =
    let
        ( page, navigationCmd ) =
            pageFromUrl (PageBuilder pages) key url
    in
    case Decode.decodeValue flagsDecoder flagsJson of
        Ok flags ->
            let
                ( pageModels, pageCmds ) =
                    pages.init flags
            in
            ( FlagsParsed
                { page = page
                , key = key

                -- We can't know the window size since flags are user defined so we make a guess.
                , windowSize = { width = Pixels.pixels 1920, height = Pixels.pixels 1080 }
                , minimizeSidebar = False
                , flags = flags
                , pageModel = pageModels
                }
            , Cmd.batch
                [ navigationCmd
                , Browser.Dom.getViewport
                    |> Task.map
                        (\{ viewport } ->
                            -- The window size should always be integer values so it's safe to round here.
                            { width = Pixels.pixels (round viewport.width)
                            , height = Pixels.pixels (round viewport.height)
                            }
                        )
                    |> Task.perform WindowResized
                , Cmd.map PageMsg pageCmds
                ]
            )

        Err error ->
            ( FlagsDidNotParse { errorMessage = Decode.errorToString error }, Cmd.none )


update :
    PageBuilder pageModel pageMsg flags
    -> Msg pageMsg
    -> Model pageModel flags
    -> ( Model pageModel flags, Cmd (Msg pageMsg) )
update pages msg model =
    case model of
        FlagsParsed successModel ->
            updateSuccess pages msg successModel |> Tuple.mapFirst FlagsParsed

        FlagsDidNotParse _ ->
            ( model, Cmd.none )


updateSuccess :
    PageBuilder pageModel pageMsg flags
    -> Msg pageMsg
    -> SuccessModel pageModel flags
    -> ( SuccessModel pageModel flags, Cmd (Msg pageMsg) )
updateSuccess (PageBuilder pages) msg model =
    case msg of
        UrlChanged url ->
            let
                ( page, pageCmd ) =
                    pageFromUrl (PageBuilder pages) model.key url
            in
            ( { model | page = page }, pageCmd )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        NoOp ->
            ( model, Cmd.none )

        WindowResized size ->
            ( { model | windowSize = size }, Cmd.none )

        UserPressedMinimizeSidebar ->
            ( { model | minimizeSidebar = not model.minimizeSidebar }, Cmd.none )

        PageMsg pageMsg ->
            let
                ( pageModel, pageCmd ) =
                    pages.update pageMsg model.pageModel
            in
            ( { model | pageModel = pageModel }, Cmd.map PageMsg pageCmd )

        PressedChangePageHotkey pageId ->
            ( model
            , Cmd.batch
                [ Browser.Navigation.pushUrl model.key pageId
                , Browser.Dom.focus pageId |> Task.attempt (always NoOp)
                ]
            )


view :
    ApplicationConfig (Msg pageMsg) flags
    -> PageBuilder pageModel pageMsg flags
    -> Model pageModel flags
    -> { title : String, body : List (Html (Msg pageMsg)) }
view config pages model =
    case model of
        FlagsParsed successModel ->
            viewSuccess config pages successModel

        FlagsDidNotParse { errorMessage } ->
            { title = "Error"
            , body =
                [ Element.layoutWith { options = config.layoutOptions }
                    (Element.width Element.fill
                        :: Element.height Element.fill
                        :: config.layoutAttributes
                    )
                    (errorView errorMessage)
                ]
            }


textColor : Element.Color
textColor =
    Element.rgb255 56 60 67


errorView : String -> Element msg
errorView errorMessage =
    Element.column
        [ Element.Region.announce
        , Element.width Element.fill
        , Element.Background.color (Element.rgb255 250 237 236)
        , Element.Font.color textColor
        , Element.padding 16
        , Element.spacing 16
        ]
        [ Element.el [ Element.Font.size 20 ] <| Element.text "Failed to parse flags"
        , Html.div
            [ Html.Attributes.style "white-space" "pre-wrap"
            , Html.Attributes.style "line-height" "1.25"
            , Html.Attributes.style "padding-top" "0"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "word-break" "break-word"
            ]
            [ Html.text errorMessage ]
            |> Element.html
            |> Element.el [ Element.Font.size 16 ]
        ]


viewSuccess :
    ApplicationConfig (Msg pageMsg) flags
    -> PageBuilder pageModel pageMsg flags
    -> SuccessModel pageModel flags
    -> Browser.Document (Msg pageMsg)
viewSuccess config (PageBuilder pages) model =
    { title = "UI Explorer"
    , body =
        [ Element.layoutWith { options = config.layoutOptions }
            (Element.width Element.fill
                :: Element.height Element.fill
                :: Element.inFront
                    (Element.el
                        [ Element.height <| Element.px (Pixels.inPixels model.windowSize.height)
                        , Element.Font.size 16
                        ]
                        (viewSidebar (PageBuilder pages) model.minimizeSidebar model.page)
                    )
                :: config.layoutAttributes
            )
            (Element.row
                [ Element.width Element.fill
                , Element.height Element.fill

                -- Parts of the page (things using inFront, above, onLeft, etc) will appear in front of the sidebar if we don't set a negative index
                , Element.htmlAttribute <| Html.Attributes.style "z-index" "-100"
                ]
                [ Element.el
                    [ if isMobile model.windowSize || model.minimizeSidebar then
                        Element.width (Element.px 0)

                      else
                        Element.width (Element.px sidebarWidth)
                    ]
                    Element.none
                , Element.el
                    [ Element.alignTop
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (pages.view model.page (contentSize model) model.pageModel
                        |> Element.map PageMsg
                    )
                ]
            )
        ]
    }


isMobile : PageSize -> Bool
isMobile =
    .width >> Quantity.lessThan (Pixels.pixels 700)


sidebarWidth : number
sidebarWidth =
    210


viewSidebar : PageBuilder pageModel pageMsg flags -> Bool -> String -> Element (Msg pageMsg)
viewSidebar pages minimized pageId =
    if minimized then
        minimizeSidebarButton minimized

    else
        Element.column
            [ Element.width (Element.px sidebarWidth)
            , Element.height Element.fill

            -- For some reason a horizontal scrollbar pops up unless we include this.
            , Element.htmlAttribute <| Html.Attributes.style "overflow-x" "hidden"
            , Element.scrollbars
            , Element.Background.color lightGray
            ]
            [ Element.row
                [ Element.width Element.fill ]
                [ title, minimizeSidebarButton minimized ]
            , Element.el
                [ Element.scrollbarY, Element.width Element.fill, Element.height Element.fill ]
                (viewSidebarLinks pages pageId)
            ]


title : Element msg
title =
    Element.el
        [ Element.Font.size 20, Element.padding 16 ]
        (Element.text "UI Explorer")


contentSize : SuccessModel pageModel flags -> PageSize
contentSize model =
    if isMobile model.windowSize then
        model.windowSize

    else if model.minimizeSidebar then
        { width = model.windowSize.width
        , height = model.windowSize.height
        }

    else
        { width = model.windowSize.width |> Quantity.minus (Pixels.pixels sidebarWidth)
        , height = model.windowSize.height
        }


minimizeSidebarButton : Bool -> Element (Msg pageMsg)
minimizeSidebarButton minimized =
    Element.row
        [ Element.alignRight ]
        [ -- We include the title here (but hide it) so that the button has the correct height.
          Element.el [ Element.width <| Element.px 0, Element.transparent True ] title
        , Element.Input.button
            [ Element.paddingXY 20 0
            , Element.height Element.fill
            , Element.Font.size 20
            , Element.alpha 0.5
            , Element.Background.color lightGray
            , Element.mouseOver
                [ Element.alpha 1
                , Element.Background.color gray
                ]
            , Element.focused []
            ]
            { onPress = Just UserPressedMinimizeSidebar
            , label =
                Element.text
                    (if minimized then
                        "❯"

                     else
                        "❮"
                    )
            }
        ]


lightBlue : Element.Color
lightBlue =
    Element.rgb255 176 208 225


lightGray : Element.Color
lightGray =
    Element.rgb255 228 234 241


gray : Element.Color
gray =
    Element.rgb255 206 215 225


type ArrowKey
    = ArrowUp
    | ArrowDown


onKey : (ArrowKey -> msg) -> Element.Attribute msg
onKey msg =
    Element.htmlAttribute
        (Html.Events.custom "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowUp" ->
                                Decode.succeed { message = msg ArrowUp, stopPropagation = True, preventDefault = True }

                            "ArrowDown" ->
                                Decode.succeed { message = msg ArrowDown, stopPropagation = True, preventDefault = True }

                            _ ->
                                Decode.fail "Not the up or down arrow key."
                    )
            )
        )


listNeighbors : List a -> List { previous : Maybe a, current : a, next : Maybe a }
listNeighbors list =
    listNeighborsHelper list { previous = Nothing, current = Nothing, next = Nothing } [] |> List.reverse


listNeighborsHelper :
    List a
    -> { previous : Maybe a, current : Maybe a, next : Maybe a }
    -> List { previous : Maybe a, current : a, next : Maybe a }
    -> List { previous : Maybe a, current : a, next : Maybe a }
listNeighborsHelper list { previous, current, next } newList =
    case list of
        head :: rest ->
            let
                newState =
                    { previous = current
                    , current = next
                    , next = Just head
                    }
            in
            case next of
                Just next_ ->
                    listNeighborsHelper rest newState ({ previous = current, current = next_, next = Just head } :: newList)

                _ ->
                    listNeighborsHelper rest newState newList

        [] ->
            case next of
                Just next_ ->
                    { previous = current, current = next_, next = Nothing } :: newList

                Nothing ->
                    newList


viewSidebarLinks : PageBuilder pageModel pageMsg flags -> String -> Element (Msg pageMsg)
viewSidebarLinks (PageBuilder pages) currentPageId =
    pages.ids
        |> List.sort
        |> listNeighbors
        |> List.map
            (\pageIds ->
                Element.link
                    [ Element.paddingEach { left = 16, right = 8, top = 8, bottom = 8 }
                    , Element.width Element.fill
                    , onKey
                        (\arrowKey ->
                            case ( arrowKey, pageIds.previous, pageIds.next ) of
                                ( ArrowUp, Just previous, _ ) ->
                                    PressedChangePageHotkey previous

                                ( ArrowDown, _, Just next ) ->
                                    PressedChangePageHotkey next

                                _ ->
                                    NoOp
                        )
                    , Element.htmlAttribute <| Html.Attributes.id pageIds.current
                    , if pageIds.current == currentPageId then
                        Element.Background.color lightBlue

                      else
                        Element.mouseOver [ Element.Background.color gray ]
                    ]
                    { url = uiUrl pageIds.current
                    , label = Element.paragraph [] [ Element.text pageIds.current ]
                    }
            )
        |> Element.column
            [ Element.width Element.fill
            , Element.Font.medium
            ]


subscriptions : Model pageModel flags -> Sub (Msg pageMsg)
subscriptions _ =
    Browser.Events.onResize
        (\width height ->
            WindowResized
                { width = Pixels.pixels width, height = Pixels.pixels height }
        )


{-| Settings we can change when creating our UI explorer application.

  - `flagsDecoder` lets us parse json flags we pass to our app. This gets passed along to the init function in our pages (or the view function if you're creating a static page).
  - `layoutOptions` and `layoutAttributes` are used in our app's Element.layoutWith to control things like the default font or Element.focusStyle

-}
type alias ApplicationConfig msg flags =
    { flagsDecoder : Decode.Decoder flags
    , layoutOptions : List Element.Option
    , layoutAttributes : List (Element.Attribute msg)
    }


{-| Default application configuration.
-}
defaultConfig : ApplicationConfig msg ()
defaultConfig =
    { flagsDecoder = Decode.succeed ()
    , layoutOptions = []
    , layoutAttributes = []
    }


{-| Here we create our UI explorer app.

    import MyCoolUi
    import UiExplorer

    pages =
        UiExplorer.firstPage "Button" (UiExplorer.static MyCoolUi.button)
            |> UiExplorer.nextPage "Footer" (UiExplorer.static MyCoolUi.footer)
            |> UiExplorer.nextPage
                "Login Form"
                { init = MyCoolUi.loginInit
                , update = MyCoolUi.loginUpdate
                , view = \pageSize model -> MyCoolUi.loginView model
                , subscriptions = always Sub.none
                }

    main =
        UiExplorer.application UiExplorer.defaultConfig pages

Note that we didn't add type signatures for `pages` and `main` in the example.
If we did, we'd have to update it every time we add a new page and the type signatures would get messy.
Instead it's best to just let the compiler infer it automatically.

-}
application :
    ApplicationConfig (Msg pageMsg) flags
    -> PageBuilder pageModel pageMsg flags
    -> Platform.Program Decode.Value (Model pageModel flags) (Msg pageMsg)
application config pages =
    Browser.application
        { init = init config.flagsDecoder pages
        , view = view config pages
        , update = update pages
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
