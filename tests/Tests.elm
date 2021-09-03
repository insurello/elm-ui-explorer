module Tests exposing (..)

import Element
import Expect
import Test exposing (describe, test)
import UiExplorer


pages =
    UiExplorer.firstPage "First page" (UiExplorer.static (\_ _ -> Element.none))
        |> UiExplorer.groupPages "/nested/"
            (UiExplorer.nextPage "nested_page" (UiExplorer.static (\_ _ -> Element.none))
                >> UiExplorer.groupPages "DoubleNested"
                    (UiExplorer.nextPage
                        "DoubleNestedPage"
                        (UiExplorer.static (\_ _ -> Element.none))
                    )
            )


{-| Not a lot we can test really.
-}
tests =
    describe "UI explorer tests"
        [ test "Get page paths" <|
            \_ ->
                UiExplorer.getPagePaths [] pages
                    |> Expect.equal
                        [ "/%2Fnested%2F/DoubleNested/DoubleNestedPage"
                        , "/%2Fnested%2F/nested_page"
                        , "/First%20page"
                        ]
        , test "Get page paths with relative path" <|
            \_ ->
                UiExplorer.getPagePaths [ "relative", "path" ] pages
                    |> Expect.equal
                        [ "/relative/path/%2Fnested%2F/DoubleNested/DoubleNestedPage"
                        , "/relative/path/%2Fnested%2F/nested_page"
                        , "/relative/path/First%20page"
                        ]
        ]
