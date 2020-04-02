# elm-ui-explorer

![example-image](https://raw.githubusercontent.com/insurello/elm-ui-explorer/master/example-image.png)

### Create an app that lets you browse and interact with UI you've created.

_Why not use one of the several other UI viewing packages like [`alexkorban/uicards`](https://package.elm-lang.org/packages/alexkorban/uicards/latest/), [`kalutheo/elm-ui-explorer`](https://package.elm-lang.org/packages/kalutheo/elm-ui-explorer/latest/), [`miyamoen/bibliopola`](https://package.elm-lang.org/packages/miyamoen/bibliopola/latest/), and [`owanturist/elm-bulletproof`](https://package.elm-lang.org/packages/owanturist/elm-bulletproof/latest/)?_

Most of these packages don't support interacting with UI beyond adding extra buttons to toggle the UIs appearance.
An exception is `alexkorban/uicards` but it requires that all your UIs have the same msg and model type.

## Example app

Here's a small example app with 3 pages. The first two pages show static content and the last page is interactive.
To get it working, swap out "MyCoolUi" with whatever you want to show.
```elm
import MyCoolUi
import UiExplorer

pages =
    UiExplorer.firstPage 
        "Button" 
        (UiExplorer.static MyCoolUi.button)
        |> UiExplorer.nextPage 
            "Footer" 
            (UiExplorer.static MyCoolUi.footer)
        |> UiExplorer.nextPage
            "Login Form"
            { init = MyCoolUi.loginInit
            , update = MyCoolUi.loginUpdate
            , view = 
                \pageSize model -> MyCoolUi.loginView model
            , subscriptions = always Sub.none
            }

main =
    UiExplorer.application UiExplorer.defaultConfig pages
```

For a real world use case [here's](https://insurello.github.io/elm-ui-explorer/) how we use it at Insurello (the code can be found [here](https://github.com/insurello/elm-ui-explorer/tree/master/example)).

[![Insurello](https://gitcdn.xyz/repo/insurello/elm-swedish-bank-account-number/master/insurello.svg)](https://jobb.insurello.se/departments/product-tech)