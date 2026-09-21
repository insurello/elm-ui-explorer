# elm-ui-explorer

Create an app that lets you browse and interact with UI you've created.

## Example app

Here's a small example app with 3 pages. The first two pages show static content and the last page is interactive.
To get it working, swap out "MyUI" with whatever you want to show.

```elm
import MyUI
import UiExplorer

pages =
    UiExplorer.firstPage 
        "Button" 
        (UiExplorer.static MyUI.button)
        |> UiExplorer.nextPage 
            "Footer" 
            (UiExplorer.static MyUI.footer)
        |> UiExplorer.nextPage
            "Login Form"
            { init = MyUI.loginInit
            , update = MyUI.loginUpdate
            , view = 
                \pageSize model -> MyUI.loginView model
            , subscriptions = always Sub.none
            }

main =
    UiExplorer.application UiExplorer.defaultConfig pages
```
