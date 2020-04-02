module App.UserAgent exposing (UserAgent)


type alias UserAgent =
    { os : OS
    , browser : Browser
    , engine : Engine
    }


type alias OS =
    { name : String
    }


type alias Browser =
    { name : String
    }


type alias Engine =
    { name : String
    }
