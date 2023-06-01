module Type exposing (..)

import Expression exposing (Breathing)


type alias BreathingTechnique =
    { title : String
    , breathing : Breathing
    }


type alias BreathingId =
    Breathing


type alias BreathingResult =
    { breathingId : BreathingId
    , seconds : Float
    , phases : Int
    }
