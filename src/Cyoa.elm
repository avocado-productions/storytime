module Cyoa exposing (main)

import Runtime
import ScriptTypes as Script


start : Script.Scene
start =
    { key = ""
    , contents = "Welcome"
    , options = [ { key = "intro", text = "Come into the forest" } ]
    }


script : List Script.Scene
script =
    [ { key = "intro"
      , contents =
          "Two roads diverged in a yellow wood, and sorry I could not travel both and be one traveler, long I stood and looked down one as far as I could to where it bent in the undergrowth."
      , options =
          [ { key = "first", text = "Take the first, less grassy and worn" }
          , { key = "second", text = "Take the second, perhaps wanting wear" }
          ]
      }
    , { key = "first"
      , contents = "It made no difference at all. The end."
      , options = []
      }
    , { key = "second"
      , contents = "It made all the difference. The end."
      , options = []
      }
    ]


main : Runtime.StorytimeProgram
main =
    Runtime.app start script