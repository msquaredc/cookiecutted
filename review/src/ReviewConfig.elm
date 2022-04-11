module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoAlways
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDeprecated
import NoDuplicatePorts
import NoEmptyText
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoUnsafePorts
import NoUnusedPorts
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoAlways.rule
    , NoBooleanCase.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoDuplicatePorts.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnusedPorts.rule
    , NoEmptyText.rule
    , NoExposingEverything.rule
    , NoDeprecated.rule NoDeprecated.defaults
    , NoImportingEverything.rule []
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
        |> Review.Rule.ignoreErrorsForFiles [ "src/Type/Database/TypeMatching.elm" ]
    , NoMissingTypeExpose.rule
    , NoPrematureLetComputation.rule
    ]
