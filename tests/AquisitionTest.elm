module AquisitionTest exposing (..)

import Expect
import Test exposing (..)
import Type.Database exposing (..)
import Type.IO exposing (..)

suite : Test
suite = 
    describe "Main" [
        fuzz string.fuzzer "string" <|
            \val ->
                Expect.equal val val
    ]