module ExpressionTest exposing (..)

import Expect
import Expression as E
import ExpressionParser as P
import ExpressionSerializer as S
import Fuzz exposing (..)
import Parser
import Test exposing (fuzz)
import Parser.Advanced


{-| Breathing fuzzers
-}
fuzzBreathing : Fuzzer E.Breathing
fuzzBreathing =
    Fuzz.map4 E.Breathing
        fuzzInhale
        (Fuzz.maybe fuzzHold)
        fuzzExhale
        (Fuzz.maybe fuzzHold)


{-| Inhale fuzzers
-}
fuzzInhale : Fuzzer E.Inhale
fuzzInhale =
    Fuzz.oneOf
        [ Fuzz.map E.Inhale fuzzDuration
        ]


{-| Exhale fuzzers
-}
fuzzExhale : Fuzzer E.Exhale
fuzzExhale =
    Fuzz.oneOf
        [ Fuzz.map E.Exhale fuzzDuration
        ]


{-| Hold fuzzers
-}
fuzzHold : Fuzzer E.Hold
fuzzHold =
    Fuzz.map E.Hold fuzzDuration


{-| Duration fuzzers
-}
fuzzDuration : Fuzzer E.Duration
fuzzDuration =
    let
        go number =
            Fuzz.frequency <|
                ( 1, Fuzz.map2 E.NumberDuration (fuzzNumberExpr 0) fuzzTimeUnit )
                    :: ( 1, Fuzz.constant E.AsLongAsPossible )
                    :: ( 1, Fuzz.constant E.Naturally )
                    :: (if number > 1 then
                            []

                        else
                            [ ( 3
                              , Fuzz.map3 E.ConditionalDuration (fuzzBooleanExpr 0) (go (number + 1)) (go (number + 1))
                              )
                            ]
                       )
    in
    go 0


{-| NumberExpr fuzzers
-}
fuzzNumberExpr : Int -> Fuzzer E.NumberExpr
fuzzNumberExpr =
    let
        go number =
            if number > 1 then
                Fuzz.map E.Number fuzzNumber

            else
                Fuzz.frequency <|
                    [ ( 3, Fuzz.map3 E.ArithmeticExpr fuzzNumber fuzzArithmeticOperator (go (number + 1)) )
                    , ( 3, Fuzz.map3 E.MinMaxExpr fuzzMinMaxOperator fuzzNumber (go (number + 1)) )
                    ]
    in
    \n -> lazy <| \_ -> go n


{-| ArithmeticOperator fuzzers
-}
fuzzArithmeticOperator : Fuzzer E.ArithmeticOperator
fuzzArithmeticOperator =
    Fuzz.oneOf [ constant E.Plus, constant E.Minus, constant E.Times, constant E.Divide ]


{-| MinMaxOperator fuzzers
-}
fuzzMinMaxOperator : Fuzzer E.MinMaxOperator
fuzzMinMaxOperator =
    Fuzz.oneOf [ constant E.Min, constant E.Max ]


{-| ComparisonOperator fuzzers
-}
fuzzComparisonOperator : Fuzzer E.ComparisonOperator
fuzzComparisonOperator =
    Fuzz.oneOf
        [ constant E.GreaterThanOrEqual
        , constant E.LessThanOrEqual
        , constant E.GreaterThan
        , constant E.LessThan
        , constant E.EqualTo
        , constant E.NotEqualTo
        ]


{-| ComparisonExpr fuzzers
-}
fuzzComparisonExpr : Int -> Fuzzer E.ComparisonExpr
fuzzComparisonExpr number =
    Fuzz.map3 E.Comparison (fuzzNumberExpr (number + 1)) fuzzComparisonOperator (fuzzNumberExpr (number + 1))


{-| BooleanExpr fuzzers
-}
fuzzBooleanExpr : Int -> Fuzzer E.BooleanExpr
fuzzBooleanExpr =
    let
        go number =
            lazy <|
                \_ ->
                    if number > 1 then
                        Fuzz.map E.ComparisonExpr (fuzzComparisonExpr (number + 1))

                    else
                        Fuzz.frequency <|
                            [ ( 3
                              , Fuzz.map3 E.BooleanOperator (fuzzComparisonExpr (number + 1)) fuzzBooleanOperator (go (number + 1))
                              )
                            ]
    in
    go


{-| BooleanOperator fuzzers
-}
fuzzBooleanOperator : Fuzzer E.BooleanOperator
fuzzBooleanOperator =
    Fuzz.oneOf [ constant E.And, constant E.Or ]


{-| Number fuzzers
-}
fuzzNumber : Fuzzer E.Number
fuzzNumber =
    Fuzz.oneOf
        [ Fuzz.map E.StaticNumber <| Fuzz.filter ((<=) 0) int
        , Fuzz.constant E.PhaseNumber
        ]


{-| TimeUnit fuzzers
-}
fuzzTimeUnit : Fuzzer E.TimeUnit
fuzzTimeUnit =
    Fuzz.oneOf
        [ Fuzz.constant E.Seconds
        , Fuzz.constant E.Minutes
        ]


test : Test.Test
test =
    Test.describe "Expression"
        [ fuzz fuzzBreathing "breathing" <|
            \breathing ->
                Expect.equal
                    (breathing |> S.breathingToString |> P.parseBreathing)
                    (Ok breathing)
        , fuzz fuzzInhale "inhale" <|
            \inhale ->
                Expect.equal
                    (inhale |> S.inhaleToString |> Parser.run P.inhale)
                    (Ok inhale)
        , fuzz fuzzExhale "exhale" <|
            \exhale ->
                Expect.equal
                    (exhale |> S.exhaleToString |> Parser.run P.exhale)
                    (Ok exhale)
        , fuzz fuzzHold "hold" <|
            \hold ->
                Expect.equal
                    (hold |> S.holdToString |> Parser.run P.hold)
                    (Ok hold)
        , fuzz fuzzDuration "duration" <|
            \duration ->
                Expect.equal
                    (duration |> S.durationToString |> Parser.Advanced.run P.duration)
                    (Ok duration)
                    |> Expect.onFail
                        (String.join "\n"
                            [ "input:" ++ (duration |> S.durationToString)
                            , "expected:" ++ Debug.toString duration
                            , "actual:" ++ Debug.toString (duration |> S.durationToString |> Parser.run P.duration)
                            ]
                        )
        , fuzz (fuzzNumberExpr 0) "numberExpr" <|
            \numberExpr ->
                Expect.equal
                    (numberExpr |> S.numberExprToString |> Parser.run P.numberExpr)
                    (Ok numberExpr)
                    |> Expect.onFail
                        (String.join "\n"
                            [ "input:" ++ (numberExpr |> S.numberExprToString)
                            , "expected:" ++ Debug.toString numberExpr
                            , "actual:" ++ Debug.toString (numberExpr |> S.numberExprToString |> Parser.run P.numberExpr)
                            ]
                        )
        , fuzz fuzzTimeUnit "timeUnit" <|
            \timeUnit ->
                Expect.equal
                    (timeUnit |> S.timeUnitToString |> Parser.run P.timeUnit)
                    (Ok timeUnit)
        , fuzz (fuzzBooleanExpr 0) "booleanExpr" <|
            \booleanExpr ->
                Expect.equal
                    (booleanExpr |> S.booleanExprToString |> Parser.run (P.booleanExpr ()))
                    (Ok booleanExpr)
                    |> Expect.onFail
                        (String.join "\n"
                            [ "input:" ++ (booleanExpr |> S.booleanExprToString)
                            , "expected:" ++ Debug.toString booleanExpr
                            , "actual:" ++ Debug.toString (booleanExpr |> S.booleanExprToString |> Parser.run (P.booleanExpr ()))
                            ]
                        )
        , fuzz fuzzNumber "number" <|
            \number ->
                Expect.equal
                    (number |> S.numberToString |> Parser.run P.number)
                    (Ok number)
        , fuzz fuzzArithmeticOperator "arithmeticOperator" <|
            \arithmeticOperator ->
                Expect.equal
                    (arithmeticOperator |> S.arithmeticOperatorToString |> Parser.run P.arithmeticOperator)
                    (Ok arithmeticOperator)
        , fuzz fuzzMinMaxOperator "minMaxOperator" <|
            \minMaxOperator ->
                Expect.equal
                    (minMaxOperator |> S.minMaxOperatorToString |> Parser.run P.minMaxOperator)
                    (Ok minMaxOperator)
        , fuzz fuzzComparisonOperator "comparisonOperator" <|
            \comparisonOperator ->
                Expect.equal
                    (comparisonOperator |> S.comparisonOperatorToString |> Parser.run P.comparisonOperator)
                    (Ok comparisonOperator)
        , fuzz fuzzBooleanOperator "booleanOperator" <|
            \booleanOperator ->
                Expect.equal
                    (booleanOperator |> S.booleanOperatorToString |> Parser.run P.booleanOperator)
                    (Ok booleanOperator)
        ]
