module ExpressionParser exposing (arithmeticOperator, booleanExpr, booleanOperator, comparisonOperator, duration, exhale, hold, inhale, minMaxOperator, number, numberExpr, optional, parseBreathing, timeUnit)

import Expression exposing (..)
import Parser exposing (..)


optional : Parser a -> Parser (Maybe a)
optional parser =
    oneOf
        [ parser |> map Just |> backtrackable
        , succeed Nothing |> backtrackable
        ]


breathing : Parser Breathing
breathing =
    succeed Breathing
        |= inhale
        |. spaces
        |= optional hold
        |. spaces
        |= exhale
        |. spaces
        |= optional hold


inhale : Parser Inhale
inhale =
    oneOf
        [ succeed Inhale
            |. symbol "inhale"
            |. spaces
            |= duration
        ]


exhale : Parser Exhale
exhale =
    oneOf
        [ succeed Exhale
            |. symbol "exhale"
            |. spaces
            |= duration
        ]


hold : Parser Hold
hold =
    succeed Hold
        |. symbol "hold breath"
        |. spaces
        |= duration


duration : Parser Duration
duration =
    Parser.lazy <|
        \_ ->
            oneOf
                [ backtrackable <|
                    succeed NumberDuration
                        |= numberExpr
                        |. spaces
                        |= timeUnit
                , backtrackable <|
                    succeed AsLongAsPossible
                        |. symbol "as long as possible"
                , backtrackable <|
                    succeed Naturally
                        |. symbol "naturally"
                , Parser.lazy conditionalDuration
                ]


conditionalDuration : () -> Parser Duration
conditionalDuration _ =
    succeed ConditionalDuration
        |. symbol "if"
        |. spaces
        |= booleanExpr ()
        |. spaces
        |. symbol "then"
        |. spaces
        |= duration
        |. spaces
        |. symbol "else"
        |. spaces
        |= duration


numberExpr : Parser NumberExpr
numberExpr =
    Parser.lazy <|
        \_ ->
            oneOf
                [ backtrackable <| Parser.lazy arithmeticNumberExpr
                , backtrackable <| Parser.lazy minMaxNumberExpr
                , succeed Number
                    |= number
                ]


arithmeticNumberExpr : () -> Parser NumberExpr
arithmeticNumberExpr _ =
    succeed ArithmeticExpr
        |= number
        |. spaces
        |= arithmeticOperator
        |. spaces
        |= numberExpr


minMaxNumberExpr : () -> Parser NumberExpr
minMaxNumberExpr _ =
    succeed MinMaxExpr
        |= minMaxOperator
        |. spaces
        |= number
        |. spaces
        |= numberExpr


arithmeticOperator : Parser ArithmeticOperator
arithmeticOperator =
    oneOf
        [ succeed Plus
            |. symbol "+"
        , succeed Minus
            |. symbol "-"
        , succeed Times
            |. symbol "*"
        , succeed Divide
            |. symbol "/"
        ]


minMaxOperator : Parser MinMaxOperator
minMaxOperator =
    oneOf
        [ succeed Min
            |. symbol "min"
        , succeed Max
            |. symbol "max"
        ]


comparisonOperator : Parser ComparisonOperator
comparisonOperator =
    oneOf
        [ succeed GreaterThanOrEqual
            |. symbol ">="
        , succeed LessThanOrEqual
            |. symbol "<="
        , succeed GreaterThan
            |. symbol ">"
        , succeed LessThan
            |. symbol "<"
        , succeed EqualTo
            |. symbol "="
        , succeed NotEqualTo
            |. symbol "!="
        ]


booleanExpr : () -> Parser BooleanExpr
booleanExpr _ =
    succeed
        (\left mbOpRight ->
            case mbOpRight of
                Just ( op, right ) ->
                    BooleanOperator left op right

                Nothing ->
                    ComparisonExpr left
        )
        |= comparisonExpr
        |= optional
            (succeed Tuple.pair
                |. spaces
                |= booleanOperator
                |. spaces
                |= lazy booleanExpr
            )


comparisonExpr : Parser ComparisonExpr
comparisonExpr =
    succeed Comparison
        |= numberExpr
        |. spaces
        |= comparisonOperator
        |. spaces
        |= numberExpr


booleanOperator : Parser BooleanOperator
booleanOperator =
    oneOf
        [ succeed And
            |. symbol "and"
        , succeed Or
            |. symbol "or"
        ]


number : Parser Number
number =
    oneOf
        [ succeed StaticNumber
            |= int
        , succeed PhaseNumber
            |. symbol "phase"
        ]


timeUnit : Parser TimeUnit
timeUnit =
    oneOf
        [ succeed Seconds
            |. symbol "s"
        , succeed Minutes
            |. symbol "m"
        ]


parseBreathing : String -> Result (List DeadEnd) Breathing
parseBreathing =
    run breathing
