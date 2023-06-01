module ExpressionSerializer exposing
    ( arithmeticOperatorToString
    , booleanExprToString
    , booleanOperatorToString
    , breathingToString
    , comparisonOperatorToString
    , durationToString
    , exhaleToString
    , holdToString
    , inhaleToString
    , minMaxOperatorToString
    , numberExprToString
    , numberToString
    , timeUnitToString
    )

import Expression exposing (..)


breathingToString : Breathing -> String
breathingToString (Breathing inhale maybeHold1 exhale maybeHold2) =
    String.join " " <|
        List.filterMap identity
            [ Just <| inhaleToString inhale
            , Maybe.map holdToString maybeHold1
            , Just <| exhaleToString exhale
            , Maybe.map holdToString maybeHold2
            ]


inhaleToString : Inhale -> String
inhaleToString inhale =
    case inhale of
        Inhale duration ->
            "inhale " ++ durationToString duration


exhaleToString : Exhale -> String
exhaleToString exhale =
    case exhale of
        Exhale duration ->
            "exhale " ++ durationToString duration


holdToString : Hold -> String
holdToString (Hold duration) =
    "hold breath " ++ durationToString duration


durationToString : Duration -> String
durationToString duration =
    case duration of
        NumberDuration numberExpr timeUnit ->
            numberExprToString numberExpr ++ " " ++ timeUnitToString timeUnit

        AsLongAsPossible ->
            "as long as possible"

        Naturally ->
            "naturally"

        ConditionalDuration booleanExpr duration1 duration2 ->
            "if "
                ++ booleanExprToString booleanExpr
                ++ " then "
                ++ durationToString duration1
                ++ " else "
                ++ durationToString duration2


numberExprToString : NumberExpr -> String
numberExprToString numberExpr =
    case numberExpr of
        Number number ->
            numberToString number

        ArithmeticExpr number operator numberExpr2 ->
            numberToString number
                ++ " "
                ++ arithmeticOperatorToString operator
                ++ " "
                ++ numberExprToString numberExpr2

        MinMaxExpr operator number1 numberExpr2 ->
            minMaxOperatorToString operator
                ++ " "
                ++ numberToString number1
                ++ " "
                ++ numberExprToString numberExpr2


timeUnitToString : TimeUnit -> String
timeUnitToString timeUnit =
    case timeUnit of
        Seconds ->
            "s"

        Minutes ->
            "m"


booleanExprToString : BooleanExpr -> String
booleanExprToString booleanExpr =
    case booleanExpr of
        ComparisonExpr comparisonExpr ->
            comparisonExprToString comparisonExpr

        BooleanOperator comparisonExpr operator booleanExpr2 ->
            comparisonExprToString comparisonExpr
                ++ " "
                ++ booleanOperatorToString operator
                ++ " "
                ++ booleanExprToString booleanExpr2


comparisonExprToString : ComparisonExpr -> String
comparisonExprToString comparisonExpr =
    case comparisonExpr of
        Comparison numberExpr1 operator numberExpr2 ->
            numberExprToString numberExpr1
                ++ " "
                ++ comparisonOperatorToString operator
                ++ " "
                ++ numberExprToString numberExpr2


numberToString : Number -> String
numberToString number =
    case number of
        StaticNumber intValue ->
            String.fromInt intValue

        PhaseNumber ->
            "phase"


arithmeticOperatorToString : ArithmeticOperator -> String
arithmeticOperatorToString operator =
    case operator of
        Plus ->
            "+"

        Minus ->
            "-"

        Times ->
            "*"

        Divide ->
            "/"


minMaxOperatorToString : MinMaxOperator -> String
minMaxOperatorToString operator =
    case operator of
        Min ->
            "min"

        Max ->
            "max"


comparisonOperatorToString : ComparisonOperator -> String
comparisonOperatorToString operator =
    case operator of
        GreaterThanOrEqual ->
            ">="

        LessThanOrEqual ->
            "<="

        GreaterThan ->
            ">"

        LessThan ->
            "<"

        EqualTo ->
            "="

        NotEqualTo ->
            "!="


booleanOperatorToString : BooleanOperator -> String
booleanOperatorToString operator =
    case operator of
        And ->
            "and"

        Or ->
            "or"

