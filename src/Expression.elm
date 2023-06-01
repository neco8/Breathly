module Expression exposing
    ( ArithmeticOperator(..)
    , BooleanExpr(..)
    , BooleanOperator(..)
    , Breathing(..)
    , ComparisonExpr(..)
    , ComparisonOperator(..)
    , Duration(..)
    , Exhale(..)
    , Hold(..)
    , Inhale(..)
    , MinMaxOperator(..)
    , Number(..)
    , NumberExpr(..)
    , TimeUnit(..)
    )


type Breathing
    = Breathing Inhale (Maybe Hold) Exhale (Maybe Hold)


type Inhale
    = Inhale Duration


type Exhale
    = Exhale Duration


type Hold
    = Hold Duration


type Duration
    = NumberDuration NumberExpr TimeUnit
    | AsLongAsPossible
    | Naturally
    | ConditionalDuration BooleanExpr Duration Duration


type NumberExpr
    = Number Number
    | ArithmeticExpr Number ArithmeticOperator NumberExpr
    | MinMaxExpr MinMaxOperator Number NumberExpr


type ArithmeticOperator
    = Plus
    | Minus
    | Times
    | Divide


type MinMaxOperator
    = Min
    | Max


type ComparisonOperator
    = GreaterThanOrEqual
    | LessThanOrEqual
    | GreaterThan
    | LessThan
    | EqualTo
    | NotEqualTo


type ComparisonExpr
    = Comparison NumberExpr ComparisonOperator NumberExpr


type BooleanExpr
    = ComparisonExpr ComparisonExpr
    | BooleanOperator ComparisonExpr BooleanOperator BooleanExpr


type BooleanOperator
    = And
    | Or


type Number
    = StaticNumber Int
    | PhaseNumber


type TimeUnit
    = Seconds
    | Minutes
