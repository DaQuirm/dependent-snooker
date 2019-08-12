module Main

data ColorBall
    = Yellow
    | Green
    | Brown
    | Blue
    | Pink
    | Black

colorScore : ColorBall -> Nat
colorScore Yellow = 2
colorScore Green  = 3
colorScore Brown  = 4
colorScore Blue   = 5
colorScore Pink   = 6
colorScore Black  = 7

data Ball = Red | Color

data BallState
    = Reds Nat Ball
    | Colors ColorBall
    | End

data ShotResult
    = Pot Nat
    | Foul Bool

data Shot : (a : Type) -> BallState -> (a -> BallState) -> Type where
    red : Shot ShotResult (Reds (S reds) Red) (\res => case res of
                                                            Pot _ => Reds reds Color
                                                            Foul True => case reds of
                                                                            Z => Colors Yellow
                                                                            n => Reds n Red
                                                            Foul False => Reds (S reds) Red)

    color : ColorBall -> Shot ShotResult (Reds (S reds) Color) (const (Reds (S reds) Red))
    last_red_color : ColorBall -> Shot ShotResult (Reds Z Color) (const (Colors Yellow))

    yellow : Shot ShotResult (Colors Yellow) (const (Colors Green))
    green : Shot ShotResult (Colors Green) (const (Colors Brown))
    brown : Shot ShotResult (Colors Brown) (const (Colors Blue))
    blue : Shot ShotResult (Colors Blue) (const (Colors Pink))
    pink : Shot ShotResult (Colors Pink) (const (Colors Black))
    black : Shot ShotResult (Colors Black) (const End)

    Pure  : a -> Shot a i o
    (>>=) : Shot a s1 f1 -> ((res : a) -> Shot a (f1 res) f2) -> Shot a s1 f2

break_end : Shot ShotResult (Colors Yellow) (const End)
break_end = do
    yellow
    green
    brown
    blue
    pink
    black

break : Shot ShotResult (Reds 3 Red) (const End)
break = do
    Pot _ <- red
    color Black
    Pot _ <- red
    color Black

    res <- red
    case res of
        Pot _ => do
            last_red_color Black
            break_end

        Foul False => do
            Pot _ <- red
            last_red_color Black
            break_end

        Foul True => do
            break_end

main : IO ()
main = putStrLn "Hello world"
