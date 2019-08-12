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

data GameState = State Nat Ball

data Shot : Type -> GameState -> GameState -> Type where
    Pure  : a -> Shot a i o
    (>>=) : Shot a s1 s2 -> (a -> Shot b s2 s3) -> Shot b s1 s3

red : Shot Nat (State (S reds) Red) (State reds Color)
red = Pure 1

color : ColorBall -> Shot Nat (State reds Color) (State reds Red)
color = Pure . colorScore

break : Shot Nat (State 15 Red) (State 12 Red)
break = do
    red
    color Blue
    red
    color Pink
    red
    color Black
    -- red
    -- color Black

main : IO ()
main = putStrLn "Hello world"
