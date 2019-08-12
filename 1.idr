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

data Shot : Type -> Type where
    Pure  : a -> Shot a
    (>>=) : Shot a -> (a -> Shot b) -> Shot b

red : Shot Nat
red = Pure 1

color : ColorBall -> Shot Nat
color = Pure . colorScore

yellow : Shot Nat
yellow = color Yellow

green : Shot Nat
green = color Green

brown : Shot Nat
brown = color Brown

blue : Shot Nat
blue = color Blue

pink : Shot Nat
pink = color Pink

black : Shot Nat
black = color Black

break : Shot Nat
break = do
    red
    color Blue
    color Green
    red
    color Pink
    red
    color Black

    yellow
    green
    brown
    blue
    pink
    black

main : IO ()
main = putStrLn "Hello world"
