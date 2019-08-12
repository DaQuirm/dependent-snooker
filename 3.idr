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

data State
    = Reds Nat Ball
    | Colors ColorBall
    | End

data Shot : Type -> State -> State -> Type where
    Pure  : a -> Shot a i o
    (>>=) : Shot a s1 s2 -> (a -> Shot a s2 s3) -> Shot a s1 s3

red : Shot Nat (Reds (S reds) Red) (Reds reds Color)
red = Pure 1

color : ColorBall -> Shot Nat (Reds (S reds) Color) (Reds (S reds) Red)
color = Pure . colorScore

last_red_color : ColorBall -> Shot Nat (Reds Z Color) (Colors Yellow)
last_red_color = Pure . colorScore

yellow : Shot Nat (Colors Yellow) (Colors Green)
yellow = Pure 2

green : Shot Nat (Colors Green) (Colors Brown)
green = Pure 3

brown : Shot Nat (Colors Brown) (Colors Blue)
brown = Pure 4

blue : Shot Nat (Colors Blue) (Colors Pink)
blue = Pure 5

pink : Shot Nat (Colors Pink) (Colors Black)
pink = Pure 6

black : Shot Nat (Colors Black) End
black = Pure 7

break : Shot Nat (Reds 3 Red) End
break = do
    red
    color Black
    red
    color Black
    red
    last_red_color Black

    yellow
    green
    brown
    -- pink
    blue
    pink
    black

runBreak : Shot Nat i o -> Nat
runBreak (Pure a) = a
runBreak (shot >>= f) =
    let x = runBreak shot in x + (runBreak (f x))

main : IO ()
main = putStrLn "Hello world"
