module Main where

import Prelude
import Math (pi, cos, sin, abs)
import Math as Math
import Data.String (length, charCodeAt)
import Data.Array ((..), filter)
import Data.Int (floor, toNumber, even)
import Signal.Time (every)
import Signal.DOM (mousePos, keyPressed, windowDimensions, animationFrame)
import Flare (UI, number, radioGroup, runFlare, runFlareShow, lift, wrap, string, int, button, numberSlider, color, boolean)
import Flare.Drawing
import Color.Scale (sample)
import Color.Scale.Perceptual (inferno)
import Color.Scheme.MaterialDesign (orange, indigo)
import Test.FlareCheck (flareCheck')

demo fn suffix flare = fn div_inp div_out flare
  where
    div_inp = "inp_" <> suffix
    div_out = "out_" <> suffix

flareCheck div fn = flareCheck' ("out_" <> div) "" fn

-------------------------------------------------------------

pow :: Int -> Int -> Int
pow x y = floor (Math.pow (toNumber x) (toNumber y))

motivation =
  pow <$> int "Base"     2
      <*> int "Exponent" 8

-------------------------------------------------------------

canvasSize = 800.0

drawClock :: Int -> Drawing
drawClock n =
  translate (canvasSize / 2.0) (canvasSize / 2.0) $
    filled (fillColor orange) (circle 0.0 0.0 rad)
    <>
    rotate angle (filled (fillColor black) (rectangle (-8.0) 0.0 16.0 (-rad)))
  where rad = 0.9 * canvasSize / 2.0
        angle = toNumber n / 60.0 * 2.0 * pi

-------------------------------------------------------------

progressBar :: Number -> Drawing
progressBar ratio =
  filled (fillColor black)
         (rectangle 0.0 0.0 canvasSize height)
  <>
  filled (fillColor (sample inferno ratio))
         (rectangle 0.0 0.0 (ratio * canvasSize) height)
  where height = 100.0

-------------------------------------------------------------

components =
  sayHello <$> string   "Name"    "LambdaConf"
           <*> int      "Year"    2016
           <*> boolean  "Shout"   false
  where
    suffix true = "!!!"
    suffix false = ""
    sayHello str num shout = "<br>Hello " <> str <> " " <> show num <> (suffix shout)

-------------------------------------------------------------

plot m n1 s col animated time =
      filled (fillColor col) $
        path (map point angles)

      where point phi = { x: 400.0 + radius phi * cos phi
                        , y: 400.0 + radius phi * sin phi }
            angles = map (\i -> 2.0 * pi / toNumber points * toNumber i)
                         (0 .. points)
            points = 400
            time' = if animated then time else 0.0
            n2 = s + 3.0 * sin (0.005 * time')
            n3 = s + 3.0 * cos (0.005 * time')
            radius phi = 60.0 * Math.pow expr (- 1.0 / n1)
              where expr = first + second
                    first = Math.pow (abs (cos (m * phi / 4.0))) n2
                    second = Math.pow (abs (sin (m * phi / 4.0))) n3

superformula =
  plot <$> (numberSlider "m" 2.0 10.0 1.0  5.0)
       <*> (numberSlider "n" 3.0 10.0 0.1  4.0)
       <*> (numberSlider "s" 4.0 16.0 0.1 14.0)
       <*> (color "Fill color" indigo)
       <*> (boolean "Animated" false)
       <*> lift animationFrame

-------------------------------------------------------------

main = do
  demo runFlareShow "motivation" motivation

  mousePosSig <- mousePos
  aPressed <- keyPressed 65
  let
    mouseToStr { x, y } = "(" <> show x <> ", " <> show y <> ")"
    mousePos' = mouseToStr <$> mousePosSig

    seconds = (floor <<< (_ / 1000.0)) <$> every 1000.0

    combine m s k = m <> "<br>" <> s <> "<br>" <> k

    combined = combine <$> mousePos' <*> (show <$> seconds) <*> (show <$> aPressed)

  demo runFlare "signals" (wrap combined)


  let clock = map drawClock seconds

  demo runFlareDrawing "clock" (wrap clock)

  windowDim <- windowDimensions

  let mousePosX = (toNumber <<< _.x) <$> mousePosSig
      windowWidth = (toNumber <<< _.w) <$> windowDim

      ratio = (/) <$> mousePosX <*> windowWidth
      progress = progressBar <$> ratio

  demo runFlareDrawing "progress" (wrap progress)

  demo runFlare "components" components

  demo runFlareDrawing "superformula" superformula

  flareCheck "flarecheck_length" length

  flareCheck "flarecheck_charCodeAt" charCodeAt

  let
    xor :: Boolean -> Boolean -> Boolean
    xor a b = a && not b || not a && b

  flareCheck "flarecheck_xor" xor

  flareCheck "flarecheck_filterEven" (filter even)
