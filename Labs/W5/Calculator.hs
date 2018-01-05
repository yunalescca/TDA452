
import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe(fromJust)

import Pages
import Expr


canWidth, canHeight :: Num a => a
canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw input can =
    do v <- getProp input "value" 
       let expr = readExpr v
       case expr of
           Nothing -> render can (stroke (line (0,300) (0,300)))
           _       -> drawCanvas (fromJust expr) can 0.04


drawCanvas :: Expr -> Canvas -> Double -> IO ()
drawCanvas expr can scale = render can (stroke (path (points expr scale (canWidth, canHeight))))



readAndDrawDiff :: Elem -> Canvas -> IO ()
readAndDrawDiff input can = 
    do v <- getProp input "value"
       let expr = differentiate $ fromJust $ readExpr v
       set input [prop "value" =: showExpr expr]
       readAndDraw input can 



-- expr scale (width, height)
points :: Expr -> Double -> (Int, Int) -> [Point] -- Point = (Double, Double)
points expr scale (width, height) = 
    [(d, realToPix (eval expr (pixToReal d))) | d <- [0 .. fromIntegral width]]

    where
        -- converts a pixel x-coordinate to a real x-coordinate
        pixToReal :: Double -> Double
        pixToReal x = (x - (canWidth / 2)) * scale

        -- converts a real y-coordinate to a pixdel y-coordinate
        realToPix :: Double -> Double
        realToPix y = (canWidth / 2) - (y / scale)
    

zoom input can scale =
    do v <- getProp input "value" 
       let expr = readExpr v
       case expr of
           Nothing -> render can (stroke (line (0,300) (0,300)))
           _       -> drawCanvas (fromJust expr) can scale


main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight        -- The drawing area
    fx      <- mkHTML   "<i>f</i>(<i>x</i>)="     -- The text "f(x)="
    input   <- mkInput  20 ""                     -- The formula input
    draw    <- mkButton "Draw graph"              -- The draw button
    diff    <- mkButton "Differentiate"
    zoomO   <- mkButton "Zoom out"
    zoomI   <- mkButton "Zoom in"
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw, diff, zoomI, zoomO]

    -- Styling
    setStyle documentBody "backgroundColor" "pink"
    setStyle documentBody "textAlign" "center"

    setStyle input  "fontSize" "14pt"
    setStyle input  "font-family" "Courier New"

    setStyle draw   "margin" "10px"
    setStyle draw   "fontSize" "14pt"
    setStyle draw   "font-family" "Courier New"
    setStyle draw   "width" "240px"

    setStyle diff   "margin" "10px"
    setStyle diff   "fontSize" "14pt"
    setStyle diff   "font-family" "Courier New"
    setStyle diff   "width" "240px"

    setStyle zoomO  "margin" "10px"
    setStyle zoomO  "fontSize" "14pt"
    setStyle zoomO  "font-family" "Courier New"
    setStyle zoomO  "width" "240px"

    setStyle zoomI  "margin" "10px"
    setStyle zoomI  "fontSize" "14pt"
    setStyle zoomI  "font-family" "Courier New"
    setStyle zoomI  "width" "240px"

    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click  $ \ _ -> readAndDraw input can
    onEvent input Change $ \ _ -> readAndDraw input can
    onEvent diff  Click  $ \ _ -> readAndDrawDiff input can
    onEvent zoomO Click  $ \ _ -> zoom input can 0.05 
    onEvent zoomI Click  $ \ _ -> zoom input can 0.03



























