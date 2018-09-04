module Myth.Render (drawStatus, drawClock) where
import qualified Myth.Internal as M
import Graphics.Rendering.Cairo
import Control.Monad (zipWithM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

drawBigText win_w win_h s = do
    setSourceRGBA 1 1 1 0.5
    selectFontFace "monospace" FontSlantNormal FontWeightNormal
    setFontSize 150
    TextExtents xb yb w h _ _ <- textExtents s
    moveTo ((win_w - w) / 2 - xb) ((win_h - h) / 2 - yb)
    showText s

drawLittleText win_w win_h ss = do
    let lines = reverse . filter (not . null) $ ss
    selectFontFace "sans-serif" FontSlantItalic FontWeightBold
    setFontSize 30
    ys <- zipWith (\i (TextExtents _ yb _ h _ _) -> win_h -  i * (h - yb)) [1..] <$> mapM textExtents lines
    zipWithM_ (\y s -> setSourceRGBA 1 0.2 0.2 0.6
                    >> arc (win_w / 5 - 20) (y - 10) 10 0 (fromIntegral 2 * pi)
                    >> fill
                    >> setSourceRGBA 1 1 1 0.3
                    >> moveTo (win_w / 5) y
                    >> showText s
              ) ys lines

drawSquare w x y isGreen isPurple = do
    let h = w
        aspect = 1
        corner_radius = h / 10
        radius = corner_radius / aspect
        degrees = pi / 180
        red = (239, 41, 41)
        green = (148, 194, 105)
        purple = (152, 107, 194)
    let (fc1, fc2, fc3) = if isPurple then purple else if isGreen then green else red  -- Fill colour
        (bc1, bc2, bc3) = if isPurple then green else (fc1, fc2, fc3)                  -- Border colour
    newPath
    arc (x + w - radius) (y + radius) radius (-90 * degrees) (0 * degrees)
    arc (x + w - radius) (y + h - radius) radius (0 * degrees) (90 * degrees)
    arc (x + radius) (y + h - radius) radius (90 * degrees) (180 * degrees)
    arc (x + radius) (y + radius) radius (180 * degrees) (270 * degrees)
    closePath
    setSourceRGB (fc1 / 256) (fc2 / 256) (fc3 / 256)
    fillPreserve
    setSourceRGBA (bc1 / 256) (bc2 / 256) (bc3 / 256) 0.5
    setLineWidth 10
    stroke

drawStatus :: MonadIO m => Surface -> Double -> Double -> [M.Encoder] -> m ()
drawStatus xpsurface w h encoders = renderWith xpsurface $ do
    setOperator OperatorSource
    setSourceRGBA 0 0 0 0
    paint
    setOperator OperatorOver
    let sq_dim = 100
    let init_x = 160
    let y1 = (h - 2 * sq_dim) / 3
    let y2 = h - y1 - sq_dim
    let ((M.Encoder c11 a11 _):(M.Encoder c12 a12 _):
         (M.Encoder c21 a21 _):(M.Encoder c22 a22 _):
         (M.Encoder c31 a31 _):(M.Encoder c32 a32 _):_) = encoders
    drawSquare sq_dim init_x y1 c11 a11
    drawSquare sq_dim init_x y2 c12 a12
    drawSquare sq_dim ((w - sq_dim) / 2) y1 c21 a21
    drawSquare sq_dim ((w - sq_dim) / 2) y2 c22 a22
    drawSquare sq_dim (w - init_x - sq_dim) y1 c31 a31
    drawSquare sq_dim (w - init_x - sq_dim) y2 c32 a32

drawClock :: MonadIO m => Surface -> Double -> Double -> [M.Encoder] -> m ()
drawClock xpsurface w h encoders = renderWith xpsurface $ do
    setOperator OperatorSource
    setSourceRGBA 0 0 0 0
    paint
    setOperator OperatorOver
    t <- liftIO $ getCurrentTime
    tz <- liftIO $ getCurrentTimeZone
    let s = formatTime defaultTimeLocale "%l %M" <$> localTimeOfDay $ utcToLocalTime tz t
    drawBigText w h s
    drawLittleText w h (M.encoderRecordingTitle <$> encoders)

