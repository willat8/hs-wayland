module Myth.Render (drawStatus, drawClock) where
import qualified Myth.Internal as M
import Control.Monad (zipWithM_)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as B
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Types as XP

-- | Draws a grid of coloured squares to a Cairo surface.
-- Requires the screen dimensions to position the squares correctly.
-- A green square represents a connected encoder and a purple square an encoder which is actively recording.
drawStatus surface w h encoders = renderWith surface $ do
    setOperator OperatorSource
    setSourceRGBA 0 0 0 0
    paint
    setOperator OperatorOver
    let sq_dim = 100
        init_x = 160
    let y1 = (h - 2 * sq_dim) / 3
        y2 = h - y1 - sq_dim
    let x1 = init_x
        x2 = ((w - sq_dim) / 2)
        x3 = (w - init_x - sq_dim)
    let coords = [(x, y) | x <- [x1, x2, x3], y <- [y1, y2]]
    zipWithM_ (drawEncoder sq_dim) coords encoders

-- | Draws a vertically and horizontally-centered 12h time to a Cairo surface.
-- Requires the screen dimensions to position the text correctly.
-- Beneath is drawn a list of titles currently being recorded.
drawClock surface w h encoders = renderWith surface $ do
    setOperator OperatorSource
    setSourceRGBA 0 0 0 0
    paint
    setOperator OperatorOver
    t <- liftIO $ getCurrentTime
    tz <- liftIO $ getCurrentTimeZone
    let s = formatTime defaultTimeLocale "%l %M" <$> localTimeOfDay $ utcToLocalTime tz t
    drawTime w h s
    drawRecordingTitles w h (M.encoderRecordingTitle <$> encoders)

drawEncoder w (x, y) (M.Encoder isConnected isActive _ channelIcon)
    | isActive && channelIcon /= S.empty = drawChannelIcon (x - 16) y channelIcon
    | isConnected = drawSquare w x y (148, 194, 105) -- Green
    | otherwise = drawSquare w x y (239, 41, 41) -- Red

drawSquare w x y colour = do
    let h = w
        aspect = 1
        cornerRadius = h / 10
        radius = cornerRadius / aspect
        degrees = pi / 180
    let (fc1, fc2, fc3) = colour -- Fill colour
        (bc1, bc2, bc3) = colour -- Border colour
    newPath
    arc (x + w - radius) (y + radius) radius (-90 * degrees) (0 * degrees)
    arc (x + w - radius) (y + h - radius) radius (0 * degrees) (90 * degrees)
    arc (x + radius) (y + h - radius) radius (90 * degrees) (180 * degrees)
    arc (x + radius) (y + radius) radius (180 * degrees) (270 * degrees)
    closePath
    setSourceRGBA (fc1 / 256) (fc2 / 256) (fc3 / 256) 0
    fillPreserve
    setSourceRGBA (bc1 / 256) (bc2 / 256) (bc3 / 256) 0.5
    setLineWidth 10
    stroke

drawTime win_w win_h t = do
    setSourceRGBA 1 1 1 0.5
    selectFontFace "monospace" FontSlantNormal FontWeightNormal
    setFontSize 150
    TextExtents xb yb w h _ _ <- textExtents t
    moveTo ((win_w - w) / 2 - xb) ((win_h - h) / 2 - yb)
    showText t

drawRecordingTitles win_w win_h rts = do
    let rts' = reverse . filter (not . null) $ rts
    selectFontFace "sans-serif" FontSlantItalic FontWeightBold
    setFontSize 30
    ys <- zipWith (\i (TextExtents _ yb _ h _ _) -> win_h -  i * (h - yb)) [1..] <$> mapM textExtents rts'
    zipWithM_ (\y rt -> setSourceRGBA 1 0.2 0.2 0.6
                     >> arc (win_w / 5 - 20) (y - 10) 10 0 (fromIntegral 2 * pi)
                     >> fill
                     >> setSourceRGBA 1 1 1 0.3
                     >> moveTo (win_w / 5) y
                     >> showText rt
              ) ys rts'

drawChannelIcon x y ci = do
    channelIconSurface <- pngSurfaceFromByteString ci
    setSourceSurface channelIconSurface x y
    paint

pngSurfaceFromByteString bs = do
    let (bs_fp, _, bs_size) = B.toForeignPtr bs
    liftIO $ withForeignPtr bs_fp $ \bs_ptr -> withCString "rb" $ \mode_cs -> do
        file_ptr <- M.c_fmemopen bs_ptr (fromIntegral bs_size) mode_cs
        read_funp <- M.mkReadFromPngStreamForeign readFromPngStream
        surface <- XP.mkSurface =<< M.c_cairo_image_surface_create_from_png_stream read_funp file_ptr
        XP.manageSurface surface
        freeHaskellFunPtr read_funp
        M.c_fclose file_ptr
        return surface

readFromPngStream file_ptr buffer_ptr count = do
  bytes_read <- M.c_fread buffer_ptr 1 count file_ptr
  return $ if (bytes_read /= count) then M.cairoStatusReadError else M.cairoStatusSuccess

