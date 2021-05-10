module Myth.Render (drawStatus, drawClock, drawAlert) where
import qualified Myth.Internal as M
import Control.Monad (zipWithM_)
import Data.Bits.Bitwise
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as B
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG
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
        init_x = 80
    let y = (h - sq_dim) / 2
    let x1 = init_x
        x2 = (w + x1 - sq_dim) / 3
        x3 = w - x2 - sq_dim
        x4 = w - x1 - sq_dim
    let coords = [(x, y) | x <- [x1, x2, x3, x4]]
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
    setFontSize 36
    let ys = (-) win_h <$> [20,55..]
    zipWithM_ (\y rt -> setSourceRGBA 1 0.2 0.2 0.6
                     >> arc (win_w / 5 - 24) (y - 11) 11 0 (fromIntegral 2 * pi)
                     >> fill
                     >> setSourceRGBA 1 1 1 0.8
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

drawAlert surface True babyMonitorHealth _ = renderWith surface $ do
    let microphoneUnhealthy:speakerUnhealthy:_ = toListLE babyMonitorHealth
    drawBlank
    setOperator OperatorOver
    scale 0.1 0.1
    drawBabyIcon $ if (microphoneUnhealthy || speakerUnhealthy) then "red" else "green"
    translate 400 75
    scale 0.4 0.4
    drawMicrophoneIcon $ if (microphoneUnhealthy) then "red" else "green"
    translate 0 500
    drawSpeakerIcon $ if (speakerUnhealthy) then "red" else "green"
    return ()
drawAlert surface _ 0 _ = renderWith surface $ do
    drawBlank
drawAlert surface False _ time = renderWith surface $ do
    drawBlank
    setOperator OperatorOver
    setSourceRGBA 1 0.2 0.2 0.6
    selectFontFace "sans-serif" FontSlantItalic FontWeightBold
    setFontSize 36
    let x = 900 - fromIntegral (time `mod` 6000) / 5
    let y = 36
    moveTo x y
    lineTo (x + 20) (y - 20)
    moveTo (x + 20) y
    lineTo x (y - 20)
    stroke
    setSourceRGBA 1 1 1 0.6
    moveTo (x + 30) y
    showText "Alert!"

drawBlank = do
    setOperator OperatorSource
    setSourceRGBA 0 0 0 0
    paint

drawBabyIcon colour = do svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"baby\" class=\"svg-inline--fa fa-baby fa-w-12\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 384 512\"><path fill=\"" ++ colour ++ "\" d=\"M192 160c44.2 0 80-35.8 80-80S236.2 0 192 0s-80 35.8-80 80 35.8 80 80 80zm-53.4 248.8l25.6-32-61.5-51.2L56.8 383c-11.4 14.2-11.7 34.4-.8 49l48 64c7.9 10.5 19.9 16 32 16 8.3 0 16.8-2.6 24-8 17.7-13.2 21.2-38.3 8-56l-29.4-39.2zm142.7-83.2l-61.5 51.2 25.6 32L216 448c-13.2 17.7-9.7 42.8 8 56 7.2 5.4 15.6 8 24 8 12.2 0 24.2-5.5 32-16l48-64c10.9-14.6 10.6-34.8-.8-49l-45.9-57.4zM376.7 145c-12.7-18.1-37.6-22.4-55.7-9.8l-40.6 28.5c-52.7 37-124.2 37-176.8 0L63 135.3C44.9 122.6 20 127 7.3 145-5.4 163.1-1 188 17 200.7l40.6 28.5c17 11.9 35.4 20.9 54.4 27.9V288h160v-30.8c19-7 37.4-16 54.4-27.9l40.6-28.5c18.1-12.8 22.4-37.7 9.7-55.8z\"></path></svg>")

drawMicrophoneIcon colour = do svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"microphone\" class=\"svg-inline--fa fa-microphone fa-w-11\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 352 512\"><path fill=\"" ++ colour ++ "\" d=\"M176 352c53.02 0 96-42.98 96-96V96c0-53.02-42.98-96-96-96S80 42.98 80 96v160c0 53.02 42.98 96 96 96zm160-160h-16c-8.84 0-16 7.16-16 16v48c0 74.8-64.49 134.82-140.79 127.38C96.71 376.89 48 317.11 48 250.3V208c0-8.84-7.16-16-16-16H16c-8.84 0-16 7.16-16 16v40.16c0 89.64 63.97 169.55 152 181.69V464H96c-8.84 0-16 7.16-16 16v16c0 8.84 7.16 16 16 16h160c8.84 0 16-7.16 16-16v-16c0-8.84-7.16-16-16-16h-56v-33.77C285.71 418.47 352 344.9 352 256v-48c0-8.84-7.16-16-16-16z\"></path></svg>")

drawSpeakerIcon colour = do svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"volume-down\" class=\"svg-inline--fa fa-volume-down fa-w-12\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 384 512\"><path fill=\"" ++ colour ++ "\" d=\"M215.03 72.04L126.06 161H24c-13.26 0-24 10.74-24 24v144c0 13.25 10.74 24 24 24h102.06l88.97 88.95c15.03 15.03 40.97 4.47 40.97-16.97V89.02c0-21.47-25.96-31.98-40.97-16.98zm123.2 108.08c-11.58-6.33-26.19-2.16-32.61 9.45-6.39 11.61-2.16 26.2 9.45 32.61C327.98 229.28 336 242.62 336 257c0 14.38-8.02 27.72-20.92 34.81-11.61 6.41-15.84 21-9.45 32.61 6.43 11.66 21.05 15.8 32.61 9.45 28.23-15.55 45.77-45 45.77-76.88s-17.54-61.32-45.78-76.87z\"></path></svg>")

