module Myth.Render (drawStatus, drawClock, drawAlert) where
import qualified Myth.Internal as M
import Myth.Alert
import Control.Monad (zipWithM_, when)
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

drawAlert surface showDashboard@True babyMonitorHealth isHDHomeRunHealthy isMythTVHealthy isPiholeHealthy hueHealth _ = renderWith surface $ do
    drawDashboard babyMonitorHealth isHDHomeRunHealthy isMythTVHealthy isPiholeHealthy hueHealth
drawAlert surface showDashboard@False babyMonitorHealth isHDHomeRunHealthy isMythTVHealthy isPiholeHealthy hueHealth time
    | (babyMonitorHealth == healthy &&
       isHDHomeRunHealthy &&
       isMythTVHealthy &&
       isPiholeHealthy &&
       hueHealth == healthy) = renderWith surface $ do drawBlank
    | otherwise              = renderWith surface $ do drawMarquee time

drawBlank = do
    setOperator OperatorSource
    setSourceRGBA 0 0 0 0
    paint

drawMarquee time = do
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

drawDashboard babyMonitorHealth isHDHomeRunHealthy isMythTVHealthy isPiholeHealthy hueHealth = do
    let isMicrophoneUnhealthy:isSpeakerUnhealthy:_ = toListLE babyMonitorHealth
    drawBlank
    setOperator OperatorOver
    translate 95 0
    drawBabyIcon $ if (isMicrophoneUnhealthy || isSpeakerUnhealthy) then "red" else "green"
    save
    translate 40 7.5
    drawMicrophoneIcon $ if (isMicrophoneUnhealthy) then "red" else "green"
    translate 0 20
    drawSpeakerIcon $ if (isSpeakerUnhealthy) then "red" else "green"
    restore
    translate 140 0
    drawDockerIcon $ if (isMythTVHealthy) then "green" else "red"
    translate 140 0
    drawBroadcastTowerIcon $ if (isHDHomeRunHealthy) then "green" else "red"
    translate 140 0
    drawRaspberryPiIcon $ if (isPiholeHealthy) then "green" else "red"
    translate 140 0
    drawLightbulbIcon $ if (hueHealth == healthy) then "green" else "red"
    identityMatrix
    moveTo 700 25
    setSourceRGBA 1 1 1 0.8
    selectFontFace "sans-serif" FontSlantItalic FontWeightBold
    setFontSize 24
    when (hueHealth > 0) $ showText (show hueHealth) -- One of more lights are unreachable
    return ()

drawBabyIcon colour = do
  save
  scale 0.1 0.1
  svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"baby\" class=\"svg-inline--fa fa-baby fa-w-12\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 384 512\"><path fill=\"" ++ colour ++ "\" d=\"M192 160c44.2 0 80-35.8 80-80S236.2 0 192 0s-80 35.8-80 80 35.8 80 80 80zm-53.4 248.8l25.6-32-61.5-51.2L56.8 383c-11.4 14.2-11.7 34.4-.8 49l48 64c7.9 10.5 19.9 16 32 16 8.3 0 16.8-2.6 24-8 17.7-13.2 21.2-38.3 8-56l-29.4-39.2zm142.7-83.2l-61.5 51.2 25.6 32L216 448c-13.2 17.7-9.7 42.8 8 56 7.2 5.4 15.6 8 24 8 12.2 0 24.2-5.5 32-16l48-64c10.9-14.6 10.6-34.8-.8-49l-45.9-57.4zM376.7 145c-12.7-18.1-37.6-22.4-55.7-9.8l-40.6 28.5c-52.7 37-124.2 37-176.8 0L63 135.3C44.9 122.6 20 127 7.3 145-5.4 163.1-1 188 17 200.7l40.6 28.5c17 11.9 35.4 20.9 54.4 27.9V288h160v-30.8c19-7 37.4-16 54.4-27.9l40.6-28.5c18.1-12.8 22.4-37.7 9.7-55.8z\"></path></svg>")
  restore

drawMicrophoneIcon colour = do
  save
  scale 0.04 0.04
  svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"microphone\" class=\"svg-inline--fa fa-microphone fa-w-11\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 352 512\"><path fill=\"" ++ colour ++ "\" d=\"M176 352c53.02 0 96-42.98 96-96V96c0-53.02-42.98-96-96-96S80 42.98 80 96v160c0 53.02 42.98 96 96 96zm160-160h-16c-8.84 0-16 7.16-16 16v48c0 74.8-64.49 134.82-140.79 127.38C96.71 376.89 48 317.11 48 250.3V208c0-8.84-7.16-16-16-16H16c-8.84 0-16 7.16-16 16v40.16c0 89.64 63.97 169.55 152 181.69V464H96c-8.84 0-16 7.16-16 16v16c0 8.84 7.16 16 16 16h160c8.84 0 16-7.16 16-16v-16c0-8.84-7.16-16-16-16h-56v-33.77C285.71 418.47 352 344.9 352 256v-48c0-8.84-7.16-16-16-16z\"></path></svg>")
  restore

drawSpeakerIcon colour = do
  save
  scale 0.04 0.04
  svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"volume-down\" class=\"svg-inline--fa fa-volume-down fa-w-12\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 384 512\"><path fill=\"" ++ colour ++ "\" d=\"M215.03 72.04L126.06 161H24c-13.26 0-24 10.74-24 24v144c0 13.25 10.74 24 24 24h102.06l88.97 88.95c15.03 15.03 40.97 4.47 40.97-16.97V89.02c0-21.47-25.96-31.98-40.97-16.98zm123.2 108.08c-11.58-6.33-26.19-2.16-32.61 9.45-6.39 11.61-2.16 26.2 9.45 32.61C327.98 229.28 336 242.62 336 257c0 14.38-8.02 27.72-20.92 34.81-11.61 6.41-15.84 21-9.45 32.61 6.43 11.66 21.05 15.8 32.61 9.45 28.23-15.55 45.77-45 45.77-76.88s-17.54-61.32-45.78-76.87z\"></path></svg>")
  restore

drawDockerIcon colour = do
  save
  scale 1.5 1.5
  translate 0 (-6)
  svgRenderFromString ("<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"48\" width=\"48\"><path fill=\"" ++ colour ++ "\" d=\"M7.95 30.15h7.35q1 0 1.75-.75t.75-1.75v-7.3q0-1-.75-1.75t-1.75-.75H7.95Zm2.5-2.5v-7.3h4.85v7.3Zm12.1 2.5h2.5l3.85-12.3h-2.5l-2.45 8.75-2.7-8.75h-2.2Zm7.7 0h2.5V25.7h2.9l1.65 4.45h2.5l-1.75-4.7q.85-.2 1.45-.825t.6-1.425v-2.85q0-1-.75-1.75t-1.75-.75h-7.35Zm2.5-6.95v-2.85h4.85v2.85ZM5 40q-1.2 0-2.1-.9Q2 38.2 2 37V11q0-1.2.9-2.1Q3.8 8 5 8h38q1.25 0 2.125.9T46 11v26q0 1.2-.875 2.1-.875.9-2.125.9Zm0-3h38V11H5v26Zm0 0V11v26Z\"/></svg>")
  restore

drawBroadcastTowerIcon colour = do
  save
  scale 1.5 1.5
  translate 0 (-6)
  svgRenderFromString ("<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"48\" width=\"48\"><path fill=\"" ++ colour ++ "\" d=\"M12 30.15h2.5V25.7h5v4.45H22v-12.3h-2.5v5.35h-5v-5.35H12Zm14 0h7.5q1 0 1.75-.75t.75-1.75v-7.3q0-1-.75-1.75t-1.75-.75H26Zm2.5-2.5v-7.3h5v7.3ZM7 40q-1.2 0-2.1-.9Q4 38.2 4 37V11q0-1.2.9-2.1Q5.8 8 7 8h34q1.2 0 2.1.9.9.9.9 2.1v26q0 1.2-.9 2.1-.9.9-2.1.9Zm0-3h34V11H7v26Zm0 0V11v26Z\"/></svg>")
  restore

drawRaspberryPiIcon colour = do
  save
  scale 0.1 0.1
  svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fab\" data-icon=\"raspberry-pi\" class=\"svg-inline--fa fa-raspberry-pi fa-w-13\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 407 512\"><path fill=\"" ++ colour ++ "\" d=\"M372 232.5l-3.7-6.5c.1-46.4-21.4-65.3-46.5-79.7 7.6-2 15.4-3.6 17.6-13.2 13.1-3.3 15.8-9.4 17.1-15.8 3.4-2.3 14.8-8.7 13.6-19.7 6.4-4.4 10-10.1 8.1-18.1 6.9-7.5 8.7-13.7 5.8-19.4 8.3-10.3 4.6-15.6 1.1-20.9 6.2-11.2.7-23.2-16.6-21.2-6.9-10.1-21.9-7.8-24.2-7.8-2.6-3.2-6-6-16.5-4.7-6.8-6.1-14.4-5-22.3-2.1-9.3-7.3-15.5-1.4-22.6.8C271.6.6 269 5.5 263.5 7.6c-12.3-2.6-16.1 3-22 8.9l-6.9-.1c-18.6 10.8-27.8 32.8-31.1 44.1-3.3-11.3-12.5-33.3-31.1-44.1l-6.9.1c-5.9-5.9-9.7-11.5-22-8.9-5.6-2-8.1-7-19.4-3.4-4.6-1.4-8.9-4.4-13.9-4.3-2.6.1-5.5 1-8.7 3.5-7.9-3-15.5-4-22.3 2.1-10.5-1.3-14 1.4-16.5 4.7-2.3 0-17.3-2.3-24.2 7.8C21.2 16 15.8 28 22 39.2c-3.5 5.4-7.2 10.7 1.1 20.9-2.9 5.7-1.1 11.9 5.8 19.4-1.8 8 1.8 13.7 8.1 18.1-1.2 11 10.2 17.4 13.6 19.7 1.3 6.4 4 12.4 17.1 15.8 2.2 9.5 10 11.2 17.6 13.2-25.1 14.4-46.6 33.3-46.5 79.7l-3.7 6.5c-28.8 17.2-54.7 72.7-14.2 117.7 2.6 14.1 7.1 24.2 11 35.4 5.9 45.2 44.5 66.3 54.6 68.8 14.9 11.2 30.8 21.8 52.2 29.2C159 504.2 181 512 203 512h1c22.1 0 44-7.8 64.2-28.4 21.5-7.4 37.3-18 52.2-29.2 10.2-2.5 48.7-23.6 54.6-68.8 3.9-11.2 8.4-21.3 11-35.4 40.6-45.1 14.7-100.5-14-117.7zm-22.2-8c-1.5 18.7-98.9-65.1-82.1-67.9 45.7-7.5 83.6 19.2 82.1 67.9zm-43 93.1c-24.5 15.8-59.8 5.6-78.8-22.8s-14.6-64.2 9.9-80c24.5-15.8 59.8-5.6 78.8 22.8s14.6 64.2-9.9 80zM238.9 29.3c.8 4.2 1.8 6.8 2.9 7.6 5.4-5.8 9.8-11.7 16.8-17.3 0 3.3-1.7 6.8 2.5 9.4 3.7-5 8.8-9.5 15.5-13.3-3.2 5.6-.6 7.3 1.2 9.6 5.1-4.4 10-8.8 19.4-12.3-2.6 3.1-6.2 6.2-2.4 9.8 5.3-3.3 10.6-6.6 23.1-8.9-2.8 3.1-8.7 6.3-5.1 9.4 6.6-2.5 14-4.4 22.1-5.4-3.9 3.2-7.1 6.3-3.9 8.8 7.1-2.2 16.9-5.1 26.4-2.6l-6 6.1c-.7.8 14.1.6 23.9.8-3.6 5-7.2 9.7-9.3 18.2 1 1 5.8.4 10.4 0-4.7 9.9-12.8 12.3-14.7 16.6 2.9 2.2 6.8 1.6 11.2.1-3.4 6.9-10.4 11.7-16 17.3 1.4 1 3.9 1.6 9.7.9-5.2 5.5-11.4 10.5-18.8 15 1.3 1.5 5.8 1.5 10 1.6-6.7 6.5-15.3 9.9-23.4 14.2 4 2.7 6.9 2.1 10 2.1-5.7 4.7-15.4 7.1-24.4 10 1.7 2.7 3.4 3.4 7.1 4.1-9.5 5.3-23.2 2.9-27 5.6.9 2.7 3.6 4.4 6.7 5.8-15.4.9-57.3-.6-65.4-32.3 15.7-17.3 44.4-37.5 93.7-62.6-38.4 12.8-73 30-102 53.5-34.3-15.9-10.8-55.9 5.8-71.8zm-34.4 114.6c24.2-.3 54.1 17.8 54 34.7-.1 15-21 27.1-53.8 26.9-32.1-.4-53.7-15.2-53.6-29.8 0-11.9 26.2-32.5 53.4-31.8zm-123-12.8c3.7-.7 5.4-1.5 7.1-4.1-9-2.8-18.7-5.3-24.4-10 3.1 0 6 .7 10-2.1-8.1-4.3-16.7-7.7-23.4-14.2 4.2-.1 8.7 0 10-1.6-7.4-4.5-13.6-9.5-18.8-15 5.8.7 8.3.1 9.7-.9-5.6-5.6-12.7-10.4-16-17.3 4.3 1.5 8.3 2 11.2-.1-1.9-4.2-10-6.7-14.7-16.6 4.6.4 9.4 1 10.4 0-2.1-8.5-5.8-13.3-9.3-18.2 9.8-.1 24.6 0 23.9-.8l-6-6.1c9.5-2.5 19.3.4 26.4 2.6 3.2-2.5-.1-5.6-3.9-8.8 8.1 1.1 15.4 2.9 22.1 5.4 3.5-3.1-2.3-6.3-5.1-9.4 12.5 2.3 17.8 5.6 23.1 8.9 3.8-3.6.2-6.7-2.4-9.8 9.4 3.4 14.3 7.9 19.4 12.3 1.7-2.3 4.4-4 1.2-9.6 6.7 3.8 11.8 8.3 15.5 13.3 4.1-2.6 2.5-6.2 2.5-9.4 7 5.6 11.4 11.5 16.8 17.3 1.1-.8 2-3.4 2.9-7.6 16.6 15.9 40.1 55.9 6 71.8-29-23.5-63.6-40.7-102-53.5 49.3 25 78 45.3 93.7 62.6-8 31.8-50 33.2-65.4 32.3 3.1-1.4 5.8-3.2 6.7-5.8-4-2.8-17.6-.4-27.2-5.6zm60.1 24.1c16.8 2.8-80.6 86.5-82.1 67.9-1.5-48.7 36.5-75.5 82.1-67.9zM38.2 342c-23.7-18.8-31.3-73.7 12.6-98.3 26.5-7 9 107.8-12.6 98.3zm91 98.2c-13.3 7.9-45.8 4.7-68.8-27.9-15.5-27.4-13.5-55.2-2.6-63.4 16.3-9.8 41.5 3.4 60.9 25.6 16.9 20 24.6 55.3 10.5 65.7zm-26.4-119.7c-24.5-15.8-28.9-51.6-9.9-80s54.3-38.6 78.8-22.8 28.9 51.6 9.9 80c-19.1 28.4-54.4 38.6-78.8 22.8zM205 496c-29.4 1.2-58.2-23.7-57.8-32.3-.4-12.7 35.8-22.6 59.3-22 23.7-1 55.6 7.5 55.7 18.9.5 11-28.8 35.9-57.2 35.4zm58.9-124.9c.2 29.7-26.2 53.8-58.8 54-32.6.2-59.2-23.8-59.4-53.4v-.6c-.2-29.7 26.2-53.8 58.8-54 32.6-.2 59.2 23.8 59.4 53.4v.6zm82.2 42.7c-25.3 34.6-59.6 35.9-72.3 26.3-13.3-12.4-3.2-50.9 15.1-72 20.9-23.3 43.3-38.5 58.9-26.6 10.5 10.3 16.7 49.1-1.7 72.3zm22.9-73.2c-21.5 9.4-39-105.3-12.6-98.3 43.9 24.7 36.3 79.6 12.6 98.3z\"></path></svg>")
  restore

drawLightbulbIcon colour = do
  save
  scale 0.1 0.1
  svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"lightbulb\" class=\"svg-inline--fa fa-lightbulb fa-w-11\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 352 512\"><path fill=\"" ++ colour ++ "\" d=\"M96.06 454.35c.01 6.29 1.87 12.45 5.36 17.69l17.09 25.69a31.99 31.99 0 0 0 26.64 14.28h61.71a31.99 31.99 0 0 0 26.64-14.28l17.09-25.69a31.989 31.989 0 0 0 5.36-17.69l.04-38.35H96.01l.05 38.35zM0 176c0 44.37 16.45 84.85 43.56 115.78 16.52 18.85 42.36 58.23 52.21 91.45.04.26.07.52.11.78h160.24c.04-.26.07-.51.11-.78 9.85-33.22 35.69-72.6 52.21-91.45C335.55 260.85 352 220.37 352 176 352 78.61 272.91-.3 175.45 0 73.44.31 0 82.97 0 176zm176-80c-44.11 0-80 35.89-80 80 0 8.84-7.16 16-16 16s-16-7.16-16-16c0-61.76 50.24-112 112-112 8.84 0 16 7.16 16 16s-7.16 16-16 16z\"></path></svg>")
  restore

