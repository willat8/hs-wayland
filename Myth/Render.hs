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
    drawKubernetesIcon "green"
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

-- https://fonts.gstatic.com/s/i/materialiconsround/fiber_dvr/v13/24px.svg
drawDockerIcon colour = do
  save
  scale 1.5 1.5
  translate 0 (-6)
  svgRenderFromString ("<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"48px\" viewBox=\"0 0 24 24\" width=\"48px\" fill=\"" ++ colour ++ "\"><path d=\"M4.5 10.5h2v3h-2zm13 0h2v1h-2zM21 3H3c-1.11 0-2 .89-2 2v14c0 1.1.89 2 2 2h18c1.11 0 2-.9 2-2V5c0-1.11-.89-2-2-2zM8 13.5c0 .83-.67 1.5-1.5 1.5h-3c-.28 0-.5-.22-.5-.5v-5c0-.28.22-.5.5-.5h3c.83 0 1.5.67 1.5 1.5v3zm6.1-3.58l-1.27 4.36c-.12.43-.52.72-.96.72s-.84-.29-.96-.72L9.64 9.92c-.14-.46.21-.92.69-.92.32 0 .6.21.69.52l.85 2.91.85-2.91c.09-.31.37-.52.69-.52.48 0 .83.46.69.92zM21 11.5c0 .6-.4 1.15-.9 1.4l.63 1.48c.19.45-.14.96-.63.96-.28 0-.53-.16-.63-.42L18.65 13H17.5v1.31c0 .38-.31.69-.69.69h-.12c-.38 0-.69-.31-.69-.69V9.64c0-.35.29-.64.64-.64h2.86c.83 0 1.5.67 1.5 1.5v1z\"/></svg>")
  restore

drawBroadcastTowerIcon colour = do
  save
  scale 0.9 0.9
  svgRenderFromString ("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 89.68 70.95\"><defs><style>.cls-1{fill:" ++ colour ++ ";}</style></defs><g id=\"Layer_2\" data-name=\"Layer 2\"><g id=\"Layer_2-2\" data-name=\"Layer 2\"><path fill=\"" ++ colour ++ "\" class=\"cls-1\" d=\"M87.44,13.34H83.3l2.13-2.13A2.25,2.25,0,1,0,82.25,8L77,13.34H67.72l4-4a2.25,2.25,0,1,0-3.17-3.18l-7.16,7.15H52.15L58,7.52A2.24,2.24,0,0,0,54.8,4.35l-9,9H36.58l7.67-7.66A2.25,2.25,0,0,0,41.07,2.5L30.23,13.34H21l9.51-9.51A2.24,2.24,0,1,0,27.35.66L.66,27.35a2.24,2.24,0,1,0,3.17,3.17L16.52,17.84h9.22L18.07,25.5a2.26,2.26,0,0,0,0,3.18,2.27,2.27,0,0,0,3.18,0l6.07-6.07V71H34.5V17.84h6.81l-5.82,5.82a2.25,2.25,0,0,0,3.18,3.17l9-9h9.22l-4,4a2.25,2.25,0,0,0,0,3.18,2.26,2.26,0,0,0,3.17,0l7.15-7.15h9.23L70.32,20a2.25,2.25,0,0,0,3.18,3.17l5.31-5.3h8.63a2.25,2.25,0,0,0,0-4.5Z\"/></g></g></svg>")
  restore

drawRaspberryPiIcon colour = do
  save
  scale 0.4 0.4
  svgRenderFromString ("<svg viewBox=\"0 0 90 130\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><defs><linearGradient y2=\"0\" y1=\"0\" x2=\".8\" x1=\"0\" id=\"a\"><stop stop-color=\"" ++ colour ++ "\" offset=\"0\"/><stop stop-color=\"" ++ colour ++ "\" offset=\"1\"/></linearGradient><path d=\"M1 85.459c0-1.717 1.707-16.993 16.907-16.993 9.998-.717 17.766 7.169 27.464 7.169 24.208-1.814 20.952-34.293-.252-34.293-5.3-.014-10.179 2.22-13.929 5.954L6.738 71.747C2.68 75.792 1 80.972 1 85.459z\" id=\"b\"/></defs><path fill=\"url(#a)\" d=\"M37.4 40.145C21.18 38.415 4.84 26.155 3.55.215c25.17 0 38.63 14.9 39.93 38.51 4.76-28.32 27.07-25 27.07-25 1.06 16.05-12.12 25.78-27.07 26.59-4.2-8.85-29.36-30.56-29.36-30.56a.07.07 0 0 0-.11.08s24.28 21.15 23.39 30.31\"/><use xlink:href=\"#b\" fill=\"" ++ colour ++ "\" transform=\"rotate(180 45 85.3)\"/><use xlink:href=\"#b\" fill=\"" ++ colour ++ "\" transform=\"rotate(90 45 85.3)\"/><use xlink:href=\"#b\" fill=\"" ++ colour ++ "\" transform=\"rotate(-90 45 85.3)\"/><use xlink:href=\"#b\" fill=\"" ++ colour ++ "\"/></svg>")
  restore

drawLightbulbIcon colour = do
  save
  scale 0.1 0.1
  svgRenderFromString ("<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"lightbulb\" class=\"svg-inline--fa fa-lightbulb fa-w-11\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 352 512\"><path fill=\"" ++ colour ++ "\" d=\"M96.06 454.35c.01 6.29 1.87 12.45 5.36 17.69l17.09 25.69a31.99 31.99 0 0 0 26.64 14.28h61.71a31.99 31.99 0 0 0 26.64-14.28l17.09-25.69a31.989 31.989 0 0 0 5.36-17.69l.04-38.35H96.01l.05 38.35zM0 176c0 44.37 16.45 84.85 43.56 115.78 16.52 18.85 42.36 58.23 52.21 91.45.04.26.07.52.11.78h160.24c.04-.26.07-.51.11-.78 9.85-33.22 35.69-72.6 52.21-91.45C335.55 260.85 352 220.37 352 176 352 78.61 272.91-.3 175.45 0 73.44.31 0 82.97 0 176zm176-80c-44.11 0-80 35.89-80 80 0 8.84-7.16 16-16 16s-16-7.16-16-16c0-61.76 50.24-112 112-112 8.84 0 16 7.16 16 16s-7.16 16-16 16z\"></path></svg>")
  restore

-- https://raw.githubusercontent.com/kubernetes/kubernetes/0fcc3dbd55732c501d4948840f639e00a2fe33c5/logo/logo.svg
-- Original colour #326ce5
drawKubernetesIcon colour = do
  save
  scale 0.1 0.1
  svgRenderFromString ("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><!-- Created with Inkscape (http://www.inkscape.org/) --><svg width=\"777\" height=\"753.91779\" id=\"svg2\" version=\"1.1\" inkscape:version=\"1.2.1 (9c6d41e410, 2022-07-14)\" sodipodi:docname=\"logo.svg\" inkscape:export-filename=\"logo.png\" inkscape:export-xdpi=\"444.78766\" inkscape:export-ydpi=\"444.78766\" xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:svg=\"http://www.w3.org/2000/svg\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:cc=\"http://creativecommons.org/ns#\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"> <title id=\"title1221\">Kubernetes logo with no border</title> <defs id=\"defs4\" /> <sodipodi:namedview id=\"base\" pagecolor=\"#ffffff\" bordercolor=\"#666666\" borderopacity=\"1.0\" inkscape:pageopacity=\"0.0\" inkscape:pageshadow=\"2\" inkscape:zoom=\"0.75130096\" inkscape:cx=\"-148.40923\" inkscape:cy=\"310.79423\" inkscape:document-units=\"px\" inkscape:current-layer=\"layer1\" showgrid=\"false\" inkscape:window-width=\"1795\" inkscape:window-height=\"1114\" inkscape:window-x=\"73\" inkscape:window-y=\"36\" inkscape:window-maximized=\"0\" inkscape:snap-global=\"false\" fit-margin-top=\"10\" fit-margin-left=\"10\" fit-margin-right=\"10\" fit-margin-bottom=\"10\" inkscape:showpageshadow=\"2\" inkscape:pagecheckerboard=\"0\" inkscape:deskcolor=\"#d1d1d1\" /> <metadata id=\"metadata7\"> <rdf:RDF> <cc:Work rdf:about=\"\"> <dc:format>image/svg+xml</dc:format> <dc:type rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" /> <dc:title>Kubernetes logo with no border</dc:title> <dc:description>&quot;kubectl&quot; is pronounced &quot;kyoob kuttel&quot;</dc:description> </cc:Work> </rdf:RDF> </metadata> <g inkscape:label=\"Layer 1\" inkscape:groupmode=\"layer\" id=\"layer1\" transform=\"translate(-21.475962,-178.535)\"> <g id=\"g1350\" transform=\"matrix(1.0556855,0,0,1.0556855,-1.1958992,-9.9418072)\"> <path style=\"fill:" ++ colour ++ ";fill-opacity:1;stroke:none;stroke-width:0;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1\" d=\"m 386.93188,178.59794 a 48.929668,48.529248 0 0 0 -18.75129,4.74509 L 112.30567,305.60274 A 48.929668,48.529248 0 0 0 85.831333,338.52385 L 22.705266,613.15006 a 48.929668,48.529248 0 0 0 6.643127,37.20805 48.929668,48.529248 0 0 0 2.781605,3.86153 L 209.23642,874.42456 a 48.929668,48.529248 0 0 0 38.25525,18.26042 l 284.01821,-0.0654 a 48.929668,48.529248 0 0 0 38.25525,-18.22769 L 746.8061,654.15419 a 48.929668,48.529248 0 0 0 9.45745,-41.06958 L 693.03931,338.4584 A 48.929668,48.529248 0 0 0 666.56498,305.53729 L 410.65734,183.34303 a 48.929668,48.529248 0 0 0 -23.72546,-4.74509 z\" id=\"path3055\" inkscape:connector-curvature=\"0\" inkscape:export-filename=\"new.png\" inkscape:export-xdpi=\"250.55\" inkscape:export-ydpi=\"250.55\" /> <path inkscape:connector-curvature=\"0\" id=\"path3059\" d=\"m 389.46729,272.05685 c -8.45813,8.6e-4 -15.31619,7.61928 -15.31519,17.01687 10e-6,0.14423 0.0295,0.28205 0.0327,0.42542 -0.0125,1.27691 -0.0741,2.81523 -0.0327,3.92697 0.20171,5.42027 1.38324,9.56871 2.09439,14.56252 1.28834,10.68834 2.36788,19.54832 1.70169,27.78333 -0.64789,3.10534 -2.93516,5.94534 -4.97417,7.91939 l -0.35997,6.4795 c -9.19102,0.76149 -18.44352,2.1559 -27.68515,4.25422 -39.76672,9.02908 -74.00517,29.5131 -100.07232,57.17016 -1.69145,-1.15393 -4.65062,-3.27681 -5.53054,-3.92697 -2.7344,0.36926 -5.49798,1.21295 -9.09748,-0.88357 -6.85378,-4.61354 -13.09606,-10.98183 -20.64933,-18.65311 -3.46095,-3.66956 -5.96724,-7.16386 -10.07923,-10.701 -0.9338,-0.80327 -2.35888,-1.88971 -3.40337,-2.71616 -3.21476,-2.56307 -7.00645,-3.89976 -10.66827,-4.02514 -4.70807,-0.16121 -9.24037,1.67954 -12.20634,5.39958 -5.27283,6.61342 -3.58466,16.72163 3.76335,22.58009 0.0746,0.0594 0.15396,0.10554 0.22907,0.16362 1.00973,0.81851 2.24619,1.86728 3.1743,2.55254 4.36352,3.22174 8.34948,4.87096 12.69721,7.42852 9.15979,5.65673 16.75337,10.34716 22.77644,16.00241 2.35201,2.50715 2.7631,6.925 3.07612,8.83568 l 4.90872,4.38512 c -26.27764,39.54584 -38.43915,88.39294 -31.2521,138.16395 l -6.41405,1.86531 c -1.69048,2.18299 -4.07925,5.61791 -6.57773,6.64313 -7.88026,2.48206 -16.74905,3.39352 -27.45608,4.51601 -5.02684,0.41799 -9.36418,0.16855 -14.69342,1.17809 -1.17293,0.2222 -2.80722,0.64798 -4.09059,0.94902 -0.0446,0.009 -0.0863,0.0226 -0.1309,0.0327 -0.07,0.0162 -0.16185,0.0502 -0.22907,0.0654 -9.02695,2.18109 -14.82588,10.47821 -12.95901,18.65312 1.86731,8.17682 10.68465,13.14935 19.76576,11.19187 0.0656,-0.015 0.16074,-0.0175 0.22907,-0.0327 0.10252,-0.0235 0.19278,-0.0732 0.29452,-0.0981 1.2659,-0.27788 2.85232,-0.58705 3.9597,-0.88357 5.23946,-1.40285 9.03407,-3.46407 13.7444,-5.26868 10.13362,-3.63457 18.52665,-6.67085 26.70341,-7.85395 3.41508,-0.26747 7.01316,2.10712 8.80296,3.10886 l 6.67585,-1.14537 c 15.3625,47.62926 47.55736,86.12636 88.32413,110.28245 l -2.7816,6.67585 c 1.0026,2.59224 2.10843,6.09958 1.36158,8.65957 -2.97265,7.70859 -8.0644,15.84504 -13.86244,24.91604 -2.80737,4.19078 -5.68053,7.44303 -8.21392,12.23906 -0.60622,1.14761 -1.37829,2.91048 -1.96348,4.12332 -3.93623,8.4219 -1.04891,18.12187 6.51223,21.76197 7.60863,3.66295 17.05297,-0.20037 21.14019,-8.63934 0.006,-0.0119 0.0269,-0.0207 0.0327,-0.0327 0.004,-0.009 -0.004,-0.0236 0,-0.0327 0.58217,-1.19647 1.40694,-2.76916 1.89804,-3.89424 2.16992,-4.97105 2.89194,-9.23107 4.41784,-14.03893 4.05224,-10.17885 6.27862,-20.85905 11.85692,-27.51404 1.52752,-1.82236 4.01788,-2.52321 6.59985,-3.21451 l 3.46882,-6.28315 c 35.53987,13.64156 75.32106,17.30219 115.06027,8.27936 9.06551,-2.05833 17.81739,-4.72226 26.27798,-7.91939 0.97492,1.72926 2.78672,5.05344 3.27248,5.89046 2.62384,0.85365 5.48775,1.29447 7.82122,4.74509 4.17347,7.13031 7.0276,15.56563 10.50465,25.75439 1.52615,4.80777 2.28038,9.06798 4.45057,14.03892 0.49463,1.13301 1.31527,2.72779 1.89803,3.92697 4.07863,8.46638 13.55289,12.34291 21.17292,8.67206 7.56021,-3.64203 10.45071,-13.34112 6.51223,-21.76196 -0.58526,-1.2128 -1.38994,-2.97575 -1.99621,-4.12332 -2.53364,-4.79589 -5.40634,-8.01572 -8.21392,-12.20634 -5.79852,-9.0707 -10.60772,-16.60606 -13.58077,-24.3145 -1.24313,-3.97574 0.20973,-6.44834 1.17809,-9.03203 -0.57991,-0.66473 -1.82087,-4.41925 -2.55253,-6.18498 42.36668,-25.0155 73.61612,-64.94823 88.29141,-111.06785 1.9817,0.31146 5.42607,0.92086 6.54495,1.14537 2.30334,-1.51916 4.42118,-3.50131 8.57389,-3.1743 8.17681,1.18266 16.5696,4.2199 26.7034,7.85394 4.71043,1.80438 8.50488,3.89883 13.7444,5.30141 1.1074,0.29645 2.69378,0.57303 3.9597,0.85085 0.10179,0.0249 0.19195,0.0748 0.29452,0.0981 0.0684,0.0153 0.16352,0.0177 0.22908,0.0327 9.08163,1.95506 17.90054,-3.01456 19.76575,-11.19187 1.86478,-8.17539 -3.93147,-16.47444 -12.959,-18.65311 -1.31311,-0.29859 -3.17535,-0.80569 -4.45057,-1.0472 -5.32929,-1.00926 -9.66655,-0.76036 -14.69342,-1.17809 -10.70708,-1.12194 -19.57569,-2.03437 -27.45607,-4.51601 -3.21306,-1.24646 -5.49884,-5.06971 -6.61046,-6.64313 l -6.18498,-1.79986 c 3.20678,-23.19994 2.3421,-47.34497 -3.20703,-71.50361 -5.60079,-24.38357 -15.49883,-46.68472 -28.69961,-66.33309 1.58655,-1.44229 4.58271,-4.09548 5.43231,-4.87599 0.24835,-2.74801 0.035,-5.62922 2.87978,-8.67206 6.02276,-5.65557 13.61694,-10.3452 22.77643,-16.00241 4.3476,-2.55779 8.36659,-4.20655 12.72993,-7.42852 0.98672,-0.7286 2.33409,-1.88243 3.37065,-2.71616 7.34646,-5.86043 9.03788,-15.96803 3.76335,-22.58009 -5.27453,-6.61205 -15.49543,-7.23487 -22.84188,-1.37444 -1.04569,0.82818 -2.4646,1.90853 -3.40338,2.71616 -4.1118,3.53737 -6.65119,7.03126 -10.11195,10.701 -7.55286,7.67168 -13.79578,14.07193 -20.64932,18.68584 -2.96985,1.72897 -7.31984,1.13073 -9.29389,1.01446 l -5.82501,4.15605 C 500.27311,376.86318 455.0492,354.59475 406.3533,350.26897 c -0.1362,-2.04069 -0.31463,-5.72937 -0.35997,-6.83948 -1.99355,-1.90762 -4.40179,-3.53622 -5.00689,-7.65759 -0.66619,-8.23501 0.44607,-17.09499 1.73441,-27.78333 0.71115,-4.99381 1.89268,-9.14225 2.09439,-14.56252 0.0459,-1.23215 -0.0277,-3.02011 -0.0327,-4.35239 -10e-4,-9.39759 -6.85705,-17.01772 -15.31518,-17.01687 z m -19.17671,118.79088 -4.54874,80.33929 -0.32725,0.16363 c -0.30509,7.18725 -6.22028,12.92628 -13.4826,12.92628 -2.97487,0 -5.72075,-0.95534 -7.95212,-2.58526 l -0.1309,0.0654 -65.87494,-46.69823 c 20.24605,-19.90834 46.14234,-34.62059 75.9869,-41.39683 5.45167,-1.23781 10.90091,-2.15627 16.32965,-2.81433 z m 38.38615,0 c 34.84372,4.28545 67.06745,20.06297 91.76023,44.24388 l -65.44952,46.40371 -0.22908,-0.0981 c -5.80924,4.2429 -13.99408,3.19016 -18.52221,-2.48708 -1.85491,-2.32577 -2.82817,-5.06044 -2.94523,-7.82122 l -0.0655,-0.0327 z m -154.59178,74.21976 60.14811,53.79951 -0.0654,0.32725 c 5.42904,4.71967 6.22963,12.90973 1.70169,18.58767 -1.85478,2.32586 -4.33755,3.88585 -7.0031,4.61419 l -0.0654,0.2618 -77.09954,22.25283 c -3.92412,-35.88222 4.53283,-70.7626 22.38374,-99.84325 z m 270.33926,0.0327 c 8.93685,14.48535 15.70428,30.66403 19.73304,48.20357 3.98044,17.32923 4.97939,34.62748 3.33792,51.34515 l -77.49224,-22.31828 -0.0654,-0.32725 c -6.93922,-1.89651 -11.20383,-8.95519 -9.58835,-16.03514 0.66186,-2.90032 2.20143,-5.35391 4.28694,-7.16672 l -0.0327,-0.16362 59.82087,-53.53771 z M 377.13006,523.023 h 24.64174 l 15.31519,19.14398 -5.49776,23.88908 -22.12194,10.63555 -22.18739,-10.66828 -5.49776,-23.88907 z m 78.99757,65.51497 c 1.04717,-0.0529 2.08976,0.0415 3.10886,0.22907 l 0.1309,-0.16362 79.75024,13.4826 c -11.67148,32.79084 -34.00528,61.19811 -63.84601,80.20839 l -30.95762,-74.77608 0.0981,-0.1309 c -2.84377,-6.60777 0.002,-14.35654 6.54495,-17.50774 1.67514,-0.80677 3.42525,-1.2535 5.17051,-1.34172 z m -133.94245,0.32725 c 6.08601,0.0853 11.5449,4.30946 12.95901,10.50465 0.66202,2.90028 0.33981,5.77395 -0.75267,8.31209 l 0.22907,0.29452 -30.63038,74.02341 C 275.35248,663.62313 252.54242,636.1077 240.34055,602.34787 l 79.06303,-13.41715 0.1309,0.16362 c 0.88436,-0.16274 1.78127,-0.24127 2.6507,-0.22907 z m 66.79124,32.43029 c 2.11999,-0.0779 4.27113,0.35707 6.31588,1.34172 2.6803,1.29069 4.75083,3.32294 6.05408,5.75955 h 0.29452 l 38.9752,70.42369 c -5.05825,1.69565 -10.25839,3.1448 -15.57699,4.3524 -29.80784,6.7679 -59.52097,4.71725 -86.4261,-4.45057 l 38.87702,-70.29279 h 0.0654 c 2.3328,-4.36094 6.75698,-6.96265 11.42094,-7.134 z\" style=\"color:#000000;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;font-size:medium;line-height:normal;font-family:Sans;-inkscape-font-specification:Sans;text-indent:0;text-align:start;text-decoration:none;text-decoration-line:none;letter-spacing:normal;word-spacing:normal;text-transform:none;writing-mode:lr-tb;direction:ltr;baseline-shift:baseline;text-anchor:start;display:inline;overflow:visible;visibility:visible;fill:#ffffff;fill-opacity:1;stroke:none;stroke-width:0;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;marker:none;enable-background:accumulate\" sodipodi:nodetypes=\"ccccccccsccccscssccsccccccccscccsccccccccccccccscccscsccsccccscscsccccccccscccscsccccsccccscscscccccccccccccccscccsccccccccccccscccccscccccccccccccccccccccccscccscccccccccscccscccc\" inkscape:export-filename=\"./path3059.png\" inkscape:export-xdpi=\"250.55\" inkscape:export-ydpi=\"250.55\" /> </g> </g></svg>")
  restore

