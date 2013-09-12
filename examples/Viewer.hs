{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import System.IO.Unsafe(unsafePerformIO)
import Data.IORef                 as IORef
import Control.Monad(ap)

-- OpenGL stuff 
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT          as GL
import Graphics.Rendering.OpenGL(($=))

-- PDB reading stuff
import Bio.PDB.Structure          as PDBS
import Bio.PDB.Structure.Elements as PDBE
import Bio.PDB.IO                 as PDBIO
import Bio.PDB.Iterable           as PDBI
import Data.ByteString.Char8      as BS
import Bio.PDB.Structure.Vector

data UIState = UIState { manipulationCenter :: Maybe GL.Position
                       , pressedButton      :: Maybe GL.MouseButton
                       , manipulatedMatrix  :: Maybe (GL.GLmatrix GL.GLdouble)
                       , windowSize         :: GL.Position
                       , currentViewMatrix  :: GL.GLmatrix GL.GLdouble --[GL.GLdouble]
                       }

initialUIState = do GL.loadIdentity
                    initialViewMatrix :: GL.GLmatrix GL.GLdouble <- GL.get $ GL.matrix $ Nothing
                    --m <- GL.getMatrixComponents GL.ColumnMajor initialViewMatrix
                    return $ cleanUIState initialViewMatrix

cleanUIState initialViewMatrix = UIState { manipulationCenter = Nothing
                                         , pressedButton      = Nothing
                                         , manipulatedMatrix  = Nothing
                                         , windowSize         = GL.Position 1 1
                                         , currentViewMatrix  = initialViewMatrix
                                         }

setWindowSize :: GL.GLsizei -> GL.GLsizei -> UIState -> UIState
setWindowSize w h uiState = uiState { windowSize = GL.Position (fromIntegral w) (fromIntegral h) }

main :: IO ()
main = do
  (progname, [filename]) <- GL.getArgsAndInitialize
  -- First read structure
  Just structure <- PDBIO.parse filename
  -- Initialize OpenGL
  --displayMode $= [With DisplayDepth, With DisplayRGB] -- is it glutInitDisplayModel (GLUT_SINGLE | GLUT_RGB | GLUT_DEPTH)
  setup progname -- setup OpenGL scene model
  -- Initialize camera
  initUIState <- initialUIState
  uiState <- newIORef initUIState
  translateViewMatrix uiState $ vec3DToVector3 $ (-(center structure))
  translateViewMatrix uiState $ GL.Vector3 0 0 (-frustumMiddle)
  -- Now set callbacks
  GL.displayCallback        $= display uiState structure
  GL.reshapeCallback        $= Just (reshape           uiState)
  GL.keyboardMouseCallback  $= Just (handleKeys        uiState)
  GL.motionCallback         $= Just (handleMouseMotion uiState)
  GL.closeCallback          $= Just (appClosing        uiState)
  display uiState structure
  GL.mainLoop

appClosing uiState = do uiSt <- readIORef uiState
                        let mat = currentViewMatrix uiSt
                        cm <- GL.getMatrixComponents GL.ColumnMajor mat
                        Prelude.putStr "Final view matrix:"
                        Prelude.print cm


modifyViewMatrix :: IORef UIState -> IO () -> IO ()
modifyViewMatrix uiState modification =
  do uiSt <- GL.get uiState
     GL.matrixMode $= GL.Modelview 0
     GL.matrix Nothing $= currentViewMatrix uiSt
     --m :: GL.GLmatrix GL.GLdouble <- GL.newMatrix GL.ColumnMajor $ currentViewMatrix uiSt
     --GL.matrix Nothing $= m
     modification
     m :: GL.GLmatrix GL.GLdouble <- GL.get $ GL.matrix Nothing
     --cm <- GL.getMatrixComponents GL.ColumnMajor m
     --print cm
     uiState $= uiSt { currentViewMatrix = m }
     GL.postRedisplay Nothing

manipulateViewMatrix :: IORef UIState -> IO () -> IO (GL.GLmatrix GL.GLdouble)
manipulateViewMatrix uiState modification =
  do uiSt <- GL.get uiState
     GL.matrixMode     $= GL.Modelview 0
     GL.matrix Nothing $= unJust (manipulatedMatrix uiSt)
     --GL.matrix Nothing $= unJust (manipulatedMatrix uiSt)
     --GL.loadIdentity
     --GL.multMatrix $ unJust $ manipulatedMatrix uiSt
     modification
     --GL.matrix Nothing $= unJust (manipulatedMatrix uiSt)
     m :: GL.GLmatrix GL.GLdouble <- GL.get $ GL.matrix Nothing
     cm <- GL.getMatrixComponents GL.ColumnMajor m
     --print cm
     uiState $= uiSt { currentViewMatrix = m }
     GL.postRedisplay Nothing
     return m

xaxis = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLdouble

yaxis = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLdouble

zaxis = GL.Vector3 0 0 1 :: GL.Vector3 GL.GLdouble

negateVector3 :: (Num a) => GL.Vector3 a -> GL.Vector3 a
negateVector3 = fmap (\a -> (-a))

handleKeys :: IORef UIState -> GL.Key -> GL.KeyState -> GL.Modifiers -> GL.Position -> IO ()
handleKeys uiState (GL.Char 'q')                    GL.Down _modifiers _position = rotateViewMatrix    uiState   10.0  yaxis
handleKeys uiState (GL.Char 'e')                    GL.Down _modifiers _position = rotateViewMatrix    uiState (-10.0) yaxis
handleKeys uiState (GL.Char 'r')                    GL.Down _modifiers _position = rotateViewMatrix    uiState   10.0  xaxis
handleKeys uiState (GL.Char 'f')                    GL.Down _modifiers _position = rotateViewMatrix    uiState (-10.0) xaxis
handleKeys uiState (GL.Char 'a')                    GL.Down _modifiers _position = translateViewMatrix uiState                 xaxis
handleKeys uiState (GL.Char 'd')                    GL.Down _modifiers _position = translateViewMatrix uiState $ negateVector3 xaxis
handleKeys uiState (GL.Char 'w')                    GL.Down _modifiers _position = translateViewMatrix uiState                 zaxis
handleKeys uiState (GL.Char 's')                    GL.Down _modifiers _position = translateViewMatrix uiState $ negateVector3 zaxis
handleKeys uiState (GL.MouseButton GL.RightButton)  GL.Down _modifiers  position = startMouseTracking uiState GL.RightButton  position 
handleKeys uiState (GL.MouseButton GL.MiddleButton) GL.Down _modifiers  position = startMouseTracking uiState GL.MiddleButton position 
handleKeys uiState (GL.MouseButton GL.LeftButton)   GL.Down _modifiers  position = startMouseTracking uiState GL.LeftButton position 
handleKeys uiState (GL.MouseButton GL.MiddleButton) GL.Up   _modifiers  position = do matrix <- mouseMoveViewMatrix xyAxes uiState position
                                                                                      modifyIORef uiState $ clearManipulatedMatrix matrix
handleKeys uiState (GL.MouseButton GL.RightButton ) GL.Up   _modifiers  position = do matrix <- mouseMoveViewMatrix xzAxes uiState position
                                                                                      modifyIORef uiState $ clearManipulatedMatrix matrix
handleKeys uiState (GL.MouseButton GL.LeftButton)   GL.Up   _modifiers  position = do matrix <- mouseRotateViewMatrix uiState position
                                                                                      modifyIORef uiState $ clearManipulatedMatrix matrix
handleKeys _       _                                _      _          _         = return ()

startMouseTracking uiState button position = modifyIORef uiState newState
  where
    newState uiState = uiState { manipulationCenter = Just position
                               , pressedButton      = Just button
                               , manipulatedMatrix  = Just $ currentViewMatrix uiState
                               }


unJust (Just a) = a
unJust Nothing  = error "unJust of Nothing"

rotateViewMatrix :: IORef UIState -> GL.GLdouble -> GL.Vector3 GL.GLdouble -> IO ()
rotateViewMatrix uiState angle axis =
     modifyViewMatrix uiState $ GL.rotate angle axis

clearManipulatedMatrix matrix uiState = uiState { manipulationCenter = Nothing
                                                , pressedButton      = Nothing
                                                , manipulatedMatrix  = Nothing
                                                , currentViewMatrix  = matrix
                                                }

translateViewMatrix :: IORef UIState -> GL.Vector3 GL.GLdouble -> IO ()
translateViewMatrix uiState translation =
     modifyViewMatrix uiState $ GL.translate translation

mouseMoveViewMatrix axesExpansion uiState pos1 = do uiSt <- readIORef uiState
                                                    let pos2 = unJust $ manipulationCenter uiSt
                                                    let windowDim = windowSize uiSt
                                                    let (x, y) = getPosChange pos1 pos2 windowDim
                                                    manipulateViewMatrix uiState $ GL.translate $ axesExpansion x y

handleMouseMotion uiState position = do btn <- pressedButton `fmap` readIORef uiState
                                        case btn of
                                          Just GL.MiddleButton -> do mouseMoveViewMatrix xyAxes uiState position
                                                                     return ()
                                          Just GL.RightButton  -> do mouseMoveViewMatrix xzAxes uiState position
                                                                     return ()
                                          Just GL.LeftButton   -> do mouseRotateViewMatrix uiState position
                                                                     return ()

xyAxes x y = GL.Vector3 (x * frustumMiddle) (y * frustumMiddle) 0
xzAxes x y = GL.Vector3 (x * frustumMiddle) 0                   ((-y) * frustumMiddle)

mouseRotateViewMatrix :: IORef UIState -> GL.Position -> IO (GL.GLmatrix GL.GLdouble)
mouseRotateViewMatrix uiState pos1 = do uiSt <- readIORef uiState
                                        let pos2 = unJust $ manipulationCenter uiSt
                                        let windowDim = windowSize uiSt
                                        let (angle, axis) = computeRotation pos1 pos2 windowDim
                                        --Prelude.putStrLn $ "AA" ++ show (angle, axis)
                                        manipulateViewMatrix uiState $ --do --GL.translate $ GL.Vector3 0 0 (-frustumMiddle)
                                                                       GL.rotate angle axis
                                                                          --GL.translate $ GL.Vector3 0 0 ( frustumMiddle)

computeRotation :: GL.Position -> GL.Position -> GL.Position -> (GL.GLdouble, GL.Vector3 GL.GLdouble)
computeRotation (GL.Position curX  curY )
                (GL.Position initX initY)
                (GL.Position w     h    ) = (angle, GL.Vector3 dy dx 0.0)
  where
    [xf, yf, wf, hf] = Prelude.map fromIntegral [curX - initX, curY - initY, w, h]
    dx = xf / (min hf wf)
    dy = yf / (min hf wf)
    angle = 180.0 * sqrt (dx*dx + dy*dy)
    
getPosChange :: GL.Position -> GL.Position -> GL.Position -> (GL.GLdouble, GL.GLdouble)
getPosChange (GL.Position x y) (GL.Position x' y') (GL.Position w h) = (fromIntegral (x  - x')/fromIntegral w,
                                                                        fromIntegral (y' - y )/fromIntegral h)

reflector = GL.Light 0

setup :: Prelude.String -> IO ()
setup progname = do
  GL.initialDisplayMode                $= [GL.RGBMode, GL.WithDepthBuffer, GL.DoubleBuffered]
  GL.createWindow progname
  GL.clearColor                        $= GL.Color4  0 0 0 0
  GL.shadeModel                        $= GL.Smooth
  GL.materialSpecular  GL.FrontAndBack $= GL.Color4  1 1 1 1
  GL.materialShininess GL.FrontAndBack $= 50
  GL.position reflector                $= GL.Vertex4 1 1 1 0
  GL.lighting                          $= GL.Enabled
  GL.light reflector                   $= GL.Enabled
  GL.depthFunc                         $= Just GL.Less
   
-- Assessing dimensions for initial focus
center :: PDBS.Structure -> Vec3D
center structure = average 
  where
    (!average, _count) = PDBI.ifoldl' step (fromIntegral 0, 0) structure
    step :: (Vec3D, Double) -> PDBS.Atom -> (Vec3D, Double)
    step (!r, !i) at = let i' = i + 1
                       in (coord at |* (1/i') + r |* (i/i'), i')

dims structure = maxv - minv
  where
    (!minv, !maxv) = PDBI.ifoldl' (\(!minv, !maxv) at -> let c = coord (at :: PDBS.Atom)
                                                         in (vzip min minv c,
                                                             vzip max maxv c)) (cs, cs) structure
    !cs = center structure

vec3DToVector3 :: Vec3D -> GL.Vector3 GL.GLdouble
vec3DToVector3 v = GL.Vector3 x' y' z'
  where
    (x, y, z) = unpackVec3D v
    [x', y', z'] :: [GL.GLdouble]  = Prelude.map realToFrac [x, y, z]

renderStructure :: IORef UIState -> PDBS.Structure -> IO ()
renderStructure uiState structure = PDBI.ifoldM (\_ -> renderAtom) () structure
  where renderAtom (at :: Atom) = GL.preservingMatrix $ do GL.matrixMode $= GL.Modelview 0
                                                           uiSt <- readIORef uiState
                                                           --m :: GL.GLmatrix GL.GLdouble <- GL.newMatrix GL.ColumnMajor $ currentViewMatrix uiSt
                                                           GL.matrix Nothing $= currentViewMatrix uiSt
                                                           --cm <- GL.getMatrixComponents GL.ColumnMajor $ currentViewMatrix uiSt
                                                           --print cm
                                                           GL.translate  $ vec3DToVector3 $ PDBS.coord at
                                                           GL.renderObject GL.Solid $ GL.Sphere' (realToFrac radius) 40 32 -- much smoother looks!
          where
            radius            = realToFrac $ PDBE.vanDerWaalsRadius $ PDBS.element at

display :: IORef UIState -> PDBS.Structure -> IO ()
display uiState structure = do GL.clear [GL.ColorBuffer,
                                         GL.DepthBuffer]
                               renderStructure uiState structure
                               GL.swapBuffers

myOrtho w h = let hw = fromIntegral h/fromIntegral w
                  wh = fromIntegral w/fromIntegral h
              in if w <= h
                   then GL.ortho (-1.5)    (1.5)    (-1.5*hw) (1.5*hw) (-10) (10)
                   else GL.ortho (-1.5*wh) (1.5*wh) (-1.5)    (1.5)    (-10) (10)

frustumFront  =    5.0
frustumBack   = 1000.0
frustumMiddle = frustumFront + abs (frustumBack - frustumFront)/2.0
--frustumMiddle = abs (frustumFront - frustumBack) / 2.0


reshape uiState s@(GL.Size w h) = do GL.viewport   $= (GL.Position 0 0, s)
                                     modifyIORef uiState $ setWindowSize w h
                                     GL.matrixMode $= GL.Projection
                                     GL.loadIdentity
                                     GL.frustum (-x) x (-y) y frustumFront frustumBack
                                     GL.matrixMode $= GL.Modelview 0
  where
    x = fromIntegral w/fromIntegral h
    y = 1.0


