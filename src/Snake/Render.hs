-- |
-- Module      : Snake.Render
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created January 3 2016

-- TODO | - Look into FRP, signals, etc.
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
-- {-# LANGUAGE OverlappingInstances #-}


--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Snake.Render where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import System.IO (stdout, hFlush) --
import System.FilePath ((</>))    --
-- import System.Console.ANSI        --

import Linear.Projection
import Linear.Quaternion
import Linear.Matrix
import Linear.V3
import Linear.V4

import           Data.IORef
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Lens hiding (argument)
import Control.Monad

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL as GL hiding (projection, perspective, Line)
import Graphics.Rendering.OpenGL.GL.BufferObjects
import Graphics.Rendering.OpenGL.GL.Shaders as GLS --

import Graphics.Rendering.OpenGL.Raw as GLRaw

import Foreign.Marshal as Marshal
import Foreign.Ptr     as Ptr

import Graphics.GLUtil.JuicyTextures (readTexture)
import Graphics.GLUtil as GLU

import           Control.Monad.Trans.Class as S
import qualified Control.Monad.Trans.State as S

import           Graphics.Michelangelo.Transformations
import qualified Graphics.Michelangelo.Shaders as Shaders
import qualified Graphics.Michelangelo.Shapes  as Shapes
import           Graphics.Michelangelo.Shapes (π)

import Cartesian.Space.Types
import Cartesian.Plane.Types
import Cartesian.Plane

import Snake.Types
import Snake.Lenses as L
import Snake.Core



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
setupOpenGL :: IO ()
setupOpenGL = do
  clearColor $= Color4 0.812 0.957 0.969 1
  blend $= Enabled
  -- depth $= Enabled
  -- cullFace $= Just Back
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  depthFunc $= Just Less
  -- textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  texture2DWrap $= (Repeated, ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Move to Michelangelo
attribute :: (GL.AttribLocation, GL.BufferObject, Int) -> IO ()
attribute (loc, buffer, count) = do
  GL.vertexAttribArray loc     $= GL.Enabled                                                                         --
  GL.bindBuffer GL.ArrayBuffer $= Just buffer                                                                        --
  GL.vertexAttribPointer loc   $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral count) GL.Float 0 GLU.offset0) --

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
draw :: Program -> GL.PrimitiveMode -> (GL.BufferObject, GL.BufferObject, Int) -> M44 Float -> M44 Float -> IO ()
draw program primitive mesh pm mv = do
  [locv, locc] <- mapM (get . attribLocation program) ["aVertexPosition", "aVertexColor"] --
  [uMV, uPr] <- mapM (GL.get . GL.uniformLocation program) ["uMVMatrix", "uPMatrix"]

  mapM attribute [(locv, vb, 3), (locc, cb, 4)]
  GL.uniform uMV $= pm
  GL.uniform uPr $= mv

  GL.drawArrays primitive 0 (fromIntegral n) --
  where
    (vb, cb, n) = mesh

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
render :: GLFW.Window -> Program -> Game -> [(GL.BufferObject, GL.BufferObject, Int)] -> IO ()
render window program game meshes = do

  -- OpenGL config
  -- GL.currentProgram $= Just program
  -- GL.activeTexture $= (GL.TextureUnit 0)                --
  -- GL.textureBinding GL.Texture2D $= Just texture -- Is this needed (?)§

  -- texture2DWrap $= (Repeated, ClampToEdge)
  -- textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  -- textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  -- textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)

  -- Set attributes
  -- mapM (attribute) attribs

  -- Set uniforms
  Just secs <- liftM (liftM realToFrac) GLFW.getTime -- Ugly hack

  let modelview  = rotateX (2*π*mx) !*! rotateY (2*π*my) !*! identity & (translation .~ (V3 0.0 0.0 (z'+dz*realToFrac mx :: Float)))
      projection = perspective (40.0 * π/180.0) 1.0 1.0 40.0
      (Vector2D mx my) = dotmap (realToFrac . (/500.0)) $ game^.mouse.L.position
      dz =  30.0
      z' = -20.0

  -- Clear
  GL.clearColor $= Color4 1.0 0.24 0.7 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- TODO: Use map or type for the meshes
  -- TODO: Scenes and cameras

  --
  GL.lineWidth $= 4.2
  draw program GL.Lines     (meshes !! 1) modelview projection -- Grid

  GL.lineWidth $= 0.85
  draw program GL.LineStrip (meshes !! 2) modelview projection -- Snake

  -- Collisions
  forM (zip [0..] . map (dotmap realToFrac) . collisions . connect $ game^.snake.body) $ \(n, p@(Vector2D x' y')) -> do
    draw program GL.LineStrip (meshes !! 1) (foldl1 (!*!) [modelview, translate (from2D p)]) projection
    -- draw program GL.Triangles (_) modelview projection

  -- Render snake tiles
  when False $ do
    forM (connect $ game^.snake.body) $ \(Line a@(Vector2D ax ay) b@(Vector2D bx by)) -> do
      -- TODO: Convert between logical coordinates and graphical coordinates
      let θ      = realToFrac . argument $ b - a
          middle = dotmap realToFrac $ (a+b) `mult` Vector2D 0.5 0.0
          -- middle = Vector2D 0.8 0.8
          delta  = V3 (middle^.x) (middle^.y) 0 --
          shift  = modelview !*! (translate (delta) !*! rotateZ (secs) !*! translate (neg delta))
      draw program GL.Triangles (head meshes) shift projection
      draw program GL.Triangles (head meshes) modelview projection
    return ()

  -- Render
  forM (zip [0.0, 1.0..] $ take 1 meshes) $ \(nth, mesh) -> do
    --
    let v = V3 0 0 (-1.5)
    draw program GL.Triangles mesh (translate $ V3 0 0 (3)) projection -- (translate (neg v) !*! rotateZ 0 !*! translate v) projection
    -- translate (V3 4.0 4.0 0) !*! rotateZ secs !*! translate (V3 (-4.0) (-4.0) 0)

  -- Swap buffers
  GLFW.swapBuffers window
  where
    translate :: Num n => V3 n -> M44 n
    translate by = identity & (translation .~ by)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: GLFW.Window -> Program -> IORef Game -> [(GL.BufferObject, GL.BufferObject, Int)] -> IO ()
mainloop window program gameref meshes = do
  game <- readIORef gameref
  render window program game meshes
  GLFW.pollEvents
  closing <- GLFW.windowShouldClose window
  unless closing $ mainloop window program gameref meshes


-- |
attachListeners :: GLFW.Window -> IORef Game -> IO ()
attachListeners window gameref = do
  -- GLFW.setCharCallback        window $ Just (onkeypress gameref)
  GLFW.setWindowSizeCallback  window $ Just (onwindowresize gameref)
  GLFW.setKeyCallback         window $ Just (onkeypress gameref)
  GLFW.setCursorPosCallback   window $ Just (onmousemotion gameref)
  GLFW.setMouseButtonCallback window $ Just (onmousepress gameref)

-- Listeners -------------------------------------------------------------------------------------------------------------------------------

-- |
onmousepress :: IORef Game -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
onmousepress gameref w button state modkeys = do
  modifyIORef gameref (mouse.buttons %~ update state button) -- TODO: Fix button release logic
  return ()
  where
    update GLFW.MouseButtonState'Pressed  = S.insert
    update GLFW.MouseButtonState'Released = S.delete


-- |
onkeypress :: IORef Game -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
onkeypress gameref w key repeats keystate modifiers = do
  modifyIORef gameref (keyboard %~ update keystate key)
  return ()
  where
    update GLFW.KeyState'Pressed   = S.insert
    update GLFW.KeyState'Repeating = S.insert
    update GLFW.KeyState'Released  = S.delete


-- |
-- TODO: Use type synonym from GLFW (?)
onmousemotion :: IORef Game -> GLFW.Window -> Double -> Double -> IO ()
onmousemotion gameref w mx my = do
  modifyIORef gameref (mouse.L.position .~ Vector2D mx my)


-- |
onwindowresize :: IORef Game -> GLFW.WindowSizeCallback
onwindowresize gameref _ cx cy = do
  viewport $= (Position 0 0, Size (fromIntegral cx) (fromIntegral cy)) --

-- Utilities -------------------------------------------------------------------------------------------------------------------------------

-- |
perhaps :: b -> Maybe a -> (a -> b) -> b
perhaps b ma f = maybe b f ma

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
tick :: RealFloat r => r -> Game -> IO Game
tick dt game' = do
  return game'

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
loadGame :: IO Game
loadGame = do
  return game'
  where
    snake' = Snake { _body=body', _velocity=Vector2D 0 0 }
    body'  = fromAngles (Vector2D 2.0 1.5) [(2*π*0.05, 2.45), (-2*π*0.05, 2.45), (2*π*0.05, 2.45), (-2*π*0.05, 2.45), (2*π*0.05, 2.45)]
    game'  = Game { _snake=snake', _keyboard=S.empty, _mouse=Mouse { _position=Vector2D 0 0, _buttons=S.empty } }


type Mesh = (GL.BufferObject, GL.BufferObject, Int)


-- |
loadMeshes :: RealFloat f => Snake f -> IO [Mesh]
loadMeshes snake' = forM [triangleMesh, gridMesh, (thesnake, cycle [[1.0, 0.8, 0.23, 1.0]])] $ \(shape, colours) -> do
  vs <- makeBuffer ArrayBuffer (concat $ shape :: [Float])
  cs <- makeBuffer ArrayBuffer (concat . zipWith const colours $ shape :: [Float])
  return (vs, cs, length shape)
  where
    triangle = Shapes.polygon (\x y -> [x, y, 0.0]) 3 0.8
    triangleMesh = (cube, cycle . concat . map (replicate 6) $ [[0.03, 0.31, 0.24, 1.00], [0.53, 0.00, 0.51, 1.00], [0.78, 0.65, 0.07, 1.00]])
    cube     = map (Shapes.cuboid Shapes.vlist 2.45 0.32 0.08 !!) (concat $ concat Shapes.cuboidIndices)

    grid = [[-30.0, 0.0, 0.0], [30.0, 0.0, 0.0], [0.0, -30.0, 0.0], [0.0, 30.0, 0.0], [0.0, 0.0, -30.0], [0.0, 0.0, 30.0]]
    gridMesh = (grid, cycle . concat . map (replicate 2) $ [[1,0,0,1], [0,1,0,1], [0,0,1,1]])

    thesnake = map (toList . dotmap realToFrac . to3D) (snake'^.body)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do

  --
  let (w, h) = (500, 500)

  --
  GLFW.init

  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  mwindow <- GLFW.createWindow w h "Snakes on a Plane. Get it? Aren't I funny..." Nothing Nothing

  perhaps (putStrLn "Failed to create window") mwindow $ \window -> do
    GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does

    game'   <- loadGame
    gameref <- newIORef game'

    attachListeners window gameref
    setupOpenGL --

    meshes <- loadMeshes (game'^.snake)

    --
    let shaderpath = "C:/Users/Jonatan/Desktop/Haskell/projects/Snake/assets/shaders"
    Right program <- Shaders.loadShaderProgram (shaderpath </> "shader-vertex.glsl") (shaderpath </> "shader-pixel.glsl")
    GL.currentProgram $= Just program

    --
    mainloop window program gameref meshes

    GLFW.destroyWindow window
    GLFW.terminate
