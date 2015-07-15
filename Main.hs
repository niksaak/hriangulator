{-# LANGUAGE Arrows, DataKinds, TypeOperators, FlexibleContexts #-}
module Main where

import Prelude hiding ((.))

import Control.Arrow hiding ((<+>))
import Control.Monad
import Control.Monad.Fix (fix)
import Data.ByteString (ByteString)
import Data.Maybe
import Data.StateVar
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import System.IO

import Data.Vinyl
import Linear
import qualified Data.Vector.V2 as ACV -- For Delaunay

import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Triangulation.Delaunay as Delaunay
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.VinylGL as VinylGL

import qualified Control.Wire as W
import Control.Wire (Wire)
import qualified FRP.Netwire as NW
import FRP.Netwire ((.))

--------------------------------------------------------------------------------
-- Data

projection = SField :: SField '("projection", M44 GL.GLfloat)
modelview  = SField :: SField '("modelview",  M44 GL.GLfloat)
position   = SField :: SField '("position",   V2  GL.GLfloat)

vertexSource :: ByteString
vertexSource = GL.packUtf8 $ unlines [
    "#version 430",
    "uniform mat4 projection;",
    "uniform mat4 modelview;",
    "in vec2 position;",
    "void main() {",
    "    gl_Position = projection * modelview * vec4(position, 0, 1);",
    "}"
    ]

fragmentSource :: ByteString
fragmentSource = GL.packUtf8 $ unlines [
    "#version 430",
    "void main() {",
    "    gl_FragColor = vec4(1, 1, 1, 1);",
    "}"
    ]

data Resources = Resources {
    polygon :: Vector (V2 Double)
}

data Renderer = Renderer {
    vertices :: GL.BufferObject,
    program  :: GLUtil.ShaderProgram,
    vao      :: GL.VertexArrayObject,
    draw     :: IO ()
}

--------------------------------------------------------------------------------
-- Helpers

circlePolygon :: Double -> Int -> Vector (V2 Double)
circlePolygon r count =
    V.map ((* (2*pi / n)) >>> angle >>> (r *^)) $ V.enumFromN 0 count
    where n = fromIntegral count

triangulate :: Vector (V2 Double) -> Vector (V2 Double)
triangulate = V.toList
          >>> map (\(V2 x y) -> ACV.Vector2 x y)
          >>> Delaunay.triangulate
          >>> concatMap (\(x, y, z) -> [x, y, z])
          >>> map (\(ACV.Vector2 x y) -> V2 x y)
          >>> V.fromList

--------------------------------------------------------------------------------
-- Init

initGL :: String -> (Int, Int) -> IO GLFW.Window
initGL title (w, h) = do
    GLFW.setErrorCallback . Just $ \err string ->
        putStrLn $ "GLFW:" ++ show err ++ " " ++ string

    ok <- GLFW.init
    unless ok $ error "Error initializing GLFW."

    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True

    mb@(~(Just win)) <- GLFW.createWindow w h title Nothing Nothing
    when (isNothing mb) $ error "Unable to create window."

    GLFW.makeContextCurrent mb
    GLFW.setWindowTitle win title

    GL.debugMessageCallback $= Just (\(GL.DebugMessage src t id sev str) ->
        hPutStrLn stderr $ show src ++ " " ++ show t ++ " " ++ show id ++ " "
                        ++ show sev ++ " " ++ show str)

    return win

initRender :: Resources -> IO Renderer
initRender res = do
    vb  <- VinylGL.bufferVertices $ V.map ((position =:) . fmap realToFrac) $ polygon res
    sh  <- GLUtil.simpleShaderProgramBS vertexSource fragmentSource
    vao <- GLUtil.makeVAO $ do
        GL.currentProgram $= Just (GLUtil.program sh)
        VinylGL.setAllUniforms sh $ projection =: Linear.ortho (-250) 250 (-250) 250 0 1
                                <+> modelview  =: identity
        VinylGL.enableVertices' sh vb
        VinylGL.bindVertices vb
        GL.polygonMode $= (GL.Line, GL.Line)

    return $ Renderer {
        vertices = VinylGL.getVertexBuffer vb,
        program  = sh,
        vao      = vao,
        draw     = do
            GL.currentProgram $= Just (GLUtil.program sh)
            GLUtil.withVAO vao $
                GL.drawArrays GL.Triangles 0 $ fromIntegral . (*2) . V.length . polygon $ res
    }

-------------------------------------------------------------------------------
-- Wires (TODO)

mousePosition :: GLFW.Window -> Wire s e IO a (Double, Double)
mousePosition win = W.mkGen_ $ \_ -> liftM Right (GLFW.getCursorPos win)

isMousePressed :: (Monoid e) => GLFW.Window -> GLFW.MouseButton -> Wire s e IO a a
isMousePressed win button = W.mkGen_ $ \x -> do
    state <- GLFW.getMouseButton win button
    case state of
        GLFW.MouseButtonState'Pressed  -> return $ Right x
        GLFW.MouseButtonState'Released -> return $ Left mempty

--updateShape :: Wire s e IO (Renderer, V2 Double) Renderer
--updateShape = mkGen_ -- TODO

-- runNetwork :: (HasTime t s) => Session IO s -> Wire s e IO a Double -> IO ()

--------------------------------------------------------------------------------
-- Main

main = do
    win <- initGL "Test" (500, 500)

    let res = Resources {
        --polygon = triangulate $ V2 <$> [-100, 100] <*> [-100, 100]
        polygon = triangulate $ circlePolygon 200 8
    }

    r <- initRender res
    fix $ \loop -> do draw r
                      GLFW.swapBuffers win
                      GLFW.waitEvents
                      ks <- GLFW.getKey win GLFW.Key'Escape
                      unless (ks == GLFW.KeyState'Pressed) loop

