{-# LANGUAGE Arrows, DataKinds, TypeOperators, FlexibleContexts #-}
module Main where

import Prelude hiding ((.))

import qualified Control.Arrow as Arrow
import Control.Arrow hiding ((<+>))
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.StateVar
import Data.Vector
import System.IO

import Data.Vinyl
import Linear
import qualified Data.Vector.V2 as ACV2

import qualified Graphics.GLUtil as GLUtil
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Triangulation.Delaunay as Delaunay
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.VinylGL as VinylGL

import qualified Control.Wire
import Control.Wire (Wire(..))
import qualified FRP.Netwire as Netwire
import FRP.Netwire ((.))

projection = SField :: SField '("projection", M44 GL.GLfloat)
modelview  = SField :: SField '("modelview",  M44 GL.GLfloat)
position   = SField :: SField '("position",   V2  GL.GLfloat)

vertexSource :: (GL.ShaderType, ByteString)
vertexSource = (GL.VertexShader, GL.packUtf8 $ unlines [
    "#version 430",
    "uniform mat4 projection;",
    "uniform mat4 modelview;",
    "in vec2 position;",
    "void main() {",
    "    gl_Position = projection * modelview * vec4(position, 0, 1);",
    "}"
    ])

fragmentSource :: (GL.ShaderType, ByteString)
fragmentSource = (GL.FragmentShader, GL.packUtf8 $ unlines [
    "#version 430",
    "void main() {",
    "    gl_FragColor = vec4(1, 1, 1, 1);",
    "}"
    ])

data Resources = Resources {
    polygon :: [V2 Double]
}

data Renderer = Renderer {
    vertices :: GL.BufferObject,
    program  :: GLUtil.ShaderProgram,
    vao      :: GL.VertexArrayObject,
    draw     :: IO ()
}

circlePolygon :: Double -> Double -> [V2 Double]
circlePolygon r count = map ((r *^) . angle . (* (2*pi / count))) [0..count]

triangulate :: [V2 Double] -> [V2 Double]
triangulate =
    map toAC >>> Delaunay.triangulate >>> map fromTriple >>> concatMap fromAC
    where toAC (V2 x y) = ACV2.Vector2 x y
          fromTriple (x, y, z) = [x, y, z]
          fromAC (ACV2.Vector2 x y) = V2 x y

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
    vb  <- VinylGL.bufferVertices $ map ((position =:) . fmap realToFrac) $ polygon res
    sh  <- GLUtil.loadShaderProgramBS [vertexSource, fragmentSource]
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
                GL.drawArrays GL.Triangles 0 $ fromIntegral . (*2) . length . polygon $ res
    }

{-
runNetwork :: (HasTime t s) => Session IO s -> Wire s e IO a Double -> IO ()
runNetwork session wire = 
-}

main = do
    win <- initGL "Test" (500, 500)

    let res = Resources {
        --polygon = triangulate $ V2 <$> [-100, 100] <*> [-100, 100]
        polygon = triangulate $ circlePolygon 200 32
    }

    r <- initRender res
    let go = do draw r
                GLFW.swapBuffers win
                GLFW.waitEvents
                ks <- GLFW.getKey win GLFW.Key'Escape
                unless (ks == GLFW.KeyState'Pressed) go

    go
