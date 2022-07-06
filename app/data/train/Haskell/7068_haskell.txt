module GL.Types
    (
        V2FL,
        V3FL,
        Coor2(..),
        Coor3(..),
        fromDegrees
    ) where
import Graphics.Rendering.OpenGL as GL
type V2FL = Vertex2 GLfloat
type V3FL = Vertex3 GLfloat
data Coor2 = D2 {x,y::GLfloat} deriving Show
data Coor3 = D3 {xx,yy,zz::GLfloat} deriving (Show)
fromDegrees deg = deg * pi / 180 
