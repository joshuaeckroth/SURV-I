{-# LANGUAGE ScopedTypeVariables #-}
module Polyhedron
(
  Polyhedron
  
, construct -- :: [(a, a, a)] -> Polyhedron a
, overlaps  -- :: Polyhedron a -> Polyhedron a -> Bool
, intersect -- :: Polyhedron a -> Polyhedron a -> Polyhedron a
, encloses  -- :: Polyhedron a -> Polyhedron a -> Bool
, volume    -- :: Polyhedron a -> a
, vertices  -- :: Polyhedron a -> [(a, a, a)]
--, surfaceArea -- :: Polyhedron a -> a 
)
where

{-
Notes:

This module is "self-contained."  All needed structures are defined here but
not necessarily exported.

Floating numbers are needed due to a square root that is needed in some
computations (e.g., volume).  

It is believed that all functions that are O(n^2) can be rewritten to be
O(n log n) if need requires.
-}

import Data.List hiding (intersect)
import Data.Maybe

-- Added by Mark. Awaiting placement and comments.

vertices :: Polyhedron a -> [(a, a, a)]
vertices polyhedron = [ (x, y, z) | Point3 x y z <- poly3Points polyhedron ]

{-
-- These are imported only for testing.
import Debug.Trace
import System.Random
import Control.Monad
-}

-- O(n^2) due to the use of nub in convexHull3 to get points of convex hull.
-- Point3s are used because they are strict (for efficiency reasons).
-- Tests:  success.
construct :: (Ord a, Tol a, Floating a) => [(a, a, a)] -> Polyhedron a
construct tupleList
  = Polyhedron
    {
      poly3Center   = cHull3Center ch
    , poly3Points   = cHull3Points ch
    , poly3LineSegs = cHull3LineSegs ch
    , poly3Faces    = cHull3Triangles ch -- These are Triangle3s
    , poly3Box      = box3FromPoints pl
    }
    where
      pl = map tupleToPoint3 tupleList
      ch = convexHull3 pl

-- O(n^2).  This one is pretty bad.  
-- It is not necessary that at least one point from one must be in the other
-- for the two polyhedrons to overlap.  At least, I think this is the case.
-- This should handle all edge cases, but it's remarkably ugly. And 
-- potentially inefficient.
-- Tests:  success.
overlaps :: (Ord a, Floating a, Tol a) => Polyhedron a -> Polyhedron a -> Bool
overlaps p1 p2 
  = (box3sOverlap (poly3Box p1) (poly3Box p2)) &&
    (  (or [ point3InPolyhedron p p1 | p <- poly3Points p2 ]) ||
       (or [ point3InPolyhedron p p2 | p <- poly3Points p1 ]) ||
       (any isJust [ lineSeg3TriangleIntersection ls t | ls <- poly3LineSegs p1, t <- poly3Faces p2 ]) ||
       (any isJust [ lineSeg3TriangleIntersection ls t | ls <- poly3LineSegs p2, t <- poly3Faces p1 ]) 
    )

-- O(n^2).  Simplistic and unoptimized.
-- Tests:  success modulo accuracy in operations with Doubles.
intersect :: (Ord a, Floating a, Tol a) => Polyhedron a -> Polyhedron a -> Polyhedron a
intersect p1 p2
  = if ((box3sOverlap (poly3Box p1) (poly3Box p2)) && (isJust i))
      then fromJust i
      else error "intersect:  No intersection."
    where
      i = poly3PolyIntersect p1 p2

-- O(n^2).  All points in second are checked to see if they are all points inside the first.
-- Tests:  success.
encloses :: (Ord a, Floating a) => Polyhedron a -> Polyhedron a -> Bool
encloses p1 p2 = and [ point3InPolyhedron p p1 | p <- poly3Points p2 ]

-- O(n).  Sum the volumes of all the oblique pyramids (apex at the center for all).
-- Tests:  success.
volume :: Floating a => Polyhedron a -> a
volume p
  = (1/3) * (strictSum [ (abs $ triangle3Area face) * (point3PlaneDistance c (tri3Plane face)) | face <- poly3Faces p ])
    where
      c = poly3Center p

-- O(n).
-- Tests:  success.
surfaceArea :: Floating a => Polyhedron a -> a
surfaceArea p = strictSum [ abs $ triangle3Area face | face <- poly3Faces p ]










      

{-
The math library has more detailed versions of some of these; however, this is
good enough for a quick-and-dirty implementation.
-}
class Num a => Tol a where
  tol    :: a
  tolSqr :: a
  tolSqr = tol * tol
  
instance Tol Float where tol = 0.000001
instance Tol Double where tol = 0.00000000001

data Point3 a
  = Point3
    {
      xCoord3 :: !a
    , yCoord3 :: !a
    , zCoord3 :: !a
    } deriving (Eq, Ord)
    
type Vector3 a = Point3 a 

data Line3 a 
  = Line3 
    {
      line3Point  :: Point3 a
    , line3Vector :: Vector3 a -- unit vector
    } deriving Show
    
data LineSeg3 a
  = LineSeg3
    {
      lineSeg3Points :: (Point3 a, Point3 a)
    , lineSeg3Vector :: Vector3 a -- unit vector
    , lineSeg3Box    :: Box3 a
    } deriving (Eq, Ord, Show)

data Plane3 a
  = Plane3
    {
      plane3Normal :: Vector3 a
    , plane3Const  :: a
    , plane3Point  :: Point3 a
    } deriving (Eq, Ord, Show)

data Triangle3 a
  = Triangle3
    {
      tri3Points :: [Point3 a]
    , tri3Plane  :: Plane3 a
    , tri3Box    :: Box3 a
    } deriving (Eq, Ord, Show)
    
data Box3 a
  = Box3 
    {
      box3LLPoint :: Point3 a -- least coord values in all 3 dimensions
    , box3URPoint :: Point3 a -- most coord values in all 3 dimensions
    } deriving (Eq, Ord, Show)
    
data Polyhedron a
  = Polyhedron
    {
      poly3Center   :: Point3 a
    , poly3Points   :: [Point3 a]
    , poly3LineSegs :: [LineSeg3 a]
    , poly3Faces    :: [Triangle3 a]
    , poly3Box      :: Box3 a -- to make intersection functions faster
    } deriving (Eq, Ord, Show)
    
-- This is more streamlined than the derived version.
instance Show a => Show (Point3 a) where
  show (Point3 x y z) = "(Pt: " ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ ")"
    
tupleToPoint3 :: (a, a, a) -> Point3 a
tupleToPoint3 (x, y, z) = Point3 x y z

point3ToTuple :: Point3 a -> (a, a, a)
point3ToTuple (Point3 x y z) = (x, y, z)
      
-- Well, ok, * defined for Point3s is silly, but not for Vector3s, which is a type synonym.
instance Num a => Num (Point3 a) where
  (+) (Point3 a b c) (Point3 x y z) = Point3 (a + x) (b + y) (c + z)
  (-) (Point3 a b c) (Point3 x y z) = Point3 (a - x) (b - y) (c - z)
  (*) (Point3 a b c) (Point3 x y z) = Point3 (b * z - c * y) (c * x - a * z) (a * y - b * x)
  negate (Point3 a b c)             = Point3 (negate a) (negate b) (negate c)
  abs _                             = error "abs:  no instance for Point3."
  signum _                          = error "signum:  no instance for Point3."
  fromInteger _                     = error "fromInteger:  no instance for Point3."
  
vectorDot :: Num a => Vector3 a -> Vector3 a -> a
vectorDot (Point3 a b c) (Point3 x y z) = a * x + b * y + c * z
  
vectorSize :: Floating a => Vector3 a -> a
vectorSize v = sqrt $ vectorDot v v
  
unsafeUnitVector :: Floating a => Vector3 a -> Vector3 a
unsafeUnitVector v@(Point3 x y z) = let n = (1 / (vectorSize v)) in Point3 (x * n) (y * n) (z * n)
  
scalarVector3 :: Num a => a -> Vector3 a -> Vector3 a
scalarVector3 s (Point3 x y z) = Point3 (s * x) (s * y) (s * z)
  
vectorsParallel :: (Ord a, Num a, Tol a) => Vector3 a -> Vector3 a  -> Bool
vectorsParallel u v = let d = vectorDot u v in (abs $ d * d - (vectorDot u u) * (vectorDot v v)) < tol
  
line3FromPoints :: Floating a => Point3 a -> Point3 a -> Line3 a
line3FromPoints  p1 p2
  = Line3 
    {
      line3Point  = p1
    , line3Vector = unsafeUnitVector $ p2 - p1
    }
      
point3OnLine :: (Ord a, Num a, Tol a) => Point3 a -> Line3 a -> Bool
point3OnLine p l = vectorsParallel (line3Vector l) (p - (line3Point l))

lineSeg3 :: (Eq a, Ord a, Floating a) => Point3 a -> Point3 a -> LineSeg3 a
lineSeg3 p1 p2
  = LineSeg3
    {
      lineSeg3Points = (p1, p2)
    , lineSeg3Vector = unsafeUnitVector $ p2 - p1
    , lineSeg3Box    = box3FromPoints [p1, p2]
    }
    
point3OnLineSeg3 :: (Ord a, Num a, Tol a) => Point3 a -> LineSeg3 a -> Bool
point3OnLineSeg3 p ls = (point3InBox p (lineSeg3Box ls)) && (vectorsParallel (p - (fst $ lineSeg3Points ls)) (lineSeg3Vector ls))

-- ax + by + cz + d = 0, where normal = (a, b, c).
plane3 :: (Num a, Floating a) => Point3 a -> Point3 a -> Point3 a -> Plane3 a
plane3 p1 p2 p3
  = Plane3
    {
      plane3Normal = n
    , plane3Const  = negate $ vectorDot p1 n
    , plane3Point  = p1
    }
    where n = unsafeUnitVector $ (p2 - p1) * (p3 - p2)
    
point3OnPlane :: (Ord a, Num a, Tol a) => Point3 a -> Plane3 a -> Bool
point3OnPlane p plane = (abs $ (vectorDot p (plane3Normal plane)) + (plane3Const plane)) < tol

point3AbovePlane :: (Ord a, Floating a, Tol a) => Point3 a -> Plane3 a -> Bool
point3AbovePlane p plane = (vectorDot (p - (plane3Point plane)) (plane3Normal plane)) > 0

point3BelowPlane :: (Ord a, Num a) => Point3 a -> Plane3 a -> Bool
point3BelowPlane p plane = (vectorDot (p - (plane3Point plane)) (plane3Normal plane)) < 0

point3PlaneDistance :: Num a => Point3 a -> Plane3 a -> a
point3PlaneDistance point plane = abs $ vectorDot (plane3Normal plane) (point - (plane3Point plane))

line3PlaneIntersection :: Floating a => Line3 a -> Plane3 a -> Maybe (Point3 a)
line3PlaneIntersection l p
  = if (d == 0) then Just (linePt + (scalarVector3 u lineVec)) else Nothing
    where
      linePt  = line3Point l
      lineVec = line3Vector l
      n       = plane3Normal p
      d       = vectorDot n lineVec
      u       = (vectorDot n ((plane3Point p) - linePt)) / d

lineSeg3PlaneIntersection :: (Ord a, Floating a) => LineSeg3 a -> Plane3 a -> Maybe (Point3 a)
lineSeg3PlaneIntersection ls p
  = if ((d /= 0) && (point3InBox pt (lineSeg3Box ls))) then Just pt else Nothing
    where
      linePt  = fst $ lineSeg3Points ls
      lineVec = lineSeg3Vector ls
      n       = plane3Normal p
      d       = vectorDot n lineVec
      u       = (vectorDot n ((plane3Point p) - linePt)) / d
      pt      = linePt + (scalarVector3 u lineVec)

triangle3WithBelow :: (Ord a, Num a, Floating a) => Point3 a -> Point3 a -> Point3 a -> Point3 a -> Triangle3 a
triangle3WithBelow p1 p2 p3 b 
  = Triangle3
    {
      tri3Points = [p1, p2, p3]
    , tri3Plane  = if ((vectorDot (plane3Normal p) (b - p1)) < 0)
                     then p
                     else p { plane3Normal = negate $ plane3Normal p }
    , tri3Box    = box3FromPoints [p1, p2, p3]
    } 
    where p = plane3 p1 p2 p3
    
triangle3Area :: forall a. Floating a => Triangle3 a -> a
triangle3Area t = (0.5::a) * (sqrt $ triangle3AreaAux t)
     
triangle3AreaAux :: Num a => Triangle3 a -> a
triangle3AreaAux t
  = detA*detA + detB*detB + detC*detC
    where
      (p1:p2:p3:_) = tri3Points t
      (x1, y1, z1) = point3ToTuple p1
      (x2, y2, z2) = point3ToTuple p2
      (x3, y3, z3) = point3ToTuple p3
      
      detA         = specialDet x1 y1 x2 y2 x3 y3
      detB         = specialDet y1 z1 y2 z2 y3 z3
      detC         = specialDet z1 x1 z2 x2 z3 x3
      
{-
To evaluate the determinant:
| a b 1 |
| c d 1 |
| e f 1 |
-}
specialDet :: Num a => a -> a -> a -> a -> a -> a -> a
specialDet a b c d e f = a*(d-f) - b*(c-e) + (c*f - d*e)

-- So this was the culprit overall.  Points on the line segments of triangles
-- were being kicked out because the cross product test was zero despite the
-- other tests being consistent.  This is now changed.
-- This assumes the point is on the plane of the triangle; this condition is
-- already checked in the function that calls this one.
point3InTriangle :: (Eq a, Ord a, Num a, Tol a) => Point3 a -> Triangle3 a -> Bool
point3InTriangle p t
  = and [ ct1 == ct2 | let ct = filter ((/=) 0) [crossTestA, crossTestB, crossTestC], (ct1, ct2) <- zip ct (tail ct) ]
    where
      (p1:p2:p3:_) = tri3Points t      
      v1           = p2 - p1
      v2           = p3 - p2
      v3           = p1 - p3
      cross1       = v1 * (p - p2)
      cross2       = v2 * (p - p3)
      cross3       = v3 * (p - p1)
      crossTestA   = signum $ vectorDot cross1 cross2
      crossTestB   = signum $ vectorDot cross2 cross3
      crossTestC   = signum $ vectorDot cross3 cross1

-- Could probably include a box test here before seeing if the point is in
-- the triangle.  Not sure if that'd be faster.  The box test between the
-- line segment and the triangle is probably good enough.
lineSeg3TriangleIntersection :: (Ord a, Floating a, Tol a) => LineSeg3 a -> Triangle3 a -> Maybe (Point3 a)
lineSeg3TriangleIntersection ls t
  = if (box3sOverlap (lineSeg3Box ls) (tri3Box t))
      then (lineSeg3PlaneIntersection ls (tri3Plane t)) >>= 
              (\p -> if (point3InTriangle p t) then Just p else Nothing)
      else Nothing
    
box3FromPoints :: (Eq a, Ord a) => [Point3 a] -> Box3 a
box3FromPoints ((Point3 x y z):ps)
  = Box3 (Point3 xm ym zm) (Point3 xM yM zM)
    where
      ((xm, xM), (ym, yM), (zm, zM)) = maxesMins ps x x y y z z
  
(!$) f a = a `seq` f a
  
-- This is done so that the data are traversed only once.
-- Strictness insures speed.
maxesMins :: (Eq a ,Ord a) => [Point3 a] -> a -> a -> a -> a -> a -> a -> ((a, a), (a, a), (a, a))
maxesMins ((Point3 x y z):ps) xm xM ym yM zm zM
  = maxesMins ps !$ min x xm !$ max x xM !$ min y ym !$ max y yM !$ min z zm !$ max z zM
maxesMins [] xm xM ym yM zm zM = ((xm, xM), (ym, yM), (zm, zM))

point3InBox :: (Eq a, Ord a) => Point3 a -> Box3 a -> Bool
point3InBox (Point3 x y z) (Box3 (Point3 xm ym zm) (Point3 xM yM zM))
  = (x >= xm) && (x <= xM) && (y >= ym) && (y <= yM) && (z >= zm) && (z <= zM)
      
-- Perhaps a little messy.  In the math library, it's a bit more streamlined.  I think.
box3sOverlap :: (Eq a, Ord a) => Box3 a -> Box3 a -> Bool
box3sOverlap b1 b2
  = xsOverlap && ysOverlap && zsOverlap
    where
      -- Do the intervals [a, b] and [c, d] overlap?
      intervalsOverlap    = \a b c d -> not $ (b < c) || (a > d)
      boxIntervalsOverlap = \u v w -> intervalsOverlap
                                       (w $ box3LLPoint u) (w $ box3URPoint u)
                                       (w $ box3LLPoint v) (w $ box3URPoint v)
                                      
      xsOverlap           = boxIntervalsOverlap b1 b2 xCoord3
      ysOverlap           = boxIntervalsOverlap b1 b2 yCoord3
      zsOverlap           = boxIntervalsOverlap b1 b2 zCoord3
            
-- The second part is a bit clumsy, but I'm not sure how else to do it.
point3InPolyhedron :: (Ord a, Num a) => Point3 a -> Polyhedron a -> Bool
point3InPolyhedron p poly = (point3InBox p (poly3Box poly)) && (and [ point3BelowPlane p (tri3Plane t) | t <- (poly3Faces poly) ])

-- Yay!  It works!
poly3PolyIntersect :: (Ord a, Floating a, Tol a) => Polyhedron a -> Polyhedron a -> Maybe (Polyhedron a)
poly3PolyIntersect p1 p2
  = case pts of
      (a:b:c:d:_) -> Just $ 
                       Polyhedron
                       {
                         poly3Center   = cHull3Center ch
                       , poly3Points   = cHull3Points ch
                       , poly3LineSegs = cHull3LineSegs ch
                       , poly3Faces    = cHull3Triangles ch
                       , poly3Box      = box3FromPoints pts
                       }
      _           -> Nothing
    where
      p1Pts    = filter (\p -> point3InPolyhedron p p2) (poly3Points p1)
      p2Pts    = filter (\p -> point3InPolyhedron p p1) (poly3Points p2)
      
      p1IntPts = catMaybes [ lineSeg3TriangleIntersection ls t | ls <- poly3LineSegs p1, t <- poly3Faces p2 ]
      p2IntPts = catMaybes [ lineSeg3TriangleIntersection ls t | ls <- poly3LineSegs p2, t <- poly3Faces p1 ]
    
      pts      = p1Pts ++ p2Pts ++ p1IntPts ++ p2IntPts  
      ch       = convexHull3 pts
  
{-
This appears in the math library; however, it's been "translated" so that it
does not require all the classes, etc., that the version in the math library
requires.
-}      
      
data ConvexHull3 a
  = ConvexHull3
    {
      cHull3Center    :: Point3 a
    , cHull3Points    :: [Point3 a]
    , cHull3LineSegs  :: [LineSeg3 a]
    , cHull3Triangles :: [Triangle3 a]
    }
      
data Face a 
  = Face
    {
      faceTriangle       :: Triangle3 a
    , faceExteriorPoints :: [Point3 a]
    } deriving Eq
    
data FacesState a
  = FacesState 
    { 
      activeFacesState   :: [Face a]
    , finishedFacesState :: [Face a]
    , centerPointState   :: Point3 a 
    }
    
-- Yech, extracting points from triangles is probably horribly inefficient.
-- Heh, extracting line segments is even worse.
-- Let's hope the number of points remains under four digits.
-- Face normals are pointing "out" away from the "center" of the convex hull.
convexHull3 :: (Eq a, Ord a, Tol a, Floating a) => [Point3 a] -> ConvexHull3 a
convexHull3 points
  = ConvexHull3 center hullPoints hullLineSegs hullTriangles
    where
      (center, faces)              = convexHullMakeTetrahedron points
      (activeFaces, finishedFaces) = partition (not . null . faceExteriorPoints) faces
      workedFaces                  = workOnFaces (FacesState activeFaces finishedFaces center) 0
      hullTriangles                = map faceTriangle workedFaces
      
      hullPoints                   = nub $ concatMap tri3Points hullTriangles
      
      pointPairsEqual              = \(a, b) p -> ((a, b) == p) || ((b, a) == p)
      pointPairs                   = concatMap ((\(p1:p2:p3:_) -> [(p1, p2), (p2, p3), (p3, p1)]) . tri3Points) hullTriangles
      hullLineSegs                 = [ lineSeg3 p1 p2 | (p1, p2) <- nubBy pointPairsEqual pointPairs ]
      
{-
This is actually pretty important.  

When the external points are assigned to a face, those external points are then
removed from the list of the rest of the external points.  This has the effect
of not allowing a point to be external to more than one face, i.e., points are
not repeated.
-}      
makeFaces :: (Ord a, Floating a, Tol a) => [(Point3 a, Point3 a, Point3 a)] -> Point3 a -> [Point3 a] -> [Face a]
makeFaces ((p1, p2, p3):restPoints) internalPoint extPoints
  = f:(makeFaces restPoints internalPoint (extPoints \\ s))
    where
      t     = triangle3WithBelow p1 p2 p3 internalPoint
      plane = tri3Plane t
      s     = filter (\p -> point3AbovePlane p plane) extPoints
      f     = Face t s
makeFaces _ _ _ = []
    
{-
Gotta start somewhere.  From the given point list, take four points and make a
tetrahedron out of it (which is necessarily convex!).  If the four points 
happen to be coplanar, shift the list (better way?) and try again.

This function used to be able to infinitely loop.  Now it can't due to a hack.
-}
convexHullMakeTetrahedron :: forall a. (Ord a, Tol a, Floating a) => [Point3 a] -> (Point3 a, [Face a])
convexHullMakeTetrahedron points
  | not atLeast4Points        = error "convexHullMakeTetrahedron:  not at least 4 non-coplanar points."
  | notAllPlanar && notLinear = (center, faces)
  | otherwise                 = convexHullMakeTetrahedron $ (tail points) ++ [head points]
    where
      -- Yech.
      atLeast4Points   = case points of
                           (a:b:c:d:_) -> let tempPlane = plane3 a b c
                                          in not $ null $ filter (\p -> not $ point3OnPlane p tempPlane) (drop 3 points)
                           _           -> False
    
      (p1:p2:p3:pss)   = points
      plane            = plane3 p1 p2 p3
      
      -- *Complete* hack but needed in edge cases.
      (p4, ps)         = case (filter (\p -> not $ point3OnPlane p plane) pss) of
                           (q:_) -> (q, delete q pss)
                           _     -> (head pss, tail pss)

      line             = line3FromPoints p1 p2
      notAllPlanar     = not $ point3OnPlane p4 plane
      notLinear        = not $ point3OnLine p3 line
      
      center           = scalarVector3 (0.25::a) (p1 + p2 + p3 + p4)
      faces            = makeFaces [(p1, p2, p3), (p1, p2, p4), (p2, p3, p4), (p3, p1, p4)] center ps

{-
4.  Repeat:
    4a.  Pick a triangular face that has external points.
    4b.  Pick the furthest point from the triagular face.
    4c.  Find all faces the furthest point can see.
    4d.  Remove the seen faces.
    4e.  Connect the furthest point with hole edges.
    4f.  Add all external points from seen faces and apply them to new faces.
    4g.  If at least one face has external points, repeat.
-}

workOnFaces :: (Ord a, Floating a, Tol a) => FacesState a -> Int -> [Face a]
workOnFaces (FacesState activeFaces finishedFaces centerPoint) i
  | null activeFaces = finishedFaces -- trace ("Faces:\n" ++ (myShowList $ map (tri3Points . faceTriangle) finishedFaces) ++ "\n\n") $ finishedFaces
  | otherwise        = workOnFaces (FacesState newActiveFaceList newFinishedFaceList centerPoint) (i + 1)
    where
      -- Pick out the face we want to work on.
      activeFace                = head activeFaces
      activeTriangle            = faceTriangle activeFace
      externalPoints            = faceExteriorPoints activeFace

      -- Find the furthest external point from the face upon which we're acting.
      extPoint                  = snd $ maximum [(point3PlaneDistance p (tri3Plane activeTriangle), p) | p <- externalPoints]

      -- Find all faces the external point can see.
      seenFaces                 = filter (\f -> point3AbovePlane extPoint (tri3Plane $ faceTriangle f)) (activeFaces ++ finishedFaces)

      -- Find all external points to all seen faces.
      allSeenFacesExtPts        = (concatMap faceExteriorPoints seenFaces) \\ [extPoint]

      -- Find segments of the hole left by removing the seen faces.
      seenFacesSegs             = concatMap (auxList . tri3Points . faceTriangle) seenFaces
      holeSegs                  = [ seg | seg <- seenFacesSegs, (segCount seg seenFacesSegs) == 1 ]

      -- Make the new faces using the hole segments and the exterior point.
      newFaces                  = makeFaces [ (p1, p2, extPoint) | (p1, p2) <- holeSegs ] centerPoint allSeenFacesExtPts
      (newActives, newFinished) = partition (not . null . faceExteriorPoints) newFaces

      newActiveFaceList         = (activeFaces \\ seenFaces) ++ newActives
      newFinishedFaceList       = (finishedFaces \\ seenFaces) ++ newFinished
  
-- Yech.  Horrible.  Re-write using fliter.
segCount :: Eq a => (Point3 a, Point3 a) -> [(Point3 a, Point3 a)] -> Int
segCount (a, b) segList = length $ [ undefined | (c, d) <- segList, (((a == c) && (b == d)) || ((a == d) && (b == c))) ]

auxList :: [a] -> [(a, a)]
auxList list = zip list ((tail list) ++ [head list])

-- Yes yes yes, there's a strict fold somewhere.
strictSum :: Num a => [a] -> a
strictSum (x:xs) = auxStrictSum xs x
strictSum []     = 0

auxStrictSum :: Num a => [a] -> a -> a
auxStrictSum (x:xs) s = auxStrictSum xs !$ x + s
auxStrictSum _      s = s


{-
-- These functions were used for testing, etc.  

-- Actual volume should be 4.188790.
-- When j = 1000, 4.128800
-- When j = 5000, 4.176502 (run time ~10m)
vTest :: IO ()
vTest
  = do
      let j    = 5000
      pts      <- replicateM j vPoint
      let hull = construct $ map point3ToTuple $ cHull3Points $ convexHull3 pts
      putStrLn $ show $ volume hull
      
      return ()
      
twoPi :: Double
twoPi = 2 * pi
      
vPoint :: IO (Point3 Double)
vPoint
  = do
      theta <- randomRIO (0, twoPi)
      phi   <- randomRIO (0, twoPi)
      
      let sinphi   = sin phi
          cosphi   = cos phi
          sintheta = sin theta
          costheta = cos theta
      
      return $ Point3 (sinphi * costheta) (sinphi * sintheta) cosphi

iTest :: IO ()
iTest
  = do
      let i               = 100
          j               = 5000
          (xr1, yr1, zr1) = ((0, 1), (0, 1), (0, 1))
          (xr2, yr2, zr2) = ((0, 1), (0, 1), (0, 1))
          (xr3, yr3, zr3) = (intersectRanges xr1 xr2, intersectRanges yr1 yr2, intersectRanges zr1 zr2)
          (xrt, yrt, zrt) = (unionRanges xr1 xr2, unionRanges yr1 yr2, unionRanges zr1 zr2)
          
      pts1 <- cHullPts xr1 yr1 zr1 i
      pts2 <- cHullPts xr2 yr2 zr2 i
      
      let hull1 = construct pts1
          hull2 = construct pts2
          hull3 = intersect hull1 hull2
          
      putStrLn $ "Hull 1 volume:  " ++ (show $ volume hull1)
      putStrLn $ "Hull 2 volume:  " ++ (show $ volume hull2)
      putStrLn $ "Hull 3 volume:  " ++ (show $ volume hull3)
      
          
      (inIntNotInParts, inPartsNotInInt, inIntCount, inPartsCount) <- randomlyTest xrt yrt zrt hull1 hull2 hull3 j 0 0 0 0
      
      putStrLn $ "In intersection but not in parts:  " ++ (show inIntNotInParts)
      putStrLn $ "In parts but not in intersection:  " ++ (show inPartsNotInInt)
      putStrLn $ "Total in intersection:  " ++ (show inIntCount)
      putStrLn $ "Total in parts (both):  " ++ (show inPartsCount)
          
      return ()
      
intersectRanges :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a)
intersectRanges (a, b) (c, d)
  | not $ (b < c) || (a > d) = (max a c, min b d)
  | otherwise                = (0, 0)
  
unionRanges :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a)
unionRanges (a, b) (c, d)
  | not $ (b < c) || (a > d) = (min a c, max b d)
  | otherwise                = (0, 0)
       
cHullPts :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Int -> IO [(Double, Double, Double)] 
cHullPts xr yr zr i
  = replicateM i $ cHullPt xr yr zr
             
cHullPt :: (Double, Double) -> (Double, Double) -> (Double, Double) -> IO (Double, Double, Double)
cHullPt xr yr zr
  = do
      x <- randomRIO xr
      y <- randomRIO yr
      z <- randomRIO zr
      
      return (x, y, z)
      
randomlyTest :: (Double, Double) -> (Double, Double) -> (Double, Double) -> 
                Polyhedron Double -> Polyhedron Double -> Polyhedron Double -> 
                Int -> Int -> Int -> Int -> Int -> IO (Int, Int, Int, Int)
randomlyTest xr yr zr poly1 poly2 polyInt iterations inIntNotInParts inPartsNotInInt inIntCount inPartsCount
  = do
      x <- randomRIO xr
      y <- randomRIO yr
      z <- randomRIO zr
      
      let p       = Point3 x y z
          inInt   = point3InPolyhedron p polyInt
          inParts = (point3InPolyhedron p poly1) && (point3InPolyhedron p poly2)
          
      if (iterations == 0)
        then return (inIntNotInParts, inPartsNotInInt, inIntCount, inPartsCount)
        else randomlyTest xr yr zr poly1 poly2 polyInt !$ 
                          (iterations - 1)  !$
                          (if (inInt && (not inParts)) then (inIntNotInParts + 1) else inIntNotInParts) !$
                          (if (inParts && (not inInt)) then (inPartsNotInInt + 1) else inPartsNotInInt) !$
                          (if inInt then (inIntCount + 1) else inIntCount) !$
                          (if inParts then (inPartsCount + 1) else inPartsCount)

poly3PolyIntersectDEBUG :: (Ord a, Floating a, Tol a) => Polyhedron a -> Polyhedron a -> Maybe (Polyhedron a)
poly3PolyIntersectDEBUG p1 p2
  = case pts of
      (a:b:c:d:_) -> trace ( --"\npts:  " ++ (myShowList $ map show pts) ++
                            "\np1Pts   :  " ++ (show $ length p1Pts) ++
                            "\np1IntPts:  " ++ (show $ length p1IntPts) ++ 
                            "\np2Pts   :  " ++ (show $ length p2Pts) ++
                            "\np2IntPts:  " ++ (show $ length p2IntPts) ++ 
                            "\ncard pts:  " ++ (show $ length pts) ++ 
                            "\nhull Pts:  " ++ (show $ length $ cHull3Points ch) ++ "\n\n") $
                     Just $ 
                       Polyhedron
                       {
                         poly3Center   = cHull3Center ch
                       , poly3Points   = cHull3Points ch
                       , poly3LineSegs = cHull3LineSegs ch
                       , poly3Faces    = cHull3Triangles ch -- These are Triangle3s
                       , poly3Box      = box3FromPoints pts
                       }
      _           -> trace ("\np1Pts   :  " ++ (show $ length p1Pts) ++
                            "\np1IntPts:  " ++ (show $ length p1IntPts) ++ 
                            "\np2Pts   :  " ++ (show $ length p2Pts) ++
                            "\np2IntPts:  " ++ (show $ length p2IntPts) ++ 
                            "\ncard pts:  " ++ (show $ length pts) ++ "\n\n") $
                     Nothing
    where
      p1Pts    = filter (\p -> point3InPolyhedron p p2) (poly3Points p1)
      p2Pts    = filter (\p -> point3InPolyhedron p p1) (poly3Points p2)
      
      -- Holy cow!  Major nastiness!
      p1IntPts = map fromJust $ filter isJust $ [ lineSeg3TriangleIntersection ls t | ls <- poly3LineSegs p1, t <- poly3Faces p2 ]
      p2IntPts = map fromJust $ filter isJust $ [ lineSeg3TriangleIntersection ls t | ls <- poly3LineSegs p2, t <- poly3Faces p1 ]
    
      pts      = p1Pts ++ p2Pts ++ p1IntPts ++ p2IntPts  
      ch       = convexHull3 pts
      
myShowList :: Show a => [a] -> String
myShowList [] = "[]"
myShowList xs = "[ " ++ (tail $ tail $ tail $ concat [ "\n, " ++ (show x) | x <- xs ]) ++ " ]"
-}
