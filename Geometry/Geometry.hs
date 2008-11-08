module Geometry where
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import qualified Polyhedron as Region

-- Basic types

type Number = Double

data Location = Location
    {
      x :: !Number,
      y :: !Number,
      z :: !Number
    } deriving (Eq, Ord, Show)

data Direction = Direction
    {
      azimuth     :: !Number,
      inclination :: !Number
    } deriving (Eq, Ord, Show)

data SensorPosition = SensorPosition
    {
      sensorLocation :: Location,
      sensorForward  :: Direction,
      sensorUp       :: Direction
    } deriving (Eq, Ord, Show)

type Region = Region.Polyhedron Number

-- The size of the theater, for bounding pyramid constraints from cameras

theaterSize :: Number

theaterSize = 1e6

-- Standard vector functions, adapted from the previous system

-- A Vector [x, y, z] is just like a Location x y z

data Vector a = Vector [a] deriving Show

instance Functor Vector where
    fmap f (Vector v) = Vector (map f v)

vectorMerge :: (Floating a, Ord a) => (a -> a -> a) -> Vector a -> Vector a -> Vector a

vectorMerge f (Vector x) (Vector y) =
    Vector (zipWith f x y)

magnitude :: (Floating a, Ord a) => Vector a -> a

magnitude x =
    sqrt (dotProduct x x)

dotProduct :: (Floating a, Ord a) => Vector a -> Vector a -> a

dotProduct x y =
    let Vector xy = vectorMerge (*) x y
     in sum xy

unitVector :: (Floating a, Ord a) => Vector a -> Maybe (Vector a)

unitVector v =
    case magnitude v of
      0 -> Nothing
      size -> Just (fmap (/ size) v)

angleBetween :: (Floating a, Ord a) => Vector a -> Vector a -> Maybe a

angleBetween v1 v2 =
    do v1u <- unitVector v1
       v2u <- unitVector v2
       return $ acos (dotProduct v1u v2u)

vectorAdd :: (Floating a, Ord a) => Vector a -> Vector a -> Vector a

vectorAdd = vectorMerge (+)

vectorSubtract :: (Floating a, Ord a) => Vector a -> Vector a -> Vector a

vectorSubtract = vectorMerge (-)

crossProduct :: (Floating a, Ord a) => Vector a -> Vector a -> Vector a

crossProduct (Vector [x1, y1, z1]) (Vector [x2, y2, z2]) =
    Vector [y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2]

planeProjection :: (Floating a, Ord a) => Vector a -> Vector a -> Maybe (Vector a)

planeProjection normal vector =
    do unitNormal <- unitVector normal
       return (crossProduct (crossProduct unitNormal vector) unitNormal)

-- Simple conversion functions

locationToVector :: Location -> Vector Number

locationToVector (Location x y z) = Vector [x, y, z]

directionToUnitVector :: Direction -> Vector Number

directionToUnitVector (Direction a i) =
    Vector [sin a * cos i, cos a * cos i, sin i]

vectorPosition :: SensorPosition -> (Vector Number, Vector Number, Vector Number)

vectorPosition position =
    (locationToVector (sensorLocation position),
     directionToUnitVector (sensorForward position),
     directionToUnitVector (sensorUp position))

-- Coordinate conversion between sensor reference frame and absolute

toCartesian :: SensorPosition -> Direction -> Number -> Location

toCartesian position (Direction a i) range =
    let (location, forward, up) = vectorPosition position
        Vector [tx, ty, tz] = foldr1 vectorAdd [location,
                                                fmap (* (range * cos i * sin a)) (crossProduct forward up), -- relative x component
                                                fmap (* (range * cos i * cos a)) forward,                   -- relative y component
                                                fmap (* (range * sin i)) up]                                -- relative z component
    in Location tx ty tz

-- returns Nothing if sensor and target are colocated

toSpherical :: SensorPosition -> Location -> Maybe (Direction, Number)

toSpherical position targetLocation =
    do guard $ magnitude target < theaterSize
       flatTarget <- planeProjection up target
       azimuth <- azimuthOf up forward flatTarget
       angleFromUp <- angleBetween target up
       return (Direction azimuth (pi/2 - angleFromUp), magnitude target)
    where
      (sensorLocation, forward, up) = vectorPosition position
      target = vectorSubtract (locationToVector targetLocation) sensorLocation
      -- find azimuth of vector in plane
      azimuthOf up forward toTarget =
          do sideAngle    <- angleBetween (crossProduct forward up) toTarget
             forwardAngle <- angleBetween forward toTarget
             return $ (if sideAngle > pi / 2 then negate else id) forwardAngle

-- Some geometric sanity-checking; only used by testCartesian

-- These should help to make clear how the coordinate systems are oriented
-- Apologies if it's a bit unconventional!

upC      = Location 0 0 1
forwardC = Location 0 1 0
rightC   = Location 1 0 0

[downC, backwardC, leftC] = [ Location (negate x) (negate y) (negate z) | Location x y z <- [upC, forwardC, rightC] ]

upS      = Direction 0      (pi/2)
forwardS = Direction 0      0
rightS   = Direction (pi/2) 0

[downS, backwardS, leftS] = [ Direction (a + pi) (negate i) | Direction a i <- [upS, forwardS, rightS] ]

-- For each pair of values in the two lists, they should indicate the same location
-- Not used by anything else

testCartesian :: ([(Location, Location)], [((Direction, Number), (Direction, Number))])

testCartesian = 
    unzip $ zipWith expressSimilar cartesianDirections sphericalDirections
    where
      testSensorPosition = SensorPosition (Location 0 0 0) forwardS upS
      cartesianDirections = [upC, downC, forwardC, backwardC, rightC, leftC]
      sphericalDirections = [upS, downS, forwardS, backwardS, rightS, leftS]
      expressSimilar cartesian spherical =
          ((cartesian, toCartesian testSensorPosition spherical 1), 
           ((spherical, 1), (\(Just x) -> x) $ toSpherical testSensorPosition cartesian))

-- Useful simple data type we'll use for numeric intervals

data Range a =
    Range
    {
      lower  :: !a,
      higher :: !a
    } deriving (Eq, Ord, Show)

-- Now, our camera stuff

data Camera = Camera
    {
      xViewAngle :: Number,
      yViewAngle :: Number,
      xDiscrimination :: Number,
      yDiscrimination :: Number
    } deriving (Eq, Ord, Show)

data Sighting = Sighting
    {
      xRange :: Range Number,
      yRange :: Range Number
    } deriving (Eq, Ord, Show)

regionToSighting :: Camera -> SensorPosition -> Region -> Maybe Sighting

regionToSighting camera sensorPosition region =
           -- We find the region's vertices, use toSpherical to turn them into
           -- directions in the camera's reference frame.
        do let (azimuths, inclinations) = 
                   unzip [ (a, i) | (x, y, z) <- Region.vertices region,
                                    (Direction a i, _) <- maybeToList (toSpherical sensorPosition (Location x y z)) ] :: ([Number], [Number])
           -- We make sure that we do actually have some points out of
           -- this. We might not, for instance, if the region and the
           -- camera were colocated.
           guard $ not (null azimuths)
           -- We fit the region into a box aligned with our relative
           -- coordinate system.
           let (smallestAzimuth,     largestAzimuth)     = (minimum azimuths,     maximum azimuths)
           let (smallestInclination, largestInclination) = (minimum inclinations, maximum inclinations)
           -- We turn the region into a fuzzier box
           let (smallestFuzzyAzimuth,     largestFuzzyAzimuth)     = (smallestAzimuth     - xDiscrimination camera,
                                                                      largestAzimuth      + xDiscrimination camera)
           let (smallestFuzzyInclination, largestFuzzyInclination) = (smallestInclination - yDiscrimination camera,
                                                                      largestInclination  + yDiscrimination camera)
           -- We ignore regions that are so small that we can hardly see them
           guard $ largestAzimuth     - smallestAzimuth     >= xDiscrimination camera
           guard $ largestInclination - smallestInclination >= yDiscrimination camera
           -- We ignore regions that do not fully fit on our sensor screen.
           -- This simplification is rather ill-founded and ought not to
           -- persist.
           guard $ abs smallestFuzzyAzimuth     <= xViewAngle camera && abs largestFuzzyAzimuth     <= xViewAngle camera
           guard $ abs smallestFuzzyInclination <= yViewAngle camera && abs largestFuzzyInclination <= yViewAngle camera
           -- We turn the region into a sighting.
           return $ Sighting (Range smallestFuzzyAzimuth     largestFuzzyAzimuth)
                             (Range smallestFuzzyInclination largestFuzzyInclination)

sightingToRegion :: SensorPosition -> Sighting -> Region

sightingToRegion sensorPosition@(SensorPosition (Location sx sy sz) _ _) (Sighting (Range x1 x2) (Range y1 y2)) = 
        Region.construct $ (sx, sy, sz) : 
                           [ (tx, ty, tz) | a <- [x1, x2], i <- [y1, y2],
                                            let Location tx ty tz = toCartesian sensorPosition (Direction a i) theaterSize ]

-- Average not-too-dissimilar directions

averageDirection :: [Direction] -> Direction

averageDirection directions =
    let position = SensorPosition (Location 0 0 0) (Direction 0 0) (Direction 0 (pi/2))
        (xs, ys, zs) = unzip3 [ (x, y, z) | direction <- directions,
                                            let Vector [x, y, z] = directionToUnitVector direction ]
        Just (averageDirection, _) = toSpherical position $ Location (average xs) (average ys) (average zs)
    in averageDirection
    where average xs = sum xs / fromIntegral (length xs)
