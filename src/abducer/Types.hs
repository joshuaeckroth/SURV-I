{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module Types where
import Vocabulary
import Text.XML.HaXml.XmlContent                          
import Text.XML.HaXml.OneOfN                              
import Text.XML.HaXml.Types
import WrappedInts.Types
import Reasoner.Types (CategoryID(..), HypothesisID(..), ExplainsID(..))
import Data.Dynamic
import Data.HashTable (hashString)
import Data.Maybe

type Entity = Dynamic

category = HasInt 0 :: CategoryID

data Hypothesis a =
    Hyp
    { entity    :: a
    , hypId     :: HypothesisID
    , aPriori   :: Level -> Level
    , explains  :: [HypothesisID]
    , implies   :: [HypothesisID]
    , conflicts :: [HypothesisID]
    }

type Time = Double
type Latitude = Double
type Longitude = Double

{- hashes camera detections -}
mkDetHypId :: CameraDetection -> HypothesisID
mkDetHypId (CameraDetection camera lat lon _ startTime endTime) =
    fromIntegral $ hashString (camera ++ (show lat) ++ (show lon)
                                          ++ (show startTime) ++ (show endTime))

{- hashes list of detections -}
mkMovHypId :: [Detection] -> HypothesisID
mkMovHypId dets =
    fromIntegral $ hashString $
                     (foldl1 (++) $
                             map (\(Detection id lat lon startTime endTime area _) ->
                                  (show id) ++ (show lat) ++ (show lon)
                                                ++ (show startTime) ++ (show endTime) ++ (show area)) dets)

{- hashes the hashes of constituent movements of a path -}
mkPathHypId :: [Movement] -> HypothesisID
mkPathHypId movs = fromIntegral $ hashString $
                   foldl1 (++) $ map (\(Movement _ (NonEmpty dets)) -> show $ mkMovHypId dets) movs

mkHypPairId :: (Num a) => HypothesisID -> HypothesisID -> a
mkHypPairId subject object = fromIntegral $ hashString $ (show subject) ++ (show object)

-- | Calculate Euclidean distance between two camera detections
cameraDetDist :: CameraDetection -> CameraDetection -> Double
cameraDetDist (CameraDetection { cameraDetectionLat = x1, cameraDetectionLon = y1 })
                  (CameraDetection { cameraDetectionLat = x2, cameraDetectionLon = y2 }) =
                      sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- | Calculate Euclidean distance between two detections
detDist :: Detection -> Detection -> Double
detDist (Detection { detectionLat = x1, detectionLon = y1 })
        (Detection { detectionLat = x2, detectionLon = y2 }) =
            sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- | Calculate time span between two detections
detDelta :: Detection -> Detection -> Time
detDelta (Detection { detectionStartTime = start1
                    , detectionEndTime   = end1 })
         (Detection { detectionStartTime = start2
                    , detectionEndTime   = end2 }) =
         start2 - end1

detAscOrdering :: Detection -> Detection -> Ordering
detAscOrdering det1 det2
    | (avgDetTime det1) <  (avgDetTime det2) = LT
    | (avgDetTime det1) == (avgDetTime det2) = EQ
    | (avgDetTime det1) >  (avgDetTime det2) = GT
    where
      avgDetTime det = ((detectionStartTime det) + (detectionStartTime det)) / 2.0

-- | A detection is "before" another detection if its most likely occurrence time
--   (mean of start and end times) is before the other's most likely occurrence time.
--
-- Note that we are requiring that the two detections be at least 0.5 seconds apart,
-- to discount two detections in the same camera view that must be two different objects
-- because they were detected at the same moment.
detBefore :: Detection -> Detection -> Bool
detBefore (Detection { detectionStartTime = start1
                     , detectionEndTime   = end1 })
          (Detection { detectionStartTime = start2
                     , detectionEndTime   = end2 }) =
          ((start1 + end1) / 2.0) < (0.5 + (start2 + end2) / 2.0)

-- | Gather a list of detections that have movements explaining them.
detsExplained :: [Movement] -> [Detection]
detsExplained [] = []
detsExplained ((Movement _ (NonEmpty dets)):movs) = dets ++ (detsExplained movs)

extractEntity :: Hypothesis a -> a
extractEntity (Hyp {entity = e}) = e

extractEntities :: [Hypothesis a] -> [a]
extractEntities = map extractEntity

{-- Camera Detections Types --}

{-Type decls-}

data CameraDetections = CameraDetections [CameraDetection]
                      deriving (Eq,Show)
data CameraDetection = CameraDetection
    { cameraDetectionCamera :: String
    , cameraDetectionLat :: Latitude
    , cameraDetectionLon :: Longitude
    , cameraDetectionArea :: Double
    , cameraDetectionStartTime :: Time
    , cameraDetectionEndTime :: Time
    } deriving (Eq,Show)


{-Instance decls-}

instance HTypeable CameraDetections where
    toHType x = Defined "CameraDetections" [] []
instance XmlContent CameraDetections where
    toContents (CameraDetections a) =
        [CElem (Elem "CameraDetections" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["CameraDetections"]
        ; interior e $ return (CameraDetections) `apply` many parseContents
        } `adjustErr` ("in <CameraDetections>, "++)


instance HTypeable CameraDetection where
    toHType x = Defined "CameraDetection" [] []
instance XmlContent CameraDetection where
    toContents as =
        [CElem (Elem "CameraDetection" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["CameraDetection"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <CameraDetection>, "++)
instance XmlAttributes CameraDetection where
    fromAttrs as =
        CameraDetection
          { cameraDetectionCamera = definiteA fromAttrToStr "CameraDetection" "camera" as
          , cameraDetectionLat = read $ definiteA fromAttrToStr "CameraDetection" "lat" as
          , cameraDetectionLon = read $ definiteA fromAttrToStr "CameraDetection" "lon" as
          , cameraDetectionArea = read $ definiteA fromAttrToStr "CameraDetection" "area" as
          , cameraDetectionStartTime = read $ definiteA fromAttrToStr "CameraDetection" "startTime" as
          , cameraDetectionEndTime = read $ definiteA fromAttrToStr "CameraDetection" "endTime" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "camera" (show $ cameraDetectionCamera v)
        , toAttrFrStr "lat" (show $ cameraDetectionLat v)
        , toAttrFrStr "lon" (show $ cameraDetectionLon v)
        , toAttrFrStr "area" (show $ cameraDetectionArea v)
        , toAttrFrStr "startTime" (show $ cameraDetectionStartTime v)
        , toAttrFrStr "endTime" (show $ cameraDetectionEndTime v)
        ]



{-Done-}


{-- Results Types --}

{-Type decls-}

data Results = Results Accepted Rejected
               deriving (Eq,Show)
data Accepted = Accepted [Detection] [Movement] [Path]
                deriving (Eq,Show)
data Rejected = Rejected [Detection] [Movement] [Path]
                deriving (Eq,Show)
data Detection = Detection
    { detectionId :: HypothesisID
    , detectionLat :: Latitude
    , detectionLon :: Longitude
    , detectionStartTime :: Time
    , detectionEndTime :: Time
    , detectionArea :: Double
    , detectionCamera :: Maybe CameraDetection
    } deriving (Eq,Show,Typeable)
data Movement = Movement Movement_Attrs (List1 Detection)
              deriving (Eq,Show,Typeable)
data Movement_Attrs = Movement_Attrs
    { movementId :: HypothesisID
    } deriving (Eq,Show)
data Path = Path Path_Attrs (List1 Movement)
          deriving (Eq,Show,Typeable)
data Path_Attrs = Path_Attrs
    { pathId :: HypothesisID
    } deriving (Eq,Show)


{-Instance decls-}


instance HTypeable Results where
    toHType x = Defined "Results" [] []
instance XmlContent Results where
    toContents (Results a b) =
        [CElem (Elem "Results" [] (toContents a ++ toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Results"]
        ; interior e $ return (Results) `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <Results>, "++)

instance HTypeable Accepted where
    toHType x = Defined "Accepted" [] []
instance XmlContent Accepted where
    toContents (Accepted a b c) =
        [CElem (Elem "Accepted" [] (concatMap toContents a ++
                                    concatMap toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Accepted"]
        ; interior e $ return (Accepted) `apply` many parseContents
                       `apply` many parseContents `apply` many parseContents
        } `adjustErr` ("in <Accepted>, "++)

instance HTypeable Rejected where
    toHType x = Defined "Rejected" [] []
instance XmlContent Rejected where
    toContents (Rejected a b c) =
        [CElem (Elem "Rejected" [] (concatMap toContents a ++
                                    concatMap toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Rejected"]
        ; interior e $ return (Rejected) `apply` many parseContents
                       `apply` many parseContents `apply` many parseContents
        } `adjustErr` ("in <Rejected>, "++)

instance HTypeable Detection where
    toHType x = Defined "Detection" [] []
instance XmlContent Detection where
    toContents as =
        [CElem (Elem "Detection" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Detection"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Detection>, "++)
instance XmlAttributes Detection where
    fromAttrs as =
        Detection
          { detectionId = HasInt $ read $ definiteA fromAttrToStr "Detection" "id" as
          , detectionLat = read $ definiteA fromAttrToStr "Detection" "lat" as
          , detectionLon = read $ definiteA fromAttrToStr "Detection" "lon" as
          , detectionStartTime = read $ definiteA fromAttrToStr "Detection" "startTime" as
          , detectionEndTime = read $ definiteA fromAttrToStr "Detection" "endTime" as
          , detectionArea = read $ definiteA fromAttrToStr "Detection" "area" as
          , detectionCamera = Nothing
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (show $ detectionId v)
        , toAttrFrStr "lat" (show $ detectionLat v)
        , toAttrFrStr "lon" (show $ detectionLon v)
        , toAttrFrStr "startTime" (show $ detectionStartTime v)
        , toAttrFrStr "endTime" (show $ detectionEndTime v)
        , toAttrFrStr "area" (show $ detectionArea v)
        ]

instance HTypeable Movement where
    toHType x = Defined "Movement" [] []
instance XmlContent Movement where
    toContents (Movement as a) =
        [CElem (Elem "Movement" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Movement"]
        ; interior e $ return (Movement (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <Movement>, "++)
instance XmlAttributes Movement_Attrs where
    fromAttrs as =
        Movement_Attrs
          { movementId = HasInt $ read $ definiteA fromAttrToStr "Movement" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (show $ movementId v)
        ]

instance HTypeable Path where
    toHType x = Defined "Path" [] []
instance XmlContent Path where
    toContents (Path as a) =
        [CElem (Elem "Path" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Path"]
        ; interior e $ return (Path (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <Path>, "++)
instance XmlAttributes Path_Attrs where
    fromAttrs as =
        Path_Attrs
          { pathId = HasInt $ read $ definiteA fromAttrToStr "Path" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (show $ pathId v)
        ]



{-Done-}

