{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module Types where
import Vocabulary
import Text.XML.HaXml.XmlContent                          
import Text.XML.HaXml.OneOfN                              
import Text.XML.HaXml.Types
import WrappedInts.Types
import Reasoner.Types (CategoryID(..), HypothesisID(..), ExplainsID(..))
import Data.Dynamic
import Data.Maybe
import Data.HashTable (hashString)
import Debug.Trace

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

instance Eq (Hypothesis a) where
    (Hyp {hypId = hypId1}) == (Hyp {hypId = hypId2}) = hypId1 == hypId2

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
mkMovHypId dets = fromIntegral $ hashString $ foldl1 (++) (map (show . detectionId) dets)

{- hashes the hashes of constituent movements of a path -}
mkPathHypId :: [Movement] -> HypothesisID
mkPathHypId movs = fromIntegral $ hashString $ foldl1 (++) $ map (show . movementId) movs

mkExplainsId :: (Num a) => HypothesisID -> HypothesisID -> a
mkExplainsId subject object = fromIntegral $ hashString $ "explains" ++ (show subject) ++ (show object)

mkImpliesId :: (Num a) => HypothesisID -> HypothesisID -> a
mkImpliesId subject object = fromIntegral $ hashString $ "implies" ++ (show subject) ++ (show object)

mkConflictsId :: (Num a) => HypothesisID -> HypothesisID -> a
mkConflictsId subject object = fromIntegral $ hashString $ "conflicts" ++ (show subject) ++ (show object)

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

detAreaDiff :: Detection -> Detection -> Double
detAreaDiff det1 det2 = abs $ (detectionArea det1) - (detectionArea det2)

detAreaChange :: Detection -> Detection -> Double
detAreaChange det1 det2 = let detArea1 = detectionArea det1
                              detArea2 = detectionArea det2
                          in (abs $ detArea1 - detArea2) / (max detArea1 detArea2)

detSpeed :: Detection -> Detection -> Double
detSpeed det1 det2 = (detDist det1 det2) / (abs $ detDelta det1 det2)

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

extractDetHypId :: Detection -> HypothesisID
extractDetHypId (Detection {detectionId = hypId}) = hypId

mkDetectionRef :: HypothesisID -> DetectionRef
mkDetectionRef hypId = DetectionRef hypId

extractMovHypId :: Movement -> HypothesisID
extractMovHypId (Movement hypId _ _ _) = hypId

mkMovementRef :: HypothesisID -> MovementRef
mkMovementRef hypId = MovementRef hypId

extractPathHypId :: Path -> HypothesisID
extractPathHypId (Path (Path_Attrs hypId _ _) _) = hypId

mkPathRef :: HypothesisID -> PathRef
mkPathRef hypId = PathRef hypId

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

data Results  = Results Entities Accepted Rejected
                deriving (Eq,Show)
data Entities = Entities [Detection] [Movement] [Path]
                deriving (Eq,Show)
data Accepted = Accepted [DetectionRef] [MovementRef] [PathRef]
                deriving (Eq,Show)
data Rejected = Rejected [DetectionRef] [MovementRef] [PathRef]
                deriving (Eq,Show)
data Detection = Detection
    { detectionId        :: HypothesisID
    , detectionLat       :: Latitude
    , detectionLon       :: Longitude
    , detectionStartTime :: Time
    , detectionEndTime   :: Time
    , detectionArea      :: Double
    , detectionScore     :: String
    , detectionCamera    :: Maybe CameraDetection
    } deriving (Eq,Show,Typeable)
data DetectionRef = DetectionRef
    { detectionRefDetId :: HypothesisID
    } deriving (Eq,Show)
data Movement = Movement
    { movementId     :: HypothesisID
    , movementDetId1 :: HypothesisID
    , movementDetId2 :: HypothesisID
    , movementScore  :: String
    } deriving (Eq,Show,Typeable)
data MovementRef = MovementRef
    { movementRefMovId :: HypothesisID
    } deriving (Eq,Show)
data Path = Path Path_Attrs (List1 MovementRef)
          deriving (Eq,Show,Typeable)
data Path_Attrs = Path_Attrs
    { pathId    :: HypothesisID
    , pathScore :: String
    , pathConflicts :: String
    } deriving (Eq,Show)
data PathRef = PathRef
    { pathRefPathId :: HypothesisID
    } deriving (Eq,Show)


{-Instance decls-}


instance HTypeable Results where
    toHType x = Defined "Results" [] []
instance XmlContent Results where
    toContents (Results a b c) =
        [CElem (Elem "Results" [] (toContents a ++ toContents b ++
                                   toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Results"]
        ; interior e $ return (Results) `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <Results>, "++)

instance HTypeable Entities where
    toHType x = Defined "Entities" [] []
instance XmlContent Entities where
    toContents (Entities a b c) =
        [CElem (Elem "Entities" [] (concatMap toContents a ++
                                    concatMap toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Entities"]
        ; interior e $ return (Entities) `apply` many parseContents
                       `apply` many parseContents `apply` many parseContents
        } `adjustErr` ("in <Entities>, "++)

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
          , detectionScore = definiteA fromAttrToStr "Detection" "score" as
          , detectionCamera = Nothing
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (show $ detectionId v)
        , toAttrFrStr "lat" (show $ detectionLat v)
        , toAttrFrStr "lon" (show $ detectionLon v)
        , toAttrFrStr "startTime" (show $ detectionStartTime v)
        , toAttrFrStr "endTime" (show $ detectionEndTime v)
        , toAttrFrStr "area" (show $ detectionArea v)
        , toAttrFrStr "score" (detectionScore v)
        ]

instance HTypeable DetectionRef where
    toHType x = Defined "DetectionRef" [] []
instance XmlContent DetectionRef where
    toContents as =
        [CElem (Elem "DetectionRef" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["DetectionRef"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <DetectionRef>, "++)
instance XmlAttributes DetectionRef where
    fromAttrs as =
        DetectionRef
          { detectionRefDetId = HasInt $ read $ definiteA fromAttrToStr "DetectionRef" "detId" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "detId" (show $ detectionRefDetId v)
        ]

instance HTypeable Movement where
    toHType x = Defined "Movement" [] []
instance XmlContent Movement where
    toContents as =
        [CElem (Elem "Movement" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Movement"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Movement>, "++)
instance XmlAttributes Movement where
    fromAttrs as =
        Movement
          { movementId = HasInt $ read $ definiteA fromAttrToStr "Movement" "id" as
          , movementDetId1 = HasInt $ read $ definiteA fromAttrToStr "Movement" "detId1" as
          , movementDetId2 = HasInt $ read $ definiteA fromAttrToStr "Movement" "detId2" as
          , movementScore = definiteA fromAttrToStr "Movement" "score" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (show $ movementId v)
        , toAttrFrStr "detId1" (show $ movementDetId1 v)
        , toAttrFrStr "detId2" (show $ movementDetId2 v)
        , toAttrFrStr "score" (movementScore v)
        ]

instance HTypeable MovementRef where
    toHType x = Defined "MovementRef" [] []
instance XmlContent MovementRef where
    toContents as =
        [CElem (Elem "MovementRef" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["MovementRef"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <MovementRef>, "++)
instance XmlAttributes MovementRef where
    fromAttrs as =
        MovementRef
          { movementRefMovId = HasInt $ read $ definiteA fromAttrToStr "MovementRef" "movId" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "movId" (show $ movementRefMovId v)
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
          , pathScore = definiteA fromAttrToStr "Path" "score" as
          , pathConflicts = definiteA fromAttrToStr "Path" "conflicts" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (show $ pathId v)
        , toAttrFrStr "score" (pathScore v)
        , toAttrFrStr "conflicts" (pathConflicts v)
        ]

instance HTypeable PathRef where
    toHType x = Defined "PathRef" [] []
instance XmlContent PathRef where
    toContents as =
        [CElem (Elem "PathRef" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["PathRef"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <PathRef>, "++)
instance XmlAttributes PathRef where
    fromAttrs as =
        PathRef
          { pathRefPathId = HasInt $ read $ definiteA fromAttrToStr "PathRef" "pathId" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "pathId" (show $ pathRefPathId v)
        ]


{-Done-}

