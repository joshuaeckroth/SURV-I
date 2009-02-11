module Types where
import Reasoner.Types
import Vocabulary
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Char (isSpace)
import WrappedInts.Types

type DetectionID = HypothesisID

type DetectionIDs = HypothesisIDs

type NoiseID = HypothesisID

type TrackID = HypothesisID

type DetectionMap = HypothesisMap Detection

type NoiseMap = HypothesisMap DetectionID

type TrackMap = HypothesisMap Track

data Track
    -- | Tracks have a detection, own track ID, and possibly next and\/or prior track IDs
    --
    -- Tracks without a next ID are heads, tracks without a prior ID are ends.
    = Track Detection TrackID (Maybe TrackID) (Maybe TrackID)
    deriving (Eq)

-- | Calculate Euclidean distance between two detections
detDist :: Detection -> Detection -> Double
detDist (Detection (Detection_Attrs { detCx = x1, detCy = y1 }) _ _)
        (Detection (Detection_Attrs { detCx = x2, detCy = y2 }) _ _) =
            sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- | Calculate time span between two detections
detDelta :: Detection -> Detection -> Double
detDelta (Detection (Detection_Attrs { detFrame = f1 }) _ _)
         (Detection (Detection_Attrs { detFrame = f2 }) _ _) = abs $
                                                               (frameProp frameTime f1) -
                                                               (frameProp frameTime f2)

-- | Return a frame property
frameProp :: (Frame_Attrs -> a) -> Frame -> a
frameProp prop (Frame attrs _) = prop attrs

-- | Return a detection property
detProp :: (Detection_Attrs -> a) -> Detection -> a
detProp prop (Detection attrs _ _) = prop attrs

-- | Return an epiline property
epiProp :: (Epiline -> a) -> Epiline -> a
epiProp prop e = prop e

-- | Return a contour property
conProp :: (Contour -> a) -> Contour -> a
conProp prop c = prop c

{-Type decls-}

data Frame = Frame Frame_Attrs [Detection]
             deriving (Eq,Show)
data Frame_Attrs = Frame_Attrs
    { frameCamera :: String
    , frameNumber :: Int
    , frameTime :: Double
    } deriving (Eq,Show)
data Detection = Detection Detection_Attrs Epiline [Contour]
                 deriving (Eq,Show)
data Detection_Attrs = Detection_Attrs
    { detArea :: Double
    , detCx :: Double
    , detCy :: Double
    , detFrame :: Frame
    } deriving (Eq,Show)
data Epiline = Epiline
    { epilineTo :: String
    , epilineEa :: String
    , epilineEb :: String
    , epilineEc :: String
    } deriving (Eq,Show)
data Contour = Contour
    { contourX1 :: String
    , contourY1 :: String
    , contourX2 :: String
    , contourY2 :: String
    } deriving (Eq,Show)

{-Instance decls-}

instance HTypeable Frame where
    toHType x = Defined "Frame" [] []
instance XmlContent Frame where
    toContents (Frame as a) =
        [CElem (Elem "Frame" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Frame"]
        ; interior e $ return (Frame (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <Frame>, "++)
instance XmlAttributes Frame_Attrs where
    fromAttrs as =
        Frame_Attrs
          { frameCamera = definiteA fromAttrToStr "Frame" "camera" as
          , frameNumber = read $ definiteA fromAttrToStr "Frame" "number" as
          , frameTime = read $ definiteA fromAttrToStr "Frame" "time" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "camera" (frameCamera v)
        , toAttrFrStr "number" (show $ frameNumber v)
        , toAttrFrStr "time" (show $ frameTime v)
        ]

instance HTypeable Detection where
    toHType x = Defined "Detection" [] []
instance XmlContent Detection where
    toContents (Detection as a b) =
        [CElem (Elem "Detection" (toAttrs as) (toContents a ++
                                               concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Detection"]
        ; interior e $ return (Detection (fromAttrs as))
                       `apply` parseContents `apply` many parseContents
        } `adjustErr` ("in <Detection>, "++)
instance XmlAttributes Detection_Attrs where
    fromAttrs as =
        Detection_Attrs
          { detArea = read $ definiteA fromAttrToStr "Detection" "area" as
          , detCx = read $ definiteA fromAttrToStr "Detection" "cx" as
          , detCy = read $ definiteA fromAttrToStr "Detection" "cy" as
          , detFrame = Frame (Frame_Attrs "" 0 0) []
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "area" (show $ detArea v)
        , toAttrFrStr "cx" (show $ detCx v)
        , toAttrFrStr "cy" (show $ detCy v)
        ]

instance HTypeable Epiline where
    toHType x = Defined "Epiline" [] []
instance XmlContent Epiline where
    toContents as =
        [CElem (Elem "Epiline" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Epiline"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Epiline>, "++)
instance XmlAttributes Epiline where
    fromAttrs as =
        Epiline
          { epilineTo = definiteA fromAttrToStr "Epiline" "to" as
          , epilineEa = definiteA fromAttrToStr "Epiline" "ea" as
          , epilineEb = definiteA fromAttrToStr "Epiline" "eb" as
          , epilineEc = definiteA fromAttrToStr "Epiline" "ec" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "to" (epilineTo v)
        , toAttrFrStr "ea" (epilineEa v)
        , toAttrFrStr "eb" (epilineEb v)
        , toAttrFrStr "ec" (epilineEc v)
        ]

instance HTypeable Contour where
    toHType x = Defined "Contour" [] []
instance XmlContent Contour where
    toContents as =
        [CElem (Elem "Contour" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Contour"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Contour>, "++)
instance XmlAttributes Contour where
    fromAttrs as =
        Contour
          { contourX1 = definiteA fromAttrToStr "Contour" "x1" as
          , contourY1 = definiteA fromAttrToStr "Contour" "y1" as
          , contourX2 = definiteA fromAttrToStr "Contour" "x2" as
          , contourY2 = definiteA fromAttrToStr "Contour" "y2" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "x1" (contourX1 v)
        , toAttrFrStr "y1" (contourY1 v)
        , toAttrFrStr "x2" (contourX2 v)
        , toAttrFrStr "y2" (contourY2 v)
        ]



{-Done-}
