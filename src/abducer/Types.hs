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
detDist (Detection (Detection_Attrs { detCx = x1, detCy = y1 }))
        (Detection (Detection_Attrs { detCx = x2, detCy = y2 })) =
            sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- | Calculate time span between two detections
detDelta :: Detection -> Detection -> Double
detDelta (Detection (Detection_Attrs { detFrame = f1 }))
         (Detection (Detection_Attrs { detFrame = f2 })) =
             abs $ (frameProp frameTime f1) - (frameProp frameTime f2)

-- | Return a frame property
frameProp :: (Frame_Attrs -> a) -> Frame -> a
frameProp prop (Frame attrs _) = prop attrs

-- | Return a detection property
detProp :: (Detection_Attrs -> a) -> Detection -> a
detProp prop (Detection attrs) = prop attrs

{-Type decls-}

data Frame = Frame Frame_Attrs [Detection]
             deriving (Eq,Show)
data Frame_Attrs = Frame_Attrs
    { frameNumber :: Int
    , frameTime :: Double
    } deriving (Eq,Show)
data Detection = Detection Detection_Attrs
                 deriving (Eq,Show)
data Detection_Attrs = Detection_Attrs
    { detCamera :: String
    , detArea :: Double
    , detCx :: Double
    , detCy :: Double
    , detFrame :: Frame
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
          { frameNumber = read $ definiteA fromAttrToStr "Frame" "number" as
          , frameTime = read $ definiteA fromAttrToStr "Frame" "time" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "number" (show $ frameNumber v)
        , toAttrFrStr "time" (show $ frameTime v)
        ]

instance HTypeable Detection where
    toHType x = Defined "Detection" [] []
instance XmlContent Detection where
    toContents (Detection as) =
        [CElem (Elem "Detection" (toAttrs as) []) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Detection"]
        ; interior e $ return (Detection (fromAttrs as))
        } `adjustErr` ("in <Detection>, "++)
instance XmlAttributes Detection_Attrs where
    fromAttrs as =
        Detection_Attrs
          { detCamera = definiteA fromAttrToStr "Detection" "camera" as
          , detArea = read $ definiteA fromAttrToStr "Detection" "area" as
          , detCx = read $ definiteA fromAttrToStr "Detection" "cx" as
          , detCy = read $ definiteA fromAttrToStr "Detection" "cy" as
          , detFrame = Frame (Frame_Attrs 0 0) []
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "camera" (detCamera v)
        , toAttrFrStr "area" (show $ detArea v)
        , toAttrFrStr "cx" (show $ detCx v)
        , toAttrFrStr "cy" (show $ detCy v)
        ]

{-Done-}
