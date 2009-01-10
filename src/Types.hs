module Types where
import Reasoner.Types
import Vocabulary
import Text.XML.HaXml.Xml2Haskell
import Char (isSpace)
import WrappedInts.Types

type AcquisitionID = HypothesisID

type AcquisitionIDs = HypothesisIDs

type NoiseID = HypothesisID

type TrackID = HypothesisID

type AcquisitionMap = HypothesisMap Acquisition

type NoiseMap = HypothesisMap AcquisitionID

type TrackMap = HypothesisMap Track

data Track
    -- | Tracks have an acquisition, own track ID, and possibly next and\/or prior track IDs
    --
    -- Tracks without a next ID are heads, tracks without a prior ID are ends.
    = Track Acquisition TrackID (Maybe TrackID) (Maybe TrackID)
    deriving (Eq)

area :: Acquisition -> Double
area Acquisition { acquisitionWidth = w, acquisitionHeight = h } = w * h

distance :: Acquisition -> Acquisition -> Double
distance Acquisition { acquisitionX = x1, acquisitionY = y1 }
         Acquisition { acquisitionX = x2, acquisitionY = y2 } =
    sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)


{-Type decls-}

newtype Frames = Frames [Frame] 		deriving (Eq,Show)
data Frame = Frame Frame_Attrs [Acquisition]
           deriving (Eq,Show)
data Frame_Attrs = Frame_Attrs
    { frameTime :: Double
    , frameNumber :: Int
    } deriving (Eq,Show)
data Acquisition = Acquisition
    { acquisitionSource :: String
    , acquisitionX :: Double
    , acquisitionY :: Double
    , acquisitionWidth :: Double
    , acquisitionHeight :: Double
    , acquisitionTime :: Double
    } deriving (Eq,Show)

{-Instance decls-}

instance XmlContent Frames where
    fromElem (CElem (Elem "Frames" [] c0):rest) =
        (\(a,ca)->
           (Just (Frames a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Frames a) =
        [CElem (Elem "Frames" [] (concatMap toElem a))]
instance XmlContent Frame where
    fromElem (CElem (Elem "Frame" as c0):rest) =
        (\(a,ca)->
           (Just (Frame (fromAttrs as) a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Frame as a) =
        [CElem (Elem "Frame" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Frame_Attrs where
    fromAttrs as =
        Frame_Attrs
          { frameTime = read $ definiteA fromAttrToStr "Frame" "time" as
          , frameNumber = read $ definiteA fromAttrToStr "Frame" "number" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "time" (show $ frameTime v)
        , toAttrFrStr "number" (show $ frameNumber v)
        ]
instance XmlContent Acquisition where
    fromElem (CElem (Elem "Acquisition" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "Acquisition" (toAttrs as) [])]
instance XmlAttributes Acquisition where
    fromAttrs as =
        Acquisition
          { acquisitionSource = definiteA fromAttrToStr "Acquisition" "source" as
          , acquisitionX = read $ definiteA fromAttrToStr "Acquisition" "x" as
          , acquisitionY = read $ definiteA fromAttrToStr "Acquisition" "y" as
          , acquisitionWidth = read $ definiteA fromAttrToStr "Acquisition" "w" as
          , acquisitionHeight = read $ definiteA fromAttrToStr "Acquisition" "h" as
          , acquisitionTime = read $ definiteA fromAttrToStr "Acquisition" "t" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "source" (acquisitionSource v)
        , toAttrFrStr "x" (show $ acquisitionX v)
        , toAttrFrStr "y" (show $ acquisitionY v)
        , toAttrFrStr "w" (show $ acquisitionWidth v)
        , toAttrFrStr "h" (show $ acquisitionHeight v)
        , toAttrFrStr "t" (show $ acquisitionTime v)
        ]


{-Done-}
