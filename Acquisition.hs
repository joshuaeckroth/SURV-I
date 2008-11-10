module Acquisition where
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.IDSet (fromList)
import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)

type AcquisitionID = HypothesisID

generateAcquisitionHypotheses :: forall s r c.
                                 (Ord r, Show r, Metric c)
                              => [Acquisition]
                              -> [HypothesisID]
                              -> CategoryID
                              -> Mind s r c
                              -> ([AcquisitionID], [HypothesisID], Mind s r c)

generateAcquisitionHypotheses = generateAcquisitionHypotheses' []

generateAcquisitionHypotheses' :: forall s r c.
                                  (Ord r, Show r, Metric c)
                               => [AcquisitionID]
                               -> [Acquisition]
                               -> [HypothesisID]
                               -> CategoryID
                               -> Mind s r c
                               -> ([AcquisitionID], [HypothesisID], Mind s r c)

generateAcquisitionHypotheses' acqIDs []     hs _     mind = (acqIDs, hs, mind)
generateAcquisitionHypotheses' acqIDs (a:as) hs catID mind =
    generateAcquisitionHypotheses' newAcqIDs as newHs catID newMind
        where
          hypID     = 1 + (head hs)
          newHs     = [hypID] ++ hs
          newAcqIDs = acqIDs ++ [hypID]
          newMind   = setFactual (fromList [hypID])
                      (addHypothesis hypID catID (const Medium) mind)

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
    { acquisitionId :: String
    , acquisitionX :: Double
    , acquisitionY :: Double
    , acquisitionWidth :: Double
    , acquisitionHeight :: Double
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
          { acquisitionId = definiteA fromAttrToStr "Acquisition" "id" as
          , acquisitionX = read $ definiteA fromAttrToStr "Acquisition" "x" as
          , acquisitionY = read $ definiteA fromAttrToStr "Acquisition" "y" as
          , acquisitionWidth = read $ definiteA fromAttrToStr "Acquisition" "width" as
          , acquisitionHeight = read $ definiteA fromAttrToStr "Acquisition" "height" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (acquisitionId v)
        , toAttrFrStr "x" (show $ acquisitionX v)
        , toAttrFrStr "y" (show $ acquisitionY v)
        , toAttrFrStr "width" (show $ acquisitionWidth v)
        , toAttrFrStr "height" (show $ acquisitionHeight v)
        ]


{-Done-}
