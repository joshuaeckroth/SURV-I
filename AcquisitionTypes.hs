module AcquisitionTypes where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


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
