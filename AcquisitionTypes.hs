module AcquisitionTypes where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

newtype Frames = Frames [Frame] 		deriving (Eq,Show)
data Frame = Frame Frame_Attrs [Acquisition]
           deriving (Eq,Show)
data Frame_Attrs = Frame_Attrs
    { frameTime :: String
    , frameNumber :: String
    } deriving (Eq,Show)
data Acquisition = Acquisition
    { acquisitionId :: String
    , acquisitionX :: String
    , acquisitionY :: String
    , acquisitionWidth :: String
    , acquisitionHeight :: String
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
          { frameTime = definiteA fromAttrToStr "Frame" "time" as
          , frameNumber = definiteA fromAttrToStr "Frame" "number" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "time" (frameTime v)
        , toAttrFrStr "number" (frameNumber v)
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
          , acquisitionX = definiteA fromAttrToStr "Acquisition" "x" as
          , acquisitionY = definiteA fromAttrToStr "Acquisition" "y" as
          , acquisitionWidth = definiteA fromAttrToStr "Acquisition" "width" as
          , acquisitionHeight = definiteA fromAttrToStr "Acquisition" "height" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (acquisitionId v)
        , toAttrFrStr "x" (acquisitionX v)
        , toAttrFrStr "y" (acquisitionY v)
        , toAttrFrStr "width" (acquisitionWidth v)
        , toAttrFrStr "height" (acquisitionHeight v)
        ]


{-Done-}
