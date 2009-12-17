module Context where

import System.IO
import Types
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN

loadContext :: String -> IO Context
loadContext file = do
  h <- openFile file ReadMode
  hGetXml h

{-Type decls-}

data Context = Context Cameras Map Regions PointsOfInterest AgentTemplates
             deriving (Eq,Show)
data Cameras = Cameras [CameraInput] [FileInput]
             deriving (Eq,Show)
data CameraInput = CameraInput
    { cameraInputName :: String
    , cameraInputStreamId :: String
    , cameraInputWarpFile :: String
    } deriving (Eq,Show)
data FileInput = FileInput
    { fileInputName :: String
    , fileInputFile :: String
    , fileInputWarpFile :: String
    } deriving (Eq,Show)
data Map = Map
    { mapMapFile :: String
    , mapWarpFile :: String
    } deriving (Eq,Show)
newtype Regions = Regions [Region]
    deriving (Eq,Show)
data Region = Region Region_Attrs (List1 RegionPoint)
            deriving (Eq,Show)
data Region_Attrs = Region_Attrs
    { regionName :: String
    } deriving (Eq,Show)
data RegionPoint = RegionPoint
    { regionPointLat :: Latitude
    , regionPointLon :: Longitude
    } deriving (Eq,Show)
newtype PointsOfInterest = PointsOfInterest [PointOfInterest]
    deriving (Eq,Show)
data PointOfInterest = PointOfInterest
    { pointOfInterestName :: String
    , pointOfInterestLat :: Latitude
    , pointOfInterestLon :: Longitude
    , pointOfInterestRange :: Double
    } deriving (Eq,Show)
newtype AgentTemplates = AgentTemplates [AgentTemplate]
    deriving (Eq,Show)
data AgentTemplate = AgentTemplate
    { agentName :: String
    , agentArea :: Double
    , agentSpeed :: Double
    } deriving (Eq,Show)


{-Instance decls-}

instance HTypeable Context where
    toHType x = Defined "Context" [] []
instance XmlContent Context where
    toContents (Context a b c d e) =
        [CElem (Elem "Context" [] (toContents a ++ toContents b ++
                                   toContents c ++ toContents d ++ toContents e)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Context"]
        ; interior e $ return (Context) `apply` parseContents
                       `apply` parseContents `apply` parseContents `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <Context>, "++)

instance HTypeable Cameras where
    toHType x = Defined "Cameras" [] []
instance XmlContent Cameras where
    toContents (Cameras a b) =
        [CElem (Elem "Cameras" [] (concatMap toContents a ++
                                   concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Cameras"]
        ; interior e $ return (Cameras) `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <Cameras>, "++)

instance HTypeable CameraInput where
    toHType x = Defined "CameraInput" [] []
instance XmlContent CameraInput where
    toContents as =
        [CElem (Elem "CameraInput" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["CameraInput"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <CameraInput>, "++)
instance XmlAttributes CameraInput where
    fromAttrs as =
        CameraInput
          { cameraInputName = definiteA fromAttrToStr "CameraInput" "name" as
          , cameraInputStreamId = definiteA fromAttrToStr "CameraInput" "streamId" as
          , cameraInputWarpFile = definiteA fromAttrToStr "CameraInput" "warpFile" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (cameraInputName v)
        , toAttrFrStr "streamId" (cameraInputStreamId v)
        , toAttrFrStr "warpFile" (cameraInputWarpFile v)
        ]

instance HTypeable FileInput where
    toHType x = Defined "FileInput" [] []
instance XmlContent FileInput where
    toContents as =
        [CElem (Elem "FileInput" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["FileInput"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <FileInput>, "++)
instance XmlAttributes FileInput where
    fromAttrs as =
        FileInput
          { fileInputName = definiteA fromAttrToStr "FileInput" "name" as
          , fileInputFile = definiteA fromAttrToStr "FileInput" "file" as
          , fileInputWarpFile = definiteA fromAttrToStr "FileInput" "warpFile" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (fileInputName v)
        , toAttrFrStr "file" (fileInputFile v)
        , toAttrFrStr "warpFile" (fileInputWarpFile v)
        ]

instance HTypeable Map where
    toHType x = Defined "Map" [] []
instance XmlContent Map where
    toContents as =
        [CElem (Elem "Map" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["Map"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <Map>, "++)
instance XmlAttributes Map where
    fromAttrs as =
        Map
          { mapMapFile = definiteA fromAttrToStr "Map" "mapFile" as
          , mapWarpFile = definiteA fromAttrToStr "Map" "warpFile" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "mapFile" (mapMapFile v)
        , toAttrFrStr "warpFile" (mapWarpFile v)
        ]

instance HTypeable Regions where
    toHType x = Defined "Regions" [] []
instance XmlContent Regions where
    toContents (Regions a) =
        [CElem (Elem "Regions" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Regions"]
        ; interior e $ return (Regions) `apply` many parseContents
        } `adjustErr` ("in <Regions>, "++)

instance HTypeable Region where
    toHType x = Defined "Region" [] []
instance XmlContent Region where
    toContents (Region as a) =
        [CElem (Elem "Region" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Region"]
        ; interior e $ return (Region (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <Region>, "++)
instance XmlAttributes Region_Attrs where
    fromAttrs as =
        Region_Attrs
          { regionName = definiteA fromAttrToStr "Region" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (regionName v)
        ]

instance HTypeable RegionPoint where
    toHType x = Defined "RegionPoint" [] []
instance XmlContent RegionPoint where
    toContents as =
        [CElem (Elem "RegionPoint" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["RegionPoint"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <RegionPoint>, "++)
instance XmlAttributes RegionPoint where
    fromAttrs as =
        RegionPoint
          { regionPointLat = read $ definiteA fromAttrToStr "RegionPoint" "lat" as
          , regionPointLon = read $ definiteA fromAttrToStr "RegionPoint" "lon" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "lat" (show $ regionPointLat v)
        , toAttrFrStr "lon" (show $ regionPointLon v)
        ]

instance HTypeable PointsOfInterest where
    toHType x = Defined "PointsOfInterest" [] []
instance XmlContent PointsOfInterest where
    toContents (PointsOfInterest a) =
        [CElem (Elem "PointsOfInterest" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PointsOfInterest"]
        ; interior e $ return (PointsOfInterest) `apply` many parseContents
        } `adjustErr` ("in <PointsOfInterest>, "++)

instance HTypeable PointOfInterest where
    toHType x = Defined "PointOfInterest" [] []
instance XmlContent PointOfInterest where
    toContents as =
        [CElem (Elem "PointOfInterest" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["PointOfInterest"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <PointOfInterest>, "++)
instance XmlAttributes PointOfInterest where
    fromAttrs as =
        PointOfInterest
          { pointOfInterestName = definiteA fromAttrToStr "PointOfInterest" "name" as
          , pointOfInterestLat = read $ definiteA fromAttrToStr "PointOfInterest" "lat" as
          , pointOfInterestLon = read $ definiteA fromAttrToStr "PointOfInterest" "lon" as
          , pointOfInterestRange = read $ definiteA fromAttrToStr "PointOfInterest" "range" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (pointOfInterestName v)
        , toAttrFrStr "lat" (show $ pointOfInterestLat v)
        , toAttrFrStr "lon" (show $ pointOfInterestLon v)
        , toAttrFrStr "range" (show $ pointOfInterestRange v)
        ]

instance HTypeable AgentTemplates where
    toHType x = Defined "AgentTemplates" [] []
instance XmlContent AgentTemplates where
    toContents (AgentTemplates a) =
        [CElem (Elem "AgentTemplates" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["AgentTemplates"]
        ; interior e $ return (AgentTemplates) `apply` many parseContents
        } `adjustErr` ("in <AgentTemplates>, "++)

instance HTypeable AgentTemplate where
    toHType x = Defined "AgentTemplate" [] []
instance XmlContent AgentTemplate where
    toContents as =
        [CElem (Elem "AgentTemplate" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["AgentTemplate"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <AgentTemplate>, "++)
instance XmlAttributes AgentTemplate where
    fromAttrs as =
        AgentTemplate
          { agentName = definiteA fromAttrToStr "AgentTemplate" "name" as
          , agentArea = read $ definiteA fromAttrToStr "AgentTemplate" "area" as
          , agentSpeed = read $ definiteA fromAttrToStr "AgentTemplate" "speed" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (agentName v)
        , toAttrFrStr "area" (show $ agentArea v)
        , toAttrFrStr "speed" (show $ agentSpeed v)
        ]



{-Done-}
