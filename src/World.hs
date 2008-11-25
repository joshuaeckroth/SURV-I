module World where
import Types
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types (HasInt(..), wrappedInts)
import WrappedInts.IDMap (empty, toList, keysSet)
import qualified WrappedInts.IDSet as IDSet
import qualified Text.XML.HaXml.Types as HaXml
import Text.XML.HaXml.Combinators
import qualified Text.XML.HaXml.Pretty as HaXml

data WorldState = WorldState { mind     :: Mind Level Level Level, -- ^ The Mark-II Mind
                               hypIDs   :: [HypothesisID],         -- ^ Current set of hypothesis IDs
                               acqIDs   :: [AcquisitionID],        -- ^ Most recent acquisitions
                               acqMap   :: AcquisitionMap,         -- ^ Current map of acquisitions
                               noiseIDs :: [NoiseID],              -- ^ Most recent noise hypotheses
                               noiseMap :: NoiseMap,               -- ^ Current map of noise hypotheses
                               trackIDs :: [TrackID],              -- ^ Most recent tracks
                               trackMap :: TrackMap,               -- ^ Current map of tracks
                               frame    :: Frame                   -- ^ Most recent frame
                             }

-- |Create a new blank world state
newWorldState :: WorldState
newWorldState = WorldState
                (newMind confidenceBoost suggestStatus SparseTransitive)
                [HasInt 0]
                []
                empty
                []
                empty
                []
                empty
                (Frame (Frame_Attrs 0 0) [])

type WorldLog = ([String], HaXml.Content) -- ^ Human and XML representation of events

newtype World a = World { worldState :: (a, WorldLog) } -- ^ The World: world state plus the log

instance Monad World where
    return a = World (a, ([], HaXml.CElem (HaXml.Elem "WorldEvents" [] [])))

    -- (>>=) :: World a
    --      -> (a -> World b)
    --      -> World b
    m >>= f = let (a, (s,  x))  = worldState m
                  n             = f a
                  (b, (s', x')) = worldState n
              in World (b, (s ++ s', joinWorldEvents x x'))

-- | Writes a human and XML log
recordWorldEvent :: ([String], HaXml.Content) -- ^ Human and XML content
                 -> World ()                  -- ^ Resulting world with logged content
recordWorldEvent (s, e) = World ((), (s, e))

-- | Writes an XML log in a particular frame number and frame time
recordWorldEventInFrame :: String          -- ^ Frame number
                        -> String          -- ^ Frame time
                        -> [HaXml.Content] -- ^ XML log content
                        -> HaXml.Content   -- ^ XML log content inside the frame
recordWorldEventInFrame framenum frametime c =
    HaXml.CElem (HaXml.Elem "WorldEvents" [] 
                          [(HaXml.CElem (HaXml.Elem "Frame" [("framenum", HaXml.AttValue [Left framenum]),
                                                             ("time", HaXml.AttValue [Left frametime])]
                                         c))])

-- | Create an XML element with attributes
--
-- This is just a helper function.
worldElem :: String             -- ^ Element name
          -> [(String, String)] -- ^ Element attributes
          -> [HaXml.Content]    -- ^ Child content
          -> HaXml.Content      -- ^ Resulting content
worldElem name attrs content =
    HaXml.CElem (HaXml.Elem name (map (\(f,s) -> (f, HaXml.AttValue [Left s])) attrs) content)

emptyElem :: HaXml.Content
emptyElem = HaXml.CElem (HaXml.Elem "WorldEvents" [] [])

isEmptyElem :: HaXml.Content -> Bool
isEmptyElem c = 0 == (length $ (children `o` tag "WorldEvents") c)

joinWorldEvents :: HaXml.Content -> HaXml.Content -> HaXml.Content
joinWorldEvents c1 c2
    | isEmptyElem c1 = c2
    | isEmptyElem c2 = c1
    | otherwise      =
        if framenum1 == framenum2 then
            joinWorldEventsOneFrame framenum1 frametime1 c1 c2
        else
            joinWorldEventsTwoFrames c1 c2
    where
      frameattr s c = extractAttr s ((attr s `o` tag "Frame" `o` children `o` tag "WorldEvents") c)
      framenum1     = frameattr "framenum" c1
      frametime1    = frameattr "time" c1
      framenum2     = frameattr "framenum" c2

joinWorldEventsOneFrame :: String -> String -> HaXml.Content -> HaXml.Content -> HaXml.Content
joinWorldEventsOneFrame framenum frametime c1 c2 =
    recordWorldEventInFrame framenum frametime ((filterFrameEvents framenum $ c1)
                                                ++
                                                (filterFrameEvents framenum $ c2))

joinWorldEventsTwoFrames :: HaXml.Content -> HaXml.Content -> HaXml.Content
joinWorldEventsTwoFrames c1 c2 =
    HaXml.CElem (HaXml.Elem "WorldEvents" [] 
                          ((children `o` tag "WorldEvents") c1 ++ (children `o` tag "WorldEvents") c2))

filterFrameEvents :: String -> CFilter
filterFrameEvents framenum =
    children `o` attrval ("framenum", HaXml.AttValue [Left framenum]) `o` tag "Frame"
             `o` children `o` tag "WorldEvents"

-- | This function relies on the situation where the first element has the attribute of interest
extractAttr :: String -> [HaXml.Content] -> String
extractAttr s ((HaXml.CElem (HaXml.Elem _ attrs _)):c) = extractAttr' s attrs
extractAttr s c = "here: " ++ (show $ length c)

extractAttr' :: String -> [HaXml.Attribute] -> String
extractAttr' s ((name, (HaXml.AttValue [Left value])):as) = if name == s then value
                                                            else extractAttr' s as

outputXmlHeader :: World WorldState -> IO ()
outputXmlHeader m = putStrLn "<xml>"

outputHuman :: World WorldState -> String
outputHuman m = (unlines s) ++ "\n" ++ (unlines (showMind $ mind ws))
    where
      (ws, (s, _)) = worldState m

outputXml :: World WorldState -> IO ()
outputXml m = (putStrLn . show) (HaXml.document d)
    where
      (_, (_, (HaXml.CElem e))) = worldState m
      d = HaXml.Document xmlProlog HaXml.emptyST e [HaXml.Comment $ outputHuman m]

xmlProlog :: HaXml.Prolog
xmlProlog = HaXml.Prolog (Just (HaXml.XMLDecl "1.0" (Just (HaXml.EncodingDecl "UTF-8")) Nothing))
            [] (Just (HaXml.DTD "classifications.dtd" Nothing [])) []

nextConstrainer ws = 1 + (foldr max 0 $ (\(a, _, _) -> a) $ unzip3 $ getConstrainers (mind ws))

nextID field mind = if (null . toList) (field mind) then HasInt 1
                    else 1 + (IDSet.findMax . keysSet) (field mind)
