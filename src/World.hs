module World where
import Types
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types (HasInt(..))
import WrappedInts.IDMap (empty)
import Text.XML.HaXml.Types as HaXml
import Text.XML.HaXml.Combinators as HaXml
import Text.XML.HaXml.Pretty as HaXml.Pretty

data WorldState = WorldState { mind   :: Mind Level Level Level, -- ^ The Mark-II Mind
                               hypIDs :: [HypothesisID],         -- ^ Current set of hypothesis IDs
                               acqMap :: AcquisitionMap,         -- ^ Current map of acquisitions
                               acqIDs :: [AcquisitionID],        -- ^ Most recent acquisitions
                               frame  :: Frame,                  -- ^ Most recent frame
                               tracks :: TrackMap                -- ^ Current map of tracks
                             }

-- | Create a new blank world state
newWorldState :: WorldState
newWorldState = WorldState
                (newMind confidenceBoost suggestStatus SparseTransitive)
                [HasInt 0]
                empty
                []
                (Frame (Frame_Attrs 0 0) [])
                empty

type WorldLog = ([String], HaXml.Content) -- ^ Human-readable and XML representation

newtype World a = World { worldState :: (a, WorldLog) }

instance Monad World where
    return a = World (a, ([], HaXml.CElem (HaXml.Elem "WorldEvents" [] [])))

    -- (>>=) :: World a
    --      -> (a -> World b)
    --      -> World b
    m >>= f = let (a, (s,  x))  = worldState m
                  n             = f a
                  (b, (s', x')) = worldState n
              in World (b, (s ++ s', joinWorldEvents x x'))

recordWorldEvent :: ([String], HaXml.Content) -> World ()
recordWorldEvent (s, e) = World ((), (s, e))

recordWorldEventInFrame :: String -> String -> [HaXml.Content] -> HaXml.Content
recordWorldEventInFrame framenum frametime c =
    HaXml.CElem (HaXml.Elem "WorldEvents" [] 
                          [(HaXml.CElem (HaXml.Elem "Frame" [("framenum", AttValue [Left framenum]),
                                                             ("time", AttValue [Left frametime])]
                                         c))])

worldElem :: String -> [(String, String)] -> [HaXml.Content] -> HaXml.Content
worldElem name attrs content =
    HaXml.CElem (HaXml.Elem name (map (\(f,s) -> (f, AttValue [Left s])) attrs) content)

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
    recordWorldEventInFrame framenum frametime
                                ((filterFrameEvents framenum $ c1)
                                 ++
                                 (filterFrameEvents framenum $ c2))

joinWorldEventsTwoFrames :: HaXml.Content -> HaXml.Content -> HaXml.Content
joinWorldEventsTwoFrames c1 c2 =
    HaXml.CElem (HaXml.Elem "WorldEvents" [] 
                          ((children `o` tag "WorldEvents") c1 ++ (children `o` tag "WorldEvents") c2))

filterFrameEvents :: String -> HaXml.CFilter
filterFrameEvents framenum =
    children `o` attrval ("framenum", AttValue [Left framenum]) `o` tag "Frame"
             `o` children `o` tag "WorldEvents"

-- | This function relies on the situation where the first element has the attribute of interest
extractAttr :: String -> [HaXml.Content] -> String
extractAttr s ((HaXml.CElem (HaXml.Elem _ attrs _)):c) = extractAttr' s attrs
extractAttr s c = "here: " ++ (show $ length c)

extractAttr' :: String -> [HaXml.Attribute] -> String
extractAttr' s ((name, (HaXml.AttValue [Left value])):as) = if name == s then value
                                                            else extractAttr' s as

outputXML :: World WorldState -> IO ()
outputXML m = (putStrLn . show) (HaXml.Pretty.document d)
    where
      (_, (_, (HaXml.CElem e))) = worldState m
      d = HaXml.Document (HaXml.Prolog Nothing [] Nothing []) HaXml.emptyST e []

outputHuman :: World WorldState -> IO ()
outputHuman m = putStrLn $ unlines (s ++ [""] ++ (showMind $ mind ws))
    where
      (ws, (s, _)) = worldState m


