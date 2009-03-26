module World where
import Types
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types (HasInt(..), wrappedInts)
import WrappedInts.IDMap (empty, toList, keysSet)
import qualified WrappedInts.IDSet as IDSet
import qualified WrappedInts.IDMap as IDMap
import qualified Text.XML.HaXml.Types as HaXml
import Text.XML.HaXml.Combinators
import qualified Text.XML.HaXml.Pretty as HaXml
import qualified Data.Sequence as Seq
import Data.List ((\\))

data WorldState = WorldState { mind     :: Mind Level Level Level, -- ^ The Mark-II Mind
                               hypIDs   :: [HypothesisID],         -- ^ Current set of hypothesis IDs
                               detIDs   :: [DetectionID],          -- ^ Most recent detections
                               detMap   :: DetectionMap,           -- ^ Current map of detections
                               noiseIDs :: [NoiseID],              -- ^ Most recent noise hypotheses
                               noiseMap :: NoiseMap,               -- ^ Current map of noise hypotheses
                               trackIDs :: [TrackID],              -- ^ Most recent tracks
                               trackMap :: TrackMap,               -- ^ Current map of tracks
                               curFrame :: Frame                   -- ^ Most recent frame
                             }

-- | Create a new blank world state
newWorldState :: WorldState
newWorldState = WorldState
                -- (setTrace True (newMind confidenceBoost suggestStatus SparseTransitive))
                -- mind
                (newMind confidenceBoost suggestStatus SparseTransitive)
                -- hypIDs
                [HasInt 0]
                -- detIDs
                []
                -- detMap
                empty
                -- noiseIDs
                []
                -- noiseMap
                empty
                -- trackIDs
                []
                -- trackMap
                empty
                -- curFrame
                (Frame (Frame_Attrs 0 0) [])

type WorldLog = ([String], HaXml.Content ()) -- ^ Human and XML representation of events

newtype World a = World { worldState :: (a, WorldLog) } -- ^ World state plus the log

instance Monad World where
    return a = World (a, ([], HaXml.CElem (HaXml.Elem "WorldEvents" [] []) ()))

    -- (>>=) :: World a
    --      -> (a -> World b)
    --      -> World b
    m >>= f = let (a, (s,  x))  = worldState m
                  n             = f $! a
                  (b, (s', x')) = worldState n
              in World (b, (s ++ s', joinWorldEvents x x'))

cleanWorld :: Frame -> WorldState -> WorldState
cleanWorld frame ws = ws { mind = newMind''', curFrame = frame, detIDs = [], detMap = empty, hypIDs = newHypIDs,
                       noiseIDs = [], noiseMap = empty }
    where
      newMind    = foldl (\m (c, _, _) -> removeConstrainer c m) (mind ws) (getConstrainers (mind ws))
      newMind'   = foldl (\m (a, _, _) -> removeAdjuster a m) newMind (getAdjusters newMind)

      -- remove detections
      newMind''  = foldl (\m h -> removeHypothesis h m) newMind' (detIDs ws)

      -- remove noise
      newMind''' = foldl (\m h -> removeHypothesis h m) newMind' (noiseIDs ws)

      -- only tracks are left
      newHypIDs  = [0] ++ (trackIDs ws)

-- | Writes a human and XML log
recordWorldEvent :: ([String], HaXml.Content ()) -- ^ Human and XML content
                 -> World ()                     -- ^ Resulting world with logged content
recordWorldEvent (s, e) = World ((), (s, e))

-- | Writes an XML log in a particular frame
recordWorldEventInFrame :: Frame             -- ^ Frame of event
                        -> [HaXml.Content ()] -- ^ XML log content
                        -> HaXml.Content ()   -- ^ XML log content inside the frame
recordWorldEventInFrame frame c =
    HaXml.CElem (HaXml.Elem "WorldEvents" [] 
                          [(HaXml.CElem (HaXml.Elem "Frame" [("number", HaXml.AttValue [Left $ show number]),
                                                             ("time", HaXml.AttValue [Left $ show time])]
                                         c) ())]) ()
    where
      number = frameProp frameNumber frame
      time   = frameProp frameTime frame

-- | Create an XML element with attributes
--
-- This is just a helper function.
worldElem :: String             -- ^ Element name
          -> [(String, String)] -- ^ Element attributes
          -> [HaXml.Content ()]  -- ^ Child content
          -> HaXml.Content ()    -- ^ Resulting content
worldElem name attrs content =
    HaXml.CElem (HaXml.Elem name (map (\(f,s) -> (f, HaXml.AttValue [Left s])) attrs) content) ()

-- | Create an empty "WorldEvents" top-level element
emptyElem :: HaXml.Content ()
emptyElem = HaXml.CElem (HaXml.Elem "WorldEvents" [] []) ()

-- | Determine if some XML content is empty
--
-- > isEmptyElem emptyElem == True
isEmptyElem :: HaXml.Content () -> Bool
isEmptyElem c = 0 == (length $ (children `o` tag "WorldEvents") c)

-- | Join two XML logs
--
-- The two logs may be from the same frame or separate frames.
joinWorldEvents :: HaXml.Content () -> HaXml.Content () -> HaXml.Content ()
joinWorldEvents c1 c2
    | isEmptyElem c1 = c2
    | isEmptyElem c2 = c1
    | otherwise      =
        if framenum1 == framenum2 then
            joinWorldEventsOneFrame (Frame (Frame_Attrs framenum1 frametime1) []) c1 c2
        else
            joinWorldEventsTwoFrames c1 c2
    where
      frameattr s c = extractAttr s ((attr s `o` tag "Frame" `o` children `o` tag "WorldEvents") c)
      framenum1     = read $ frameattr "number" c1
      frametime1    = read $ frameattr "time" c1
      framenum2     = read $ frameattr "number" c2

-- | Join two XML logs from the same frame
joinWorldEventsOneFrame :: Frame            -- ^ Frame of interest
                        -> HaXml.Content () -- ^ XML content
                        -> HaXml.Content () -- ^ XML content
                        -> HaXml.Content () -- ^ Resulting joined content
joinWorldEventsOneFrame frame c1 c2 =
    recordWorldEventInFrame frame ((filterFrameEvents frame c1)
                                   ++
                                   (filterFrameEvents frame c2))

-- | Join two XML logs from different frames
joinWorldEventsTwoFrames :: HaXml.Content () -> HaXml.Content () -> HaXml.Content ()
joinWorldEventsTwoFrames c1 c2 =
    HaXml.CElem (HaXml.Elem "WorldEvents" [] 
                          ((children `o` tag "WorldEvents") c1 ++ (children `o` tag "WorldEvents") c2)) ()

-- | Filter events in an XML log from the specified frame
filterFrameEvents :: Frame     -- ^ Frame of interest
                  -> CFilter () -- ^ Resulting XML filter
filterFrameEvents frame =
    children `o` attrval ("number", HaXml.AttValue [Left (show $ frameProp frameNumber frame)]) `o` tag "Frame"
             `o` children `o` tag "WorldEvents"

-- | Extract an XML attribute of interest
--
-- This function requires that the first element has the attribute of interest.
extractAttr :: String             -- ^ Attribute name
            -> [HaXml.Content ()] -- ^ XML content
            -> String             -- ^ Attribute value
extractAttr s ((HaXml.CElem (HaXml.Elem _ attrs _) _):c) = extractAttr' s attrs
    where
      extractAttr' :: String -> [HaXml.Attribute] -> String
      extractAttr' s ((name, (HaXml.AttValue [Left value])):as) = if name == s then value
                                                                  else extractAttr' s as
extractAttr s c = error ("extractAttr: first element of XML content does not have attribute of interest; " ++
                         "attribute: " ++ s)

-- | XML header for output
xmlHeader :: WorldState -> String
xmlHeader ws =
    do
      (show (HaXml.prolog xmlProlog)) ++ "<WorldEvents>"

xmlFooter :: WorldState -> String
xmlFooter ws = "</WorldEvents>"

-- | Return human log
outputHuman :: World WorldState -> String
outputHuman m = (unlines $ ["Mind:"] ++ (showMind $ mind ws)
                 -- ++ ["Mind trace:"] ++ (formatTrace $ mindTrace $ mind ws)
                 -- ++ ["Constrainers:"] ++ (map show $ getConstrainers $ mind ws)
                 -- ++ ["Explainers:"] ++ (map show $ getExplainers $ mind ws)
                 -- ++ ["Hypotheses:"] ++ (map (\h -> unlines $ showHypothesis h (mind ws)) ((hypIDs ws) \\ [0]))
                )
    where
      (ws, _) = worldState m

-- | Print log
outputLog :: World WorldState -> String
outputLog m = unlines $ map (show . HaXml.content) $ buildLog m
    where
      buildLog :: World WorldState -> [HaXml.Content ()]
      buildLog m = [frameLog] ++ ((children `o` tag "WorldEvents") e)
          where
            (_, (s, e)) = worldState m
            frameLog    = HaXml.CElem (HaXml.Elem "FrameLog" [] [HaXml.CString True ((outputHuman m) ++ (unlines $ map ((++) "     ") s)) ()]) ()

formatTrace :: (Show a) => Seq.Seq a -> [String]
formatTrace s = formatTrace' s 0 (Seq.length s)
    where
      formatTrace' s i n
          | i == n = []
          | otherwise  = [show $ Seq.index s i] ++ formatTrace' s (i+1) n

-- | Construct XML prolog for outputting the XML log 
xmlProlog :: HaXml.Prolog
xmlProlog = HaXml.Prolog (Just (HaXml.XMLDecl "1.0" (Just (HaXml.EncodingDecl "UTF-8")) Nothing))
            [] (Just (HaXml.DTD "classifications.dtd" Nothing [])) []

nextHypID ws = 1 + maximum [head $ hypIDs ws,
                            foldr max 0 $ IDSet.toList $ unacceptableHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ refutedHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ consideringHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ acceptedHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ irrefutableHypotheses $ mind ws]

-- | Get next constrainer ID
nextConstrainer ws = 1 + (foldr max 0 $ (\(a, _, _) -> a) $ unzip3 $ getConstrainers (mind ws))

nextExplainer ws = 1 + (foldr max 0 $ IDMap.keys (explainers $ mind ws))

nextAdjuster ws = 1 + (foldr max 0 $ (\(a, _, _) -> a) $ unzip3 $ getAdjusters (mind ws))