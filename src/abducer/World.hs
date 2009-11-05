{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module World where
import Types
import qualified Reasoner.Core as R
import Reasoner.Types (HypothesisID(..), HypothesisMap(..), HypothesisIDs, ExplainsID(..))
import qualified Reasoner.Constrainers as RC
import Vocabulary
import WrappedInts.Types (HasInt(..))
import qualified WrappedInts.IDSet as IDSet
import qualified WrappedInts.IDMap as IDMap
import Text.XML.HaXml.XmlContent (showXml)
import Text.XML.HaXml.XmlContent.Parser (List1(..))
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Dynamic
import Data.List (sortBy, (\\))
import Debug.Trace

data World = World { mind      :: R.Mind Level Level Level
                   , entityMap :: HypothesisMap Entity
                   }

mkWorld :: World
mkWorld = World { mind      = (R.setTrace True $ R.newMind confidenceBoost suggestStatus R.NoTransitive)
                , entityMap = IDMap.empty
                }

allHypotheses :: World -> HypothesisIDs
allHypotheses world = R.acceptedHypotheses (mind world)
                      `IDSet.union` (R.consideringHypotheses (mind world))
                      `IDSet.union` (R.refutedHypotheses (mind world))

cleanWorld :: World -> World
cleanWorld world = 
    let allDetHypIds      = map detectionId $ gatherEntities (entityMap world) (allHypotheses world)
        newerDetHypIds    = newerDetections (entityMap world) (allHypotheses world)
        oldDetHypIds      = allDetHypIds \\ newerDetHypIds
        invalidMovHypIds  = invalidMovements oldDetHypIds (entityMap world) (allHypotheses world)
        invalidPathHypIds = invalidPaths invalidMovHypIds (entityMap world) (allHypotheses world)
        removable         = oldDetHypIds ++ invalidMovHypIds ++ invalidPathHypIds
    in foldr removeHypothesis world removable

newerDetections :: HypothesisMap Entity
                -> HypothesisIDs
                -> [HypothesisID]
newerDetections entityMap allHypotheses =
    take 20 $ map detectionId $
    reverse $ sortBy detAscOrdering $
    (gatherEntities entityMap allHypotheses :: [Detection])

invalidMovements :: [HypothesisID]
                 -> HypothesisMap Entity
                 -> HypothesisIDs
                 -> [HypothesisID]
invalidMovements detHypIds entityMap allHypotheses =
    map (\(Movement (Movement_Attrs hypId) _) -> hypId) $
    filter (\(Movement _ (NonEmpty (det1:det2:[]))) ->
                (elem (detectionId det1) detHypIds) || (elem (detectionId det2) detHypIds))
    (gatherEntities entityMap allHypotheses :: [Movement])

invalidPaths :: [HypothesisID]
             -> HypothesisMap Entity
             -> HypothesisIDs
             -> [HypothesisID]
invalidPaths movHypIds entityMap allHypotheses =
    map (\(Path (Path_Attrs hypId) _) -> hypId) $
    filter (\(Path _ (NonEmpty movs)) ->
        (null $ (\\) (map (\(Movement (Movement_Attrs movId) _) -> movId) movs) movHypIds))
    (gatherEntities entityMap allHypotheses :: [Path])

hypothesize :: (Typeable a) => [Hypothesis a] -> World -> World
hypothesize hs world =
    let world1 = foldl (\w h -> addHypothesis (entity h) (hypId h) (aPriori h) w) world hs
        world2 = foldl (\w (Hyp _ subject _ explains _ _) ->
                            foldl (\w' object ->
                                   addExplains subject object w') w explains) world1 hs
        world3 = foldl (\w (Hyp _ subject _ _ implies _) ->
                            foldl (\w' object ->
                                   addImplies subject object w') w implies) world2 hs
    in world3

addHypothesis :: (Typeable a) => a -> HypothesisID -> (Level -> Level) -> World -> World
addHypothesis entity hypId scoreFunc world =
    let entityMap' = IDMap.insert hypId (toDyn entity) (entityMap world)
    in
    world { mind      = R.addHypothesis hypId category scoreFunc (mind world)
          , entityMap = entityMap'
          }

removeHypothesis :: HypothesisID -> World -> World
removeHypothesis hypId world =
    world { mind      = R.removeHypothesis hypId (mind world)
          , entityMap = IDMap.delete hypId (entityMap world)
          }

addExplains :: HypothesisID -> HypothesisID -> World -> World
addExplains subject object world =
    world { mind = R.addExplains (mkHypPairId subject object)
                   subject object (mind world)
          }

addImplies :: HypothesisID -> HypothesisID -> World -> World
addImplies subject object world =
    world { mind = R.addAdjuster (mkHypPairId subject object)
                   subject boostOnAcceptance object (mind world)
          }

addRefutes :: HypothesisID -> HypothesisID -> World -> World
addRefutes subject object world =
    world { mind = R.addAdjuster (mkHypPairId object subject)
                   object refuteLower subject (mind world)
          }

boostOnAcceptance = Left (Just $ increaseLevelBy 2, Nothing)

refuteLower = Left (Nothing, Just $ decreaseLevelBy 2)

reason :: World -> World
reason world = world { mind = R.reason (R.ReasonerSettings True) Medium (mind world) }

buildResults :: World -> Results
buildResults world =
    let accepted = R.acceptedHypotheses (mind world)
        rejected = R.refutedHypotheses (mind world)
    in
      Results (Accepted (gatherEntities (entityMap world) accepted)
                            (gatherEntities (entityMap world) accepted)
                            (gatherEntities (entityMap world) accepted))
                  (Rejected (gatherEntities (entityMap world) rejected)
                                (gatherEntities (entityMap world) rejected)
                                (gatherEntities (entityMap world) rejected))

gatherEntities :: forall a.
                  (Typeable a) =>
                  HypothesisMap Entity
               -> HypothesisIDs
               -> [a]
gatherEntities entityMap hypIds =
    map (\(Just entity) -> entity) $
    map (\dyn -> fromDynamic dyn) $
    filter (\dyn -> case (fromDynamic dyn :: Maybe a) of
                      Just entity -> True
                      Nothing     -> False) $
    IDMap.elems $
    IDMap.filterWithKey (\hypId _ -> IDSet.member hypId hypIds) entityMap

outputLog :: World -> String
outputLog world = showXml False $ buildResults world
--                  "<Log>" ++ (unlines $ R.showMind (mind world)) ++ "</Log>\n"



{--
data WorldState = WorldState { mind             :: Mind Level Level Level, -- ^ The Mark-II Mind
                               hypIDs           :: [HypothesisID],         -- ^ Current set of hypothesis IDs
                               currentDetIDs    :: [DetectionID],          -- ^ Most recent detections
                               detIDs           :: [DetectionID],          -- ^ All detections (younger than a few seconds)
                               detMap           :: DetectionMap,           -- ^ Map of most recent and \'all\' detections
                               noiseIDs         :: [NoiseID],              -- ^ Most recent noise hypotheses
                               noiseMap         :: NoiseMap,               -- ^ Current map of noise hypotheses
                               trackIDs         :: [TrackID],              -- ^ Most recent tracks
                               trackMap         :: TrackMap,               -- ^ Current map of tracks
                               curFrame         :: Frame,                  -- ^ Most recent frame
                               constrainers     :: [([HypothesisID], HypothesisID, ConstrainerType)] -- ^ Current set of constrainers
                             }

type ConstrainerType = (String, [Level] -> [Bound Level])

constrainerImplies :: ConstrainerType
constrainerImplies = ("implies", implies)

constrainerImpliedBy :: ConstrainerType
constrainerImpliedBy = ("impliedBy", impliedBy)

constrainerOneOf :: ConstrainerType
constrainerOneOf = ("oneOf", oneOf)

constrainerNotOverOneOf :: ConstrainerType
constrainerNotOverOneOf = ("notOverOneOf", notOverOneOf)

-- | Create a new blank world state
newWorldState :: WorldState
newWorldState = WorldState
                (setTrace True (newMind confidenceBoost suggestStatus NoTransitive))
                -- mind
                --(newMind confidenceBoost suggestStatus NoTransitive)
                -- hypIDs
                [HasInt 0]
                -- currentDetIDs
                []
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
                -- constrainers
                []

-- | Human and XML representation of events
type WorldLog = ([String], HaXml.Content ())

-- | World state plus the log
newtype World a = World { worldState :: (a, WorldLog) }

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
cleanWorld frame ws = ws { mind = newMind3, curFrame = frame, constrainers = [] }
    where
      newMind1 = foldl (\m (c, _, _) -> removeConstrainer c m) (mind ws) (getConstrainers (mind ws))
      newMind2 = foldl (\m (a, _, _) -> removeAdjuster a m) newMind1 (getAdjusters newMind1)
      newMind3 = setTrace True newMind2

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
xmlHeader :: String
xmlHeader = (show (HaXml.prolog xmlProlog)) ++ "<Results>\n"

xmlFooter :: String
xmlFooter = "</Results>\n"

-- | Return human log
outputHuman :: World WorldState -> String
outputHuman m = (unlines $ ["Mind:"] ++ (showMind $ mind ws)
                 ++ ["Mind trace:"] ++ (formatTrace $ mindTrace $ mind ws)
                 ++ ["Explainers:"] ++ (map show $ getExplainers $ mind ws)
                 ++ ["Hypotheses:"] ++ (map (\h -> unlines $ showHypothesis h (mind ws)) ((detIDs ws) ++ (noiseIDs ws) ++ (trackIDs ws)))
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

addConstrainerType :: ConstrainerType
                   -> HypothesisID
                   -> [HypothesisID]
                   -> WorldState -- ^ World state
                   -> WorldState -- ^ Resulting world state
addConstrainerType constrainer object subjects ws =
    ws { mind = addConstrainer (nextConstrainer ws) (IDSet.fromList subjects) (snd constrainer) object (mind ws),
         constrainers = (subjects, object, constrainer):(constrainers ws) }

addCyclicConstrainers :: [HypothesisID] -> ConstrainerType -> WorldState -> WorldState
addCyclicConstrainers hs constrainer ws = foldl (\ws (ss, o) -> if (null ss) then ws
                                                                else addConstrainerType constrainer o ss ws)
                                          ws
                                          ([(hs \\ [object], object) | object <- hs] :: [([HypothesisID], HypothesisID)])

-- | Print a list of constrainers
showConstrainers :: WorldState -- ^ World state
                 -> [String]   -- ^ List of constrainers
showConstrainers ws = foldl (\s (subjects, object, (cstring, func)) ->
                                 [cstring ++ " on object " ++ (formatHypothesis object) ++ "->" ++ (runConstrainer object subjects func)
                                  ++ " from subjects [" ++ (formatHypotheses subjects) ++ " ]"] ++ s)
                      [] (constrainers ws)
    where
      runConstrainer :: HypothesisID -> [HypothesisID] -> ([Level] -> [Bound Level]) -> String
      runConstrainer object subjects func = show $ func $ map (\h -> (getItemFromMap (aPriori (mind ws)) h) High) subjects

      formatHypotheses :: [HypothesisID] -> String
      formatHypotheses []     = ""
      formatHypotheses (h:hs) = " " ++ (formatHypothesis h) ++ (formatHypotheses hs)

      formatHypothesis :: HypothesisID -> String
      formatHypothesis h = case (hypothesisStatus h (mind ws)) of
                             (_, Just (lowerBound, upperBound), _, status) ->
                                 (show h) ++ " (" ++ (show $ (getItemFromMap (aPriori (mind ws)) h) High) ++ ")[" ++ (show lowerBound) ++ "/" ++ (show upperBound) ++ "]"
                             (_, Nothing, _, status)                       ->
                                 (show h) ++ " (" ++ (show $ (getItemFromMap (aPriori (mind ws)) h) High) ++ ")"

-- | Get next hypothesis ID
nextHypID ws = 1 + maximum [head $ hypIDs ws,
                            foldr max 0 $ IDSet.toList $ unacceptableHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ refutedHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ consideringHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ acceptedHypotheses $ mind ws,
                            foldr max 0 $ IDSet.toList $ irrefutableHypotheses $ mind ws]

-- | Get next constrainer ID
nextConstrainer ws = 1 + (foldr max 0 $ (\(a, _, _) -> a) $ unzip3 $ getConstrainers (mind ws))

-- | Get next explainer ID
nextExplainer ws = 1 + (foldr max 0 $ IDMap.keys (explainers $ mind ws))

-- | Get next adjuster ID
nextAdjuster ws = 1 + (foldr max 0 $ (\(a, _, _) -> a) $ unzip3 $ getAdjusters (mind ws))

--}