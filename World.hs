module World where
import Types
import Reasoner.Core
import Reasoner.Types
import Vocabulary
import WrappedInts.Types (HasInt(..))
import WrappedInts.IDMap (empty)
import Text.XML.HaXml.Types as HaXml

data WorldState = WorldState { mind   :: Mind Level Level Level, -- ^ The Mark-II Mind
                               hypIDs :: [HypothesisID],         -- ^ Current set of hypothesis IDs
                               acqMap :: AcquisitionMap,         -- ^ Current map of acquisitions
                               acqIDs :: [AcquisitionID],        -- ^ Most recent acquisitions
                               tracks :: TrackMap }              -- ^ Current map of tracks

-- | Create a new blank world state
newWorldState :: WorldState
newWorldState = WorldState
                (newMind confidenceBoost suggestStatus SparseTransitive)
                [HasInt 0]
                empty
                []
                empty

type WorldLog = ([String],      -- ^ Human-readable representation
                 HaXml.Element) -- ^ XML representation

newtype World a = World { worldState :: (a, WorldLog) }

instance Monad World where
    return a = World (a, ([], HaXml.Elem "World" [] []))

    -- | Perform some action on the world and record its activity in the log
    -- (>>=) :: World a
    --      -> (a -> World b)
    --      -> World b
    m >>= f = let (a, w) = worldState m
                  n      = f a
                  (b, x) = worldState n
              in World (b, (fst w ++ fst x, HaXml.Elem "World" [] []))

recordWorldEvent :: ([String], HaXml.Element) -> World ()
recordWorldEvent (ss, e) = World ((), (ss, e))

outputHuman :: World a -> IO ()
outputHuman m = putStrLn (unlines s)
    where
      (_, (s, _)) = worldState m


