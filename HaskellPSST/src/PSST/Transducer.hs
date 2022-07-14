module PSST.Transducer ( State
                       , StateSet
                       , Sequence
                       , Alphabet
                       , StringVariable
                       , Transducer
                       ) where
import qualified Data.HashMap.Strict as H

-- States are represented as a number
-- Example: 1 == q1 
type State = Int
type StateSet = [State] 
type Sequence = [StateSet]

type Alphabet = String
type StringVariable = H.HashMap String (Maybe String)


data Transducer = Transducer { numberOfStates :: StateSet
                             , alphabet :: Alphabet
                             , variables :: StringVariable
                             , nonEpsilonTransitions :: [(State, Alphabet) -> Sequence]
                             , epsilonTransitions :: [State -> (Sequence, Sequence)]
                             , varAssignment :: (State, Alphabet, State) -> StringVariable -> StringVariable
                             , initialState :: State
                             , finalStates :: State -> Maybe String
                             }