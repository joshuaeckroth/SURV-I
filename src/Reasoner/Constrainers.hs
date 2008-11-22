{-| The "Reasoner.Constrainers" module provides simple constrainer-related
    functions. It is Copyright 2004, 2008 by Aetion Technologies LLC and is
    proprietary and company confidential. -}

module Reasoner.Constrainers
where
import Reasoner.Core
import Reasoner.Types

{-| 'makeConstrainer' is a simple constrainer-building tool that should serve
    most such purposes. It is used by the other example functions below. -}

makeConstrainer :: Metric c 
                => [c -> Bound c] {- ^ The applicable 'Bound's -}
                -> Bool {- ^ If the constraint applies from the highest
                             confidence of the sources, rather than the lowest -}
                -> Bool {- ^ If the source confidence is to be negated to exert
                             an opposite bound, rather than being left as it is -}
                -> [c]       {- ^ The confidence of the subjects -}
                -> [Bound c] {- ^ The bounds on the confidence of the object -}

makeConstrainer constraintMakers maximise negate subjects =
    let findExtreme = if maximise then maximum else minimum
	maybeNegate = if negate then negateMetric else id
     in if null subjects 
	then [] 
	else map ($ maybeNegate (findExtreme subjects)) constraintMakers

{-| 'implies' is available for use with 'Reasoner.Core.addConstrainer' to
    create \'implies\' relationships. One may often wish to pair this with an
    \'impliedBy\' in the opposite direction. -}

implies :: Metric c => [c] -> [Bound c]
implies = makeConstrainer [HigherThan] True False

{-| 'impliedBy' is available for use with 'Reasoner.Core.addConstrainer' to
    create \'implied by\' relationships. -}

impliedBy :: Metric c => [c] -> [Bound c]
impliedBy = makeConstrainer [LowerThan] False False

{-| 'oneOf' is available for use with 'Reasoner.Core.addConstrainer' to create
    relationships among sets of hypotheses that specify that exactly one of
    them is true. -}

oneOf :: Metric c => [c] -> [Bound c]
oneOf = makeConstrainer [LowerThan, HigherThan] True True

{-| 'notOverOneOf' is available for use with 'Reasoner.Core.addConstrainer' to
    create relationships among sets of hypotheses that specify that no more
    than one of them is true. -}

notOverOneOf :: Metric c => [c] -> [Bound c]
notOverOneOf = makeConstrainer [LowerThan] True True
