package sets

import isEqual.*

type Relation[X, Y] = PartialFunction[X, Set[Y]]

