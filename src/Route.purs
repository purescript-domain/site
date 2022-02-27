module PurescriPT.Site.Route where

import Prelude

import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', path, root)
import Routing.Duplex as R
import Routing.Duplex.Generic (sum, noArgs)

data Route
  = Home
  | Terms

derive instance Generic Route _
derive instance Eq Route
derive instance Ord Route

_route :: RouteDuplex' Route
_route = root $ sum
  { "Home": noArgs
  , "Terms": path "terms" noArgs
  }

parse :: String -> Maybe Route
parse "" = pure Home
parse r = hush $ R.parse _route r

print :: Route -> String
print = R.print _route
