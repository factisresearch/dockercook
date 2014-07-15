module Cook.State.Graph where

type Node = Int
type Adj = (Node, [Node])

data Graph a
   = Graph
   { g_fwd :: Adj
   , g_rev :: Adj
   , g_data :: HM.HashMap Node a
   }
