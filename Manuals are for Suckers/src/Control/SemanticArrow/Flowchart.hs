{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Control.SemanticArrow.Flowchart where

import qualified Control.SemanticArrow as S
import Data.Functor.Foldable
import Data.GraphViz.Types.Graph
import qualified Data.GraphViz.Attributes.Complete as G
import Data.GraphViz.Attributes.Colors.X11(X11Color(..))
import qualified Data.GraphViz.Types.Canonical as Gc

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe(listToMaybe)
import Control.Arrow
import Control.Monad.Identity

import Control.Monad.State
data Connector n a = Basic n | Split a a | Parallel a a

data ConnectorPair n a = ConnectorPair (Connector n a, Connector n a)

deriving instance (Show a, Show n) => Show (Connector n a)
deriving instance Functor (Connector n)
deriving instance (Show a, Show n) => Show (ConnectorPair n a)


data FlowChartState = FlowChartState {
  nextNodeId      :: Int,
  graph           :: DotGraph Int,
  nextClusterId   :: Int,
  clusterPath     :: [GraphID]
}

curCluster :: FlowChartState -> Maybe GraphID
curCluster = listToMaybe . clusterPath

popCluster :: FlowChartState -> FlowChartState
popCluster x = case clusterPath x of
  [] -> x
  (c:cls) -> x{clusterPath = cls}

pushCluster :: GraphID -> FlowChartState -> FlowChartState
pushCluster cl x = x{clusterPath = cl : clusterPath x}


popId :: (FlowChartState -> Int)
      -> (Int -> FlowChartState -> FlowChartState)
      -> State FlowChartState Int
popId fxn set =
  do
    i <- liftM fxn get
    modify (set $ i+1)
    return i

popNodeId = popId nextNodeId (\i s -> s{nextNodeId = i})
popClusterId = popId nextClusterId (\i s -> s{nextClusterId = i})


nodeAttr =  [
    G.Style [G.SItem G.Filled []],
    G.FontName "consolas bold",
    G.FillColor (G.toColorList [G.toColor White]),
    G.PenWidth 2.0,
    G.Shape G.Circle
  ]

getNewNode :: G.Attributes
           -> State FlowChartState (DotNode Int)
getNewNode attr =
  do
    ndId <- popNodeId
    s <- get
    let g = graph s
    let cl = curCluster s
    let g' = addNode ndId cl attr g
    put $ s{graph = g'}
    return $ DotNode ndId attr

newCluster :: G.Attributes -> State FlowChartState ()
newCluster attr =
  do
    clId <- popClusterId
    let cl = Num (G.Int clId)
    s <- get
    let g = graph s
    let cl0 = curCluster s

    let g' = setClusterAttributes cl [Gc.GraphAttrs attr] $ setClusterParent cl cl0 g

    put $ pushCluster cl $ s{graph = g'}

nextGraphID :: State Int GraphID
nextGraphID =
  do
    i <- get
    put $ i + 1
    return $ Num (G.Int i)

toDotGraph :: S.SemanticArrow (S.ASem Text) eff b c
        -> State FlowChartState (
                                  Fix (Connector (DotNode Int)),
                                  Fix (Connector (DotNode Int))
                                )
toDotGraph (S.Pure f) =
  do
    n <- getNewNode nodeAttr
    return (Fix (Basic n), Fix (Basic n))

toDotGraph (S.Effect f) =
  do
    n <- getNewNode nodeAttr
    return (Fix (Basic n), Fix (Basic n))

toDotGraph (S.Seq a1 a2) =
  do
    (a, b1) <- toDotGraph a1
    (b2, c) <- toDotGraph a2
    connect b1 b2
    return (a, c)

toDotGraph (S.Par a1 a2) =
  do
    (a1, b1) <- toDotGraph a1
    (a2, b2) <- toDotGraph a2
    return (Fix (Parallel a1 a2),
            Fix (Parallel b1 b2))

toDotGraph (S.Spl a1 a2) =
  do
    (a1, b1) <- toDotGraph a1
    (a2, b2) <- toDotGraph a2
    return (Fix (Split a1 a2),
            Fix (Split b1 b2))

toDotGraph (S.FanIn a1 a2) =
  do
    (a1, b1) <- toDotGraph a1
    (a2, b2) <- toDotGraph a2
    n <- getNewNode nodeAttr
    let b = Fix (Basic n)
    connect b1 b
    connect b2 b
    return (Fix (Split a1 a2), b)

toDotGraph ((S.:?) a w) =
  do
    case w of
      S.Tag lbl -> newCluster $ attr lbl -- [G.Label $ G.StrLabel lbl]
      _         -> newCluster []

    (a1, b) <- toDotGraph a
    modify popCluster

    return (a1, b)
  where
    attr lbl =
      [
        G.Label $ G.StrLabel $ TL.fromStrict lbl,
        G.Style [G.SItem G.Filled []],
        G.FontName "consolas bold",
        G.FillColor (G.toColorList [G.toColor White]),
        G.PenWidth 2.0
      ]

leaves :: Fix (Connector n) -> [n]
leaves = cata alg
  where
    alg (Basic x) = [x]
    alg (Split xs ys) = xs ++ ys
    alg (Parallel xs ys) = xs ++ ys

cnct :: DotNode n -> DotNode n -> DotEdge n
cnct x y = DotEdge (nodeID x) (nodeID y) [G.PenWidth 2.0]

connect :: Fix (Connector (DotNode Int))
        -> Fix (Connector (DotNode Int))
        -> State FlowChartState ()
connect x y =
  do
    s <- get
    let g' = connectGraph y x $ graph s
    put $ s{graph = g'}

connectGraph ::  Ord n
              => Fix (Connector (DotNode n))
              -> Fix (Connector (DotNode n))
              -> DotGraph n
              -> DotGraph n
connectGraph (Fix (Basic x)) c2 g =
  foldl (\g' y -> addDotEdge (cnct x y) g') g $ leaves c2

connectGraph c1 (Fix (Basic y)) g =
  foldl (\g' x -> addDotEdge (cnct x y) g') g $ leaves c1

connectGraph (Fix (Split x1 x2)) (Fix (Split y1 y2)) g =
  (connectGraph x1 y1 . connectGraph x2 y2) g

connectGraph (Fix (Parallel x1 x2)) (Fix (Parallel y1 y2)) g =
  (connectGraph x1 y1 .  connectGraph x2 y2) g

connectGraph (Fix (Split x1 x2)) (Fix (Parallel y1 y2)) g =
  (connectGraph x1 y1 .  connectGraph x2 y2) g

connectGraph (Fix (Parallel x1 x2)) (Fix (Split y1 y2)) g =
  (connectGraph x1 y1 .  connectGraph x2 y2) g


egr = Gc.DotGraph False True Nothing
    (Gc.DotStmts [Gc.GraphAttrs [G.RankDir G.FromLeft]] [] [] [])
f0 = FlowChartState 0 (unsafeFromCanonical egr) 1 []
