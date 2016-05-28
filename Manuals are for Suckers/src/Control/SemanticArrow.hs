{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Control.SemanticArrow where

import Control.Applicative
import Control.Monad ((>=>), liftM)
import Control.Arrow
import Control.Monad.State
import Control.Monad.Except

import qualified Control.Category as C
import Control.Monad.Trans.Class (lift)
import Data.Maybe(fromMaybe)
import Data.Monoid((<>))
import Data.NestedList
import Data.String(IsString(..))

import Data.Text(Text)
import qualified Data.Text as T

data SemanticArrow a eff b c where
  Pure    :: (b -> c)
          -> SemanticArrow a eff b c

  Effect  :: eff b c
          -> SemanticArrow a eff b c

  Seq     :: SemanticArrow a eff b c
          -> SemanticArrow a eff c d
          -> SemanticArrow a eff b d

  Par     :: SemanticArrow a eff b1 c1
          -> SemanticArrow a eff b2 c2
          -> SemanticArrow a eff (b1,b2) (c1,c2)

  Spl     :: SemanticArrow a eff b1 c1
          -> SemanticArrow a eff b2 c2
          -> SemanticArrow a eff (Either b1 b2) (Either c1 c2)

  FanIn   :: SemanticArrow a eff b1 c
          -> SemanticArrow a eff b2 c
          -> SemanticArrow a eff (Either b1 b2) c

  (:?)    :: SemanticArrow a eff b c
          -> a b c
          -> SemanticArrow a eff b c

instance C.Category (SemanticArrow a eff) where
  id = Pure id
  (.) = flip Seq

instance Arrow (SemanticArrow a eff) where
  arr = Pure
  first f = Par f C.id
  second = Par C.id
  (***) = Par

instance ArrowChoice (SemanticArrow a eff) where
  (|||) = FanIn
  (+++) = Spl
  left f = Spl f C.id
  right = Spl C.id

instance Functor (SemanticArrow a eff b) where
  fmap f a = a >>> Pure f

instance Applicative (SemanticArrow a eff b) where
  pure x = Pure (const x)
  (<*>) f x = Pure (\x0 -> (x0,x0))
              >>> (f *** x)
              >>> Pure (\(fxn, y) -> fxn y)

instance Monoid (SemanticArrow a eff b b) where
  mempty = Pure id
  mappend = Seq


ifA :: SemanticArrow w eff a Bool
  -> SemanticArrow w eff a c
  -> SemanticArrow w eff a c
  -> SemanticArrow w eff a c
ifA aCh a1 a2 = (aCh &&& Pure id)
          >>> Pure (\(b, x) -> if b then Left x else Right x)
          >>> (a1 ||| a2)

--------------------------------------------------------------------------------
-- ASem - a type to hold semantic information
--------------------------------------------------------------------------------
data ASem e b c where
  Tag     :: Text         -- ^ freeform text label
          -> ASem e b c

  ShowIn  :: Text         -- ^ freeform text
          -> (b -> Text)  -- ^ showable text
          -> ASem e b c

  ShowOut :: Text         -- ^ freeform text
          -> (c -> Text)  -- ^ showable text
          -> ASem e b c

  ShowIO  :: Text         -- ^ freeform text
          -> (b -> Text)  -- ^ showable text
          -> (c -> Text)  -- ^ showable text
          -> ASem e b c

  TagEntity :: e
            -> ASem e b c

instance IsString (ASem e b c) where
  fromString = Tag . T.pack

reportTxt :: ASem e b c -> Maybe (b -> Text, b -> c -> Text)
reportTxt (Tag txt)         = justLbl txt
reportTxt (ShowIn txt _)    = justLbl txt
reportTxt (ShowOut txt _)   = justLbl txt
reportTxt (ShowIO txt _ _)  = justLbl txt
reportTxt (TagEntity _)     = Nothing

justLbl txt = Just (const $ "Enter : " <> txt, const $ const $ "Exit  : " <> txt)

(???) :: SemanticArrow (ASem e) eff b c -> Text -> SemanticArrow (ASem e) eff b c
(???) a tag = a :? Tag tag

(>??) :: (Show b) => SemanticArrow (ASem e) eff b c -> Text -> SemanticArrow (ASem e) eff b c
(>??) a tag = a :? ShowIn tag (T.pack . show)

(??>) :: (Show c) => SemanticArrow (ASem e) eff b c -> Text -> SemanticArrow (ASem e) eff b c
(??>) a tag = a :? ShowOut tag (T.pack . show)

(>?>) :: (Show b, Show c) => SemanticArrow (ASem e) eff b c -> Text -> SemanticArrow (ASem e) eff b c
(>?>) a tag = a :? ShowIO tag (T.pack . show) (T.pack . show)

(@@@) :: SemanticArrow (ASem e) eff b c -> e -> SemanticArrow (ASem e) eff b c
(@@@) a ent = a :? TagEntity ent

spliceAfter :: (forall b' c' . SemanticArrow w eff b' c' -> Bool)
        -> SemanticArrow w eff () ()
        -> SemanticArrow w eff b c
        -> SemanticArrow w eff b c
spliceAfter check spl x
  | check x = x >>> Pure (\x' -> (x',()))
                >>> Pure id *** spl
                >>> Pure fst
  | otherwise = x

recSpliceAfter :: (forall b' c' . SemanticArrow w eff b' c' -> Bool)
                  -> SemanticArrow w eff () ()
                  -> SemanticArrow w eff b c
                  -> SemanticArrow w eff b c
recSpliceAfter check spl = recurse (spliceAfter check spl)

recurse :: (forall b' c' . SemanticArrow w eff b' c' -> SemanticArrow w eff b' c')
          -> SemanticArrow w eff b c
          -> SemanticArrow w eff b c
recurse fxn a@(Pure g)    = fxn a
recurse fxn a@(Effect g)  = fxn a
recurse fxn (Seq a1 a2) = fxn $ Seq (recurse fxn a1) (recurse fxn a2)
recurse fxn (Par a1 a2) = fxn $ Par (recurse fxn a1) (recurse fxn a2)
recurse fxn (Spl a1 a2) = fxn $ Spl (recurse fxn a1) (recurse fxn a2)
recurse fxn (FanIn a1 a2) = fxn $ FanIn (recurse fxn a1) (recurse fxn a2)
recurse fxn (a :? w) = fxn $ recurse fxn a :? w

liftK :: Monad m => (b -> m c) -> SemanticArrow a (Kleisli m) b c
liftK eff = Effect (Kleisli $ \x -> eff x)

liftMtoSA = Effect . Kleisli . const

runA :: Monad m => SemanticArrow a (Kleisli m) b c -> Kleisli m b c
runA (Pure f)      = Kleisli (return . f)
runA (Effect k)    = k
runA (Seq a1 a2)   = runA a1 >>> runA a2
runA (Par a1 a2)   = runA a1 *** runA a2
runA ((:?) a1 _)   = runA a1
runA (FanIn a1 a2) = runA a1 ||| runA a2 -- Kleisli $
runA (Spl a1 a2)   = runA a1 +++ runA a2


runAwLog :: Monad m
  => SemanticArrow w (Kleisli m) b c
  -> (forall b' c' . w b' c' -> Maybe (b' -> z, b' -> c' -> z))
  -> b
  -> StateT (NestedListZipper z) m c
runAwLog (Pure g) _ = return . g
runAwLog (Effect k) _ = lift . runKleisli k
runAwLog (Seq a1 a2) f = runAwLog a1 f >=> runAwLog a2 f
runAwLog (Par a1 a2) f = \(x1,x2) -> do
  y1 <- runAwLog a1 f x1
  y2 <- runAwLog a2 f x2
  return (y1,y2)

runAwLog (FanIn a1 a2) f = \x ->
  case x of
    Left x'   -> runAwLog a1 f x'
    Right x'' -> runAwLog a2 f x''

runAwLog (Spl a1 a2) f = \x ->
  case x of
    Left x'   -> Left <$> runAwLog a1 f x'
    Right x'' -> Right <$> runAwLog a2 f x''

runAwLog ((:?) a w) f = \x ->
  case f w of
    Nothing          -> runAwLog a f x
    Just (pre, post) -> do
      modify (`push` pre x)
      y <- runAwLog a f x
      modify (`insert` post x y)
      modify (\z -> fromMaybe z $ up z)
      return y

liftGather :: Monad m => ExceptT e m a -> ExceptT [e] m a
liftGather m = ExceptT $
  do
    res <- runExceptT m
    return $ case res of
      Left err -> Left [err]
      Right x  -> Right x

gatherErr :: Monad m
  => SemanticArrow a (Kleisli (ExceptT e m)) b c
  -> b
  -> (ExceptT [e] m)  c
gatherErr (Pure f) = return . f
gatherErr (Effect k) = liftGather . runKleisli k
gatherErr (Seq a1 a2) = \x -> gatherErr a1 x >>= gatherErr a2
gatherErr (Par a1 a2) = \(x,y) -> do
  x' <- catchError (gatherErr a1 x)
      (\es -> do
                catchError (gatherErr a2 y)
                  (\es' -> throwError $ es ++ es')
                throwError es
      )
  y' <- gatherErr a2 y
  return (x', y')
gatherErr (Spl a1 a2) =
    \x -> case x of
        Left x'   -> do{ y <- gatherErr a1 x' ; return $ Left y}
        Right x'' -> do{ y <- gatherErr a2 x''; return $ Right y}
gatherErr ((:?) a1 _) = gatherErr a1
gatherErr (FanIn a1 a2) =
    \x -> case x of
        Left x' ->   gatherErr a1 x'
        Right x'' -> gatherErr a2 x''
