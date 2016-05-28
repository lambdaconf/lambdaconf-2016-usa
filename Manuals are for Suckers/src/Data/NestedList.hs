{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Data.NestedList where
import Control.Applicative((<$>), (<*>))
import Data.Monoid((<>))
import qualified Data.List as L

import Data.Text(Text)
import qualified Data.Text as T

data NestedList a = NL [(a, NestedList a)]

emptyZ :: NestedListZipper Text
emptyZ = toZip "Main"

test :: NestedListZipper Text
Just test = insert <$> (up (insert (insert (push (insert (toZip "hello") "world") "Jason") "William") "Adaska")) <*> (Just "!")

instance Show a => Show (NestedList a) where
  show (NL xs) = "[" ++ L.intercalate " ; " (fmap showTup xs) ++ "]"
    where
      showTup (x, el) = "(" ++ show x ++ "," ++ show el ++ ")"

instance Functor NestedList where
  fmap f (NL xs) = NL $ fmap g xs
    where
      g (y0, el) = (f y0, fmap f el)

flatten :: NestedList a -> (a -> b) -> (b,b) -> [b]
flatten (NL xs) f (dStart, dEnd) = L.concatMap g xs
  where
    g (y0, NL ys) = if null ys
      then [f y0]
      else f y0 : dStart : (flatten (NL ys) f (dStart, dEnd) ++ [dEnd])

fmapList :: (forall aa . [aa]->[aa]) -> NestedList a -> NestedList a
fmapList f (NL xs) = (NL . f . fmap g) xs
  where
    g (y0, el) = (y0, fmapList f el)

data NestedListCtx a =
  NLCtx0 [(a, NestedList a)] [(a, NestedList a)]
  | NLCtx [(a, NestedList a)] [(a, NestedList a)] a (NestedListCtx a)

data NestedListZipper a = NLZ {
  cursor :: (a, NestedList a),
  context ::  NestedListCtx a
}

toZip :: a -> NestedListZipper a
toZip x = NLZ (x, NL []) (NLCtx0 [] [])

fromZip :: NestedListZipper a -> NestedList a
fromZip (NLZ x (NLCtx0 xs ys)) = NL $ xs ++ (x:ys)
fromZip (NLZ x (NLCtx xs ys z ctx)) = fromZip $ NLZ (z, NL $ xs ++ (x:ys)) ctx

insert :: NestedListZipper a -> a -> NestedListZipper a
insert (NLZ (x, el) (NLCtx xs ys z ctx)) x' =
  NLZ (x', NL []) (NLCtx xs ((x, el):ys) z ctx)

insert (NLZ (x, el) (NLCtx0 xs ys)) x' =
  NLZ (x', NL []) (NLCtx0 xs ((x,el):ys))

up :: NestedListZipper a -> Maybe (NestedListZipper a)
up (NLZ z0@(x, el) (NLCtx xs ys z ctx)) = Just $ NLZ (z, NL $ xs ++ (z0:ys)) ctx
up (NLZ _ NLCtx0{}) = Nothing

push :: NestedListZipper a -> a -> NestedListZipper a
push (NLZ (x, NL els) ctx) x' = NLZ (x', NL []) (NLCtx [] els x ctx)

flattenHistory historyZ = history
  where
    el =  (fmapList reverse . fromZip) historyZ
    history = T.concat $ flatten el (<> " , ") ("[","]")

ppNlZ :: NestedListZipper Text -> Text
ppNlZ z = prettyPrint (-1) (T.empty, fmapList reverse $ fromZip z)

prettyPrint :: Int -> (Text, NestedList Text) -> Text
prettyPrint i (txt, NL xs) =
  T.replicate i "--" <> txt <> "\n" <> T.concat (prettyPrint (i+1) <$> xs)
