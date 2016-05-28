{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}


module Main where

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS

import qualified Control.SemanticArrow as S
import qualified Control.SemanticArrow.Flowchart as F
import qualified Data.NestedList as NL
import Control.SemanticArrow(SemanticArrow((:?)))
import Control.Arrow((+++), (***), (>>>))
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.GraphViz.Printing as Gp

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Control.Arrow(Kleisli, runKleisli)
import Data.Maybe(listToMaybe)

em :: Email
em = Email  "Please fill form"
            "jwadaska@hollandhart.com"
            "atticus@hollandhart.com"
            "App# : 12345"
            [at1]



at1 = Attachment "Form.pdf" BS.empty
at2 = Attachment "Form.doc" BS.empty

em' :: Email
em' = Email  "Please fill form"
            "jwadaska@hollandhart.com"
            "atticus@hollandhart.com"
            "Ser# : 12345"
            [at2]




action = fillLegalForm :? "Fill Legal Form"

main :: IO ()
main = do
  res <- runPlain action
  print res

main1 :: IO ()
main1 = do
  res <- runLog action
  case res of
    Left err -> print err
    Right (x, nlz) -> putStrLn $ T.unpack $ NL.ppNlZ nlz

main2 :: IO ()
main2 = do
  let fc = execState (F.toDotGraph action) F.f0
  writeFile "fillLegalForm.dot" $ TL.unpack $ Gp.renderDot $ Gp.toDot $ F.graph fc

main3 :: IO ()
main3 = do
  res <- runGather em action
  print res
  res' <- runGather em' action
  print res'

data Email = Email{
  subject :: Text,
  from    :: Text,
  to      :: Text,
  body    :: Text,
  attachments :: [Attachment]
} deriving Show

data Attachment = Attachment {
  atchName :: Text,
  atchData :: ByteString
} deriving Show

type M = ExceptT Text (ReaderT Email IO)
type SA b c = S.SemanticArrow (S.ASem Text) (Kleisli M) b c

runPlain :: SA () Attachment -> IO (Either Text Attachment)
runPlain sa = runReaderT (runExceptT $ runKleisli (S.runA sa) ()) em

runGather :: Email -> SA () Attachment -> IO (Either [Text] Attachment)
runGather eml sa = runReaderT (runExceptT (S.gatherErr sa ())) eml

runLog :: SA () Attachment
  -> IO (Either Text (Attachment, NL.NestedListZipper Text))
runLog sa = runReaderT (runExceptT $ runStateT (S.runAwLog sa S.reportTxt ()) NL.emptyZ) em

fillLegalForm :: SA () Attachment
fillLegalForm =
  S.Pure (const $ ((), ()))
  >>> (getFirstPdf :? "Get PDF" *** getAppNum   :? "Find App#")
  >>> S.Pure (uncurry fillDoc) :? "Insert Application # in PDF"

fillDoc :: Attachment -> Text -> Attachment
fillDoc at _ = at

getAppNum :: SA () Text
getAppNum =
  S.liftK (const $ body <$> ask) :? "Get Email Body"
  >>> S.liftK tryToGetAppNum     :? "Look for App# : XXX"

getFirstPdf :: SA () Attachment
getFirstPdf =
  S.liftK (const $ attachments <$> ask) :? "Get All Attachments"
  >>> S.Pure filterPdfs                 :? "Filter to Pdf docs"
  >>> S.liftK tryToGetHead              :? "Get First"

filterPdfs :: [Attachment] -> [Attachment]
filterPdfs = filter (T.isSuffixOf ".pdf" . atchName )

tryToGetHead :: Monad m => [a] -> ExceptT Text m a
tryToGetHead =
  maybe (throwError "could not find attachment") return . listToMaybe

tryToGetAppNum :: Monad m => Text -> ExceptT Text m Text
tryToGetAppNum x
  | T.isPrefixOf "App# :" x = return $ snd $ T.breakOn ":" x
  | otherwise = throwError "no App# in email body"
