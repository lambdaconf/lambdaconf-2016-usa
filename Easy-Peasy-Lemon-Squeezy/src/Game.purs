-- | This is the entry point to the game, and contains the main game logic.
module Main where
  import Prelude (Unit, (++))

  import Control.Monad.Eff(Eff())
  import Data.Either(Either(..))

  import Game.State (State)
  import Game.Input (Input(Drop, Take, Look), parse)
  import Game.Driver (GAME, Game, runGame)

  myGame :: Game State Input
  myGame = { initial: initial, describe: describe, parse: parse, update: update }
    where
      -- EXERCISE SET 4 (1/2)
      -- Add initial values to the newly-added fields in State:
      initial :: State
      initial = {}

      -- EXERCISE SET 4 (2/2)
      -- Describe the player's status and (optionally) stats.
      describe :: State -> String
      describe s = "You are standing no where, with nothing around"

      update :: State -> Input -> Either String State
      update s Look = Right s
      update s (Take v) = Left ("There is no " ++ v ++ " to take!")
      update s (Drop v) = Left ("You are not carrying a " ++ v ++ "!")

  main :: Eff ( game :: GAME ) Unit
  main = runGame myGame

-- | This is the game state. This is very bare bones at present. During the
-- | workshop, you will add additional state into here to make the game
-- | more interesting.
module Game.State where
  import Prelude ((++))

  -- EXERCISE SET 1 (1/2):
  -- data CharacterStatus = ???

  -- EXERCISE SET 1 (2/2):
  -- describeCharacterStatus :: CharacterStatus -> String

  -- EXERCISE SET 2 (1/3):
  -- data CharacterStats = ???

  -- EXERCISE SET 2 (2/3):
  -- startingStats :: CharacterStats

  -- EXERCISE SET 2 (3/3):
  -- healthOf :: CharacterStats -> Int
  -- strengthOf :: CharacterStats -> Int

  -- EXERCISE SET 3 (1/3):
  -- data Monster = ???

  -- EXERCISE SET 3 (2/3):
  -- bigBadWolf :: CharacterStats
  -- fearfulOgre :: CharacterStats

  -- EXERCISE SET 3 (3/3):
  -- monsterStrength :: Monster -> Int

  -- EXERCISE SET 4 (1/2)
  -- Add both playerStatus :: CharacterStatus and playerStats :: CharacterStats fields to State:
  type State = {}

  -- EXERCISE SET 5 (1/2)
  -- Define `defeats` type:
  -- defeats :: ???

  -- EXERCISE SET 5 (2/2)
  -- Implement `defeats` with lambda:
  -- defeats = ???

  -- EXERCISE SET 6 (1/2)
  -- newtype Health = ???
  -- newtype Strength = ???

  -- EXERCISE SET 7 (2/2)
  -- type StatsModifier = ???

  -- EXERCISE SET 8 (1/2)
  -- data Inventory a b = ???

  -- EXERCISE SET 8 (1/2)
  -- isEmpty :: ???

  -- EXERCISE SET 9 (1/2)
  -- type NonPlayerCharacter = ???
  -- type Item = ???
  -- type PlayerCharacter = ???

  -- EXERCISE SET 9 (2/2)
  -- getName :: ???
  -- getName r = r.name

  -- EXERCISE SET 10 (1/3)
  -- data Location = ???

  -- EXERCISE SET 10 (2/3)
  -- data Connection = ???

  -- EXERCISE SET 10 (3/3)
  -- gameMap :: List Connection

  -- EXERCISE SET 11 (1/3)
  -- class Describable a where
  --   ???

  -- EXERCISE SET 11 (2/3)
  -- data Weapon = ???

  -- EXERCISE SET 11 (3/3)
  -- instance describableWeapon :: Describable Weapon where
  --   ???

-- | This is the module responsible for parsing user input, and converting input
-- | into data types that are easier to work with than raw strings.
-- |
-- | You can add more commands to the game here.
module Game.Input
  ( Input(..)
  , parse
  ) where

  import Prelude (Unit, ($), (<*>), (++), return, (==), bind, void, (<<<))
  import Data.Either(Either(..))

  import Control.Alt((<|>))
  import Control.Apply((<*), (*>))

  import Data.Char as C

  import Text.Parsing.StringParser (Parser, ParseError(ParseError), runParser)
  import Text.Parsing.StringParser.Combinators (optional, many1)
  import Text.Parsing.StringParser.String (eof, anyChar, string)

  data Input
    = Look          -- Look around wherever the player is standing
    | Take String   -- Take the specified item from the environment
    | Drop String   -- Drop the specified item onto the environment

  trailingWs :: Parser Unit
  trailingWs = void <<< many1 $ string " "

  command :: forall a. String -> a -> Parser a
  command t a = string t *> (trailingWs <|> eof) *> return a

  word :: Parser String
  word = go "" <* (optional trailingWs) where
    go v = eof *> return v <|> do
      char <- anyChar
      if char == ' ' then return v else go (v ++ C.toString char)

  input :: Parser Input
  input = (look <|> take <|> drop) <* eof
    where
      look = command "look" Look

      take = command "take" Take <*> word

      drop = command "drop" Drop <*> word

  parse :: String -> Either String Input
  parse s = case runParser input s of
        Left (ParseError e) -> Left $ e
        Right i             -> Right i

-- | This is a low-level module that defines what a game is and how to run it.
-- | It can handle any type of game, as long as it fits the mold specified herein.
-- |
-- | You should not need to modify this file during the workshop.
module Game.Driver
  ( Game()
  , GAME()
  , runGame
  ) where

  import Prelude (Unit)
  import Control.Monad.Eff(Eff())
  import Data.Function(Fn2(), runFn2)
  import Data.Either(Either(), either)

  foreign import data GAME :: !

  type Game s i = {
    initial  :: s,
    describe :: s -> String,
    parse    :: String -> Either String i,
    update   :: s -> i -> Either String s }

  runGame :: forall s i. Game s i -> Eff (game :: GAME) Unit
  runGame g = runFn2 _runGame either g

  foreign import _runGame :: forall s i. Fn2 (forall a b c. (a -> c) -> (b -> c) -> (Either a b) -> c) (Game s i) (Eff (game :: GAME) Unit)
