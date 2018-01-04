module Main where

import Prelude
import Data.Map as M
import Data.Set as S
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Parallel (parApply, parTraverse)
import Data.Const (Const(..))
import Data.List (List(..), catMaybes, head, (:), fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))


main :: forall e. Eff (console :: CONSOLE | e) _
main = launchAff $ monadicProgram testInterpreter

newtype KVStore f = KVStore 
  { put :: String -> String -> f Unit
  , get :: String -> f (Maybe String)
  }

infixl 4 parApply as <&>

type Program alg a = forall f. Applicative f => alg f -> f a


program :: forall f. Apply f => String -> KVStore f -> f (List String)
program mouse (KVStore k) = (\f s _ t -> catMaybes (f : s : t : Nil)) <$>
  k.get "Cats" <*> k.get "Dogs" <*> k.put "Mice" mouse <*> k.get "Cats"

type OptimizerReqs alg f m = Monoid m =>
  { extract :: alg (Const m)
  , rebuild :: m -> alg f -> f (alg f)
  }

class (Monad f, Monoid m) <= Optimizer alg f m | alg -> f , f -> m where
  reqs :: OptimizerReqs alg f m

optimize :: forall alg f m a. Optimizer alg f m
         => Program alg a
         -> alg f
         -> f a
optimize prog interpreter =
  let (Const m2) = prog (reqs :: OptimizerReqs alg f m).extract
  in (reqs.rebuild m2 interpreter) >>= prog

helper :: forall alg f a m. Monoid m => OptimizerReqs alg f m -> Program alg a -> m
helper reqs prog = case (prog reqs.extract) of 
  Const m -> m

extract :: KVStore (Const (S.Set String))
extract = KVStore 
  { get : \key -> Const $ S.singleton key
  , put : \_ _ -> Const $ S.empty
  }

rebuild :: forall e. S.Set String -> KVStore (Aff e) -> Aff e (KVStore (Aff e))
rebuild gs (KVStore interp) = 
  precomputed <#> (\m -> KVStore $ interp
        { get = \key -> case (M.lookup key m) of
            Just a -> pure $ Just a
            Nothing -> interp.get key
        })
  where 
    tupleList :: Aff e (List (Maybe (Tuple String String)))
    tupleList =
          parTraverse (\key -> interp.get key <#> (\m -> m <#> \s -> key /\ s)) (fromFoldable gs)
    precomputed :: Aff e (M.Map String String)
    precomputed = tupleList <#> (M.fromFoldable <<< catMaybes)


instance kvStoreAffOptimizer :: Optimizer KVStore (Aff e) (S.Set String) where
  reqs = { extract , rebuild }


monadicProgram :: forall f m. Optimizer KVStore f m => KVStore f -> f Unit
monadicProgram (KVStore k) = do
  mouse <- k.get "Mice"
  list <- optimize (program $ fromMaybe "64" mouse) (KVStore k)
  k.put "Birds" (fromMaybe "128" (head list))

testInterpreter :: forall e. KVStore (Aff e)
testInterpreter = KVStore
  { put : \_ value -> do
      liftEff $ unsafeCoerceEff $ log $ "Put something " <> value
      pure unit
  , get : \key -> do
      liftEff $ unsafeCoerceEff $ log $ "Hit network for " <> key
      pure $ Just $ key <> "!"
  }