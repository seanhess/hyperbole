module Web.Hyperbole.Effect.Javascript where

import Data.Aeson as A
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import GHC.Generics
import Web.Hyperbole.Effect.Hyperbole (Hyperbole)


-- A function to execute on the client
data Javascript a = Javascript
  { functionName :: JSFunction
  , arguments :: [Value] -- all json parameters
  }
  deriving (Show, Generic, ToJSON)


newtype JSFunction = JSFunction Text
  deriving newtype (ToJSON, Show, IsString)


execute :: (FromJSON result, Hyperbole :> es) => Javascript result -> Eff es result
execute js = do
  -- TODO: execute the JS on the client
  -- let res = window[js.functionName].apply(js.arguments)
  -- sendBackToServer(JSON.stringify(res))
  res <- remoteExecute js

  -- decode the text result as JSON
  -- the client will execute
  case A.eitherDecode (cs res) of
    Right a -> pure a
    Left _ -> error "Throw/Send an error"
 where
  -- pretend to execute it
  remoteExecute :: Javascript a -> Eff es Text
  remoteExecute _ = pure "\"hello world\""


-- Example: createUser(name, age):  User {...}
data User = User Text Int
  deriving (Generic, FromJSON, Show)


-- FFI: apply the functions to create a fully applied Javascript User
createUser :: Text -> Int -> Javascript User
createUser = foreignJS "createUser"


-- Example sum:
sum :: Int -> Int -> Javascript String
sum = foreignJS "sum"


noEval :: Int -> Javascript String
noEval = foreignJS "() => console.log('this will not run, it has to be a member of window[]')"


-- This will work, because  it isn't arbitrary code. We can look for the function on window after splitting on "."
consoleLog :: Text -> Javascript ()
consoleLog = foreignJS "console.log"


-- Generic remote JS call to be serialized and executed by the client
class ForeignJS a where
  foreignJS :: JSFunction -> a


instance (ToJSON a) => ForeignJS (a -> Javascript res) where
  foreignJS name a = Javascript name [toJSON a]


instance (ToJSON a, ToJSON b) => ForeignJS (a -> b -> Javascript res) where
  foreignJS name a b = Javascript name [toJSON a, toJSON b]


instance (ToJSON a, ToJSON b, ToJSON c) => ForeignJS (a -> b -> c -> Javascript res) where
  foreignJS name a b c = Javascript name [toJSON a, toJSON b, toJSON c]


instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ForeignJS (a -> b -> c -> d -> Javascript res) where
  foreignJS name a b c d = Javascript name [toJSON a, toJSON b, toJSON c, toJSON d]


exampleHandler :: (Hyperbole :> es, IOE :> es) => Eff es ()
exampleHandler = do
  res :: User <- execute $ createUser "henry" 2
  liftIO $ print res
