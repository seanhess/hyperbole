module Web.Hyperbole.Effect.Javascript where

import Data.Aeson as A
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics


-- this isn't a response!
-- we don't currently have a way to call the client directly...
-- Encoded: Haskell ACTIONS that are callable from the client
-- Same thing? Trigger ACTIONS on the client?
-- no.... there's no return type

-- we want to help them easily create the return type
-- but it should probably be simple records or primitives
execute :: (FromJSON result) => Javascript result -> IO result
execute js = do
  print js -- pretend to execute it

  -- hmmm......
  -- let enc = Encoded "Something" []
  case A.eitherDecode "WOOT" of
    Right a -> pure a
    Left e -> fail (cs e)


-- js0 :: (FromJSON res) => Text -> IO res
-- js0 con = runJS $ JSFunction con []
--
--
-- js1 :: (FromJSON res) => Text -> Value -> IO res
-- js1 con val = runJS $ JSFunction con [val]
--
--
-- js2 :: (ToJSON a, ToJSON b, FromJSON res) => Text -> a -> b -> IO res
-- js2 con a b = runJS $ JSFunction con [toJSON a, toJSON b]

-- So effectively, I am saying that I will return JSON always
-- That's a nice simplifying assumption
--
--

data Javascript a = Javascript
  { functionName :: Text
  , arguments :: [Value] -- see how it serializes to values...
  }
  deriving (Show, Generic, ToJSON)


data User = User Text Int
  deriving (Generic, FromJSON, Show)


createUser :: Text -> Int -> Javascript User
createUser = foreignJS "createUser"


sum :: Int -> String -> Int -> Javascript String
sum = (foreignJS @(Int -> Int -> String -> Int -> Javascript String) "concat") 1


-- you can split up the function
-- has to exist on window
consoleLog :: Text -> Javascript ()
consoleLog = foreignJS "console.log"


class ForeignJS a where
  -- type Inputs a :: Type
  foreignJS :: Text -> a


-- wait you don't have a way of defining the name! Dumb!
-- it probably isn't a sum type
-- and we don't care how the information is encoded
-- so use ToJSON
instance (ToJSON a) => ForeignJS (a -> Javascript res) where
  foreignJS name a = Javascript name [toJSON a]


instance (ToJSON a, ToJSON b) => ForeignJS (a -> b -> Javascript res) where
  foreignJS name a b = Javascript name [toJSON a, toJSON b]


instance (ToJSON a, ToJSON b, ToJSON c) => ForeignJS (a -> b -> c -> Javascript res) where
  foreignJS name a b c = Javascript name [toJSON a, toJSON b, toJSON c]


instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ForeignJS (a -> b -> c -> d -> Javascript res) where
  foreignJS name a b c d = Javascript name [toJSON a, toJSON b, toJSON c, toJSON d]


exampleHandler :: IO ()
exampleHandler = do
  res :: User <- execute $ createUser "henry" 2
  -- execute $ consoleLog "HELLO"
  print res


-- well, in this case, the server is saying it knows something about the client
-- that's not true normally
-- why would we ever have a sum type?
--
--
-- javascript
--
-- type User = { name: string, age: number }
--
-- so what if the key names change?
-- yeah it's probably easier if we don't worry abou tthose?
--
-- Or Just require it to be a ToJSON?
--
-- function createUser(name:string, age:number):User {
--    return {name, age}
--    return Promise.fulfill({name, age})
-- }
--
-- It doesn't really matter what the FORMAT is...
-- for this API, because the user won't really see it
-- hmmm
-- except that we want THEM to be able to call {name: "wahoo", value: "ok"} and have it parse
-- they'll call JSON.stringify?
-- Or do we force them to always use positional arguments?
--
-- For the top-level? Why not?
-- Or we allow them to do either...
--
-- Advantage of ToJSON
--
-- return "SomeConstructor" -- that's normal JSONable strings, easy
-- return {name: "woot", age: 12} -- also normal => User { name, age }
-- return "hello" -- just a string
--
-- The ONLY downside or difference is with ADTs that have multiple constructors
-- Which is not common for return values
--
--
-- Could we use the standard ToJSON instance and just RENDER it differently?
--
-- Object [("tag", "Hello"), ("field", "y")]
--
-- data Hello = Hello Text
--
-- Automatically serializes to "tag"
--
-- And if it encounters "Hello 3" it serializes it to.... no idea, because we don't have the selector names
--
-- λ> data Boot = Boot Int String | Henry Int deriving (Generic, ToJSON, Show)
-- λ> encode $ Boot 23 "hello"
-- "{\"contents\":[23,\"hello\"],\"tag\":\"Boot\"}"
--
-- λ> data Boot = Boot Int String deriving (Generic, ToJSON, Show)
-- λ> encode $ Boot 23 "hello"
-- "[23,\"hello\"]"
--
-- λ> data Woot = Woot { name :: Int, value :: String } | Bob deriving (Generic, ToJSON, Show)
-- λ> encode $ Woot 1 "hello"
-- "{\"name\":1,\"tag\":\"Woot\",\"value\":\"hello\"}"
--
--
-- {tag: con, a,b,c}
-- {tag: con, contents:{a,b,c}} -- this is very predictable!
-- [con, {a, b, c}]
--

-- this is consistent!

data Boot
  = Boot Int String
  | Henry Int
  | Bob
  deriving (Generic, Show)


instance ToJSON Boot where
  toJSON = genericToJSON defaultOptions{sumEncoding = ObjectWithSingleField, tagSingleConstructors = True}


data Wahoo
  = Wahoo Int String
  deriving (Generic, Show)


instance ToJSON Wahoo where
  toJSON = genericToJSON defaultOptions{sumEncoding = ObjectWithSingleField, tagSingleConstructors = True}


data MrWiggles
  = MrWiggles {activity :: String}
  deriving (Generic, Show)


instance ToJSON MrWiggles where
  toJSON = genericToJSON defaultOptions{sumEncoding = ObjectWithSingleField, tagSingleConstructors = True}


test :: IO ()
test = do
  print $ toJSON $ Boot 2 "hello"
  print $ toJSON $ Henry 3
  print $ toJSON Bob
  print $ toJSON $ Wahoo 3 "wahooo"
  print $ toJSON $ MrWiggles "wiggling"
