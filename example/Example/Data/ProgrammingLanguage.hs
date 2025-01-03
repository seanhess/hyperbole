module Example.Data.ProgrammingLanguage where

import Data.Text (Text, isInfixOf, toLower)

data ProgrammingLanguage = ProgrammingLanguage
  { family :: LanguageFamily
  , name :: Text
  , description :: Text
  }
  deriving (Show, Read)
instance Eq ProgrammingLanguage where
  p1 == p2 = p1.name == p2.name

data LanguageFamily
  = ObjectOriented
  | Functional
  deriving (Show, Read, Eq, Ord)

isMatchLanguage :: Text -> ProgrammingLanguage -> Bool
isMatchLanguage term p =
  isInfixOf (toLower term) . toLower $ p.name

allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ ProgrammingLanguage ObjectOriented "JavaScript" "A versatile scripting language mainly used for web development."
  , ProgrammingLanguage ObjectOriented "Java" "A robust, platform-independent language commonly used for enterprise applications."
  , ProgrammingLanguage ObjectOriented "TypeScript" "A superset of JavaScript that adds static typing."
  , ProgrammingLanguage ObjectOriented "Python" "A beginner-friendly language with a wide range of applications, from web to data science."
  , ProgrammingLanguage ObjectOriented "PHP" "A server-side scripting language primarily used for web development."
  , ProgrammingLanguage ObjectOriented "Go" "A statically typed, compiled language designed for simplicity and efficiency."
  , ProgrammingLanguage ObjectOriented "C++" "A powerful language for system programming, game development, and high-performance applications."
  , ProgrammingLanguage ObjectOriented "C#" "A language developed by Microsoft, widely used for developing Windows and web applications."
  , ProgrammingLanguage ObjectOriented "Objective-C" "A language used primarily for macOS and iOS application development before Swift."
  , ProgrammingLanguage ObjectOriented "Rust" "A memory-safe language focused on performance and reliability."
  , ProgrammingLanguage ObjectOriented "Ruby" "A dynamic language known for its simplicity and used in web frameworks like Ruby on Rails."
  , ProgrammingLanguage ObjectOriented "Swift" "A modern language for iOS and macOS application development."
  , ProgrammingLanguage Functional "Haskell" "An elegant functional language for those with excellent taste."
  , ProgrammingLanguage Functional "Elm" "A functional language for building reliable web front-end applications."
  , ProgrammingLanguage Functional "Scheme" "A minimalist, functional dialect of Lisp."
  ]
