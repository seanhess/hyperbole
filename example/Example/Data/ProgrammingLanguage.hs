module Example.Data.ProgrammingLanguage where

import Data.Text (Text, isInfixOf, toLower)
import Web.Hyperbole (FromParam, ToParam)

data ProgrammingLanguage = ProgrammingLanguage
  { family :: LanguageFamily
  , name :: Text
  , features :: [TypeFeature]
  , description :: Text
  }
  deriving (Show, Read)
instance Eq ProgrammingLanguage where
  p1 == p2 = p1.name == p2.name

data LanguageFamily
  = ObjectOriented
  | Functional
  deriving (Show, Read, Eq, Ord, ToParam, FromParam)

data TypeFeature
  = Dynamic
  | Typed
  | Generics
  | TypeClasses
  | TypeFamilies
  deriving (Show, Read, Eq, ToParam, FromParam)

isMatchLanguage :: Text -> ProgrammingLanguage -> Bool
isMatchLanguage term p =
  isInfixOf (toLower term) . toLower $ p.name

allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ ProgrammingLanguage ObjectOriented "JavaScript" [Dynamic] "A versatile scripting language mainly used for web development."
  , ProgrammingLanguage ObjectOriented "Java" [Typed] "A robust, platform-independent language commonly used for enterprise applications."
  , ProgrammingLanguage ObjectOriented "TypeScript" [Typed, Generics] "A superset of JavaScript that adds static typing."
  , ProgrammingLanguage ObjectOriented "Python" [Dynamic] "A beginner-friendly language with a wide range of applications, from web to data science."
  , ProgrammingLanguage ObjectOriented "PHP" [Dynamic] "A server-side scripting language primarily used for web development."
  , ProgrammingLanguage ObjectOriented "Go" [Typed, Generics] "A statically typed, compiled language designed for simplicity and efficiency."
  , ProgrammingLanguage ObjectOriented "C++" [Typed] "A powerful language for system programming, game development, and high-performance applications."
  , ProgrammingLanguage ObjectOriented "C#" [Typed, Generics] "A language developed by Microsoft, widely used for developing Windows and web applications."
  , ProgrammingLanguage ObjectOriented "Objective-C" [Typed] "A language used primarily for macOS and iOS application development before Swift."
  , ProgrammingLanguage ObjectOriented "Rust" [Typed, Generics, TypeClasses, TypeFamilies] "A memory-safe language focused on performance and reliability."
  , ProgrammingLanguage ObjectOriented "Ruby" [Dynamic] "A dynamic language known for its simplicity and used in web frameworks like Ruby on Rails."
  , ProgrammingLanguage ObjectOriented "Swift" [Typed, Generics] "A modern language for iOS and macOS application development."
  , ProgrammingLanguage Functional "Haskell" [Typed, Generics, TypeClasses, TypeFamilies] "An elegant functional language for those with excellent taste."
  , ProgrammingLanguage Functional "Elm" [Typed, Generics] "A functional language for building reliable web front-end applications."
  , ProgrammingLanguage Functional "Scheme" [Dynamic] "A minimalist, functional dialect of Lisp."
  ]
