module AbstractInterfaceExample where

open import Function
open import Data.Bool
open import Data.String

-- * One-parameter interface for the type `a' with only one method.

record VoiceInterface (a : Set) : Set where
  constructor voice-interface
  field say-method-of : a → String

open VoiceInterface

-- * An overloaded method.

say : {a : Set} → ⦃ _ : VoiceInterface a ⦄ → a → String
say ⦃ instance ⦄ = say-method-of instance

-- * Some data types.

data Cat : Set where
  cat : Bool → Cat

crazy! = true
plain-cat = false

-- | This cat is crazy?
crazy? : Cat → Bool
crazy? (cat x) = x

-- | A 'plain' dog.
data Dog : Set where
  dog : Dog

-- * Implementation of the interface (and method).

instance-for-cat : VoiceInterface Cat
instance-for-cat = voice-interface case where
  case : Cat → String
  case x with crazy? x
  ... | true = "meeeoooowwwww!!!"
  ... | false = "meow!"

instance-for-dog : VoiceInterface Dog
instance-for-dog = voice-interface $ const "woof!"

-- * and then:
--
-- say dog => "woof!"
-- say (cat crazy!) => "meeeoooowwwww!!!"
-- say (cat plain-cat) => "meow!"
--
