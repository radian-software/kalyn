**Specification for the Kalyn programming language.**

## Data types

* Integer (signed, 64-bit): `Int`
* Function: `Func a b`, automatically curried in type signatures
* User-defined ADTs
* Input/output monad: `IO a`

## Syntax

* Integer literals: `42`, `0x2a`, `0o52`
* Function calls: `()`
* List literals: `[]`

## Built-ins

Declarations:

* `alias`
* `class`
* `data`
* `def`
* `defn`
* `derive`
* `instance`
* `public`

Special forms:

* `case`
* `lambda`
* `let`

Primitives (math):

* `+ :: Func Int Int Int`
* `- :: Func Int Int Int`
* `* :: Func Int Int Int`
* `/ :: Func Int Int Int`
* `% :: Func Int Int Int`
* `& :: Func Int Int Int`
* `| :: Func Int Int Int`
* `^ :: Func Int Int Int`
* `~ :: Func Int Int`
* `<< :: Func Int Int Int`
* `>> :: Func Int Int Int`
* `>>> :: Func Int Int Int`

Primitives (IO):

* `print :: Func String (IO Empty)`
* `writeFile :: Func String String (IO Empty)`
* `setFileMode :: Func String Int (IO Empty)`

Instances:

* `Eq Int`
    * `== :: Func Int Int Bool`
* `Ord Int`
    * `< :: Func Int Int Bool`
* `Monad IO`
    * `pure :: IO a`
    * `>>= :: Func (IO a) (Func a (IO b)) (IO b)`
