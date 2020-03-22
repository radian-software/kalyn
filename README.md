# Kalyn

An attempt to write a compiler from a Haskell-like Lisp directly to
x86-64 without using any pre-existing components such as GCC or the C
standard library.

**Specification for the Kalyn programming language.**

## Data types

* Integer (signed, 64-bit): `Int`
* Function: `Func a b`, automatically curried in type signatures
* User-defined ADTs
* Input/output monad: `IO a`

## Syntax

* Integer literals: `42`, `0x2a`, `0o52`
* (Virtual) function calls: `()`
* List literals: `[]`

## Indentation

Align all code to column 0. All but the first elements of
round-parenthesis lists are indented two additional spaces from the
opening parenthesis. The first elements of round-parenthesis lists and
all elements of square-bracket lists are indented one space from the
opening parenthesis, unless they are on the same line as the opening
parenthesis in which case manually inserted whitespace is preserved.
Exceptions:

* The pattern-matching lists of `case` forms are indented as if they
  used square brackets.
* The binding lists of `let` forms are indented as if they used square
  brackets.

## Built-ins

Declarations:

* `alias`
* `class`
* `data`
* `def`
* `defn`
* `derive`
* `import`
* `instance`
* `public`
* `with`

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

Primitives (misc):

* `error :: Func String a`

Instances:

* `Eq Int`
    * `== :: Func Int Int Bool`
* `Ord Int`
    * `< :: Func Int Int Bool`
* `Monad IO`
    * `pure :: IO a`
    * `>>= :: Func (IO a) (Func a (IO b)) (IO b)`
