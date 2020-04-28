# Kalyn

An attempt to write a compiler from a Haskell-like Lisp directly to
x86-64 without using any pre-existing components such as GCC or the C
standard library.

## Language specification
### Data types

* Integer (signed, 64-bit): `Int`
* Function: `Func a b`, automatically curried in type signatures
* User-defined ADTs
* Input/output monad: `IO a`

### Syntax

* Integer literals: `42`, `0x2a`, `0o52`
* (Virtual) function calls: `()`
* List literals: `[]`

### Indentation

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

### Built-ins

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
* `shl :: Func Int Int Int`
* `shr :: Func Int Int Int`
* `sal :: Func Int Int Int`
* `sar :: Func Int Int Int`

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

## Data representation

All data types use a "boxed" representation in memory. This means that
every data type is stored as a single word. (As Kalyn supports only
64-bit systems, the term "word" means 64 bits or equivalently eight
bytes.) If a data type fits into a single word, objects of that type
are stored as is. Otherwise, an object is stored as a pointer to a
heap-allocated region of memory containing the object's data. Each
field in the object's data is one word; if a field contains an object
that does not fit into a single word, then that object is again stored
as a pointer.

* Integers are stored as a single word.
* Functions start with a pointer to the absolute address in memory of
  the code for the function. Then they have one word for each lexical
  variable in the closure of the function, preceded by a word
  indicating the number of these fields. All functions are
  single-argument in the runtime; a multiple-argument lambda is really
  just a single-argument lambda whose closure has N variables and
  whose code returns a new single-argument lambda whose closure has
  N+1 variables.
* User-defined ADTs have a header word whose value as an integer
  indicates which of the data constructors is in use, indexed from
  zero. If there is only one data constructor then this word is
  omitted. After the header comes one word for each field for the
  relevant data constructor. Note that this means that wrapping an
  integer in a single-constructor ADT will not incur any overhead.
* The IO monad is just a pointer to a function that performs the IO
  action and then returns a value of the type parameterizing the
  monad.

Note that knowing the compile-time type of an object guarantees that
you know its size and memory layout, except for functions (whose size
you can determine by inspecting the second word of their data).

## Calling convention

There are two types of functions: top-level functions and function
objects.

Top-level functions include all bindings that are made at the top
level, even ones which are not of function type. To obtain the value
of a top-level binding, you invoke its top-level function. This is
done by simply calling it directly. The value is produced in `%rax`.
Note that this means multiple uses of a top-level binding will
recompute its value. This may be optimized later.

Function objects are produced by evaluating `lambda` expressions. A
`defn` declaration results in a top-level function which returns a
function object. Function objects, as discussed in the section on data
representation, include a lexical closure as a list of implicit
arguments. To call a function object, you push its arguments onto the
stack, starting with the lexical closure in the order in which it
appears in memory and then followed by the single argument used at the
current call. The return value is produced in `%rax`, and the stack
pointer is left pointing at the last argument pushed.
