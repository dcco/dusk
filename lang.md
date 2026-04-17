# Dusk Programming Language

## 1. Datatypes

### 1.1 Primitive Datatypes

#### Numerics and Booleans

Dusk has two numeric (signed) integer types `Int32` and `Int64`, and one 32-bit floating-point type `Float`. The type name `Int` may be used as an alias for `Int32`.

Integer literals may be written in decimal or hexadecimal notation. Appending an `l` or `L` to an integer literal will make it a 64-bit integer.
```
1023, -6          -- decimal
0x3A, 0xbb        -- hexadecimal
0l, 10000000000L  -- 64-bit integer
```

Float literals are written by including a decimal point anywhere in a number. Scientific notation is also available.
```
3.0, 609.       -- float
1.3e4, 766.E10  -- scientific notation
```

Dusk also has booleans with type `Bool`, with two literals:
```
true
false
```

#### Characters and Strings

Dusk also has ASCII characters of type `Char` that may be written using character literals with single quotes Ex: `'b'`. The following escape sequences may be used:

| Escape Sequence | ASCII | Description |
|:-----|:-------|:----------------------|
| `\0` | `0x00` | Null |
| `\a` | `0x07` | Bell |
| `\b` | `0x08` | Backspace |
| `\t` | `0x09` | Tab |
| `\n` | `0x0A` | New Line |
| `\v` | `0x0B` | Vertical Tab |
| `\f` | `0x0C` | Form Feed |
| `\r` | `0x0D` | Carriage Return |
| `\"` | `0x22` | Double Quote |
| `\'` | `0x27` | Single Quote |
| `\\` | `0x5C` | Backslash |
| `\xNN` | `0xNN` | - |

Character literals may also be written in pseudo-hexadecimal notation `0cNN` as well as by adding `c` or `C` to the end of an (unsigned) decimal value.
```
1c, 251C          -- decimal char
0cA0, 0c8C        -- hexadecimal char
```

Operations available on integers are generally available for characters as well, where they are treated as unsigned 8-bit integers.

Dusk also has a `String` type - a heap-allocated sequence of characters that also keeps track of its length. They are written using double quotes Ex: `"Hello world!"`.
```
"Hello world!"
"\"The quick brown fox\n jumps over the lazy dog.\""
```

#### Object IDs

All heap-allocated objects in Dusk have a unique "object id" of type `OID` associated with them which can be used for shallow equality of objects. (It is essentially the object's pointer value, but pointer arithmetic cannot be performed on it).

There are no literals to manually initialize an oid. An object's oid may be obtained using field syntax.

Ex:
```
obj.oid
```

### 1.2 Operators

From highest to lowest precedence, the following unary and binary operators are available in Dusk:
```
|_|     (unary)
-   !   (unary)
**
*   /   /.  %
+   -
=   !=  <   <=  >   >=
&&  ||
```

All binary operators are left-associative.

These operators have their standard meaning along with `**` for exponentiation, `/.` for floating-point division (distinct from `/` truncating division), and the unary `measure` operator `|_|` used for things like the absolute value, the length of a string, and later the size of arrays.

These operators may also be called as normal functions using their "explicit name", and they may be overloaded using this explicit name as well. A complete table of operators, explicit names, and what overloads are built-in to the language is given below:

| Operator | Explicit Name | Builtin Overloads | Description |
|:---------|:--------------|:------------------|:------------|
| `-` (Unary) | `neg` | `Int32`, `Int64`, `Float` | Numeric negation |
| `+` | `add` | `Int32`, `Int64`, `Float`, `Char`, `String` | Numeric addition/String concatenation |
| `-` | `sub` | `Int32`, `Int64`, `Float`, `Char` | Numeric subtraction |
| `*` | `mul` | `Int32`, `Int64`, `Float`, `Char` | Numeric multiplication |
| `/` | `div` | `Int32`, `Int64`, `Char` | Truncating numeric division |
| `/.` | `flDiv` | `Int32`, `Int64`, `Float`, `Char` | Numeric division |
| `%` | `mod` | `Int32`, `Int64`, `Char` | Numeric modulo/remainder |
| `**` | `expo` | `Int32`, `Int64`, `Float`, `Char` | Numeric exponentiation |
| `=`| `equals` | `Int32`, `Int64`, `Float`, `Bool`, `Char`, `String`, `OID` | Value Equality |
| `!=`| `neq` | `Int32`, `Int64`, `Float`, `Bool`, `Char`, `String`, `OID` | Value Inequality |
| `<`| `lt` | `Int32`, `Int64`, `Float`, `Char` | Less than comparison |
| `<=`| `leq` | `Int32`, `Int64`, `Float`, `Char` | Less than or equal comparison |
| `>=`| `gt` | `Int32`, `Int64`, `Float`, `Char` | Greater than comparison |
| `>=`| `geq` | `Int32`, `Int64`, `Float`, `Char` | Greater than or equal comparison |
| `!` (Unary) | `not` | `Bool` | Logical negation |
| `&&` | `and` | `Bool` | Logical conjunction |
| `\|\|` | `or` | `Bool` | Logical disjunction |
| `\|_\|` (Unary) | `measure` | `Int32`, `Int64`, `Float`, `String` | Absolute value / length |

Note that the `=`/`equals` function is NOT defined for all types. In fact for most types it is undefined, and must be explicitly defined. Shallow equality for heap-allocated data structures is performed through their OIDs.

### 1.3 Math

### 1.4 Tuples and Enums

#### Tuples

Dusk's most basic aggregate type is the tuple/enum type. Tuples are immutable, fixed-size lists of values (of possibly differing types). They may, for example, be used as the return type for a function returning multiple values. They are not heap-allocated, and thus care should be taken with using them to represent large objects.

Examples of tuple literals and their types include:
```
(16, false)        -- type (Int, Bool)
(4, "hello", 123)  -- type (Int, String, Int)
```

Tuples may be indexed using the `.` symbol, followed by a (`1`-indexed) number indicating which tuple field to read from.

Ex:
```
pair.2
pairTriple.1.3
```

Tuples don't have any write operation because they are declared on the heap

#### Pattern Variable Declaration

The `var` keyword can be used to index the elements of a tuple and bind them to variables in one step through the use of tuple patterns. Rather than having `var` followed by an identifier, we have `var` followed by a tuple of either identifiers, or empty `_` wildcard spaces (for elements that we want to remain unmatched).

Ex:
```
var (r, g, _) = color
```

Note that tuple patterns cannot be nested, as nested tuples are not idiomatic.

#### Enums

Standard enumeration types may be explicitly defined and named using the `enum` keyword:

```
enum WeekDay = Sun | Mon | Tue | Wed | Thur | Fri | Sat
```

Dusk generalizes enums so that they can store data, defining a "union" of tuples that may be distinguished with an extra byte of information storing a tag. 

Ex:
```
enum TileType = None | Solid(Int) | Water(Float) | Lava
```

Note that enums, like tuples, are not heap-allocated. This means that unlike other similar datatypes (Algebraic Datatypes, etc), they cannot be recursive. Such datatypes may be implemented through heap-allocated "union" datatypes instead (specified later in this section).

The `is` keyword may be used on enums to determine its tag which case is being stored. Once the tag of an enum is known, the data inside may also be indexed or unpacked using.

Ex:
```
if v is Solid then
  x = v.1
elsif v is Water then
  var (f) = v
  foo(f) 
end
```

If an enum has already been checked for all but one case, the final case does not need to be checked to unpack the data. Eg, the following is valid:

```
type Story = A(Int, Int) | B(Int, Int, Int) | C(Float)

fn f(Story s) Int
  if s is A then return s.1
  elsif s is B then return s.2 + s.3 end
  return floor(s.1)
end
```

### 1.5 Arrays and Tensors

Dusk also has mutable lists of values of variable size in the form of arrays. Arrays are heap-allocated, and function like "vectors" in other languages in the sense that they may be resized in-place.

Dusk additionally supports multi-dimensional arrays, known as "tensors". The type of an array is written with the dimensionality of the array, followed by the type of value contained by the array.

Ex:
```
1d[String]
1d[(Float, Bool)]
2d[Tile]
5d[Float]
```

#### Arrays

Examples of 1-dimensional array literals include:
```
new 1d[1, 2, 3, 4, 5, 6]
new 1d[String]    -- initializes an empty array of the given type
```

Arrays may also be initialized using "array format" syntax, which initializes an empty array up to a given size and fills it with values.

Ex:
```
new 1d(6)[.. 0]
new 1d(n + 1)[.. newFoo()]
```

Arrays may be resized and re-formatted using similar syntax.
```
reformat a(8)[.. emptyTile()]
```

Arrays may be indexed and updated using fairly standard syntax:
```
return a[x + 1]   -- array lookup
b[i] = z ** 2     -- array update
```

#### Tensors

Tensor literals have syntax similar to regular 1d array literals, however unless the array is empty, they require dimensions which specify how to lay out the data.

Ex:
```
new 2d[4, 4][
  1, 0, 0, 0
  0, 1, 0, 0
  0, 0, 1, 0
  0, 0, 0, 1
]
new 3d[Float]
```

Tensors may also be initialized and resized using formatting syntax.
```
new 2d(10, 20)[.. newTile()]
reformat aaa(100, 100, 100)[.. 0]
```

Tensors data is stored in row-major order, and they are indexed accordingly using a comma separated set of integers as the index. For example:
```
var a = new 2d[3, 2][
  0, 1, 2,
  3, 4, 5
]
return a[0, 1]   -- returns 3
```

### 1.6 Structs and Unions

#### Structs

Tuples, Enums, and Arrays are accessed through integer indices. Structs are heap-allocated, mutable sets of values accessed through a fixed set of field identifiers.

Struct types, like enums, must be explicitly defined through the `struct` keyword.

Ex:
```
struct Box{
  Int x,
  Int y,
  Int width,
  Int height
}
```

Structs are initialized using the `new` keyword and must have all their fields explicitly initialized.

Ex:
```
new Box{ x = 0, y = 0, width = 16, height = 16 }
```

Struct fields may be indexed and updated using the field identifier.

Ex:
```
return box.x + box.width   -- struct read
box.y = box.y + gravity    -- struct update
```

#### Unions

Union types define a "union" of structs which may be distinguished by use of a tag. Unions are to structs as enums are to tuples. Since Unions are heap-allocated, they may be of arbitrary size, and can be used to define recursive structures.

Ex:
```
union WordTree =
  Leaf{ String word }
  | Branch{ String word, WordTree left, WordTree right }
```

All other struct syntax also applies to unions. The `is` operator may also be used on unions to distinguish cases in the same manner as for enums.

### 1.7 Dictionaries

Dictionaries are heap-allocated, mutable sets of values accessed through a variable set of keys. Dictionaries are to structs as arrays are to tuples.

### 1.8 Nullables

Sometimes it is useful to allow "null" pointers, for instance in arrays where most of the etnries will be some empty. To interact with such values safely, we introduce a notion of a nullable datatype.

A (heap-allocated) datatype may be designated as nullable using a `?` symbol.

Ex:
```
String?
Tile?
1d[String]?
```

The fact that it must be heap-allocated is important. Primitive datatypes for example must be explicitly boxed/unboxed if this behavior is desired (although it is usually better to designate an enum for this purpose).

Null literals are the keyword `null`, with an optional type annotation. The type annotation may be useful because Dusk does not have type inferencing, and expressions are expected to have a definite type in variable declarations and array initializers.

Ex:
```
null
null[FooBar]
```

When checking whether a value is `null`, the `is` keyword should be used. The same system that governs enum/union case checking will also ensure that uses of a nullable value are typesafe.

## 2. Expressions and Statements

### 2.1 Identifiers

Identifiers are a sequence of letters, numbers, and underscores that:
- Begin with a letter, or single underscore (with at least one letter).
- Do not match any reserved keywords.

Identifiers are split into three categories:
- Constant identifiers if the first character is an underscore, and every letter is capitalized.
- Type/constructor identifiers if the first letter is capitalized.
- Variable/miscellaneous identifiers otherwise.

```
_COLORS, _TBL_XL     -- Constant identifiers
Int, _Foo32_xx       -- Type identifiers
x, _barZ_52          -- Variable identifiers
```

### 2.2 Variable Declaration/Assignment

The `var` keyword is used to declare new variables. Once declared, the `=` symbol is  used to re-assign the value stored in a variable.

Ex:
```
var x = 90
var y = "Test"
var z = f(x, y) ** 2 + 1
x = x + 1
y = "Tset"
```

### 2.3 Function Calls

Functions may be called in a standard way, with the function name surrounded by a comma-separated list of expressions wrapped in parentheses.

Ex:
```
foo()
bar(0, "Hello", 3.6)
```

Functions are often used in expressions for variable declarations/assignments, but they may also be called as standalone statements within a function body when they perform some side-effect.

Functions may also be called using "object call syntax", where the first argument is put in front, and the remainder of the function is treated as a method of the first argument. Note that this is just syntactic sugar for the normal function call syntax.

Ex:
```
obj.f(x, y)
--- is equivalent to
f(obj, x, y)

a.g()
--- is equivalent to
g(a)
```

Dusk does not have classes/objects, but this syntax is available to imitate object-like syntax when appropriate.

### 2.4 Return Statements

The `return` keyword is used within a function to terminate execution and (when applicable) return a value to the function caller.

The `return` keyword may be used on its own, or with an expression after it.

Ex:
```
return
return 9
return foo(x) + bar(a, z)
```

### 2.5 Code Blocks

Multiple statements may be sequenced together in code blocks.

Ex:
```
var i = 0
j = read()
i = i ** j
convolve(i, j)
```

Note that code blocks do not require separators between statements, not even newlines. This means that the following type of code is valid (although in poor style):

```
var a = 1
var b = a + 1 a = a + 1
c = b * a a = a + 2
```

### 2.6 Conditionals

Dusk also has branching `if`-`else` conditionals, with the `elsif` available to include additional branches. Conditionals need not have a final `else` branch.

Ex:
```
if x < 1 then
  print("A")
end

if a = 0 then
  x = foo()
elsif a = 1 then
  y = bar()
else
  z = 99
end
```

### 2.7 Loops

Dusk has two main kinds of loops. First it has simple `while` loops, which repeat a block of code until a certain condition is met.

Ex:
```
while x > 0 do
  sum = sum + x
  x = x - 1
end 
```

The `loop` keyword exists as syntactic sugar for a loop where the condition is always true (for loops that only break through the use of a `return` statement, or code that is intended to run indefinitely).

Ex:
```
loop
  x = doSomething()
  draw(0, 0, "Test: " + toString(x))
end
```

The second main type of loop that Dusk has are `for` loops.

For loops themselves come in two flavors. First, we have for loops that iterate through a sequence of numbers from `0` to `n`, either inclusive or exclusive.

Ex:
```
-- exclusive case
for i < 10 do
  print(toString(i) + "\n")
end

-- inclusive case
for j <= 10 do
  sum = sum + j
end
```

Second, we have for loops that iterate directly through the elements of either arrays or dictionaries using the `in` keyword.

Ex:
```
for v in a do
  update(v)
  draw(v)
end
```

The syntax that describes the method of iteration for a for loop is known as its "range". A for loop may have multiple ranges, which is used as syntactic sugar for a set of nested for loops.

Ex:
```
for i < width, j < height do
  draw(a, i, j)
end 
```

Note that the first range given (in this case `i < width`) will be unrolled to be the innermost loop. (This is to match the layout of multi-dimensional arrays, which will be described in the section on Datatypes).

### 2.8 Garbage Collection

Dusk is a garbage-collected language which requires manual activation of the garbage collector. The keyword for doing this is:

```
gcCollect
```

Note that the garbage collector may only be called in special functions designated as "linear" (will only be executed once). The definition of a linear function is given in the next sub-section on Function Declarations. Details of why this is the case will be given in the section dedicated to Garbage Collection.

## 3 Functions and Declarations

### 3.1 Simple Functions

Functions are declared using the `fn` keyword, followed by an identifier for the function name, a list of the function arguments (and their types), and the return type (optional). If no return type is given, the function is said to have the return type `Unit`.

Ex:
```
fn f()
  if something() then return end
  waffle()
end

fn factorial(Int x) Int
  if x <= 0 then return 1
  else return x * factorial(x - 1) end
end
```

### 3.2 Main/Linear Function(s)

Instead of the `fn` keyword, functions may also be declared using the `lin` keyword, indicating that the function is a "linear" function.

Ex:
```
lin main()
  print("Hello World!")
end
```

Linear functions are guaranteed to only execute once. This means that they may only be called inside other linear functions, and never inside of a loop.

Programs require an initial `main` function as the first function to be executed. As implied by the example, `main` must be a linear function.

### 3.3 Function Overloading

Functions may be given the same name as long as their first arguments are different* types.
 
**Some types that are different will be treated the same for the purposes of overloading, ex: arrays. More on this in the section on Datatypes (Overload Names).*

For example:
```
fn add(BigInt x, BigInt y) BigInt
  ...
end

fn add(Matrix m1, Matrix m2) Matrix
  ...
end
```

May both be declared and used in the same module without ambiguity, with the type system deciding at compile-time which one to use.

This functionality allows for builtin operators to be overloaded using their "explicit name", which we have actually already done in this example (overloading the `+` operator). Continuing from the following example, this would be a valid use of these overloads.

```
fn sum(Matrix a, Matrix b, Matrix c) Matrix
  return a + b + c + a
end
```

The following table describes how the overload name of a type is given:

| Type | Overload Name | Example |
|:-----|:--------------|:--------|
| Primitives | Taken verbatim | `Int`, `Float` => `Int`, `Float` |
| Pair  | `pair` | `(String, Bool)` => `pair`  |
| Triple | `triple` | `(Int, Int, Int)` => `triple` |
| N-uple | `t` + arity | `(Int, Float, Float, Float)` => `t4` |
| Arrays | Array dimension | `1d[Int]`, `3d[Float]` => `1d`, `3d` |
| Enums/Structs/Unions | Taken verbatim | - |
| Nullables | Ignore nullable | `String?`, `1d[Int]?` => `String`, `1d` |

The main implication is that types without explicit names (tuples, arrays, nullables) can only be overloaded to a limited extent.

### 3.4 Constant Declarations

### 3.5 Global Variables

Dusk supports the use of global variables, but only in very explicit fashion. Any function that modifies a global variable must declare that it does so explicitly,

## 3. Module System

## 4. Sulfur Runtime


