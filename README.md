# HaskellScript

## Getting started

### Comments and printing
```
// <- this is how you make single line comments

/* And this is
   how you make multiline comments
*/

// printing
_print("you can print", "variadic number of", "arguments", "with", "this special print statement(sic) starting with underscore");
print("or you can use this print, which is a normal HSS function");
```
### Types
```
// HaskellScript has 6 built-in types + user defined algebraic data types:

print(42);     // Int

print("hello"); // String

print(true); // Bool, with its only two possible values shown here
print(false);

print([1, 2, 3]); // polymorphic, homogenius lists
print(["ayo", "lets", "go"]);
// ["hi", 42, true]; // 🚨 Type Error! Can't have heterogenius lists in HSS

// function type
addNumbers :: Int -> Int -> Int
const addNumbers = \n, m => (n + m);
print(addNumbers(3, 4));

// for a function that doesn't return any value, there's a special Void type
logger :: a -> Void
const logger = \x => {
  print(x);
};
const b = logger("text"); // doesn't cause an error, but isn't very useful
print(b); // undefined

// you can also create your own algebraic data types
// they could have multiple (or none) type parameters and multiple (at least one) constructor
// type parameters don't have to be polymorphic
data Tree(a) = {
  Nil() | Node(Tree(a), a, Tree(a))
};

print(Node(Nil(), 42, Nil()));
```

### Variables
```
// Variables (as all other things) in HSS cannot be mutated, reassigned, etc:
const a = 42;
// a = 43; //🚨 Syntax Error!
// const a = 43; // 🚨 ERROR! This doesn't work because there already is a constant "a" declared 
                // at the same level/in the same scope
```
### Functions
```
// when declaring a named function, i.e. assigning a lambda to a identifier
// a type signature *should* be provided:

integerSingleton :: Int -> [Int]
const integerSingleton = \n => {
  return [n];
};

// If a function consists of a single return statement, then it can always be refactored to
// a concise form:
integerSingleton' :: Int -> [Int]
const integerSingleton' = \n => ([n]);

// Note, that in case of this function's implementation, there's no need for argument
// to be an int, it could be an argument of any type! So let's refactor it to be polymorphic:

singleton :: a -> [a]
const singleton = \x => ([x]);

print(singleton("wow"));
print(singleton(42));
print(singleton([true, false, true]));

// not typing a function will work, but it will make inc be a polymorphic function
// under the hood. So unless you know what you're doing, don't do this. 
// also typing functions explicity as polymorphic is better style as it makes code 
// easier to reason about
const inc = \x => (x + 1);
print(inc(42)); // 43
// inc("ala") <- this will cause a runtime error
// integerSingleton("hi") <- this will be caught during typecheck, before evem starting a program


// all functions can be partialy applied:

const increment = addNumbers(1);
print(increment(42)); // -> 43

// closures work as expected:

// Note that "(Void)" is a type of a function that takes no arguments
// and doesn't return anything
createPrinter :: String -> (Void) 
const createPrinter = \text => {
  printer :: Void
  const printer = \ => {
    print(text);
  };
  return printer;
};

const p1 = createPrinter("hello");
const p2 = createPrinter("world");

p1(); // hello
p2(); // world

// Functions are treated just like other values, they can be
// passed as arguments to other functions, returned from them etc.
applyMany :: [(a -> Void)] -> a -> Void
const applyMany = \funs, val => {
  foreach(\fun => {
    fun(val);
  }, funs);
};

applyMany([print, print, print], "wow");
// BTW you can pass in lambdas like in the example above (i.e. without their signature)
// but if a polymorphic function is expected, then the types won't be infered and you
// may end up with a runtime error
```
### Pattern matching
See (Wikipedia page)[https://en.wikipedia.org/wiki/Pattern_matching] on pattern matching for more info.
```
data Point() = { P(Int, Int) };

data Shape() = {
    Rect(Point(), Point()) // upper-left corner, lower right corner
  | Circle(Point(), Int) // center, radius
  | Triangle(Point(), Point(), Point())
};

abs :: Int -> Int
const abs = \x => (
  x < 0 ? -x : x
);

// results are going to be a little akward, because there are only integers in HSS
// but it is just an example of pattern matching
calculateArea :: Shape() -> Int
const calculateArea = \shape => {
  match shape:
    case Rect(P(x, y), P(x', y')) {
      return abs(x - x') * abs(y - y');
    }
    case Circle(center_, radius) { // variables ending with _ always match but don't get binded
      // print(center_); // Error: undefined name
      return 3 * radius * radius; // here 3 being poor man's π
    }
    case Triangle(P(x1, y1), P(x2, y2), P(x3, y3)) {
      return abs(x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2; 
    }
};

print(calculateArea(Rect(P(5, 5), P(10, 10)))); // 25
print(calculateArea(Circle(P(5, 5), 10))); // 300
print(calculateArea(Triangle(P(0, 0), P(3, 0), P(3, 4)))); // 6
```
concrete values can also be matched:
```
const statusCode = 200;

match statusCode:
  case 200 {
    print("OK");
  }
  case 204 {
    print("No Content");
  }
  case 400 {
    print("Bad Request");
  }
  // etc...
```

### Lists
Under the hood, lists are (defined in Overture) HaskellScripts data type, but with some special
syntax sugar for creation and pattern matching.
```
const list = [1, 2, 3];
print(list);

const emptyList = [];
print(emptyList);

match list:
  case [] {
    print("empty!"); // won't happen
  }
  case [x, ...xs] {
    _print("head: ", x); // 1
    _print("tail: ", xs); // [2, 3]
  }

isMember :: a -> [a] -> Bool
const isMember = \a, l => {
  match l:
    case [] {
      return false;
    }
    case [x, ...xs] {
      return x == a ? true : isMember(a, xs);
    }
};

print(isMember(4, list)); // false
print(isMember(2, list)); // true

const a = 1;
const l' = [4, 5, 6];

// spreading operator
const l = [a, 2, 3, ...l', 7];

print(l); // [1, 2, 3, 4, 5, 6, 7]
print([1, 2,...[]]); // [1, 2]
```