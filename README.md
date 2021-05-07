// <- this is how you make single line comments

/* And this is
   how you make multiline comments
*/

// printing
_print("you can print", "variadic number of", "arguments", "with", "this special print statement(sic) starting with underscore");
print("or you can use this print, which is a normal HSS function");

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

// # VARIABLES
// there are no variables in HSS, only constants can be declared:
const a = 42;
// a = 43; //🚨 Syntax Error!
// const a = 43; // 🚨 ERROR! This doesn't work because there already is a constant "a" declared 
                // at the same level/in the same scope

// # FUNCTIONS

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

