// Basic Language Features Example in Rust

// Constants
const MAX_POINTS: u32 = 100_000; // Const as an fn; works
static PLAYER_NAME: &str = "Player One"; // Not supported

fn main() {
    // Variables and Mutability
    let immutable_var = 5; // Works
    let mut mutable_var = 10; // Mutables uses shadowing to declare new value
    mutable_var = 12;
    
    // Shadowing
    let x = 5; 
    let x = x + 1;  // Works
    
    // Type annotations
    let explicit_type: i64 = 42; // Works
    
    // Basic Data Types
    // Integers
    let signed_int: i32 = -42; // Works
    let unsigned_int: u32 = 42; // Works
    
    // Floating point
    let float_num: f64 = 3.14159; // Works
    
    // Boolean
    let is_active: bool = true; // Works
    let is_greater = 10 > 5;  // Type inference for bool
    
    // Characters
    let letter: char = 'A'; // Works
    
    // Strings
    let string_literal: &str = "hello";
    let mut string_object: String = String::from("Hello, "); // We need alloc lib
    string_object.push_str("world!"); // We need alloc lib
    
    // Unit type
    let unit_value: () = (); // Works
    
    // Tuples - Ask Bas about tuple accessing
    let tuple: (i32, f64, &str) = (500, 6.4, "tuple");
    let (x, y, z) = tuple;  // Destructuring
    let first = tuple.0;    // Accessing by index
    
    // Control Flow
    // If/else expressions
    let condition = true;
    let number = if condition { 5 } else { 6 };
    
    // Match expressions
    let dice_roll = 6;
    match dice_roll {
        1 => println!("One!"),
        2 => println!("Between two and five"),
        6 => println!("Six!"),
        _ => println!("Invalid dice roll"),
    }

    // Functions
    let sum = add(5, 10);
    let product = multiply(3, 4);
    
    // Closures
    let add_one = |x| x + 1;
    let three = add_one(2);
    
    let multiply_by = |x, y| x * y;
    let eight = multiply_by(2, 4);
    
    // Nested functions
    fn nested_function(x: i32) -> i32 {
        x * x
    }
    let nested_result = nested_function(4);  // 16
    
    // Early returns
    let early_return_val = early_return_example(true);
}

// Function declarations
fn add(a: i32, b: i32) -> i32 {
    a + b  // Implicit return (no semicolon)
}

fn multiply(a: i32, b: i32) -> i32 {
    a * b // Explicit return
}

fn early_return_example(flag: bool) -> i32 {
    if flag {
        return 1  
    }
    
    let x = 10;
    let y = 20;
    x + y  // Normal return
}

// Recursion
fn factorial(n: u32) -> u32 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}