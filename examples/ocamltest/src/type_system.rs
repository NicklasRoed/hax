// Rust Type System Features Example

// Type Aliases - does not get translated
type UserId = u64;
type Result<T> = std::result::Result<T, MyError>;

// Custom Types
// Struct with named fields
struct User {
    id: UserId,  // Using the type alias
    name: String,
    active: bool,
}

// Tuple struct
struct Point(i32, i32, i32);

// Unit struct
struct UnitStruct;

// Enums
enum Shape {
    Circle(f64),               // single value
    Rectangle(f64, f64),       // tuple variant
    Square(f64),               // single value
    Triangle { a: f64, b: f64, c: f64 }, // struct variant
    Empty,                     // unit variant
}

// Error type for Result
enum MyError {
    NotFound,
    InvalidInput(String),
    DatabaseError { code: i32, message: String },
}

// Generics
// Generic struct
struct Container<T> {
    value: T,
}

// Generic enum (like Option)
enum Maybe<T> {
    Something(T),
    Nothing,
}

// Traits
trait Drawable {
    fn draw(&self);
    
    // Associated type
    type Color;
    fn get_color(&self) -> Self::Color;
}

// Implementing traits
impl Drawable for Shape {
    fn draw(&self) {
        match self {
            Shape::Circle(radius) => println!("Drawing circle with radius {}", radius),
            Shape::Rectangle(width, height) => println!("Drawing rectangle {}x{}", width, height),
            Shape::Square(side) => println!("Drawing square with side {}", side),
            Shape::Triangle { a, b, c } => println!("Drawing triangle with sides {}, {}, {}", a, b, c),
            Shape::Empty => println!("Drawing nothing"),
        }
    }
    
    type Color = String;
    fn get_color(&self) -> Self::Color {
        String::from("blue")
    }
}

// Generic function with trait bounds
fn print_and_draw<T: Drawable>(item: T) {
    item.draw(); // Using specific implementation
}

// Function with complex trait bounds
fn process<T, U>(item: T, processor: U) 
where 
    T: Drawable,
    U: Fn(&T) -> String,
{
    let result = processor(&item);
    println!("Processed: {}", result);
}

fn main() {
    // Constructing custom types
    let user = User {
        id: 1,
        name: String::from("Alice"),
        active: true,
    };
    
    // Struct update syntax
    let user2 = User {
        name: String::from("Bob"),
        ..user  // rest of fields come from user
    };
    
    // Tuple struct
    let origin = Point(0, 0, 0);
    let x = origin.0;  // Accessing fields
    
    // Unit struct
    let unit = UnitStruct;
    
    // Enum variants
    let circle = Shape::Circle(5.0);
    let rect = Shape::Rectangle(4.0, 3.0);
    let triangle = Shape::Triangle { a: 3.0, b: 4.0, c: 5.0 };
    
    // Pattern matching on enums
    match circle {
        Shape::Circle(radius) if radius > 10.0 => println!("Large circle"),
        Shape::Circle(radius) => println!("Circle with radius {}", radius),
        Shape::Rectangle(w, h) => println!("Rectangle {}x{}", w, h),
        Shape::Square(s) => println!("Square with side {}", s),
        Shape::Triangle { a, b, c } => println!("Triangle with sides {}, {}, {}", a, b, c),
        Shape::Empty => println!("Empty shape"),
    }
    
    // Option type
    let name = Some(String::from("Alice"));
    let empty_name: Option<String> = None;
    
    // Pattern matching on Option
    match name {
        Some(n) => println!("Name: {}", n),
        None => println!("No name provided"),
    }
    
    // Result type for error handling
    let result: Result<i32> = Ok(42);
    let error: Result<i32> = Err(MyError::NotFound);
    
    // Pattern matching on Result
    match result {
        Ok(value) => println!("Success: {}", value),
        Err(MyError::NotFound) => println!("Not found error"),
        Err(MyError::InvalidInput(msg)) => println!("Invalid input: {}", msg),
        Err(MyError::DatabaseError { code, message }) => {
            println!("Database error {}: {}", code, message)
        }
    }
    
    // Generics
    let int_container = Container { value: 42 };
    let string_container = Container { value: String::from("hello") };
    
    // Custom Option-like enum
    let maybe_value = Maybe::Something(123);
    let maybe_empty: Maybe<i32> = Maybe::Nothing;
    
    match maybe_value {
        Maybe::Something(x) => println!("Got value: {}", x),
        Maybe::Nothing => println!("Got nothing"),
    }
    
    // Using trait methods
    circle.draw();
    
    // Using the generic function with trait bounds
    print_and_draw(circle);
    
    // Complex trait bounds with closures
    process(rect, |shape| {
        match shape {
            Shape::Rectangle(w, h) => format!("Area: {}", w * h),
            _ => String::from("Not a rectangle"),
        }
    });
}