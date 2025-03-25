// Define a simple macro that prints a message
macro_rules! say_hello {
    // Pattern matching with no arguments
    () => {
        println!("Hello, world!");
    };
    // Pattern matching with one argument
    ($name:expr) => {
        println!("Hello, {}!", $name);
    };
}

fn main() {
    // Invoke the macro with no arguments
    say_hello!();
    
    // Invoke the macro with one argument
    let name = "Rust";
    say_hello!(name);
    
    // Invoke the macro with a literal string
    say_hello!("Macro");
}