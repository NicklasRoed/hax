// Define a minimal trait
trait Minimal {
    fn minimal_method(&self) -> bool;
}

// Generic function with trait bound
fn identity<T: Minimal>(x: T) -> T {
    x
}

// Implement the trait for i32
impl Minimal for i32 {
    fn minimal_method(&self) -> bool {
        true
    }
}

fn main() {
    let x: i32 = 42;
    let y = identity(x);
}

trait Creator {
    fn create(value: i32) -> Self;
}

fn factory<T: Creator>(x: i32) -> T {
    T::create(x)
}

