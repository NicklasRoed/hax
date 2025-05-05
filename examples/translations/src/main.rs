// Define a minimal trait
trait Measurable {
    fn measure(&self) -> i32;
}

// Generic function with trait bound that calls the trait function
fn get_measurement<T: Measurable>(item: &T) -> i32 {
    item.measure()
}

// Entry point
fn main() {
    // Note: This value isn't printed but could be used
}