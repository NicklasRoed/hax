// Define a trait for objects that can be measured with a type parameter U
trait Measurable {
    // Changed to accept an argument of type &U
    fn measure(self, x: i32, y: i32) -> i32;
    fn hallo(self, z: i64) -> i64;
}

// Implement the trait for Rectangle with a specific type parameter
struct Rectangle {
    width: i32,
    height: i32,
}

impl Rectangle {
    // Instance method that takes &self (borrowed reference to instance)
    fn area(self) -> i32 {
        self.width * self.height
    }
}

fn add_dimensions(rect: Rectangle) -> i32 {
    let x = rect.width + rect.height;
    rect.area()
}

// Specify the type parameter (in this case i32)
impl Measurable for i32 {
    fn measure(self, x: i32, y: i32) -> i32 {
        x * y // Area multiplied by x
    }
    fn hallo(self, z: i64) -> i64 {
        z + 2
    }
}

// Generic function with a trait bound for both T and U
fn print_measurement<T: Measurable>(item: T) -> i32 
where 
    T: Measurable
{
    // Use the trait method with value
    item.measure(5, 5)    
}

fn stupid_func (x: i32) -> i32 {
    let x1 = x + 1;
    let x2 = x1 + 1;
    let x3 = x2 + 1;
    x3
}

// Usage
fn main() {
    let num: i32 = 42;
    let result = print_measurement(num);  // This explicitly uses the i32 implementation
}