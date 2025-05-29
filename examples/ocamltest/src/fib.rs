fn native_fibonacci(n: usize) -> usize {
    if n == 0 {
        0
    } else if n == 1 {
        1
    } else {
        native_fibonacci(n - 1) + native_fibonacci(n - 2)
    }
}

fn fibonacci_u64(n: u64) -> u64 {
    if n == 0 {
        0
    } else if n == 1 {
        1
    } else {
        fibonacci_u64(n - 1) + fibonacci_u64(n - 2)
    }
}

fn main() {
    
}