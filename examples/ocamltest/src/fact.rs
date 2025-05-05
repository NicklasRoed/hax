fn native_factorial(n: usize) -> usize {
    if n == 0 || n == 1 {
        1
    } else {
        n * native_factorial(n - 1)
    }
}

fn u64_factorial(n: u64) -> u64 {
    if n == 0 || n == 1 {
        1
    } else {
        n * u64_factorial(n - 1)
    }
}

fn main() {
    
}