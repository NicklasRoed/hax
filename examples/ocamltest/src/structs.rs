struct Coords<T>(T, T);

fn main() {
    let point = Coords(3.14, 2.71);
    let x = point.0;  // Access first element
    let y = point.1;  // Access second element
}

struct Pair<T> {
    first: T,
    second: T
}

