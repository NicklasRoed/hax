trait Measurable {
    fn measure(&self) -> usize;
}

fn first<A, B>((value, _): (A, i32), y: B) -> A
where
    B: Measurable,
{
    // Using the trait
    let _ = y.measure();
    value
}

trait GenericTrait<T> {
    fn transform(&self, value: T) -> T;
    fn check(&self, a: T, b: T) -> bool;
}

fn use_generic<A, G, T>((value, _): (A, i32), generic_impl: G) -> A
where
    G: GenericTrait<T>,
{
    value
}