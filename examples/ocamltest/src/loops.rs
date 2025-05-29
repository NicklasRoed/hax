fn unconditional_loop() {
    let mut counter = 0;
    loop {

    }
}

fn while_loop() {
    let mut i = 0;
    while i < 5 {
        
    }
}

fn for_range_loop() {
    for i in 0..10 {
        // Simple iteration
    }
}

fn for_iterator_loop() {
    let values = vec![1, 2, 3, 4, 5];
    for val in values {
        // Process each value
    }
}

fn main() {
    unconditional_loop();
    while_loop();
    for_range_loop();
    for_iterator_loop();
}