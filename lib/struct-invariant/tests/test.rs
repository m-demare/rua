use struct_invariant::invariant;

#[derive(Debug)]
struct Test {
    elements: Vec<u32>,
    partial_sum: u32,
    summed_nr: usize,
}

#[invariant(self.summed_nr <= self.elements.len(), "summed_nr too big")]
#[invariant(self.elements.iter().take(self.summed_nr).sum::<u32>() == self.partial_sum, "wrong partial_sum value")]
impl Test {
    pub fn new(elements: Vec<u32>, partial_sum: u32, summed_nr: usize) -> Self {
        Self { elements, partial_sum, summed_nr }
    }

    fn new_priv(elements: Vec<u32>, partial_sum: u32, summed_nr: usize) -> Self {
        Self { elements, partial_sum, summed_nr }
    }

    pub fn sum_one(&mut self) {
        if let Some(n) = self.elements.get(self.summed_nr) {
            self.partial_sum += n;
        }
        self.summed_nr += 1;
    }

    pub fn set_sum(&mut self, n: u32) {
        self.partial_sum = n;
    }

    pub fn is_done(&self) -> bool {
        self.summed_nr == self.elements.len()
    }
}

#[test]
#[should_panic(expected = "wrong partial_sum value")]
fn test1() {
    let _test = Test::new(vec![1, 2, 3], 0, 1);
}

#[test]
fn test2() {
    let mut test = Test::new(vec![1, 2, 3], 0, 0);
    test.sum_one();
    test.sum_one();
    test.sum_one();
}

#[test]
#[should_panic(expected = "summed_nr too big")]
fn test3() {
    let mut test = Test::new(vec![1, 2, 3], 0, 0);
    test.sum_one();
    test.sum_one();
    test.sum_one();
    test.sum_one();
}

#[test]
fn test4() {
    let _test = Test::new_priv(vec![1, 2, 3], 0, 1);
}

#[test]
#[should_panic(expected = "wrong partial_sum value")]
fn test5() {
    let test = Test::new_priv(vec![1, 2, 3], 0, 1);
    test.is_done();
}
