/*
#![feature(unboxed_closures)]
#![feature(fn_traits)]

use zoet::zoet;

struct Wat;
#[zoet]
impl Wat {
    #[zoet(FnOnce)]
    fn wat(self, args: usize) -> usize {
        args
    }
}

#[test]
fn test() {
    let x = Wat;
    x(1234);
}
 */
