use core::{
    future::{ready, IntoFuture, Ready},
    pin::Pin,
    task::{Context, Poll},
};
use zoet::zoet;

struct Wat;
#[zoet]
impl Wat {
    #[zoet(Future)]
    fn future(self: Pin<&mut Self>, _cx: &mut Context) -> Poll<()> {
        Poll::Ready(())
    }
}

// Broadly nicked from https://doc.rust-lang.org/stable/std/future/trait.IntoFuture.html:

/// Eventually multiply two numbers
pub struct Multiply {
    num: u16,
    factor: u16,
}
/* Before zoet: */
// impl IntoFuture for Multiply {
//     type IntoFuture = Ready<Self::Output>;
//     type Output = u16;
//     fn into_future(self) -> Self::IntoFuture {
//         ready(self.num * self.factor)
//     }
// }

/* After zoet: */
#[zoet]
impl Multiply {
    #[zoet(IntoFuture)]
    fn into_future(self) -> Ready<u16> {
        ready(self.num * self.factor)
    }
}
