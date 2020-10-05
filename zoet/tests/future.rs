use core::{
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
