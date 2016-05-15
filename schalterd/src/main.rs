extern crate iron;
extern crate router;
extern crate iron_json_response as ijr;
extern crate sysfs_gpio;

use iron::prelude::*;
use router::Router;

mod schalter;
use schalter::SchalterHandler;
mod door;
use door::DoorHandler;


fn main() {
    let mut router = Router::new();
    router.get("/schalter.json", SchalterHandler::new().chain());
    router.post("/door/unlock", DoorHandler::new().chain());
    Iron::new(router).http("[::]:8080").unwrap();
}
