extern crate iron;
extern crate router;
extern crate iron_json_response as ijr;
extern crate sysfs_gpio;

use iron::prelude::*;
use router::Router;

mod schalter;
use schalter::SchalterHandler;
mod door;
use door::{DoorHandler, DoorState};


fn main() {
    let mut router = Router::new();
    let door_state = DoorState::new();
    router.get("/schalter.json", SchalterHandler::new().chain(), "schalter");
    router.post("/door/unlock", DoorHandler::new_unlock(&door_state).chain(), "unlock");
    router.post("/door/lock", DoorHandler::new_lock(&door_state).chain(), "lock");
    router.get("/door.json", door_state.chain(), "door");
    Iron::new(router).http("[::]:8080").unwrap();
}
