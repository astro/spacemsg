extern crate iron;
extern crate router;
extern crate iron_json_response as ijr;
extern crate sysfs_gpio;

use std::env;
use iron::prelude::*;
use router::Router;

mod schalter;
use schalter::SchalterHandler;
mod door;
use door::{DoorHandler, DoorState};


fn main() {
    let password = env::var("SCHALTER_PASSWORD").expect("$SCHALTER_PASSWORD");
    let mut router = Router::new();
    let door_state = DoorState::new();
    router.post("/door/unlock", DoorHandler::new_unlock(&door_state, password.clone()).chain(), "unlock");
    router.post("/door/lock", DoorHandler::new_lock(&door_state, password).chain(), "lock");
    router.post("/schalter.json", SchalterHandler::new().chain(), "schalter");
    router.get("/schalter.json", SchalterHandler::new().chain(), "schalter");
    router.post("/door.json", door_state.chain(), "door");
    router.get("/door.json", door_state.chain(), "door");
    Iron::new(router).http("[::]:80").unwrap();
}
