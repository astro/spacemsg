use iron::prelude::*;
use iron::status;
use iron::middleware::Handler;
use ijr::{JsonResponse, JsonResponseMiddleware};
use std::collections::BTreeMap;
use sysfs_gpio::{Pin, Direction, Error};
use std::thread::sleep;
use std::time::Duration;


const DOOR_GPIO: u64 = 18;

pub struct DoorHandler {
    gpio: Pin
}

impl DoorHandler {
    pub fn new() -> Self {
        let pin = Pin::new(DOOR_GPIO);
        pin.export()
            .unwrap_or_else(|e| println!("Cannot export GPIO #{}: {}", DOOR_GPIO, e));
        pin.set_direction(Direction::Out).unwrap_or(());
        DoorHandler {
            gpio: pin
        }
    }

    pub fn chain(self) -> Chain {
        let mut chain = Chain::new(self);
        chain.link_after(JsonResponseMiddleware);
        chain
    }

    fn unlock_door(&self) -> Result<(), Error> {
        try!(self.gpio.set_value(1));
        // 20ms between 1, 0
        sleep(Duration::from_millis(20));
        try!(self.gpio.set_value(0));
        // hold 0 to forbid a long 1 with consecutive requests
        // TODO: locking?
        sleep(Duration::from_millis(20));
        Ok(())
    }
}

impl Handler for DoorHandler {
    fn handle(&self, _: &mut Request) -> IronResult<Response> {
        let mut json: BTreeMap<String, i8> = BTreeMap::new();
        self.unlock_door()
            .unwrap_or_else(|e| {
                println!("Cannot unlock door: {:?}", e);
                json.insert("error".to_owned(), 1);
            });
        Ok(Response::with((status::Ok, JsonResponse::new(json, None))))
    }
}
