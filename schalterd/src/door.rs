use iron::prelude::*;
use iron::status;
use iron::middleware::Handler;
use ijr::{JsonResponse, JsonResponseMiddleware};
use std::collections::BTreeMap;
use sysfs_gpio::{Pin, Direction, Error};
use std::thread::sleep;
use std::time::Duration;


const GPIO_UNLOCK: u64 = 18;
const GPIO_LOCK: u64 = 27;

pub struct DoorHandler {
    gpio: Pin,
}

impl DoorHandler {
    pub fn new_unlock() -> Self {
        Self::new(GPIO_UNLOCK)
    }

    pub fn new_lock() -> Self {
        Self::new(GPIO_LOCK)
    }

    fn new(pin_num: u64) -> Self {
        let gpio = Pin::new(pin_num);
        gpio.export()
            .unwrap_or_else(|e| println!("Cannot export GPIO #{}: {}", pin_num, e));
        gpio.set_direction(Direction::Out).unwrap_or(());

        DoorHandler {
            gpio: gpio,
        }
    }

    pub fn chain(self) -> Chain {
        let mut chain = Chain::new(self);
        chain.link_after(JsonResponseMiddleware);
        chain
    }

    fn activate(&self) -> Result<(), Error> {
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
        self.activate()
            .unwrap_or_else(|e| {
                println!("Cannot activate {:?}: {:?}", self.gpio, e);
                json.insert("error".to_owned(), 1);
            });
        Ok(Response::with((status::Ok, JsonResponse::new(json, None))))
    }
}
