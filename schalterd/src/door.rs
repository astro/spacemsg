use iron::prelude::*;
use iron::status;
use iron::headers::{Authorization, Basic};
use iron::middleware::Handler;
use ijr::{JsonResponse, JsonResponseMiddleware};
use std::collections::BTreeMap;
use sysfs_gpio::{Pin, Direction, Error};
use std::thread::sleep;
use std::time::Duration;
use std::sync::{Arc, RwLock};


#[derive(Copy, Clone)]
enum DoorStatus {
    Unknown,
    Unlocked,
    Locked,
}

#[derive(Clone)]
pub struct DoorState {
    state: Arc<RwLock<DoorStatus>>
}

impl DoorState {
    pub fn new() -> Self {
        DoorState {
            state: Arc::new(RwLock::new(DoorStatus::Unknown)),
        }
    }

    pub fn chain(self) -> Chain {
        let mut chain = Chain::new(self);
        chain.link_after(JsonResponseMiddleware);
        chain
    }
}

impl Handler for DoorState {
    fn handle(&self, _: &mut Request) -> IronResult<Response> {
        let mut json: BTreeMap<String, bool> = BTreeMap::new();
        match *self.state.read().unwrap() {
            DoorStatus::Unknown =>
                json.insert("unknown".to_owned(), true),
            DoorStatus::Unlocked =>
                json.insert("locked".to_owned(), false),
            DoorStatus::Locked =>
                json.insert("locked".to_owned(), true),
        };
        Ok(Response::with((status::Ok, JsonResponse::new(json, None))))
    }
}

const GPIO_UNLOCK: u64 = 18;
const GPIO_LOCK: u64 = 27;

pub struct DoorHandler {
    gpio: Pin,
    active_status: DoorStatus,
    state: DoorState,
    password: String,
}

impl DoorHandler {
    pub fn new_unlock(state: &DoorState, password: String) -> Self {
        Self::new(GPIO_UNLOCK, DoorStatus::Unlocked, state.clone(), password)
    }

    pub fn new_lock(state: &DoorState, password: String) -> Self {
        Self::new(GPIO_LOCK, DoorStatus::Locked, state.clone(), password)
    }

    fn new(pin_num: u64, active_status: DoorStatus, state: DoorState, password: String) -> Self {
        let gpio = Pin::new(pin_num);
        gpio.export()
            .unwrap_or_else(|e| println!("Cannot export GPIO #{}: {}", pin_num, e));
        gpio.set_direction(Direction::Out).unwrap_or(());

        DoorHandler {
            gpio: gpio,
            active_status: active_status,
            state: state,
            password,
        }
    }

    pub fn chain(self) -> Chain {
        let mut chain = Chain::new(self);
        chain.link_after(JsonResponseMiddleware);
        chain
    }

    fn activate(&self) -> Result<(), Error> {
        let mut state = self.state.state.write().unwrap();

        try!(self.gpio.set_value(1));
        // 20ms between 1, 0
        sleep(Duration::from_millis(20));

        try!(self.gpio.set_value(0));
        // hold 0 to forbid a long 1 with consecutive requests
        // TODO: locking?
        sleep(Duration::from_millis(20));

        *state = self.active_status;

        Ok(())
    }

    fn authenticate(&self, auth: &Authorization<Basic>) -> bool {
        let basic = &auth.0;
        basic.password.as_ref() == Some(&self.password) ||
            (basic.password.is_none() && basic.username == self.password)
    }
}

impl Handler for DoorHandler {
    fn handle(&self, req: &mut Request) -> IronResult<Response> {
        let auth = req.headers.get();
        match auth {
            Some(ref auth) if self.authenticate(auth) => {
                let mut json: BTreeMap<String, i8> = BTreeMap::new();
                self.activate()
                    .unwrap_or_else(|e| {
                        println!("Cannot activate {:?}: {:?}", self.gpio, e);
                        json.insert("error".to_owned(), 1);
                    });
                Ok(Response::with((status::Ok, JsonResponse::new(json, None))))
            },
            _ =>
                Ok(Response::with(status::Unauthorized))
        }
    }
}
