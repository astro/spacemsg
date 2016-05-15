use iron::prelude::*;
use iron::status;
use iron::middleware::Handler;
use ijr::{JsonResponse, JsonResponseMiddleware};
use std::collections::BTreeMap;
use sysfs_gpio::{Pin, Direction};

const SCHALTER_GPIOS: &'static [u64] = &[23, 24];

pub struct SchalterHandler {
    gpios: Vec<Pin>
}

impl SchalterHandler {
    pub fn new() -> Self {
        SchalterHandler {
            gpios: SCHALTER_GPIOS.iter()
                .map(|num| {
                    let pin = Pin::new(*num);
                    pin.export()
                        .unwrap_or_else(|e| println!("Cannot export GPIO #{}: {}", num, e));
                    pin.set_direction(Direction::In).unwrap_or(());
                    pin
                })
                .collect()
        }
    }

    pub fn chain(self) -> Chain {
        let mut chain = Chain::new(self);
        chain.link_after(JsonResponseMiddleware);
        chain
    }

    fn read_values(&self) -> Option<Vec<u8>> {
        let mut failed = false;
        let values = self.gpios.iter()
            .map(|gpio| gpio.get_value()
                 .unwrap_or_else(|e| {
                     println!("Cannot read GPIO: {}", e);
                     failed = true;
                     0
                 })
                ).collect();
        if failed {
            None
        } else {
            Some(values)
        }
    }
}

impl Handler for SchalterHandler {
    fn handle(&self, _: &mut Request) -> IronResult<Response> {
        let status: i8 = match self.read_values() {
            Some(ref values) if values == &[0, 0] => 0,
            Some(ref values) if values == &[1, 0] => 1,
            Some(ref values) if values == &[0, 1] => 2,
            _ => -1
        };

        let mut json = BTreeMap::new();
        json.insert("status".to_owned(), status);
        Ok(Response::with((status::Ok, JsonResponse::new(json, None))))
    }
}
