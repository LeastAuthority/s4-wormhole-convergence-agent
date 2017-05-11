// #![deny(warnings)]

extern crate ws;
extern crate futures;
extern crate hyper;
extern crate tokio_core;

extern crate pretty_env_logger;

use std::env;
use std::collections::HashMap;

use std::time::Duration;

use std::sync::mpsc::{Sender, Receiver, channel};

use std::thread::{JoinHandle, Builder, sleep};


struct Options {
    subscription_manager_url: hyper::Uri,
    wormhole_rendezvous_url: hyper::Uri,
}


fn parse_options(env: env::Args) -> Options {
    let _ = env;
    return Options {
        subscription_manager_url:
        "http://localhost:8000/".parse::<hyper::Uri>().unwrap(),

        wormhole_rendezvous_url:
        "ws://localhost:400/v1".parse::<hyper::Uri>().unwrap(),

    }
}


struct SubscriptionClient {
    thread: JoinHandle<()>,
    subscriptions: Receiver<String>,
}


fn subscription_worker(egress: Sender<String>) {
    loop {
        let _ = egress.send("hi".to_string()).unwrap();
        // Give the server a little time to get going loooooolllll.
        // this is how networking works.!!11!!
        sleep(Duration::from_millis(10));
    }
}


fn subscription_client(url: hyper::Uri) -> SubscriptionClient {
    let _ = url;
    let (sender, receiver) = channel();
    let b = Builder::new().name("subscriptions".to_owned());
    let t = b.spawn(|| {
        subscription_worker(sender);
    }).unwrap();
    return SubscriptionClient {
        thread: t,
        subscriptions: receiver,
    };
}


struct SubscriptionDetails {
}


struct Wormhole {
}


struct SubscriptionState {
    details: SubscriptionDetails,
    wormhole: Wormhole,
}


struct WormholeThing {
    thread: JoinHandle<()>,
    wormholes: HashMap<String, SubscriptionState>,
}


fn wormhole_worker(uri: hyper::Uri, subscriptions: Receiver<String>) {
    let _ = uri;
    loop {
        let subscription = subscriptions.recv().unwrap();
        println!("Subscription: {}", subscription);
    }
}


fn wormhole_client(
    uri: hyper::Uri,
    subscriptions: Receiver<String>
) -> WormholeThing {
    let b = Builder::new().name("wormholes".to_owned());
    let t = b.spawn(|| {
        wormhole_worker(uri, subscriptions);
    }).unwrap();
    return WormholeThing {
        thread: t,
        wormholes: HashMap::new(),
    };
}


fn main() {
    pretty_env_logger::init().unwrap();

    let options = parse_options(env::args());

    let subscriptions = subscription_client(
        options.subscription_manager_url
    );

    let wormholes = wormhole_client(
        options.wormhole_rendezvous_url,
        subscriptions.subscriptions,
    );

    let _ = subscriptions.thread.join();
    let _ = wormholes.thread.join();
}
