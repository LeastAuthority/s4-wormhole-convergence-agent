// #![deny(warnings)]

extern crate ws;
extern crate futures;
extern crate hyper;
extern crate tokio_core;

extern crate pretty_env_logger;

use std::io;
use std::io::Write;
use std::env;
use std::collections::HashMap;

use futures::{Future, Stream};

use std::time::Duration;

use std::sync::mpsc::{SyncSender, Receiver, sync_channel};

use std::thread::{JoinHandle, Builder, sleep};

use tokio_core::reactor::Core;

use hyper::Client;

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
        "ws://localhost:4000/v1".parse::<hyper::Uri>().unwrap(),

    }
}


struct SubscriptionClient {
    thread: JoinHandle<()>,
    subscriptions: Receiver<String>,
}


fn subscription_worker(uri: hyper::Uri, egress: SyncSender<String>, delay: Receiver<()>) {
    let mut core = Core::new().unwrap();
    let handle = core.handle();
    let client = Client::new(&handle);

    loop {
        let subscriptions = client.get(uri).and_then(|res| {
            println!("Response: {}", res.status());
            println!("Headers: \n{}", res.headers());
            res.body().for_each(|chunk| {
                io::stdout().write_all(&chunk).map_err(From::from)
            });
        });
        let _ = egress.send("hi".to_string()).unwrap();
        let _ = delay.recv();
    }
}


fn fixed_intervals(interval: Duration) -> Receiver<()> {
    let (sender, receiver) = sync_channel(0);

    Builder::new().name("timer".to_owned()).spawn(|| {
        loop {
            sleep(interval);
            let _ = sender.send(()).unwrap();
        }
    });

    return receiver;
}


fn subscription_client(uri: hyper::Uri) -> SubscriptionClient {
    let (sender, receiver) = sync_channel(0);
    let b = Builder::new().name("subscriptions".to_owned());
    let t = b.spawn(|| {
        subscription_worker(uri, sender, fixed_intervals(Duration::from_secs(60)));
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
