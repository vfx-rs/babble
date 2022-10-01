use env_logger::fmt::Color;
use log::Level;

use anyhow::Result;
use similar::{ChangeTag, TextDiff};

#[cfg(feature = "telemetry")]
pub(crate) fn run_test<F>(closure: F) -> Result<(), Error>
where
    F: FnOnce() -> Result<(), Error>,
{
    use opentelemetry::global;
    use tracing::{error, span};
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::Registry;

    global::set_text_map_propagator(opentelemetry_jaeger::Propagator::new());
    let tracer = opentelemetry_jaeger::new_pipeline()
        .install_simple()
        .unwrap();

    // Create a tracing layer with the configured tracer
    let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

    // Use the tracing subscriber `Registry`, or any other subscriber
    // that impls `LookupSpan`
    let subscriber = Registry::default().with(telemetry);

    // Trace executed code
    let res = tracing::subscriber::with_default(subscriber, || {
        // Spans will be sent to the configured OpenTelemetry exporter
        let root = span!(tracing::Level::TRACE, "app_start", work_units = 2);
        let _enter = root.enter();

        closure()
    });
    global::shutdown_tracer_provider(); // sending remaining spans

    res.map_err(|err| {
        error!("{err}");
        for e in source_iter(&err) {
            error!("  because: {e}")
        }

        err
    })
}

#[cfg(not(feature = "telemetry"))]
pub fn run_test<F>(closure: F) -> Result<()>
where
    F: FnOnce() -> Result<()>,
{
    use tracing::error;
    /*
    use tracing::level_filters::LevelFilter;
    use tracing_subscriber::fmt::format::FmtSpan;
    tracing_subscriber::fmt()
        .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
        .with_max_level(LevelFilter::TRACE)
        .init();
    */

    init_log();

    let res = closure();

    res.map_err(|err| {
        error!("{err}");
        for e in err.chain() {
            error!("  because: {e}")
        }

        err
    })
}

fn init_log() {
    use std::io::Write;

    let _ = env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
        .format_timestamp(None)
        .format(|buf, record| -> Result<(), std::io::Error> {
            let mut level_style = buf.style();
            match record.level() {
                Level::Trace => level_style.set_color(Color::Blue),
                Level::Debug => level_style.set_color(Color::White),
                Level::Info => level_style.set_color(Color::Cyan),
                Level::Warn => level_style.set_color(Color::Yellow),
                Level::Error => level_style.set_color(Color::Red),
            };

            writeln!(
                buf,
                "{} [{}:{}] {}",
                level_style.value(record.level()),
                record.file().unwrap_or(""),
                record.line().unwrap_or(0),
                record.args()
            )
        })
        // .is_test(true)
        .try_init();
}

pub fn compare(left: &str, right: &str) -> Result<()> {
    use colored::*;
    let diff = TextDiff::from_lines(left, right);

    let mut same = true;
    for change in diff.iter_all_changes() {
        match change.tag() {
            ChangeTag::Delete => {
                same = false;
                print!("{}", format!("-| {change}").color(Color::Red));
            }
            ChangeTag::Insert => {
                same = false;
                print!("{}", format!("+| {change}").color(Color::Green));
            }
            ChangeTag::Equal => {
                print!("{}", format!(" | {change}").color(Color::BrightBlack));
            }
        };
    }

    if same {
        Ok(())
    } else {
        println!();
        anyhow::bail!("output did not match")
    }
}
