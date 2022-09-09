use bbl_write::error::Error;
use env_logger::fmt::Color;
use log::Level;
use tracing::{error, span};
use tracing_subscriber::Registry;

pub(crate) fn run_with_telemetry<F>(closure: F) -> Result<(), Error>
where
    F: FnOnce() -> Result<(), Error>,
{
    use opentelemetry::global;
    use tracing_subscriber::layer::SubscriberExt;

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

pub(crate) fn run_with_logging<F>(closure: F) -> Result<(), Error>
where
    F: FnOnce() -> Result<(), Error>,
{
    init_log();
    let res = closure();

    res.map_err(|err| {
        error!("{err}");
        for e in source_iter(&err) {
            error!("  because: {e}")
        }

        err
    })
}

pub(crate) fn init_log() {
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

pub(crate) fn source_iter(
    error: &impl std::error::Error,
) -> impl Iterator<Item = &(dyn std::error::Error + 'static)> {
    SourceIter {
        current: error.source(),
    }
}

struct SourceIter<'a> {
    current: Option<&'a (dyn std::error::Error + 'static)>,
}

impl<'a> Iterator for SourceIter<'a> {
    type Item = &'a (dyn std::error::Error + 'static);

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current;
        self.current = self.current.and_then(std::error::Error::source);
        current
    }
}
