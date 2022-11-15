use std::{ops::Deref, path::Path};

use env_logger::fmt::Color;
use log::Level;

use serde::{Deserialize, Serialize};
use similar::{ChangeTag, TextDiff};

pub enum Error {
    Any(Box<dyn std::error::Error + 'static>),
    Compare,
}

impl<E> From<E> for Error
where
    E: std::error::Error + 'static,
{
    fn from(e: E) -> Self {
        Error::Any(Box::new(e))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Any(e) => write!(f, "{e:?}"),
            Error::Compare => write!(f, "Comparison failed"),
        }
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

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
pub fn run_test<F>(closure: F) -> Result<(), Error>
where
    F: FnOnce() -> Result<(), Error>,
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

        if let Error::Any(ref err) = err {
            for e in source_iter(err.deref()) {
                error!("  because: {e}")
            }
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

pub fn compare(left: &str, right: &str) -> Result<(), Error> {
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
        Err(Error::Compare)
    }
}

pub fn source_iter(
    error: &(impl std::error::Error + ?Sized),
) -> impl Iterator<Item = &(dyn std::error::Error + 'static)> {
    SourceIter {
        current: error.source(),
    }
}

pub struct SourceIter<'a> {
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

#[derive(Debug)]
pub struct Trace(pub backtrace::Backtrace);

impl Trace {
    pub fn new() -> Trace {
        Trace(backtrace::Backtrace::new())
    }
}

impl Default for Trace {
    fn default() -> Self {
        Trace::new()
    }
}

impl std::fmt::Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match std::env::var("RUST_BACKTRACE") {
            Ok(value) if value == "1" => {
                write!(f, "Backtrace:\n{:?}", self.0)
            }
            _ => Ok(()),
        }
    }
}

impl std::error::Error for Trace {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// JSON struct to pass build config between bbl-scan and bbl-bind.
/// Better solution might be to parse the CMakeLists.txt directly since the same information is in there already?
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildConfig {
    /// Currently only "cmake" is supported
    pub build_system: String,
    /// Name of the project. Created sub-projects will be called `project-bind`, `project-c`, `project-sys` and
    /// `project-rs`
    pub project_name: String,
    pub find_packages: Vec<String>,
    pub link_libraries: Vec<String>,
    pub extra_includes: Vec<String>,
    pub cxx_standard: String,
    pub namespaces: Vec<String>,
}

impl Default for BuildConfig {
    fn default() -> Self {
        BuildConfig {
            build_system: "cmake".to_string(),
            project_name: "bind".to_string(),
            find_packages: Vec::new(),
            link_libraries: Vec::new(),
            extra_includes: Vec::new(),
            cxx_standard: "14".to_string(),
            namespaces: Vec::new(),
        }
    }
}

pub fn write_build_config(
    path: impl AsRef<Path>,
    build_config: &BuildConfig,
) -> Result<(), std::io::Error> {
    std::fs::write(path.as_ref(), serde_json::to_string_pretty(&build_config)?)
}

pub fn read_build_config(
    path: impl AsRef<Path>,
) -> Result<BuildConfig, Box<dyn std::error::Error + 'static + Send + Sync>> {
    let s = std::fs::read_to_string(path)?;
    let config = serde_json::from_str::<BuildConfig>(&s)?;
    Ok(config)
}
