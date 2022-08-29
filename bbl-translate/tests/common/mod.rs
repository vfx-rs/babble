pub fn init_log() {
    let _ = env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
        .format_timestamp(None)
        // .is_test(true)
        .try_init();
}