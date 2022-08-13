use bbl::index::Index;
use bbl::*;
use log::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug"))
        .format_timestamp(None)
        .init();

    let index = Index::new();
    let tu = index.parse_translation_unit(
        &std::env::args().into_iter().collect::<Vec<_>>()[1],
        &[
            "-x",
            "c++",
            "-std=c++14",
            "-isystem/home/anders/packages/llvm/14.0.0/include/c++/v1/",
            "-isystem/home/anders/packages/llvm/14.0.0/include/x86_64-unknown-linux-gnu/c++/v1/",
            "-isystem/usr/include/c++/9",
            "-isystem/usr/lib/gcc/x86_64-linux-gnu/9/include/", 
            "-isystem/usr/include/x86_64-linux-gnu",
            "-isystem/usr/include/linux",
            "-isystem/usr/local/include",
            "-isystem/usr/include",
            "-I/home/anders/packages/openexr/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include/Imath",
        ],
    )?;

    debug!("{} diagnostics", tu.num_diagnostics());
    for d in tu.diagnostics() {
        match d.severity() {
            Severity::Ignored => debug!("{}", d),
            Severity::Note => info!("{}", d),
            Severity::Warning => warn!("{}", d),
            Severity::Error | Severity::Fatal => error!("{}", d),
        }
    }
    let cur = tu.get_cursor()?;

    let binding = bind_namespace("Imath_3_1", cur);

    debug!("\n\n");
    binding.pretty_print(0);

    /*
    let ns_imath = cur
        .children_of_kind(CursorKind::Namespace, false)
        .get(0)
        .unwrap()
        .clone();

    let type_alias_decls = ns_imath.children_of_kind(CursorKind::TypeAliasDecl, false);
    */

    Ok(())
}
