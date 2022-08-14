use bbl::*;
use log::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug"))
        .format_timestamp(None)
        .init();

    let filename =&std::env::args().into_iter().collect::<Vec<_>>()[1];

    let tu = parse_file(filename, 
        &[            
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
            "-I/home/anders/packages/openexr/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include/Imath",
        ],
        true)?;

    let cur = tu.get_cursor()?;

    let ast = ast_from_namespace("Imath_3_1", cur);

    debug!("\n\n");
    ast.pretty_print(0);

    Ok(())
}
