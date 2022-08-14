use bbl::{*, cursor_kind::CursorKind};
use log::*;

fn dump(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<String>,
    tu: &TranslationUnit,
) {
    if depth > max_depth {
        return;
    }
    let indent = format!("{:width$}", "", width = depth * 4);

    let template_args = if c.num_template_arguments() != -1 {
        format!("[{}]", c.num_template_arguments())
    } else {
        "".to_string()
    };

    match c.kind() {
        CursorKind::IntegerLiteral => {
            println!("{}: {}", c.kind(), tu.token(c.location()).spelling());
        }
        _ => println!("{}: {} {} {}", c.kind(), c.display_name(), c.usr(), template_args),
    }

    if let Ok(ty) = c.ty() {
        let args = ty.template_argument_types()
                                        .map(|v| {
                                            v.iter()
                                             .map(|t| if let Some(t) = t { format!("{}", t)} else { "NonType".to_string() } )
                                             .collect::<Vec<String>>()
                                        });

        let template_args = if let Some(args) = args {
            format!("<{}>", args.join(", "))
        } else {
            "".to_string()
        };
        let indent = format!("{:width$}", "", width = depth.saturating_sub(1) * 4 + 2);
        println!("{indent}𝜏 {}: {} {}", ty.spelling(), ty.kind(), template_args);
    }


    if let Ok(cr) = c.referenced() {
        if cr != c {
                if already_visited.contains(&cr.usr().0) {
                let template_args = if c.num_template_arguments() != -1 {
                    format!("[{}]", c.num_template_arguments())
                } else {
                    "".to_string()
                };

                println!("{indent}↪ {}: {} {} {}", cr.kind(), cr.display_name(), cr.usr(), template_args);
            } else {
                if !cr.usr().0.is_empty() {
                    already_visited.push(cr.usr().0);
                }
                print!("{indent}↪ ");
                dump(cr, depth + 1, max_depth, already_visited, tu);
            }
        }
    }

    let children = c.children();
    if children.len() > 0 {}

    for child in children {
        if !child.usr().0.is_empty() {
            already_visited.push(child.usr().0);
        }

        let icon = match child.kind() {
            CursorKind::ClassDecl => "◈",
            CursorKind::ClassTemplate => "◇",
            _ => "▸",
        };

        print!("{indent}{icon} ");

        dump(child, depth + 1, max_depth, already_visited, tu);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
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

    let mut already_visited = Vec::new();
    dump(cur, 0, 12, &mut already_visited, &tu);

    Ok(())
}

