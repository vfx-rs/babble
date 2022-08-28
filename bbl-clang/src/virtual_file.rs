use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::PathBuf,
};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

/// Write out a temporary file we can pass to libclang to compile.
///
/// Will write to $OUT_DIR if it is set (i.e. if we are being called from build.rs), or std::env::temp_dir() otherwise
pub fn write_temp_file(file_contents: &str) -> Result<PathBuf, std::io::Error> {
    let mut filename = if let Ok(dir) = std::env::var("OUT_DIR") {
        PathBuf::from(&dir)
    } else {
        std::env::temp_dir()
    };

    let mut s = DefaultHasher::new();
    file_contents.hash(&mut s);
    let base = format!("{:x}.cpp", s.finish());
    filename.push(base);
    println!("Writing file {}", filename.display());
    std::fs::write(&filename, file_contents)?;

    Ok(filename)
}

#[cfg(test)]
mod tests {
    use crate::index::Index;
    use crate::*;
    use log::*;

    #[test]
    fn test_virtual_file() -> Result<(), crate::error::Error> {
        let contents = r#"
        #include <stddef.h>
        class TestVirtual {size_t a;};
        "#;
        let path = super::write_temp_file(contents)?;

        let index = Index::new();
        let tu = index.parse_translation_unit(
            &path,
            &["-std=c++14", "-I/usr/include", "-I/usr/local/include"],
        )?;

        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }

        let cur = tu.get_cursor()?;
        let children = cur.children();
        for child in children {
            println!("{child:?}")
        }

        Ok(())
    }
}
