use std::process::Command;

use anyhow::Context;

use crate::Result;

pub fn get() -> Result<String> {
    fn get_arch_inner() -> Result<String> {
        let output = Command::new("uname").arg("-m").output()?;

        Ok(String::from_utf8(output.stdout)
            .context("`-uname -m` returned invalid unicode.")?
            .trim()
            .to_owned())
    }

    get_arch_inner().context("couldn't get system architecture using `uname -m`")
}
