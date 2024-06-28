//! Run `valgrind` and parse output
use anyhow::{bail, Context};
use cfg_if::cfg_if;

use crate::Result;
use std::{
    fmt,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
    process::Command,
    str,
};

/// Valgrind version (parsed if possible)
pub enum Version {
    /// Couldn't parse the version
    Raw(String),
    Parsed {
        major: u32,
        minor: u32,
        patch: u32,
    },
}

impl Version {
    fn parse(input: &[u8]) -> Version {
        fn inner(input: &[u8]) -> Option<(u32, u32, u32)> {
            let input = str::from_utf8(input).ok()?.trim();
            let input = input.strip_prefix("valgrind-")?;
            let mut iter = input.split(".");
            let maj = iter.next()?.parse().ok()?;
            let min = iter.next()?.parse().ok()?;
            let patch = iter.next()?.parse().ok()?;
            if iter.next().is_some() {
                return None;
            }
            Some((maj, min, patch))
        }
        match inner(input) {
            Some((major, minor, patch)) => Version::Parsed {
                major,
                minor,
                patch,
            },
            None => Version::Raw(String::from_utf8_lossy(input).trim().to_owned()),
        }
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Version::Raw(version) => write!(f, "unparsed({version})"),
            Version::Parsed {
                major,
                minor,
                patch,
            } => write!(f, "{major}.{minor}.{patch}"),
        }
    }
}

pub fn version() -> Result<Version> {
    let cmd_output = Command::new("valgrind")
        .arg("--tool=cachegrind")
        .arg("--version")
        .output()
        .context("error launching valgrind")?;

    Ok(Version::parse(&cmd_output.stdout))
}

// valgrind command (with or without aslr)

pub fn valgrind_command(without_aslr: bool, arch: &str) -> Command {
    fn basic_valgrind() -> Command {
        Command::new("valgrind")
    }

    // Invoke Valgrind, disabling ASLR if possible because ASLR could noise up the results a bit
    cfg_if! {
        if #[cfg(target_os = "linux")] {
            fn valgrind_without_aslr(arch: &str) -> Command {
                let mut cmd = Command::new("setarch");
                cmd.arg(arch)
                    .arg("-R")
                    .arg("valgrind");
                cmd
            }
        } else if #[cfg(target_os = "freebsd")] {
            fn valgrind_without_aslr(_arch: &str) -> Command {
                let mut cmd = Command::new("proccontrol");
                cmd.arg("-m")
                    .arg("aslr")
                    .arg("-s")
                    .arg("disable");
                cmd
            }
        } else {
            fn valgrind_without_aslr(_arch: &str) -> Command {
                eprintln!("warning: cannot disable ASLR on this platform");
                basic_valgrind()
            }
        }
    }

    if without_aslr {
        basic_valgrind()
    } else {
        valgrind_without_aslr(arch)
    }
}

#[derive(Default, Clone, Debug)]
pub struct CachegrindStats {
    pub instruction_reads: Option<u64>,
    pub instruction_l1_misses: Option<u64>,
    pub instruction_cache_misses: Option<u64>,
    pub data_reads: Option<u64>,
    pub data_l1_read_misses: Option<u64>,
    pub data_cache_read_misses: Option<u64>,
    pub data_writes: Option<u64>,
    pub data_l1_write_misses: Option<u64>,
    pub data_cache_write_misses: Option<u64>,
}

impl CachegrindStats {
    pub fn parse(file: &Path) -> Result<CachegrindStats> {
        let mut events_line = None;
        let mut summary_line = None;

        //println!("{}", std::fs::read_to_string(file).unwrap());

        let file_in = File::open(file).context("unable to open cachegrind output file")?;

        for line in BufReader::new(file_in).lines() {
            let line = line.context("io error reading line from cachegrind output file")?;
            if let Some(line) = line.strip_prefix("events: ") {
                events_line = Some(line.trim().to_owned());
            }
            if let Some(line) = line.strip_prefix("summary: ") {
                summary_line = Some(line.trim().to_owned());
            }
        }

        match (events_line, summary_line) {
            (Some(events), Some(summary)) => {
                let events = events
                    .split_whitespace()
                    .zip(summary.split_whitespace().map(|s| {
                        s.parse::<u64>()
                            .context("unable to parse summary line from cachegrind output file")
                            .unwrap()
                    }));

                let mut stats = CachegrindStats::default();
                for (name, value) in events {
                    match name {
                        "Ir" => stats.instruction_reads = Some(value),
                        "I1mr" => stats.instruction_l1_misses = Some(value),
                        "ILmr" => stats.instruction_cache_misses = Some(value),
                        "Dr" => stats.data_reads = Some(value),
                        "D1mr" => stats.data_l1_read_misses = Some(value),
                        "DLmr" => stats.data_cache_read_misses = Some(value),
                        "Dw" => stats.data_writes = Some(value),
                        "D1mw" => stats.data_l1_write_misses = Some(value),
                        "DLmw" => stats.data_cache_write_misses = Some(value),
                        _ => (),
                    }
                }

                Ok(stats)
            }
            _ => bail!("unable to parse cachegrind output file"),
        }
    }
    pub fn ram_accesses(&self) -> Option<u64> {
        Some(
            self.instruction_cache_misses?
                + self.data_cache_read_misses?
                + self.data_cache_write_misses?,
        )
    }
    pub fn summarize(&self) -> Option<CachegrindSummary> {
        let ram_hits = self.ram_accesses()?;
        let l3_accesses =
            self.instruction_l1_misses? + self.data_l1_read_misses? + self.data_l1_write_misses?;
        let l3_hits = l3_accesses - ram_hits;

        let total_memory_rw = self.instruction_reads? + self.data_reads? + self.data_writes?;
        let l1_hits = total_memory_rw - (ram_hits + l3_hits);

        Some(CachegrindSummary {
            l1_hits,
            l3_hits,
            ram_hits,
        })
    }

    #[rustfmt::skip]
    pub fn subtract(&self, calibration: &CachegrindStats) -> CachegrindStats {
        macro_rules! sub {
            ($l:expr, $r:expr) => {
                match ($l, $r) {
                    (Some(l), Some(r)) => Some(l.saturating_sub(r)),
                    _ => None,
                }
            };
        }
        CachegrindStats {
            instruction_reads: sub!(self.instruction_reads, calibration.instruction_reads),
            instruction_l1_misses: sub!(self.instruction_l1_misses, calibration.instruction_l1_misses),
            instruction_cache_misses: sub!(self.instruction_cache_misses, calibration.instruction_cache_misses),
            data_reads: sub!(self.data_reads, calibration.data_reads),
            data_l1_read_misses: sub!(self.data_l1_read_misses, calibration.data_l1_read_misses),
            data_cache_read_misses: sub!(self.data_cache_read_misses, calibration.data_cache_read_misses),
            data_writes: sub!(self.data_writes, calibration.data_writes),
            data_l1_write_misses: sub!(self.data_l1_write_misses, calibration.data_l1_write_misses),
            data_cache_write_misses: sub!(self.data_cache_write_misses, calibration.data_cache_write_misses),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CachegrindSummary {
    pub l1_hits: u64,
    pub l3_hits: u64,
    pub ram_hits: u64,
}

impl CachegrindSummary {
    pub fn cycles(&self) -> u64 {
        // Uses Itamar Turner-Trauring's formula from https://pythonspeed.com/articles/consistent-benchmarking-in-ci/
        self.l1_hits + (5 * self.l3_hits) + (35 * self.ram_hits)
    }
}
