#![cfg_attr(feature = "real_blackbox", feature(test))]

#[cfg(feature = "real_blackbox")]
extern crate test;

use std::{env::args, path::PathBuf, process::Stdio};

#[cfg(feature = "macro")]
pub use iai_macro::iai;
use valgrind::CachegrindStats;

mod arch;
mod macros;
mod valgrind;

type Result<T, E = anyhow::Error> = std::result::Result<T, E>;

/// A function that is opaque to the optimizer, used to prevent the compiler from
/// optimizing away computations in a benchmark.
///
/// This variant is backed by the (unstable) test::black_box function.
#[cfg(feature = "real_blackbox")]
pub fn black_box<T>(dummy: T) -> T {
    test::black_box(dummy)
}

/// A function that is opaque to the optimizer, used to prevent the compiler from
/// optimizing away computations in a benchmark.
///
/// This variant is stable-compatible, but it may cause some performance overhead
/// or fail to prevent code from being eliminated.
#[cfg(not(feature = "real_blackbox"))]
pub fn black_box<T>(dummy: T) -> T {
    unsafe {
        let ret = std::ptr::read_volatile(&dummy);
        std::mem::forget(dummy);
        ret
    }
}

fn run_bench_inner(
    arch: &str,
    executable: &str,
    i: isize,
    name: &str,
    allow_aslr: bool,
) -> Result<(CachegrindStats, Option<CachegrindStats>)> {
    let output_file = PathBuf::from(format!("target/iai/cachegrind.out.{}", name));
    let old_file = output_file.with_file_name(format!("cachegrind.out.{}.old", name));
    std::fs::create_dir_all(output_file.parent().unwrap()).expect("Failed to create directory");

    if output_file.exists() {
        // Already run this benchmark once; move last results to .old
        std::fs::copy(&output_file, &old_file).unwrap();
    }

    let mut cmd = valgrind::valgrind_command(allow_aslr, arch);
    cmd.arg("--tool=cachegrind")
        // Set some reasonable cache sizes. The exact sizes matter less than having fixed sizes,
        // since otherwise cachegrind would take them from the CPU and make benchmark runs
        // even more incomparable between machines.
        .arg("--I1=32768,8,64")
        .arg("--D1=32768,8,64")
        .arg("--LL=8388608,16,64")
        .arg(format!("--cachegrind-out-file={}", output_file.display()))
        .arg(executable)
        .arg("--iai-run")
        .arg(i.to_string());

    let status = cmd
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("Failed to run benchmark in cachegrind");

    if !status.success() {
        panic!(
            "Failed to run benchmark in cachegrind. Exit code: {}",
            status
        );
    }

    let new_stats = CachegrindStats::parse(&output_file)?;
    let old_stats = if old_file.exists() {
        Some(CachegrindStats::parse(&old_file)?)
    } else {
        None
    };

    Ok((new_stats, old_stats))
}

fn run_bench(executable: &str, benches: &[&(&'static str, fn())]) -> Result<()> {
    let version = valgrind::version()?;
    println!("valgrind version: {version}");

    let arch = arch::get()?;
    println!("arch: {arch}");

    let allow_aslr = std::env::var_os("IAI_ALLOW_ASLR").is_some();

    let (calibration, old_calibration) =
        run_bench_inner(&arch, &executable, -1, "iai_calibration", allow_aslr)?;

    for (i, (name, _func)) in benches.iter().enumerate() {
        println!("{}", name);
        let (stats, old_stats) = run_bench_inner(&arch, &executable, i as isize, name, allow_aslr)?;
        let (stats, old_stats) = (
            stats.subtract(&calibration),
            match (&old_stats, &old_calibration) {
                (Some(old_stats), Some(old_calibration)) => {
                    Some(old_stats.subtract(old_calibration))
                }
                _ => None,
            },
        );

        fn signed_short(n: f64) -> String {
            let n_abs = n.abs();

            if n_abs < 10.0 {
                format!("{:+.6}", n)
            } else if n_abs < 100.0 {
                format!("{:+.5}", n)
            } else if n_abs < 1000.0 {
                format!("{:+.4}", n)
            } else if n_abs < 10000.0 {
                format!("{:+.3}", n)
            } else if n_abs < 100000.0 {
                format!("{:+.2}", n)
            } else if n_abs < 1000000.0 {
                format!("{:+.1}", n)
            } else {
                format!("{:+.0}", n)
            }
        }

        fn percentage_diff(new: u64, old: u64) -> String {
            if new == old {
                return " (No change)".to_owned();
            }

            let new: f64 = new as f64;
            let old: f64 = old as f64;

            let diff = (new - old) / old;
            let pct = diff * 100.0;

            format!(" ({:>+6}%)", signed_short(pct))
        }

        if let Some(ir) = stats.instruction_reads {
            println!(
                "  Instructions:     {:>15}{}",
                ir,
                match old_stats.as_ref().and_then(|stats| stats.instruction_reads) {
                    Some(old) => percentage_diff(ir, old),
                    None => "".to_owned(),
                }
            );
        }
        let summary = stats.summarize();
        let old_summary = old_stats.and_then(|stat| stat.summarize());
        if let Some(summary) = summary {
            println!(
                "  L1 Accesses:      {:>15}{}",
                summary.l1_hits,
                match &old_summary {
                    Some(old) => percentage_diff(summary.l1_hits, old.l1_hits),
                    None => "".to_owned(),
                }
            );
            println!(
                "  L2 Accesses:      {:>15}{}",
                summary.l3_hits,
                match &old_summary {
                    Some(old) => percentage_diff(summary.l3_hits, old.l3_hits),
                    None => "".to_owned(),
                }
            );
            println!(
                "  RAM Accesses:     {:>15}{}",
                summary.ram_hits,
                match &old_summary {
                    Some(old) => percentage_diff(summary.ram_hits, old.ram_hits),
                    None => "".to_owned(),
                }
            );
            println!(
                "  Estimated Cycles: {:>15}{}",
                summary.cycles(),
                match &old_summary {
                    Some(old) => percentage_diff(summary.cycles(), old.cycles()),
                    None => "".to_owned(),
                }
            );
        }
        println!();
    }
    Ok(())
}

/// Custom-test-framework runner. Should not be called directly.
#[doc(hidden)]
pub fn runner(benches: &[&(&'static str, fn())]) {
    let mut args_iter = args();
    let executable = args_iter.next().unwrap();

    if let Some("--iai-run") = args_iter.next().as_deref() {
        // In this branch, we're running under cachegrind, so execute the benchmark as quickly as
        // possible and exit
        let index: isize = args_iter.next().unwrap().parse().unwrap();

        // -1 is used as a signal to do nothing and return. By recording an empty benchmark, we can
        // subtract out the overhead from startup and dispatching to the right benchmark.
        if index == -1 {
            return;
        }

        let index = index as usize;

        (benches[index].1)();
        return;
    }

    // Otherwise we're running normally, under cargo
    if let Err(e) = run_bench(&executable, benches) {
        // anyhow prints error report for debug fmt.
        println!("{e:?}");
    }
}
