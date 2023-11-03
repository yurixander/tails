use std::fs;

use structopt::StructOpt;
use tails::{pass, symbol_table};

#[derive(StructOpt)]
enum Command {
  Build {
    #[structopt(long, parse(from_os_str))]
    path: std::path::PathBuf,
  },
  Run {
    #[structopt(long, parse(from_os_str))]
    path: std::path::PathBuf,
  },
}

#[derive(StructOpt)]
#[structopt(name = "tails", about = "Driver for the tails compiler")]
struct Opt {
  #[structopt(subcommand)]
  subcommand: Command,
}

#[derive(Debug, serde::Deserialize)]
struct PackageManifest {
  name: String,
  version: String,
}

// TODO: Need to output with the file name corresponding to the module. This is temporary.
const OUTPUT_LLVM_IR_FILENAME: &str = "output.ll";
const OUTPUT_OBJECT_FILENAME: &str = "output.o";
const OUTPUT_EXECUTABLE_FILENAME: &str = "output";

fn write_llvm_ir(
  base_path: &std::path::PathBuf,
  contents: String,
) -> Result<(), Box<dyn std::error::Error>> {
  let output_path = base_path.join(OUTPUT_LLVM_IR_FILENAME);

  fs::write(output_path, contents)?;

  Ok(())
}

fn compile_and_link_llvm_ir(
  base_path: &std::path::PathBuf,
) -> Result<(), Box<dyn std::error::Error>> {
  let llvm_ir_file_path = base_path.join(OUTPUT_LLVM_IR_FILENAME);

  // Run LLVM's `llc` to compile the output LLVM IR file.
  let llc_output = std::process::Command::new("llc")
    .arg("-filetype=obj")
    .arg(llvm_ir_file_path.clone())
    .output()?;

  if !llc_output.status.success() {
    return Err(
      format!(
        "Failed to compile LLVM IR file `{}`",
        llvm_ir_file_path.display()
      )
      .into(),
    );
  }

  // CONSIDER: Making this configurable.
  // Run the system linker to link the object file into an executable.
  let linker_output = std::process::Command::new("gcc")
    .current_dir(base_path)
    .arg("-o")
    .arg(OUTPUT_EXECUTABLE_FILENAME)
    .arg(OUTPUT_OBJECT_FILENAME)
    .output()?;

  if !linker_output.status.success() {
    return Err("Failed to link object file into executable".into());
  }

  Ok(())
}

fn run_output_executable(base_path: &std::path::PathBuf) -> Result<(), Box<dyn std::error::Error>> {
  let executable_file_path = base_path.join(OUTPUT_EXECUTABLE_FILENAME);
  let output = std::process::Command::new(executable_file_path).output()?;

  println!("{}", String::from_utf8_lossy(&output.stdout));
  println!("-- process terminated with {} --", output.status);

  if !output.status.success() {
    return Err("Failed to run executable file".into());
  }

  Ok(())
}

fn fetch_package_manifest(
  base_path: &std::path::PathBuf,
) -> Result<PackageManifest, Box<dyn std::error::Error>> {
  if !base_path.exists() || !base_path.is_dir() {
    return Err(
      format!(
        "Package directory `{}` does not exist or is not a directory",
        base_path.display()
      )
      .into(),
    );
  }

  const PACKAGE_MANIFEST_FILENAME: &str = "tails.toml";

  let package_manifest_path = base_path.join(PACKAGE_MANIFEST_FILENAME);

  if !package_manifest_path.exists() || !package_manifest_path.is_file() {
    return Err(
      format!(
        "Package manifest file `{}` does not exist or is not a file",
        package_manifest_path.display()
      )
      .into(),
    );
  }

  let toml_string = fs::read_to_string(package_manifest_path)?;

  Ok(toml::from_str(&toml_string)?)
}

fn require<T>(result: tails::diagnostic::Maybe<T>) -> T {
  match result {
    Ok(value) => value,
    Err(diagnostics) => {
      for diagnostic in diagnostics.iter() {
        println!("{:?}", diagnostic);
      }

      panic!("Encountered diagnostics");
    }
  }
}

fn build(base_path: &std::path::PathBuf) -> Result<String, Box<dyn std::error::Error>> {
  let package_manifest = fetch_package_manifest(&base_path)?;
  let package_source_directory = base_path.join("src");

  if !package_source_directory.exists() || !package_source_directory.is_dir() {
    return Err(
      format!(
        "Package source directory `{}` does not exist or is not a directory",
        package_source_directory.display()
      )
      .into(),
    );
  }

  let source_files = fs::read_dir(&package_source_directory)?
    .map(|entry| entry.map(|subentry| subentry.path()))
    .collect::<Result<Vec<_>, std::io::Error>>()?;

  if source_files.is_empty() {
    return Err(
      format!(
        "Package source directory `{}` is empty and contains no source files",
        package_source_directory.display()
      )
      .into(),
    );
  }

  let source_file_contents = source_files
    .iter()
    .map(|path| fs::read_to_string(path))
    .collect::<Result<Vec<_>, std::io::Error>>()?;

  let mut modules = tails::ast::Package::new();

  for (path, contents) in source_files.iter().zip(source_file_contents.iter()) {
    // FIXME: Use proper error handling instead of panicking.
    let module_name = path.file_stem().unwrap().to_str().unwrap().to_string();

    // OPTIMIZE: Awaiting lexer to accept some sort of reference type to avoid cloning contents which can be costly.
    let tokens = require(tails::lexer::Lexer::lex_all(contents));

    // REVISE: Need to filter out whitespace and comments on the parser instead.
    let filtered_tokens = tokens
      .into_iter()
      .filter(|token| {
        !matches!(
          token.0,
          tails::lexer::TokenKind::Whitespace(_) | tails::lexer::TokenKind::Comment(_)
        )
      })
      .collect();

    let mut parser = tails::parser::Parser::new(filtered_tokens);

    let qualifier = tails::symbol_table::Qualifier {
      package_name: package_manifest.name.clone(),
      module_name,
    };

    modules.insert(qualifier.clone(), require(parser.parse_module(qualifier)));
  }

  let mut pass_manager = tails::pass::PassManager::new(&modules);

  pass_manager.add_all_passes();

  let mut pass_results = pass_manager.run(0);

  for diagnostic in pass_results.diagnostics.iter() {
    let severity = if diagnostic.is_error() {
      "error"
    } else {
      "warning"
    };

    println!("[{}] {:?}", severity, diagnostic);
  }

  if tails::diagnostic::DiagnosticsHelper::contains_errors_(&pass_results.diagnostics) {
    panic!("Encountered irrecoverable errors; aborting build");
  }

  let llvm_lowering_pass_result = pass_results
    .results
    .remove(&pass::PassId::LlvmLowering)
    .expect("the LLVM lowering pass should have been added and run");

  let llvm_ir_output = match llvm_lowering_pass_result {
    pass::PassResult::LlvmIrOutput(llvm_ir_output) => llvm_ir_output,
    _ => panic!("the LLVM lowering pass should have returned an LLVM IR output"),
  };

  Ok(llvm_ir_output)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let opt = Opt::from_args();

  match opt.subcommand {
    Command::Build { path } => {
      let llvm_ir_output = build(&path)?;

      write_llvm_ir(&path, llvm_ir_output)?;
      compile_and_link_llvm_ir(&path)?;
    }
    Command::Run { path } => {
      let llvm_ir_output = build(&path)?;

      write_llvm_ir(&path, llvm_ir_output)?;
      compile_and_link_llvm_ir(&path)?;
      run_output_executable(&path)?;
    }
  };

  Ok(())
}
