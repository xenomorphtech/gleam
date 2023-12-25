//take an input text
//take an input scope definition
//push output scope definition
//push output code
//allow for bare option (no root dir, no deps on fs)

use serde::{ser::Error, Deserialize, Serialize};
use serde_json::{json, Result as JsonResult};

use std::{sync::Arc, time::Instant};

use crate::build::download_dependencies;
use gleam_core::{
    // manifest::Manifest,
    analyse::infer_module,
    build::Origin,
    build::{Built, Codegen, Options, ProjectCompiler},
    line_numbers::LineNumbers,
    //manifest::ManifestPackage,
    parse::parse_module,
    //parse::Parsed,
    paths::ProjectPaths,
    type_,
    type_::PRELUDE_MODULE_NAME,
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
    Result,
};

use std::collections;

use crate::{
    // build_lock::BuildLock,
    cli,
    //  dependencies::UseManifest,
    fs::{self, get_current_directory, ConsoleWarningEmitter},
};

use gleam_core::{
    build::{Mode, Target},
    Error as GleamError,
};
use std::io;

pub fn execute() -> Result<(), GleamError> {
    let target = Target::Elixir;

    let perform_codegen = Codegen::All;
    let root_config = crate::config::root_config()?;
    let telemetry = Box::new(cli::Reporter::new());
    let io = fs::ProjectIO::new();
    let start = Instant::now();
    let current_dir = get_current_directory().expect("Failed to get current directory");

    //we would want to compile the dependencies first
    tracing::info!("Compiling packages");
    let options = Options {
        codegen: Codegen::All,
        mode: Mode::Prod,
        target: Some(target),
        warnings_as_errors: false,
    };
    let manifest = download_dependencies()?;

    let compiled_project = {
        let compiler = ProjectCompiler::new(
            root_config,
            options,
            manifest.packages,
            telemetry,
            Arc::new(ConsoleWarningEmitter),
            ProjectPaths::new(current_dir),
            io,
        );
        compiler.compile()?
    };

    match perform_codegen {
        Codegen::All | Codegen::DepsOnly => cli::print_compiled(start.elapsed()),
        Codegen::None => cli::print_checked(start.elapsed()),
    };
    //ast.name = "my/mod".into();
    /*    is_root: bool,
        mut module: UntypedModule,
        origin: Origin,
        package: &EcoString,
        modules: &im::HashMap<EcoString, ModuleInterface>,
        warnings: &TypeWarningEmitter,
        direct_dependencies: &HashMap<EcoString, A>,
    */
    // why in hell the module is mutable?
    let mut repl = Repl::new(compiled_project);
    repl.run();
    Ok(())
}

struct Repl {
    env: String,
    compiled_project: Built,
    direct_dependencies: collections::HashMap<ecow::EcoString, String>,
    modules: im::HashMap<ecow::EcoString, gleam_core::type_::ModuleInterface>,
    ids: UniqueIdGenerator,
}

impl Repl {
    pub fn new(compiled_project: Built) -> Self {
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();

        let _ = modules.insert(PRELUDE_MODULE_NAME.into(), type_::build_prelude(&ids));

        Repl {
            ids,
            modules,
            direct_dependencies: collections::HashMap::new(),
            compiled_project,
            env: "".to_string(),
        }
    }

    pub fn run(&mut self) {
        loop {
            let mut src = String::new();
            match io::stdin().read_line(&mut src) {
                Ok(_) => (),
                _ => return (),
            }

            if src.starts_with("?") {
                println!("\r\noh, a command");
                match src.as_str() {
                    "?exit" => {
                        return ();
                    }
                    _ => println!("unknown command"),
                }
            } else {
                println!("\r\ngot text");

                let res = self.try_proc(&src);
                match res {
                    Err(err) => println!("got some error: {}", err),
                    Ok(_) => (),
                }
            }
        }
    }

    fn try_proc(&mut self, src: &str) -> Result<(), GleamError> {
        let st = gleam_core::parse::parse_statement_sequence(src);
        println!("{:#?}", st);

        let parsed = parse_module(src).map_err(|err| GleamError::Parse {
            path: "snippet".into(),
            src: src.into(),
            error: err,
        })?;

        let module_ast = parsed.module;
        let is_root = true;
        let eco_source: ecow::EcoString = src.into();
        let path: camino::Utf8PathBuf = "snippet".into();
        let ast = infer_module::<String>(
            Target::Elixir,
            is_root,
            &self.ids,
            module_ast,
            Origin::Src,
            &"__the_package".into(),
            &self.modules,
            &TypeWarningEmitter::null(),
            &self.direct_dependencies,
        )
        .map_err(|err| GleamError::Type {
            path: "snippet".into(),
            src: eco_source.clone(),
            error: err,
        })?;
        println!("{:#?}", ast);
        let line_numbers = LineNumbers::new(&src);
        let native_code = gleam_core::elixir::module(&ast, &line_numbers, &path, &eco_source)?;

        let reply = Proto::CompileModuleReply(CompileModuleReply {
            result: ReqResult::Ok(),
            errors: vec![],
            warnings: vec![],
            code: native_code,
        });
        println!("got native code");
        let ser = serde_json::to_string(&reply);
        if let Ok(ser) = ser {
            println!("{}", ser);
        }
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub enum Proto {
    CompileModuleReq(CompileModuleReq),
    CompileModuleReply(CompileModuleReply),
    CompileExpressionReq(CompileExpressionReq),
    CompileExpressionReply(CompileExpressionReply),
}

#[derive(Serialize, Deserialize)]
pub enum ReqResult {
    Ok(),
    Err(),
}

#[derive(Serialize, Deserialize)]
pub struct CompileModuleReq {
    name: String,
    code: String,
}

#[derive(Serialize, Deserialize)]
pub struct CompileModuleReply {
    result: ReqResult,
    errors: Vec<String>,
    warnings: Vec<String>,
    code: String,
}

#[derive(Serialize, Deserialize)]
pub struct CompileExpressionReq {
    code: String,
}

#[derive(Serialize, Deserialize)]
pub struct CompileExpressionReply {
    result: ReqResult,
    code: String,
}
