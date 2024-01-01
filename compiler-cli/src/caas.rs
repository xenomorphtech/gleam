//take an input text
//take an input scope definition
//push output scope definition
//push output code
//allow for bare option (no root dir, no deps on fs)

use gleam_core::elixir;
use std::io::Write;
use std::process::Child;
use std::process::{Command, Stdio};

use crate::fs::reader;
use gleam_core::ast::TypedExpr;
use gleam_core::ast::TypedStatement;
use gleam_core::ast::{GroupedStatements, TargetedDefinition};
use gleam_core::build::BuildTargets;
use gleam_core::type_::Environment;
use gleam_core::type_::ModuleInterface;
use std::cell::RefCell;
use std::io::Read;
use vec1::Vec1;

use serde::{ser::Error, Deserialize, Serialize};
use serde_json::{json, Result as JsonResult};

use crate::build::download_dependencies;
use gleam_core::ast::Definition;
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
use std::{sync::Arc, time::Instant};

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

pub fn send(p: Proto) {
    println!("{}", serde_json::to_string(&p).expect("should ok"));
}

pub fn execute() -> Result<(), GleamError> {
    send(Proto::Info("starting".to_string()));

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
        mode: Mode::Dev,
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
        Codegen::All | Codegen::DepsOnly => {
            send(Proto::Info(format!("compiled {:?}", start.elapsed())))
        }
        Codegen::None => (),
    };

    // TODO: wrap all this into a container
    let mut ln = LineNumbers::new("some");
    let blocks = RefCell::new(Vec::new());

    //    let mut imported_modules = im::HashMap::<ecow::EcoString, ModuleInterface>::new();
    let ids = UniqueIdGenerator::new();

    let mut importable_modules = compiled_project.module_interfaces.clone();

    let _ = importable_modules.insert(PRELUDE_MODULE_NAME.into(), type_::build_prelude(&ids));

    eprintln!(
        "{:?}",
        &importable_modules
            .keys()
            .into_iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>()
    );

    let warnings = TypeWarningEmitter::null();

    let mut typer_environment = Environment::new(
        UniqueIdGenerator::new(),
        "what".into(),
        Target::Erlang,
        &importable_modules,
        &warnings,
    );
    let mut typer = type_::ExprTyper::new(&mut typer_environment, BuildTargets::all());

    let mut repl = Repl::new(
        compiled_project,
        &mut ln,
        blocks,
        typer_environment,
        importable_modules.clone(),
    );

    repl.run();

    Ok(())
}

struct Repl<'a> {
    env: elixir::Env<'a>,
    //typer_environment: Environment<'a>,
    compiled_project: Built,
    direct_dependencies: collections::HashMap<ecow::EcoString, String>,
    modules: im::HashMap<ecow::EcoString, gleam_core::type_::ModuleInterface>,
    ids: UniqueIdGenerator,
    //child: Child,
    //line_numbers: LineNumbers,
    blocks: RefCell<Vec<Vec1<TypedStatement>>>,
    environment: Environment<'a>,
}

impl<'a> Repl<'a> {
    pub fn new(
        compiled_project: Built,
        ln: &'a mut LineNumbers,
        blocks: RefCell<Vec<Vec1<TypedStatement>>>,
        environment: Environment<'a>,
        modules: im::HashMap<ecow::EcoString, ModuleInterface>,
    ) -> Self {
        let ids = UniqueIdGenerator::new();

        let pipe = Stdio::piped();

        //run an execution thread
        //        let mut child = Command::new("elixir")
        //            .arg("shell.ex")
        //            .stdin(pipe)
        //            .spawn()
        //            .expect("failed to execute child");

        Repl {
            ids,
            modules,
            direct_dependencies: collections::HashMap::new(),
            compiled_project,
            env: elixir::Env::new("repl", "freetype", ln),
            //line_numbers: ln,
            //child,
            blocks,
            environment,
        }
    }

    pub fn run(&mut self) {
        loop {
            //let _ = io::stdout().write("> ".as_bytes());

            //io::stdout().flush().expect("flush failed!");

            let mut buf = vec![0u8; 4];
            io::stdin().read_exact(&mut buf).expect("stream error");
            let len = u32::from_le_bytes(buf[0..4].try_into().unwrap());

            let mut req_buf = vec![0u8; len as usize];
            io::stdin().read_exact(&mut req_buf).expect("stream error");

            //println!(
            //    "req: {}",
            //    String::from_utf8(req_buf.clone()).expect("utf8 plz")
            //);
            let req: Proto = serde_json::from_slice(&req_buf).expect("wrong serialization of req");

            match req {
                Proto::CompileExpressionReq(CompileExpressionReq { code }) => {
                    let res = self
                        .try_proc2(code.to_string())
                        .or_else(|a| {
                            eprintln!("error: {:?}", a);
                            Err(a)
                        })
                        .or_else(|_| self.try_proc(code.to_string()));
                    match res {
                        Err(err) => eprintln!("got some error: {}", err),
                        Ok(_) => (),
                    }
                }
                _ => {
                    send(Proto::Info("unhandled request".to_string()));
                }
            }
        }
    }

    fn try_proc2(&mut self, src: String) -> Result<(), GleamError> {
        let statement =
            gleam_core::parse::parse_definition(src.as_str()).map_err(|err| GleamError::Parse {
                path: "snippet".into(),
                src: src.clone().into(),
                error: err,
            })?;

        //let env = self.typer.environment;

        let st = vec![statement.clone()];
        let statements = GroupedStatements::new(st.into_iter().map(|a| a.definition));

        //Origin::Src
        //&self.modules

        for import in statements.imports {
            self.environment
                .register_import(Origin::Src, &self.environment.importable_modules, &import)
                .map_err(|err| GleamError::Type {
                    path: "snippet".into(),
                    src: src.clone().into(),
                    error: err,
                })?;
        }

        //let _ = self
        //    .environment
        //    .run(Origin::Src, statements.imports.clone(), &self.modules);

        //eprintln!("2 - {:?}", statements.imports);

        let mut typer = type_::ExprTyper::new(&mut self.environment, BuildTargets::all());

        // for imp in statements.imports {
        //     let statement = record_imported_items_for_use_detection(
        //         imp,
        //         package,
        //         direct_dependencies,
        //         warnings,
        //         &env,
        //     )?;
        //     // typed_statements.push(statement);
        // }
        //for t in statements.custom_types {
        //    let statement = infer_custom_type(t, &mut env)?;
        //    // typed_statements.push(statement);
        //}
        //for t in statements.type_aliases {
        //    let statement = gleam_core::analyse::insert_type_alias(t, &mut env)?;
        //    //typed_statements.push(statement);
        //}

        let gleam_core::ast::TargetedDefinition {
            definition: def, ..
        } = statement;

        match def {
            Definition::Function(f) => {
                let typed = typer.infer_statements(f.body);
                eprintln!("{:?}", typed);
            }

            Definition::TypeAlias(ta) => (),

            Definition::CustomType(ct) => (),

            Definition::Import(import) => (),

            Definition::ModuleConstant(con) => (),
        }

        send(Proto::CompileExpressionReply(CompileExpressionReply {
            result: ReqResult::Ok(),
            code: "".to_string(),
        }));

        Ok(())
    }

    fn try_proc(&mut self, src: String) -> Result<(), GleamError> {
        let statement = gleam_core::parse::parse_statement_sequence(src.as_str());

        //println!("{:#?}", statement);
        let mut typer = type_::ExprTyper::new(&mut self.environment, BuildTargets::all());

        // compile to expression
        match statement {
            Err(e) => {
                send(Proto::Info(format!("parsing error {:?}", e)));
            }
            Ok(p) => {
                let typed = typer.infer_statements(p);
                match typed {
                    Err(e) => {
                        send(Proto::Info(format!("type error {:?}", e)));
                    }
                    Ok(typed) => {
                        //println!("before codegen : {:?}", typed);
                        {
                            let mut my_ref = self.blocks.borrow_mut();
                            my_ref.push(typed.clone());
                        }
                        let mut env = self.env.clone();
                        let x = self.blocks.borrow();

                        let code = elixir::block(&typed, &mut env);

                        //println!("{:#?}", env);

                        let pcode = code.to_pretty_string(80);
                        //println!("code: {:?}", pcode);
                        // try to run it
                        let bytes = pcode.as_bytes();
                        send(Proto::CompileExpressionReply(CompileExpressionReply {
                            result: ReqResult::Ok(),
                            code: pcode,
                        }));
                    }
                }
            }
        }

        return Ok(());
    }

    /*

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
    */
}

#[derive(Serialize, Deserialize)]
pub enum Proto {
    CompileModuleReq(CompileModuleReq),
    CompileModuleReply(CompileModuleReply),
    CompileExpressionReq(CompileExpressionReq),
    CompileExpressionReply(CompileExpressionReply),
    Info(String),
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
