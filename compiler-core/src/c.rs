//#include <stdio.h>
//#include <stdlib.h>
//#include <stdint.h>
//
//
//int main() {
//  printf("HI\n");
//
//  void* a = malloc(16);
//
//__sync_add_and_fetch((uint64_t*)a, 1);
//
//
//uint64_t now = __sync_sub_and_fetch((uint64_t*)a, 1);
//if (now == 0) {
//  free(a);
//}
//
//}
mod pattern;
#[cfg(test)]
mod tests;
mod expression;

use std::path::Path;

//use crate::error::Error;
use crate::{ast::*, docvec, io::Utf8Writer, line_numbers::LineNumbers, pretty::*};
use itertools::Itertools;

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.js");

pub type Output<'a> = Result<Document<'a>, Error>;

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    module_scope: im::HashMap<String, usize>,
}

impl<'a> Generator<'a> {
    pub fn new(line_numbers: &'a LineNumbers, module: &'a TypedModule) -> Self {
        Self {
            line_numbers,
            module,
            module_scope: Default::default(),
        }
    }

    pub fn compile(&mut self) -> Output<'a> {
        // Determine what JavaScript imports we need to generate
        // let mut imports = self.collect_imports();

        // Determine what names are defined in the module scope so we know to
        // rename any variables that are defined within functions using the same
        // names.
        //self.register_module_definitions_in_scope();

        // Generate C code for each statement
        let statements = self.collect_definitions().into_iter();
        let mut statements: Vec<_> =
            statements.try_collect()?;


       // .into_iter().chain(
       //     self.module
       //         .statements
       //         .iter()
       //         .flat_map(|s| self.statement(s)),
       // );

        Ok(docvec![statements])
    }

    pub fn statement(&mut self, statement: &'a TypedStatement) -> Vec<Output<'a>> {
        match statement {
            Statement::TypeAlias { .. } | Statement::ExternalType { .. } => vec![],

            // Handled in collect_imports
            Statement::Import { .. } => vec![],

            // Handled in collect_definitions
            Statement::CustomType { .. } => vec![],

            Statement::ModuleConstant {
                public,
                name,
                value,
                ..
            } => vec![],
            //self.module_constant(*public, name, value)
            Statement::Fn {
                arguments,
                name,
                body,
                public,
                ..
            } => vec![self.module_function(*public, name, arguments, body)],

            Statement::ExternalFn {
                public,
                name,
                arguments,
                module,
                fun,
                ..
            } if module.is_empty() => vec![
            //    Ok(self.global_external_function(*public, name, arguments, fun))
            ],

            Statement::ExternalFn { .. } => vec![],
        }
    }

    fn module_function(
        &mut self,
        public: bool,
        name: &'a str,
        args: &'a [TypedArg],
        body: &'a TypedExpr,
    ) -> Output<'a> {
        let argument_names = args
            .iter()
            .map(|arg| arg.names.get_variable_name())
            .collect();
        let mut generator = expression::Generator::new(
            &self.module.name,
            self.line_numbers,
            name,
            argument_names,
            self.module_scope.clone(),
        );
        let head = if public {
            "void "
        } else {
            "void "
        };
        let body = generator.function_body(body, args)?;
        Ok(docvec![
            head,
            maybe_escape_identifier_doc(name),
            fun_args(args, generator.tail_recursion_used),
            " {",
            docvec![line(), body].nest(INDENT).group(),
            line(),
            "}",
        ])
    }

    fn collect_definitions(&mut self) -> Vec<Output<'a>> {
        self.module
            .statements
            .iter()
            .flat_map(|statement| match statement {
                Statement::CustomType {
                    public,
                    constructors,
                    opaque,
                    ..
                } => self.custom_type_definition(constructors, *public, *opaque),

                Statement::Fn { .. }
                | Statement::TypeAlias { .. }
                | Statement::ExternalFn { .. }
                | Statement::ExternalType { .. }
                | Statement::Import { .. }
                | Statement::ModuleConstant { .. } => vec![],
            })
            .collect()
    }

    fn custom_type_definition(
        &mut self,
        constructors: &'a [TypedRecordConstructor],
        public: bool,
        opaque: bool,
    ) -> Vec<Output<'a>> {
        constructors
            .iter()
            .map(|constructor| Ok(self.record_definition(constructor, public, opaque)))
            .collect()
    }

    fn record_definition(
        &self,
        constructor: &'a TypedRecordConstructor,
        public: bool,
        opaque: bool,
    ) -> Document<'a> {
        fn parameter((i, arg): (usize, &TypedRecordConstructorArg)) -> Document<'_> {
            arg.label
                .as_ref()
                .map(|s| maybe_escape_identifier_doc(s))
                .unwrap_or_else(|| Document::String(format!("x{}", i)))
        }

        let head = if public && !opaque {
            "export class "
        } else {
            "class "
        };
        let head = docvec![head, &constructor.name, " extends $CustomType {"];

        if constructor.arguments.is_empty() {
            return head.append("}");
        };

        let parameters = concat(Itertools::intersperse(
            constructor.arguments.iter().enumerate().map(parameter),
            break_(",", ", "),
        ));

        let constructor_body = concat(Itertools::intersperse(
            constructor.arguments.iter().enumerate().map(|(i, arg)| {
                let var = parameter((i, arg));
                match &arg.label {
                    None => docvec!["this[", i, "] = ", var, ";"],
                    Some(name) => docvec!["this.", name, " = ", var, ";"],
                }
            }),
            line(),
        ));

        let class_body = docvec![
            line(),
            "constructor(",
            parameters,
            ") {",
            docvec![line(), "super();", line(), constructor_body].nest(INDENT),
            line(),
            "}",
        ]
        .nest(INDENT);

        docvec![head, class_body, line(), "}"]
    }


}

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    path: &Path,
    src: &str,
    writer: &mut impl Utf8Writer,
) -> Result<(), crate::Error> {
    Generator::new(line_numbers, module)
        .compile()
        .map_err(|error| crate::Error::C {
            path: path.to_path_buf(),
            src: src.to_string(),
            error: Error::ImaTooLazyToDoc,
        })?
        .pretty_print(80, writer)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Unsupported { feature: String, location: SrcSpan },
    ImaTooLazyToDoc 
}

fn fun_args(args: &'_ [TypedArg], tail_recursion_used: bool) -> Document<'_> {
    let mut discards = 0;
    wrap_args(args.iter().map(|a| match a.get_variable_name() {
        None => {
            let doc = if discards == 0 {
                "_".to_doc()
            } else {
                Document::String(format!("_{}", discards))
            };
            discards += 1;
            doc
        }
        Some(name) if tail_recursion_used => Document::String(format!("loop${}", name)),
        Some(name) => maybe_escape_identifier_doc(name),
    }))
}

fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: IntoIterator<Item = Document<'a>>,
{
    break_("", "")
        .append(concat(Itertools::intersperse(
            args.into_iter(),
            break_(",", ", "),
        )))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}

fn wrap_object<'a>(
    items: impl IntoIterator<Item = (Document<'a>, Option<Document<'a>>)>,
) -> Document<'a> {
    let mut empty = true;
    let fields = items.into_iter().map(|(key, value)| {
        empty = false;
        match value {
            Some(value) => docvec![key, ": ", value],
            None => key.to_doc(),
        }
    });
    let fields = concat(Itertools::intersperse(fields, break_(",", ", ")));

    if empty {
        "{}".to_doc()
    } else {
        docvec![
            docvec!["{", break_("", " "), fields]
                .nest(INDENT)
                .append(break_("", " "))
                .group(),
            "}"
        ]
    }
}

fn try_wrap_object<'a>(items: impl IntoIterator<Item = (Document<'a>, Output<'a>)>) -> Output<'a> {
    let fields = items
        .into_iter()
        .map(|(key, value)| Ok(docvec![key, ": ", value?]));
    let fields: Vec<_> = Itertools::intersperse(fields, Ok(break_(",", ", "))).try_collect()?;

    Ok(docvec![
        docvec!["{", break_("", " "), fields]
            .nest(INDENT)
            .append(break_("", " "))
            .group(),
        "}"
    ])
}

fn is_usable_js_identifier(word: &str) -> bool {
    !matches!(
        word,
        // Keywords and reserved works
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar
        "await"
            | "arguments"
            | "break"
            | "case"
            | "catch"
            | "class"
            | "const"
            | "continue"
            | "debugger"
            | "default"
            | "delete"
            | "do"
            | "else"
            | "enum"
            | "export"
            | "extends"
            | "eval"
            | "false"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "implements"
            | "import"
            | "in"
            | "instanceof"
            | "interface"
            | "let"
            | "new"
            | "null"
            | "package"
            | "private"
            | "protected"
            | "public"
            | "return"
            | "static"
            | "super"
            | "switch"
            | "this"
            | "throw"
            | "true"
            | "try"
            | "typeof"
            | "var"
            | "void"
            | "while"
            | "with"
            | "yield"
            // `undefined` to avoid any unintentional overriding.
            | "undefined"
            // `then` to avoid a module that defines a `then` function being
            // used as a `thenable` in JavaScript when the module is imported
            // dynamically, which results in unexpected behaviour.
            // It is rather unfortunate that we have to do this.
            | "then"
    )
}

fn maybe_escape_identifier_string(word: &str) -> String {
    if is_usable_js_identifier(word) {
        word.to_string()
    } else {
        escape_identifier(word)
    }
}

fn escape_identifier(word: &str) -> String {
    format!("{}$", word)
}

fn maybe_escape_identifier_doc(word: &str) -> Document<'_> {
    if is_usable_js_identifier(word) {
        word.to_doc()
    } else {
        Document::String(escape_identifier(word))
    }
}


