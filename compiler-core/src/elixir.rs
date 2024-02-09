mod pattern;
//#[cfg(test)]
//mod tests;

mod typeprinter;
use crate::{
    ast::{CustomType, Import, ModuleConstant, TypeAlias, *},
    docvec,
    line_numbers::LineNumbers,
    pretty::*,
};
use crate::{
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, ValueConstructor, ValueConstructorVariant,
    },
    Result,
};
use camino::Utf8Path;
use ecow::EcoString;
use heck::ToSnakeCase;
use itertools::Itertools;
use pattern::pattern;
use regex::{Captures, Regex};
use std::sync::OnceLock;
use std::{collections::HashMap, ops::Deref, str::FromStr, sync::Arc};
use typeprinter::TypePrinter;
use vec1::Vec1;

use crate::type_::fields;

const INDENT: isize = 2;

pub const PRELUDE: &str = include_str!("../templates/prelude.mjs");

pub type Output<'a> = Result<Document<'a>, crate::Error>;

#[derive(Debug, Clone)]
pub struct Env<'a> {
    module: &'a str,
    function: &'a str,
    line_numbers: &'a LineNumbers,
    current_scope_vars: im::HashMap<String, usize>,
    erl_function_scope_vars: im::HashMap<String, usize>,
}

impl<'env> Env<'env> {
    pub fn new(module: &'env str, function: &'env str, line_numbers: &'env LineNumbers) -> Self {
        let vars: im::HashMap<_, _> = std::iter::once(("_".into(), 0)).collect();
        Self {
            current_scope_vars: vars.clone(),
            erl_function_scope_vars: vars,
            line_numbers,
            function,
            module,
        }
    }

    pub fn local_var_name<'a>(&mut self, name: &str) -> Document<'a> {
        if is_elixir_reserved_word(name) {
            return Document::String(name.to_string()).append("__reserved".to_doc());
        }
        match self.current_scope_vars.get(name) {
            None => {
                let _ = self.current_scope_vars.insert(name.to_string(), 0);
                let _ = self.erl_function_scope_vars.insert(name.to_string(), 0);
                Document::String(variable_name(name))
            }
            Some(0) => Document::String(variable_name(name)),
            Some(n) => {
                use std::fmt::Write;
                let mut name = variable_name(name);
                write!(name, "_{n}").expect("pushing number suffix to name");
                Document::String(name)
            }
        }
    }

    pub fn next_local_var_name<'a>(&mut self, name: &str) -> Document<'a> {
        let next = self.erl_function_scope_vars.get(name).map_or(0, |i| i + 1);
        let _ = self.erl_function_scope_vars.insert(name.to_string(), next);
        let _ = self.current_scope_vars.insert(name.to_string(), next);
        self.local_var_name(name)
    }
}

fn statement<'a>(statement: &'a TypedStatement, env: &mut Env<'a>) -> Document<'a> {
    match statement {
        Statement::Expression(e) => expr(e, env),
        Statement::Assignment(a) => assignment(a, env).append(line()),
        Statement::Use(_) => {
            unreachable!("Use statements must not be present for Erlang generation")
        }
    }
}

#[derive(Debug)]
pub struct Generator<'a> {
    line_numbers: &'a LineNumbers,
    module: &'a TypedModule,
    _module_scope: im::HashMap<EcoString, usize>,
    _current_module_name_segments_count: usize,
}

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    path: &Utf8Path,
    src: &EcoString,
) -> Result<String, crate::Error> {
    let document = Generator::new(line_numbers, module)
        .compile()
        .map_err(|_error| crate::Error::Elixir {
            path: path.to_path_buf(),
            src: src.clone(),
        })?;
    Ok(document.to_pretty_string(80))
}

impl<'a> Generator<'a> {
    pub fn new(line_numbers: &'a LineNumbers, module: &'a TypedModule) -> Self {
        let _current_module_name_segments_count = module.name.split('/').count();

        Self {
            _current_module_name_segments_count,
            line_numbers,
            module,
            _module_scope: Default::default(),
        }
    }

    pub fn compile(&mut self) -> Output<'a> {
        let name = gleam_module_name_to_elixir(self.module.name.as_str());

        //let type_defs = if type_defs.is_empty() {
        //    nil()
        //} else {
        //    concat(Itertools::intersperse(type_defs.into_iter(), lines(2))).append(lines(2))
        //};

        let statements = concat(Itertools::intersperse(
            self.module
                .definitions
                .iter()
                .flat_map(|s| module_statement(s, &self.module.name, self.line_numbers)),
            lines(2),
        ));

        // TODO: kernel except only if redefening kernel functions
        Ok(docvec!(
            "defmodule ",
            name,
            " do",
            line()
                .append("import Kernel, except: [min: 2, max: 2, length: 1, to_string: 1]".to_doc())
                .append(line())
                .append(line())
                .append(statements)
                .nest(INDENT),
            line(),
            "end",
            line()
        ))
    }
}

fn gleam_module_name_to_elixir(module: &str) -> Document<'_> {
    Document::String(escape_atom_string(module.replace('/', ".")))
}

fn atom_string(value: String) -> Document<'static> {
    Document::String(escape_atom_string(value))
}

fn atom_pattern() -> &'static Regex {
    static ATOM_PATTERN: OnceLock<Regex> = OnceLock::new();
    ATOM_PATTERN.get_or_init(|| Regex::new(r"^[a-z][a-z0-9_]*$").expect("atom RE regex"))
}

fn atom(value: &str) -> Document<'_> {
    // in elixir atoms are prefixed with Elixir.
    if is_elixir_reserved_word(value) {
        // Escape because of keyword collision
        Document::String(format!(":'{value}'"))
    } else if atom_pattern().is_match(value) {
        // No need to escape
        Document::String(format!(":{value}"))
    } else {
        // Escape because of characters contained
        Document::String(format!(":'{value}'"))
    }
}

fn escape_function_name(name: &str) -> Document<'_> {
    Document::Str(name)
}

fn escape_atom_string(value: String) -> String {
    // in elixir lowercase atoms are prefixed with `:`
    if atom_pattern().is_match(value.as_str()) {
        // No need to escape
        format!(":{value}")
    } else {
        // Escape because of characters contained
        format!(":'{value}'")
    }
}

fn module_statement<'a>(
    statement: &'a TypedDefinition,
    module: &'a str,
    line_numbers: &'a LineNumbers,
) -> Option<Document<'a>> {
    match statement {
        Definition::TypeAlias(TypeAlias { .. })
        | Definition::CustomType(CustomType { .. })
        | Definition::Import(Import { .. })
        | Definition::ModuleConstant(ModuleConstant { .. }) => None,

        Definition::Function(function) => module_function(function, module, line_numbers),
    }
}

fn module_function<'a>(
    function: &'a TypedFunction,
    module: &'a str,
    line_numbers: &'a LineNumbers,
) -> Option<Document<'a>> {
    // Private external functions don't need to render anything, the underlying
    // Erlang implementation is used directly at the call site.
    if function.external_elixir.is_some() && !function.public {
        return None;
    }

    let mut env = Env::new(module, &function.name, line_numbers);
    //let var_usages = HashMap::new();
    //collect_type_var_usages(
    //    HashMap::new(),
    //    std::iter::once(&function.return_type).chain(function.arguments.iter().map(|a| &a.type_)),
    //);
    let _type_printer = TypePrinter::new(module);
    //.with_var_usages(&var_usages);
    let arguments = fun_args(&function.arguments, &mut env);

    let body = function
        .external_elixir
        .as_ref()
        .map(|(module, function)| docvec![atom(module), ".", function, arguments.clone()])
        .unwrap_or_else(|| statement_sequence(&function.body, &mut env));

    let doc = Document::Str("def ")
        .append(escape_function_name(function.name.as_str()))
        .append(arguments)
        .append(" do")
        .append(line().append(body).nest(INDENT).group())
        .append(line())
        .append("end");
    Some(doc)
}

fn fun_args<'a>(args: &'a [TypedArg], env: &mut Env<'a>) -> Document<'a> {
    wrap_args(args.iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            env.next_local_var_name(name)
        }
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

fn variable_name(name: &str) -> String {
    //let mut chars = name.chars();
    //let first_char = chars.next();
    //let first_uppercased = first_char.into_iter().flat_map(char::to_uppercase);

    //first_uppercased.chain(chars).collect()
    name.to_string()
}

pub fn is_elixir_reserved_word(name: &str) -> bool {
    matches!(
        name,
        "!" | "receive"
            | "bnot"
            | "div"
            | "rem"
            | "band"
            | "bor"
            | "bxor"
            | "bsl"
            | "bsr"
            | "not"
            | "and"
            | "or"
            | "xor"
            | "when"
            | "end"
            | "fun"
            | "try"
            | "catch"
            | "after"
            | "begin"
            | "let"
            | "query"
            | "cond"
            | "if"
            | "do"
            | "case"
            | "in"
            //kernel imports
            | "min"
            | "max"
            | "size"
    )
}

fn unicode_escape_sequence_pattern() -> &'static Regex {
    static PATTERN: OnceLock<Regex> = OnceLock::new();
    PATTERN.get_or_init(|| {
        Regex::new(r#"(\\+)(u)"#).expect("Unicode escape sequence regex cannot be constructed")
    })
}

fn string_inner(value: &str) -> Document<'_> {
    let content = unicode_escape_sequence_pattern()
        // `\\u`-s should not be affected, so that "\\u..." is not converted to
        // "\\x...". That's why capturing groups is used to exclude cases that
        // shouldn't be replaced.
        .replace_all(value, |caps: &Captures<'_>| {
            let slashes = caps.get(1).map_or("", |m| m.as_str());

            if slashes.len() % 2 == 0 {
                format!("{slashes}u")
            } else {
                format!("{slashes}x")
            }
        })
        .to_string();
    Document::String(content)
}

fn string(value: &str) -> Document<'_> {
    string_inner(value).surround("\"", "\"")
}

fn tuple<'a>(elems: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    concat(Itertools::intersperse(elems.into_iter(), break_(",", ", ")))
        .nest(INDENT)
        .surround("{", "}")
        .group()
}

fn string_concatenate<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let left = string_concatenate_argument(left, env);
    let right = string_concatenate_argument(right, env);

    bit_array([left, right]).surround("(", ")")
}

fn string_concatenate_argument<'a>(value: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match value {
        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant:
                        ValueConstructorVariant::ModuleConstant {
                            literal: Constant::String { value, .. },
                            ..
                        },
                    ..
                },
            ..
        }
        | TypedExpr::String { value, .. } => docvec!['"', string_inner(value), "\"::utf8"],

        TypedExpr::Var {
            name,
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::LocalVariable { .. },
                    ..
                },
            ..
        } => docvec![env.local_var_name(name), "::binary"],

        TypedExpr::BinOp {
            name: BinOp::Concatenate,
            ..
        } => docvec![expr(value, env), "::binary"],

        _ => docvec!["(", maybe_block_expr(value, env), ")::binary"],
    }
}

fn bit_array<'a>(elems: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    concat(Itertools::intersperse(elems.into_iter(), break_(",", ", ")))
        .nest(INDENT)
        .surround("<<", ">>")
        .group()
}

fn const_segment<'a>(
    value: &'a TypedConstant,
    options: &'a [BitArrayOption<TypedConstant>],
    env: &mut Env<'a>,
) -> Document<'a> {
    let document = match value {
        // Skip the normal <<value/utf8>> surrounds
        Constant::String { value, .. } => value.to_doc().surround("\"", "\""),

        // As normal
        Constant::Int { .. } | Constant::Float { .. } | Constant::BitArray { .. } => {
            const_inline(value, env)
        }

        // Wrap anything else in parentheses
        value => const_inline(value, env).surround("(", ")"),
    };

    let size = |value: &'a TypedConstant, env: &mut Env<'a>| match value {
        Constant::Int { .. } => Some("::".to_doc().append(const_inline(value, env))),
        _ => Some(
            "::size"
                .to_doc()
                .append(const_inline(value, env).surround("(", ")")),
        ),
    };

    let unit = |value: &'a u8| Some(Document::String(format!("unit:{value}")));

    bit_array_segment(document, options, size, unit, true, env)
}

fn expr_segment<'a>(
    value: &'a TypedExpr,
    options: &'a [BitArrayOption<TypedExpr>],
    env: &mut Env<'a>,
) -> Document<'a> {
    let mut value_is_a_string_literal = false;

    let document = match value {
        // Skip the normal <<value/utf8>> surrounds and set the string literal flag
        TypedExpr::String { value, .. } => {
            value_is_a_string_literal = true;
            value.to_doc().surround("\"", "\"")
        }

        // As normal
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::BitArray { .. } => expr(value, env),

        // Wrap anything else in parentheses
        value => expr(value, env).surround("(", ")"),
    };

    let size = |expression: &'a TypedExpr, env: &mut Env<'a>| match expression {
        TypedExpr::Int { value, .. } => {
            let v = value.replace("_", "");
            let v = u64::from_str(&v).unwrap_or(0);
            Some(Document::String(format!("::size({v})")))
        }

        _ => {
            let inner_expr = expr(expression, env).surround("(", ")");
            // The value of size must be a non-negative integer, we use lists:max here to ensure
            // it is at least 0;
            let value_guard = "::size(:lists.max(["
                .to_doc()
                .append(inner_expr)
                .append(", 0]))")
                .group();
            Some(value_guard)
        }
    };

    let unit = |value: &'a u8| Some(Document::String(format!("unit{value}")));

    bit_array_segment(
        document,
        options,
        size,
        unit,
        value_is_a_string_literal,
        env,
    )
}

fn bit_array_segment<'a, Value: 'a, SizeToDoc, UnitToDoc>(
    mut document: Document<'a>,
    options: &'a [BitArrayOption<Value>],
    mut size_to_doc: SizeToDoc,
    mut unit_to_doc: UnitToDoc,
    value_is_a_string_literal: bool,
    env: &mut Env<'a>,
) -> Document<'a>
where
    SizeToDoc: FnMut(&'a Value, &mut Env<'a>) -> Option<Document<'a>>,
    UnitToDoc: FnMut(&'a u8) -> Option<Document<'a>>,
{
    let mut size: Option<Document<'a>> = None;
    let mut unit: Option<Document<'a>> = None;
    let mut others = Vec::new();

    // Erlang only allows valid codepoint integers to be used as values for utf segments
    // We want to support <<string_var:utf8>> for all string variables, but <<StringVar/utf8>> is invalid
    // To work around this we use the binary type specifier for these segments instead
    let override_type = if !value_is_a_string_literal {
        Some("binary")
    } else {
        None
    };

    for option in options {
        use BitArrayOption as Opt;
        if !others.is_empty() && !matches!(option, Opt::Size { .. } | Opt::Unit { .. }) {
            others.push("-".to_doc());
        }
        match option {
            Opt::Utf8 { .. } => others.push(override_type.unwrap_or("utf8").to_doc()),
            Opt::Utf16 { .. } => others.push(override_type.unwrap_or("utf16").to_doc()),
            Opt::Utf32 { .. } => others.push(override_type.unwrap_or("utf32").to_doc()),
            Opt::Int { .. } => others.push("integer".to_doc()),
            Opt::Float { .. } => others.push("float".to_doc()),
            Opt::Bytes { .. } => others.push("binary".to_doc()),
            Opt::Bits { .. } => others.push("bitstring".to_doc()),
            Opt::Utf8Codepoint { .. } => others.push("utf8".to_doc()),
            Opt::Utf16Codepoint { .. } => others.push("utf16".to_doc()),
            Opt::Utf32Codepoint { .. } => others.push("utf32".to_doc()),
            Opt::Signed { .. } => others.push("signed".to_doc()),
            Opt::Unsigned { .. } => others.push("unsigned".to_doc()),
            Opt::Big { .. } => others.push("big".to_doc()),
            Opt::Little { .. } => others.push("little".to_doc()),
            Opt::Native { .. } => others.push("native".to_doc()),
            Opt::Size { value, .. } => size = size_to_doc(value, env),
            Opt::Unit { value, .. } => unit = unit_to_doc(value),
        }
    }

    let others_is_empty = others.is_empty();
    let size_is_some = size.is_some();

    document = document.append(size);

    if !others_is_empty {
        if size_is_some {
            document = document.append("-").append(others);
        } else {
            document = document.append("::").append(others);
        }
    };

    if unit.is_some() {
        if !others_is_empty {
            document = document.append("-").append(unit);
        } else {
            document = document.append("::").append(unit);
        }
    };

    document
}

pub fn block<'a>(statements: &'a Vec1<TypedStatement>, env: &mut Env<'a>) -> Document<'a> {
    if statements.len() == 1 && statements.first().is_non_pipe_expression() {
        return docvec!['(', statement(statements.first(), env), ')'];
    }

    let vars = env.current_scope_vars.clone();
    let document = statement_sequence(statements, env);
    env.current_scope_vars = vars;

    begin_end(document)
}

pub fn statement_sequence<'a>(statements: &'a [TypedStatement], env: &mut Env<'a>) -> Document<'a> {
    let count = statements.len();
    let mut documents = Vec::with_capacity(count * 3);
    for (_i, expression) in statements.iter().enumerate() {
        documents.push(statement(expression, env).group().append(line()));
    }
    if count == 1 {
        documents.to_doc()
    } else {
        documents.to_doc().force_break()
    }
}

fn float_div<'a>(left: &'a TypedExpr, right: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    if right.non_zero_compile_time_number() {
        return binop_exprs(left, "/", right, env);
    }

    let left = expr(left, env);
    let right = expr(right, env);
    let denominator = env.next_local_var_name("gleam_denominator");
    let clauses = docvec![
        line(),
        "+0.0 -> +0.0;",
        line(),
        "-0.0 -> -0.0;",
        line(),
        denominator.clone(),
        " -> ",
        binop_documents(left, "/", denominator)
    ];
    docvec!["case ", right, " do", clauses.nest(INDENT), line(), "end"]
}

fn int_div<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    op: &'static str,
    env: &mut Env<'a>,
) -> Document<'a> {
    if right.non_zero_compile_time_number() {
        return binop_exprs(left, op, right, env);
    }

    let left = expr(left, env);
    let right = expr(right, env);
    let denominator = env.next_local_var_name("gleam_denominator");
    let clauses = docvec![
        line(),
        "0 -> 0;",
        line(),
        denominator.clone(),
        " -> ",
        binop_documents(left, op, denominator)
    ];
    docvec!["case ", right, " do", clauses.nest(INDENT), line(), "end"]
}

fn bin_op<'a>(
    name: &'a BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let op = match name {
        BinOp::And => "and",
        BinOp::Or => "or",
        BinOp::LtInt | BinOp::LtFloat => "<",
        BinOp::LtEqInt | BinOp::LtEqFloat => "<=",
        BinOp::Eq => "==",
        BinOp::NotEq => "!=",
        BinOp::GtInt | BinOp::GtFloat => ">",
        BinOp::GtEqInt | BinOp::GtEqFloat => ">=",
        BinOp::AddInt => "+",
        BinOp::AddFloat => "+",
        BinOp::SubInt => "-",
        BinOp::SubFloat => "-",
        BinOp::MultInt => "*",
        BinOp::MultFloat => "*",
        BinOp::DivFloat => return float_div(left, right, env),
        BinOp::DivInt => return int_div(left, right, "div", env),
        BinOp::RemainderInt => return int_div(left, right, "rem", env),
        BinOp::Concatenate => return string_concatenate(left, right, env),
    };

    binop_exprs(left, op, right, env)
}

fn binop_exprs<'a>(
    left: &'a TypedExpr,
    op: &'static str,
    right: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let left = match left {
        TypedExpr::BinOp { .. } => expr(left, env).surround("(", ")"),
        _ => maybe_block_expr(left, env),
    };
    let right = match right {
        TypedExpr::BinOp { .. } => expr(right, env).surround("(", ")"),
        _ => maybe_block_expr(right, env),
    };
    binop_documents(left, op, right)
}

fn binop_documents<'a>(left: Document<'a>, op: &'static str, right: Document<'a>) -> Document<'a> {
    // this most likely breaks bif optimization
    if op == "div" || op == "rem" || op == "and" || op == "or" {
        ":erlang."
            .to_doc()
            .append(op)
            .append("(")
            .append(left)
            .append(", ")
            .append(right)
            .append(")")
    } else {
        "(".to_doc()
            .append(left)
            .append(break_("", " "))
            .append(op)
            .group()
            .append(" ")
            .append(right)
            .append(")")
    }
}

fn let_assert<'a>(value: &'a TypedExpr, pat: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let mut vars: Vec<&str> = vec![];
    let body = maybe_block_expr(value, env);
    let (subject_var, subject_definition) = if value.is_var() {
        (body, docvec![])
    } else {
        let var = env.next_local_var_name(ASSERT_SUBJECT_VARIABLE);
        let definition = docvec![var.clone(), " = ", body, line()];
        (var, definition)
    };
    let check_pattern = pattern::to_doc_discarding_all(pat, &mut vars, env);
    let assign_pattern = pattern::to_doc(pat, &mut vars, env);
    let clauses = docvec![
        check_pattern.clone(),
        " -> ",
        subject_var.clone(),
        line(),
        env.next_local_var_name(ASSERT_FAIL_VARIABLE),
        " ->",
        docvec![
            line(),
            erlang_error(
                ":let_assert",
                &string("Assertion pattern match failed"),
                pat.location(),
                vec![("value", env.local_var_name(ASSERT_FAIL_VARIABLE))],
                env,
            )
            .nest(INDENT)
        ]
        .nest(INDENT)
    ];
    docvec![
        subject_definition,
        assign_pattern,
        " = case ",
        subject_var,
        " do",
        docvec![line(), clauses].nest(INDENT),
        line(),
        "end",
    ]
}

fn let_<'a>(value: &'a TypedExpr, pat: &'a TypedPattern, env: &mut Env<'a>) -> Document<'a> {
    let body = maybe_block_expr(value, env).group();
    pattern(pat, env).append(" = ").append(body)
}

fn float<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.ends_with('.') {
        value.push('0')
    }
    if value == "0.0" {
        return "+0.0".to_doc();
    }
    Document::String(value)
}

fn expr_list<'a>(
    elements: &'a [TypedExpr],
    tail: &'a Option<Box<TypedExpr>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    let elements = concat(Itertools::intersperse(
        elements.iter().map(|e| maybe_block_expr(e, env)),
        break_(",", ", "),
    ));
    list(elements, tail.as_ref().map(|e| maybe_block_expr(e, env)))
}

fn list<'a>(elems: Document<'a>, tail: Option<Document<'a>>) -> Document<'a> {
    let elems = if let Some(final_tail) = tail {
        elems.append(break_(" |", " | ")).append(final_tail)
    } else {
        elems
    };

    elems.to_doc().nest(INDENT).surround("[", "]").group()
}

fn var<'a>(name: &'a str, constructor: &'a ValueConstructor, env: &mut Env<'a>) -> Document<'a> {
    match &constructor.variant {
        ValueConstructorVariant::Record {
            name: record_name, ..
        } => match constructor.type_.deref() {
            Type::Fn { args, .. } => {
                let chars = incrementing_args_list(args.len());
                "fn("
                    .to_doc()
                    .append(Document::String(chars.clone()))
                    .append(") -> {")
                    .append(atom_string(record_name.to_snake_case()))
                    .append(", ")
                    .append(Document::String(chars))
                    .append("} end")
            }
            _ => {
                if constructor.type_.is_bool() {
                    record_name.to_lowercase().to_doc()
                } else {
                    record_name.to_doc()
                }
            }
        },

        ValueConstructorVariant::LocalVariable { .. } => env.local_var_name(name),

        ValueConstructorVariant::ModuleConstant { literal, .. }
        | ValueConstructorVariant::LocalConstant { literal } => const_inline(literal, env),

        ValueConstructorVariant::ModuleFn {
            arity, ref module, ..
        } if module == env.module => "&".to_doc().append(name).append("/").append(*arity),

        ValueConstructorVariant::ModuleFn {
            arity,
            module,
            name,
            ..
        } => "&"
            .to_doc()
            .append(gleam_module_name_to_elixir(module))
            .append(".")
            .append(name)
            .append("/")
            .append(*arity),
    }
}

fn int<'a>(value: &str) -> Document<'a> {
    let mut value = value.replace('_', "");
    if value.starts_with("0x") {
        value.replace_range(..2, "16#");
    } else if value.starts_with("0o") {
        value.replace_range(..2, "8#");
    } else if value.starts_with("0b") {
        value.replace_range(..2, "2#");
    }

    Document::String(value)
}

fn const_inline<'a>(literal: &'a TypedConstant, env: &mut Env<'a>) -> Document<'a> {
    match literal {
        Constant::Int { value, .. } => int(value),
        Constant::Float { value, .. } => float(value),
        Constant::String { value, .. } => string(value),
        Constant::Tuple { elements, .. } => tuple(elements.iter().map(|e| const_inline(e, env))),

        Constant::List { elements, .. } => {
            let elements = Itertools::intersperse(
                elements.iter().map(|e| const_inline(e, env)),
                break_(",", ", "),
            );
            concat(elements).nest(INDENT).surround("[", "]").group()
        }

        Constant::BitArray { segments, .. } => bit_array(
            segments
                .iter()
                .map(|s| const_segment(&s.value, &s.options, env)),
        ),

        Constant::Record { tag, typ, args, .. } if args.is_empty() => match typ.deref() {
            Type::Fn { args, .. } => record_constructor_function(tag, args.len()),
            _ => custom_type(tag),
        },

        Constant::Record {
            tag,
            args,
            field_map,
            ..
        } => {
            let args = args.iter().map(|a| const_inline(&a.value, env)).collect();
            //let tag = custom_type(tag);
            //tuple(std::iter::once(tag).chain(args))
            custom_type_constructor(tag, field_map, args, env)
        }

        Constant::Var {
            name, constructor, ..
        } => var(
            name,
            constructor
                .as_ref()
                .expect("This is guaranteed to hold a value."),
            env,
        ),
    }
}

fn custom_type(tag: &EcoString) -> Document<'_> {
    Document::Str(tag)
}

fn record_constructor_function(tag: &EcoString, arity: usize) -> Document<'_> {
    let chars = incrementing_args_list(arity);
    "fn("
        .to_doc()
        .append(Document::String(chars.clone()))
        .append(") -> {")
        .append(atom_string(tag.to_snake_case()))
        .append(", ")
        .append(Document::String(chars))
        .append("} end")
}

fn clause<'a>(clause: &'a TypedClause, env: &mut Env<'a>) -> Document<'a> {
    let Clause {
        guard,
        pattern: pat,
        alternative_patterns,
        then,
        ..
    } = clause;

    // These are required to get the alternative patterns working properly.
    // Simply rendering the duplicate erlang clauses breaks the variable
    // rewriting because each pattern would define different (rewritten)
    // variables names.
    let mut then_doc = None;
    let initial_erlang_vars = env.erl_function_scope_vars.clone();
    let mut end_erlang_vars = im::HashMap::new();

    let docs = Itertools::intersperse(
        std::iter::once(pat)
            .chain(alternative_patterns)
            .map(|patterns| {
                env.erl_function_scope_vars = initial_erlang_vars.clone();

                let patterns_doc = if patterns.len() == 1 {
                    let p = patterns.get(0).expect("Single pattern clause printing");
                    pattern(p, env)
                } else {
                    tuple(patterns.iter().map(|p| pattern(p, env)))
                };

                let guard = optional_clause_guard(guard.as_ref(), env);
                if then_doc.is_none() {
                    then_doc = Some(clause_consequence(then, env));
                    end_erlang_vars = env.erl_function_scope_vars.clone();
                }

                patterns_doc.append(
                    guard
                        .append(" ->")
                        .append(line().append(then_doc.clone()).nest(INDENT).group()),
                )
            }),
        lines(2),
    );

    let doc = concat(docs);
    env.erl_function_scope_vars = end_erlang_vars;
    doc
}

fn clause_consequence<'a>(consequence: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match consequence {
        TypedExpr::Block { statements, .. } => statement_sequence(statements, env),
        _ => expr(consequence, env),
    }
}

fn optional_clause_guard<'a>(
    guard: Option<&'a TypedClauseGuard>,
    env: &mut Env<'a>,
) -> Document<'a> {
    guard
        .map(|guard| " when ".to_doc().append(bare_clause_guard(guard, env)))
        .unwrap_or_else(nil)
}

fn bare_clause_guard<'a>(guard: &'a TypedClauseGuard, env: &mut Env<'a>) -> Document<'a> {
    match guard {
        ClauseGuard::Not { expression, .. } => docvec!["not ", bare_clause_guard(expression, env)],

        ClauseGuard::Or { left, right, .. } => clause_guard(left, env)
            .append(" or ")
            .append(clause_guard(right, env)),

        ClauseGuard::And { left, right, .. } => clause_guard(left, env)
            .append(" and ")
            .append(clause_guard(right, env)),

        ClauseGuard::Equals { left, right, .. } => clause_guard(left, env)
            .append(" == ")
            .append(clause_guard(right, env)),

        ClauseGuard::NotEquals { left, right, .. } => clause_guard(left, env)
            .append(" != ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtInt { left, right, .. } => clause_guard(left, env)
            .append(" > ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtEqInt { left, right, .. } => clause_guard(left, env)
            .append(" >= ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtInt { left, right, .. } => clause_guard(left, env)
            .append(" < ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtEqInt { left, right, .. } => clause_guard(left, env)
            .append(" <= ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtFloat { left, right, .. } => clause_guard(left, env)
            .append(" > ")
            .append(clause_guard(right, env)),

        ClauseGuard::GtEqFloat { left, right, .. } => clause_guard(left, env)
            .append(" >= ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtFloat { left, right, .. } => clause_guard(left, env)
            .append(" < ")
            .append(clause_guard(right, env)),

        ClauseGuard::LtEqFloat { left, right, .. } => clause_guard(left, env)
            .append(" <= ")
            .append(clause_guard(right, env)),

        // Only local variables are supported and the typer ensures that all
        // ClauseGuard::Vars are local variables
        ClauseGuard::Var { name, .. } => env.local_var_name(name),

        ClauseGuard::TupleIndex { tuple, index, .. } => tuple_index_inline(tuple, *index, env),

        ClauseGuard::FieldAccess {
            container, label, ..
        } => {
            //FieldAccess { location: SrcSpan { start: 330, end: 336 }, index: Some(2), label: "name2", type_: Named { public: true, module: "gleam", name: "String", args: [] }, container: Var { location: SrcSpan { start: 329, end: 330 }, type_: Named { public: true, module: "n", name: "Rolf", args: [] }, name: "n" } }
            field_access_inline(container, label, env)
        }

        ClauseGuard::ModuleSelect { literal, .. } => const_inline(literal, env),

        ClauseGuard::Constant(constant) => const_inline(constant, env),
    }
}

fn field_access_inline<'a>(
    tuple: &'a TypedClauseGuard,
    label: &'a str,
    env: &mut Env<'a>,
) -> Document<'a> {
    let tuple_doc = bare_clause_guard(tuple, env);
    tuple_doc.append(".").append(label)
}

fn tuple_index_inline<'a>(
    tuple: &'a TypedClauseGuard,
    index: u64,
    env: &mut Env<'a>,
) -> Document<'a> {
    let index_doc = Document::String(format!("{}", (index + 1)));
    let tuple_doc = bare_clause_guard(tuple, env);
    ":erlang.element"
        .to_doc()
        .append(wrap_args([index_doc, tuple_doc]))
}

fn clause_guard<'a>(guard: &'a TypedClauseGuard, env: &mut Env<'a>) -> Document<'a> {
    match guard {
        // Binary operators are wrapped in parens
        ClauseGuard::Or { .. }
        | ClauseGuard::And { .. }
        | ClauseGuard::Equals { .. }
        | ClauseGuard::NotEquals { .. }
        | ClauseGuard::GtInt { .. }
        | ClauseGuard::GtEqInt { .. }
        | ClauseGuard::LtInt { .. }
        | ClauseGuard::LtEqInt { .. }
        | ClauseGuard::GtFloat { .. }
        | ClauseGuard::GtEqFloat { .. }
        | ClauseGuard::LtFloat { .. }
        | ClauseGuard::LtEqFloat { .. } => "("
            .to_doc()
            .append(bare_clause_guard(guard, env))
            .append(")"),

        // Other expressions are not
        ClauseGuard::Constant(_)
        | ClauseGuard::Not { .. }
        | ClauseGuard::Var { .. }
        | ClauseGuard::TupleIndex { .. }
        | ClauseGuard::FieldAccess { .. }
        | ClauseGuard::ModuleSelect { .. } => bare_clause_guard(guard, env),
    }
}

fn clauses<'a>(cs: &'a [TypedClause], env: &mut Env<'a>) -> Document<'a> {
    concat(Itertools::intersperse(
        cs.iter().map(|c| {
            let vars = env.current_scope_vars.clone();
            let erl = clause(c, env);
            env.current_scope_vars = vars; // Reset the known variables now the clauses' scope has ended
            erl
        }),
        lines(2),
    ))
}

fn case<'a>(subjects: &'a [TypedExpr], cs: &'a [TypedClause], env: &mut Env<'a>) -> Document<'a> {
    let subjects_doc = if subjects.len() == 1 {
        let subject = subjects
            .get(0)
            .expect("elixir case printing of single subject");
        maybe_block_expr(subject, env).group()
    } else {
        tuple(subjects.iter().map(|e| maybe_block_expr(e, env)))
    };
    "case "
        .to_doc()
        .append(subjects_doc)
        .append(" do")
        .append(line().append(clauses(cs, env)).nest(INDENT))
        .append(line())
        .append("end")
        .group()
}

fn call<'a>(fun: &'a TypedExpr, args: &'a [CallArg<TypedExpr>], env: &mut Env<'a>) -> Document<'a> {
    docs_args_call(
        fun,
        args.iter()
            .map(|arg| maybe_block_expr(&arg.value, env))
            .collect(),
        env,
    )
}

fn module_fn_with_args<'a>(
    module: &'a str,
    name: &'a str,
    args: Vec<Document<'a>>,
    env: &Env<'a>,
) -> Document<'a> {
    let args = wrap_args(args);
    if module == env.module {
        Document::Str(name).append(args)
    } else {
        gleam_module_name_to_elixir(module)
            .append(".")
            .append(name)
            .append(args)
    }
}

fn docs_args_call<'a>(
    fun: &'a TypedExpr,
    args: Vec<Document<'a>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    docs_args_call_1(fun, args, env)
}

fn custom_type_constructor<'a>(
    name: &'a EcoString,
    field_map: &'a Option<fields::FieldMap>,
    args: Vec<Document<'a>>,
    _env: &mut Env<'a>,
) -> Document<'a> {
    let mut rev_fields: HashMap<usize, &str> = HashMap::new();
    if let Some(fields::FieldMap { fields, .. }) = field_map {
        for f in fields {
            let _ = rev_fields.insert(*f.1 as usize, f.0);
        }
    };

    let doc_args = args.iter().enumerate().map(|(index, arg)| {
        let name = match rev_fields.get(&index) {
            Some(name) => (*name).to_string(),
            None => format!("field_{}", index),
        };
        Document::String(name).append(": ").append(arg.clone())
    });

    let doc_args: Vec<Document<'a>> = Itertools::intersperse(doc_args, ", ".to_doc()).collect();

    "%{ __module__: :n, __type__: "
        .to_doc()
        .append(name)
        .append(", ".to_doc())
        .append(doc_args)
        .append(" }".to_doc())
}

fn docs_args_call_1<'a>(
    fun: &'a TypedExpr,
    mut args: Vec<Document<'a>>,
    env: &mut Env<'a>,
) -> Document<'a> {
    match fun {
        TypedExpr::ModuleSelect {
            constructor:
                ModuleValueConstructor::Record {
                    name, field_map, ..
                },
            ..
        }
        | TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant:
                        ValueConstructorVariant::Record {
                            name, field_map, ..
                        },
                    ..
                },
            ..
        } => custom_type_constructor(name, field_map, args, env),

        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => module_fn_with_args(module, name, args, env),

        // Match against a Constant::Var that contains a function.
        // We want this to be emitted like a normal function call, not a function variable
        // substitution.
        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant:
                        ValueConstructorVariant::ModuleConstant {
                            literal:
                                Constant::Var {
                                    constructor: Some(ref constructor),
                                    ..
                                },
                            ..
                        },
                    ..
                },
            ..
        } if constructor.variant.is_module_fn() => {
            if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant {
                module_fn_with_args(module, name, args, env)
            } else {
                unreachable!("The above clause guard ensures that this is a module fn")
            }
        }

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => {
            let args = wrap_args(args);
            // We use the constructor Fn variant's `module` and function `name`.
            // It would also be valid to use the module and label as in the
            // Gleam code, but using the variant can result in an optimisation
            // in which the target function is used for `external fn`s, removing
            // one layer of wrapping.
            // This also enables an optimisation in the Erlang compiler in which
            // some Erlang BIFs can be replaced with literals if their arguments
            // are literals, such as `binary_to_atom`.
            atom_string(module.replace("/", ".").to_string())
                .append(".")
                .append(name)
                .append(args)
        }

        TypedExpr::Fn {
            is_capture: true,
            body,
            ..
        } => {
            if let Statement::Expression(TypedExpr::Call {
                fun,
                args: inner_args,
                ..
            }) = body.first()
            {
                let mut merged_args = Vec::with_capacity(inner_args.len());
                for arg in inner_args {
                    match &arg.value {
                        TypedExpr::Var { name, .. } if name == CAPTURE_VARIABLE => {
                            merged_args.push(args.swap_remove(0))
                        }
                        e => merged_args.push(maybe_block_expr(e, env)),
                    }
                }
                "".to_doc().append(docs_args_call_1(fun, merged_args, env))
            } else {
                panic!("Erl printing: Capture was not a call")
            }
        }

        TypedExpr::Fn { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::TupleIndex { .. } => {
            let args = wrap_args(args);
            expr(fun, env).surround("(", ")").append(".").append(args)
        }

        other => {
            let args = wrap_args(args);
            // calling variables as functions here
            maybe_block_expr(other, env).append(".").append(args)
        }
    }
}

fn record_update<'a>(
    spread: &'a TypedExpr,
    args: &'a [TypedRecordUpdateArg],
    env: &mut Env<'a>,
) -> Document<'a> {
    let expr_doc = maybe_block_expr(spread, env);

    let args_doc = args.iter().map(|arg| {
        let index_doc = Document::String(arg.label.to_string());
        let value_doc = maybe_block_expr(&arg.value, env);

        index_doc.append(": ").append(value_doc)
    });

    let args_doc: Vec<Document<'a>> = Itertools::intersperse(args_doc, ", ".to_doc()).collect();

    "%{ "
        .to_doc()
        .append(expr_doc)
        .append(" | ")
        .append(args_doc)
        .append(" }")
}

/// Wrap a document in begin end
///
fn begin_end(document: Document<'_>) -> Document<'_> {
    docvec!["(", line().append(document).nest(INDENT), line(), ")"].force_break()
}

fn maybe_block_expr<'a>(expression: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    if needs_begin_end_wrapping(expression) {
        begin_end(expr(expression, env))
    } else {
        expr(expression, env)
    }
}

fn needs_begin_end_wrapping(expression: &TypedExpr) -> bool {
    match expression {
        TypedExpr::Pipeline { .. } => true,

        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::Block { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. } => false,
    }
}

fn todo<'a>(message: Option<&'a TypedExpr>, location: SrcSpan, env: &mut Env<'a>) -> Document<'a> {
    let message = match message {
        Some(m) => expr(m, env),
        None => string("This has not yet been implemented"),
    };
    erlang_error(":todo", &message, location, vec![], env)
}

fn panic<'a>(location: SrcSpan, message: Option<&'a TypedExpr>, env: &mut Env<'a>) -> Document<'a> {
    let message = match message {
        Some(m) => expr(m, env),
        None => string("panic expression evaluated"),
    };
    erlang_error(":panic", &message, location, vec![], env)
}

fn erlang_error<'a>(
    name: &'a str,
    message: &Document<'a>,
    location: SrcSpan,
    fields: Vec<(&'a str, Document<'a>)>,
    env: &Env<'a>,
) -> Document<'a> {
    let mut fields_doc = docvec![
        ":gleam_error => ",
        name,
        ",",
        line(),
        ":message => ",
        message.clone()
    ];

    for (key, value) in fields {
        fields_doc = fields_doc
            .append(",")
            .append(line())
            .append(":")
            .append(key)
            .append(" => ")
            .append(value);
    }
    let fields_doc = fields_doc
        .append(",")
        .append(line())
        .append(":module => ")
        .append(env.module.to_doc().surround("<<\"", "\"::utf8>>"))
        .append(",")
        .append(line())
        .append(":function => ")
        .append(string(env.function))
        .append(",")
        .append(line())
        .append(":line => ")
        .append(env.line_numbers.line_number(location.start));
    let error = "%{"
        .to_doc()
        .append(fields_doc.group().nest(INDENT))
        .append("}");
    docvec![":erlang.error", wrap_args([error.group()])]
}

fn expr<'a>(expression: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    match expression {
        TypedExpr::Todo {
            message: label,
            location,
            ..
        } => todo(label.as_deref(), *location, env),

        TypedExpr::Panic {
            location, message, ..
        } => panic(*location, message.as_deref(), env),

        TypedExpr::Int { value, .. } => int(value),
        TypedExpr::Float { value, .. } => float(value),
        TypedExpr::String { value, .. } => string(value),

        TypedExpr::Pipeline {
            assignments,
            finally,
            ..
        } => pipeline(assignments, finally, env),

        TypedExpr::Block { statements, .. } => block(statements, env),

        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index, env),

        TypedExpr::Var {
            name, constructor, ..
        } => var(name, constructor, env),

        TypedExpr::Fn { args, body, .. } => fun(args, body, env),

        TypedExpr::NegateBool { value, .. } => negate_with("not ", value, env),

        TypedExpr::NegateInt { value, .. } => negate_with("- ", value, env),

        TypedExpr::List { elements, tail, .. } => expr_list(elements, tail, env),

        TypedExpr::Call { fun, args, .. } => {
            //println!("{:#?}", expression);
            call(fun, args, env)
        }

        TypedExpr::ModuleSelect {
            constructor:
                ModuleValueConstructor::Record {
                    name,
                    type_,
                    arity: 0,
                    ..
                },
            ..
        } => {
            if type_.is_bool() {
                Document::String(name.to_string().to_lowercase())
            } else {
                atom_string(name.to_string())
            }
        }

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Constant { literal, .. },
            ..
        } => const_inline(literal, env),

        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { name, arity, .. },
            ..
        } => record_constructor_function(name, *arity as usize),

        TypedExpr::ModuleSelect {
            typ,
            constructor: ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => module_select_fn(typ.clone(), module, name),

        TypedExpr::RecordAccess { record, label, .. } => record_access(record, label, env),

        TypedExpr::RecordUpdate { spread, args, .. } => record_update(spread, args, env),

        TypedExpr::Case {
            subjects, clauses, ..
        } => case(subjects, clauses, env),

        TypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(name, left, right, env),

        TypedExpr::Tuple { elems, .. } => tuple(elems.iter().map(|e| maybe_block_expr(e, env))),

        TypedExpr::BitArray { segments, .. } => bit_array(
            segments
                .iter()
                .map(|s| expr_segment(&s.value, &s.options, env)),
        ),
    }
}

fn pipeline<'a>(
    assignments: &'a [Assignment<Arc<Type>, TypedExpr>],
    finally: &'a TypedExpr,
    env: &mut Env<'a>,
) -> Document<'a> {
    let mut documents = Vec::with_capacity((assignments.len() + 1) * 3);

    for a in assignments {
        documents.push(assignment(a, env));
        documents.push(line());
    }

    documents.push(expr(finally, env));
    documents.to_doc()
}

fn assignment<'a>(assignment: &'a TypedAssignment, env: &mut Env<'a>) -> Document<'a> {
    match assignment.kind {
        AssignmentKind::Let => let_(&assignment.value, &assignment.pattern, env),
        AssignmentKind::Assert => let_assert(&assignment.value, &assignment.pattern, env),
    }
}

fn negate_with<'a>(op: &'static str, value: &'a TypedExpr, env: &mut Env<'a>) -> Document<'a> {
    docvec![op, maybe_block_expr(value, env)]
}

fn record_access<'a>(
    tuple: &'a TypedExpr,
    label: &'a EcoString,
    env: &mut Env<'a>,
) -> Document<'a> {
    let edoc = maybe_block_expr(tuple, env);
    edoc.append(".").append(label.to_doc())
}

fn tuple_index<'a>(tuple: &'a TypedExpr, index: u64, env: &mut Env<'a>) -> Document<'a> {
    let index_doc = Document::String(format!("{}", (index + 1)));
    let tuple_doc = maybe_block_expr(tuple, env);
    ":erlang.element"
        .to_doc()
        .append(wrap_args([index_doc, tuple_doc]))
}

fn module_select_fn<'a>(typ: Arc<Type>, module_name: &'a str, label: &'a str) -> Document<'a> {
    match crate::type_::collapse_links(typ).as_ref() {
        crate::type_::Type::Fn { args, .. } => "&"
            .to_doc()
            .append(gleam_module_name_to_elixir(module_name))
            .append(".")
            .append(label)
            .append("/")
            .append(args.len()),

        _ => gleam_module_name_to_elixir(module_name)
            .append(".")
            .append(label)
            .append("()"),
    }
}

fn fun<'a>(args: &'a [TypedArg], body: &'a [TypedStatement], env: &mut Env<'a>) -> Document<'a> {
    let current_scope_vars = env.current_scope_vars.clone();
    let doc = "(fn"
        .to_doc()
        .append(fun_args(args, env).append(" ->"))
        .append(
            break_("", " ")
                .append(statement_sequence(body, env))
                .nest(INDENT),
        )
        .append(break_("", " "))
        .append("end)")
        .group();
    env.current_scope_vars = current_scope_vars;
    doc
}

fn incrementing_args_list(arity: usize) -> String {
    let arguments = (0..arity).map(|c| format!("field_{c}"));
    Itertools::intersperse(arguments, ", ".into()).collect()
}
