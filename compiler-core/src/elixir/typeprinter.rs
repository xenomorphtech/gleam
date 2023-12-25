use super::*;
use crate::type_::is_prelude_module;
use crate::type_::TypeVar;

#[derive(Debug)]
pub struct TypePrinter<'a> {
    var_as_any: bool,
    current_module: &'a str,
    var_usages: Option<&'a HashMap<u64, u64>>,
}

impl<'a> TypePrinter<'a> {
    pub fn new(current_module: &'a str) -> Self {
        Self {
            current_module,
            var_usages: None,
            var_as_any: false,
        }
    }

    pub fn print(&self, type_: &'a Type) -> Document<'a> {
        match type_ {
            Type::Var { type_: _typ } => "fixme".to_doc(),

            Type::Named {
                name, module, args, ..
            } if is_prelude_module(module) => self.print_prelude_type(name, args),

            Type::Named {
                name, module, args, ..
            } => self.print_type_app(module, name, args),

            Type::Fn { args, retrn } => self.print_fn(args, retrn),

            Type::Tuple { elems } => tuple(elems.iter().map(|e| self.print(e))),
        }
    }

    fn print_var(&self, type_: &'a TypeVar) -> Document<'a> {
        match type_ {
            TypeVar::Generic { .. } | TypeVar::Unbound { .. } if self.var_as_any => {
                "any()".to_doc()
            }
            TypeVar::Generic { id, .. } | TypeVar::Unbound { id, .. } => match &self.var_usages {
                Some(usages) => match usages.get(id) {
                    Some(&0) => nil(),
                    Some(&1) => "any()".to_doc(),
                    _ => id_to_type_var(*id),
                },
                None => id_to_type_var(*id),
            },
            TypeVar::Link { type_: typ } => self.print(typ),
        }
    }

    fn print_prelude_type(&self, name: &'a str, args: &'a [Arc<Type>]) -> Document<'a> {
        match name {
            "Nil" => "nil".to_doc(),
            "Int" | "UtfCodepoint" => "integer()".to_doc(),
            "String" => "binary()".to_doc(),
            "Bool" => "boolean()".to_doc(),
            "Float" => "float()".to_doc(),
            "BitArray" => "bitstring()".to_doc(),
            "List" => {
                let arg0 = self.print(args.get(0).expect("print_prelude_type list"));
                "list(".to_doc().append(arg0).append(")")
            }
            "Result" => {
                let arg_ok = self.print(args.get(0).expect("print_prelude_type result ok"));
                let arg_err = self.print(args.get(1).expect("print_prelude_type result err"));
                let ok = tuple([":ok".to_doc(), arg_ok]);
                let error = tuple([":error".to_doc(), arg_err]);
                docvec![ok, break_(" |", " | "), error].nest(INDENT).group()
            }
            // Getting here should mean we either forgot a built-in type or there is a
            // compiler error
            name => panic!("{name} is not a built-in type."),
        }
    }

    fn print_type_app(&self, module: &'a str, name: &str, args: &'a [Arc<Type>]) -> Document<'a> {
        let args = concat(Itertools::intersperse(
            args.iter().map(|a| self.print(a)),
            ", ".to_doc(),
        ));
        let name = Document::String(name.into());
        if self.current_module == module {
            docvec![name, "(", args, ")"]
        } else {
            docvec![
                gleam_module_name_to_elixir(module),
                ".",
                name,
                "(",
                args,
                ")"
            ]
        }
    }

    fn print_fn(&self, args: &'a [Arc<Type>], retrn: &'a Type) -> Document<'a> {
        let args = concat(Itertools::intersperse(
            args.iter().map(|a| self.print(a)),
            ", ".to_doc(),
        ));
        let retrn = self.print(retrn);
        "fn(("
            .to_doc()
            .append(args)
            .append(") -> ")
            .append(retrn)
            .append(")")
    }

    /// Print type vars as `any()`.
    fn var_as_any(mut self) -> Self {
        self.var_as_any = true;
        self
    }
}

/// When rendering a type variable to an erlang type spec we need all type variables with the
/// same id to end up with the same name in the generated erlang.
/// This function converts a usize into base 26 A-Z for this purpose.
fn id_to_type_var(id: u64) -> Document<'static> {
    if id < 26 {
        let mut name = "".to_string();
        name.push(std::char::from_u32((id % 26 + 97) as u32).expect("id_to_type_var 0"));
        return Document::String(name);
    }
    let mut name = vec![];
    let mut last_char = id;
    while last_char >= 26 {
        name.push(std::char::from_u32((last_char % 26 + 97) as u32).expect("id_to_type_var 1"));
        last_char /= 26;
    }
    name.push(std::char::from_u32((last_char % 26 + 97) as u32).expect("id_to_type_var 2"));
    name.reverse();
    name.into_iter().collect::<EcoString>().to_doc()
}
