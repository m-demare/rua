#![feature(box_patterns)]

use darling::ast::NestedMeta;
use darling::{Error, FromMeta};
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, FnArg, ItemFn, PatType, Type, TypePath, TypeReference};

enum OutputWrappers {
    Resultify,
    IntoResult,
}

#[derive(Debug, FromMeta)]
struct MacroArgs {
    #[darling(default)]
    exact_args: bool,
}

#[proc_macro_attribute]
pub fn rua_func(args: TokenStream, input: TokenStream) -> TokenStream {
    let func = parse_macro_input!(input as ItemFn);

    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(Error::from(e).write_errors());
        }
    };
    let macro_args = match MacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return TokenStream::from(e.write_errors());
        }
    };

    let name = func.sig.ident.clone();
    let name_str = func.sig.ident.to_string();
    let vis = func.vis.clone();
    let constness = func.sig.constness;

    let output_wrap = wrap_output(&func);
    let (parameters, validate_cant_args) = get_parameters(&func, &macro_args);

    let output = quote!(
        #vis #constness fn #name(ctxt: &FunctionContext) -> RuaResult {
            let name_str = #name_str;
            #validate_cant_args
            #func
            let res = #name(#(#parameters),*);
            #output_wrap
        }
    );

    TokenStream::from(quote!(#output))
}

fn wrap_output(input_fn: &ItemFn) -> proc_macro2::TokenStream {
    let wrap_output_with = match input_fn.sig.output {
        syn::ReturnType::Default => OutputWrappers::Resultify,
        syn::ReturnType::Type(_, ref t) => match t {
            box Type::Path(p) => {
                match p.path.segments.first().unwrap().ident.to_string().as_str() {
                    "Result" | "RuaResult" => OutputWrappers::IntoResult,
                    _ => OutputWrappers::Resultify,
                }
            }
            t => panic!("Invalid rua return type: {t:?}"),
        },
    };
    match wrap_output_with {
        OutputWrappers::Resultify => quote!(Ok(res.into())),
        OutputWrappers::IntoResult => quote!(match res {
            Ok(r) => Ok(r.into()),
            Err(e) => Err(e),
        }),
    }
}

fn get_parameters(
    input_fn: &ItemFn,
    args: &MacroArgs,
) -> (Vec<proc_macro2::TokenStream>, proc_macro2::TokenStream) {
    let mut inputs = input_fn.sig.inputs.iter().peekable();
    let mut cant_params = inputs.len();
    let mut params = match inputs.peek() {
        Some(FnArg::Typed(PatType {
            ty:
                box Type::Reference(TypeReference {
                    elem: box Type::Path(TypePath { path, .. }), ..
                }),
            ..
        })) if path.segments.last().unwrap().ident == "FunctionContext" => {
            inputs.next();
            cant_params -= 1;
            vec![quote!(ctxt)]
        }
        Some(_) => Vec::new(),
        None => {
            return (
                Vec::new(),
                quote!(if ctxt.args.len() > 0 {
                    return Err(EvalError::TooManyArguments(
                        ctxt.args.len() as u8,
                        name_str.into(),
                    ));
                }),
            )
        }
    };
    for (i, param) in inputs.enumerate() {
        match param {
            FnArg::Typed(PatType { ty: box Type::Path(TypePath { path, .. }), .. })
                if path.segments.first().unwrap().ident == "Option" =>
            {
                params.push(quote!(
                    match ctxt.args.get(#i).cloned() {
                        None => None,
                        Some(a) => a.try_into_opt()?,
                    }
                ))
            }
            FnArg::Typed(PatType { ty: box Type::Path(TypePath { .. }), .. }) => {
                params.push(quote!(
                    match ctxt.args.get(#i).cloned() {
                        None => return Err(EvalError::ExpectedArgument(#i as u8, name_str.into())),
                        Some(a) => a.try_into()?,
                    }
                ))
            }
            _ => panic!("Unsuported rua parameter: {param:?}"),
        }
    }

    let validate_cant_args = if args.exact_args {
        quote!(
        if ctxt.args.len() > #cant_params{
            return Err(EvalError::TooManyArguments(ctxt.args.len() as u8, name_str.into()))
        })
    } else {
        quote!()
    };

    (params, validate_cant_args)
}
