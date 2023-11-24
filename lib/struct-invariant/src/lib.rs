use proc_macro::TokenStream;
use quote::quote;
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    parse_macro_input, parse_quote, Block, Expr, ExprLit, ImplItem, ItemImpl, LocalInit,
    PathSegment, Stmt, Token,
};

#[proc_macro_attribute]
pub fn invariant(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut impl_block = parse_macro_input!(input as ItemImpl);
    let args = parse_macro_input!(args as Args);

    let is_trait = impl_block.trait_.is_some();
    for item in &mut impl_block.items {
        if let ImplItem::Fn(ref mut f) = item {
            if f.sig.constness.is_some()
                || (!is_trait
                    && !matches!(
                        f.vis,
                        syn::Visibility::Public(_) | syn::Visibility::Restricted(_)
                    ))
            {
                continue;
            }
            let mut caught_returns = false;
            if let syn::ReturnType::Type(_, ref t) = f.sig.output {
                if let syn::Type::Path(ref p) = **t {
                    match (p.path.segments.len(), p.path.segments.first()) {
                        (1, Some(PathSegment { ident: i, .. })) if i == "Self" => {
                            validate_retval(f, &args);
                            caught_returns = true;
                        }
                        _ => {}
                    }
                }
            }

            let call_site_span = proc_macro2::Span::call_site();
            let mut to_validate_at_beggining = Vec::new();
            let mut to_validate_at_end = Vec::new();
            for input in &f.sig.inputs {
                match input {
                    syn::FnArg::Receiver(r) => {
                        to_validate_at_beggining.push(syn::Ident::new("self", call_site_span));
                        if r.mutability.is_some() {
                            to_validate_at_end.push(syn::Ident::new("self", call_site_span));
                        }
                    }
                    syn::FnArg::Typed(..) => {
                        // TODO should I validate other args of type self?
                    }
                }
            }
            if !caught_returns && !to_validate_at_end.is_empty() {
                catch_returns(f);
            }
            for id in to_validate_at_end {
                validate_at_end(f, id, &args);
            }
            for id in to_validate_at_beggining.into_iter().rev() {
                validate_at_beggining(f, id, &args);
            }
        }
    }
    let output = quote!(#impl_block);
    TokenStream::from(output)
}

struct Args {
    assertions: Vec<Expr>,
    msg: String,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let args = Punctuated::<Expr, Token![,]>::parse_terminated(input)?;
        let mut args: Vec<Expr> = args.into_iter().collect();
        let msg = match args.last() {
            Some(Expr::Lit(ExprLit { lit: syn::Lit::Str(lit), .. })) => {
                let lit = lit.clone();
                args.pop();
                lit.value()
            }
            _ => "Struct invariant broken".to_owned(),
        };
        Ok(Args { assertions: args, msg })
    }
}

fn validate_at_beggining(f: &mut syn::ImplItemFn, ident: proc_macro2::Ident, args: &Args) {
    let mut validations = compile_validations(args, ident);
    validations.append(&mut f.block.stmts);
    f.block.stmts = validations;
}

fn validate_at_end(f: &mut syn::ImplItemFn, ident: proc_macro2::Ident, args: &Args) {
    let ret_stmt = f.block.stmts.pop().unwrap();

    let mut validations = compile_validations(args, ident);

    f.block.stmts.append(&mut validations);

    f.block.stmts.push(ret_stmt);
}

fn compile_validations(args: &Args, ident: proc_macro2::Ident) -> Vec<Stmt> {
    args.assertions
        .iter()
        .map(|assertion| {
            let assertion = swap_self_for(assertion.clone(), ident.clone());
            let msg = &args.msg;
            let validation = parse_quote! {
                debug_assert!(#assertion, #msg);
            };
            validation
        })
        .collect()
}

fn validate_retval(f: &mut syn::ImplItemFn, args: &Args) {
    let retval = catch_returns(f);

    validate_at_end(f, retval, args);
}

fn catch_returns(f: &mut syn::ImplItemFn) -> proc_macro2::Ident {
    let retval = syn::Ident::new("retval", proc_macro2::Span::call_site());

    let og_block = std::mem::replace(
        &mut f.block,
        Block { brace_token: Default::default(), stmts: Vec::new() },
    );

    let label_span = proc_macro2::Span::mixed_site();
    let block_label = syn::Lifetime::new("'retval_invairant", label_span);
    let modified_block = returns_to_break(og_block, &block_label);
    let block_expr = Expr::Block(syn::ExprBlock {
        attrs: Vec::new(),
        label: Some(syn::Label { name: block_label, colon_token: Default::default() }),
        block: modified_block,
    });
    let local = syn::Stmt::Local(syn::Local {
        attrs: Vec::new(),
        let_token: Default::default(),
        pat: syn::Pat::Ident(syn::PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: retval.clone(),
            subpat: None,
        }),
        init: Some(LocalInit {
            eq_token: Default::default(),
            expr: block_expr.into(),
            diverge: None,
        }),
        semi_token: Default::default(),
    });

    let segments: Vec<PathSegment> = vec![retval.clone().into()];
    f.block.stmts.push(local);

    f.block.stmts.push(syn::Stmt::Expr(
        Expr::Path(syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: syn::Path { leading_colon: None, segments: segments.into_iter().collect() },
        }),
        None,
    ));

    retval
}

#[must_use]
fn swap_self_for(assertion: Expr, ident: proc_macro2::Ident) -> Expr {
    SwapSelfFor::new(ident).fold_expr(assertion)
}

impl syn::fold::Fold for SwapSelfFor {
    fn fold_expr(&mut self, e: Expr) -> Expr {
        match e {
            Expr::Path(syn::ExprPath { path: syn::Path { segments, .. }, attrs, qself })
                if segments.len() == 1 && segments.first().unwrap().ident == "self" =>
            {
                let segments: Vec<PathSegment> = vec![self.ident.clone().into()];
                Expr::Path(syn::ExprPath {
                    attrs,
                    qself,
                    path: syn::Path {
                        leading_colon: None,
                        segments: segments.into_iter().collect(),
                    },
                })
            }
            _ => syn::fold::fold_expr(self, e),
        }
    }
}

#[must_use]
fn returns_to_break(block: syn::Block, block_label: &syn::Lifetime) -> syn::Block {
    ReturnsToBreak::new(block_label).fold_block(block)
}

impl syn::fold::Fold for ReturnsToBreak<'_> {
    fn fold_stmt(&mut self, s: Stmt) -> Stmt {
        match s {
            Stmt::Expr(Expr::Return(syn::ExprReturn { attrs, expr, return_token: _ }), _) => {
                Stmt::Expr(
                    Expr::Break(syn::ExprBreak {
                        attrs: attrs.into_iter().map(|attr| self.fold_attribute(attr)).collect(),
                        expr: expr.map(|expr| Box::new(self.fold_expr(*expr))),
                        break_token: Default::default(),
                        label: Some(self.block_label.clone()),
                    }),
                    Some(Default::default()),
                )
            }
            s @ Stmt::Item(syn::Item::Fn(_)) => s,
            _ => syn::fold::fold_stmt(self, s),
        }
    }

    fn fold_expr(&mut self, e: Expr) -> Expr {
        match e {
            e @ Expr::Closure(_) => e,
            _ => syn::fold::fold_expr(self, e),
        }
    }
}

struct SwapSelfFor {
    ident: proc_macro2::Ident,
}

impl SwapSelfFor {
    fn new(ident: proc_macro2::Ident) -> Self {
        Self { ident }
    }
}

struct ReturnsToBreak<'a> {
    block_label: &'a syn::Lifetime,
}

impl<'a> ReturnsToBreak<'a> {
    fn new(block_label: &'a syn::Lifetime) -> Self {
        Self { block_label }
    }
}
