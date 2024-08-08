use darling::{ast, util, FromDeriveInput, FromField, FromMeta};
use ident_case::RenameRule;
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::Parse, parse_macro_input, punctuated::Punctuated, spanned::Spanned as _, DeriveInput,
    Generics, Ident, Path, Token,
};

#[proc_macro_derive(DeserializePositional, attributes(jsonrpc))]
pub fn deserialize_positional(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    expand_deserialize_positional(item)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand_deserialize_positional(item: DeriveInput) -> syn::Result<TokenStream> {
    let DeriveInput {
        attrs,
        vis: _,
        ident,
        generics:
            generics @ Generics {
                lt_token: _,
                params,
                gt_token: _,
                where_clause,
            },
        data,
    } = &item;
    if !params.is_empty()
        || where_clause
            .as_ref()
            .is_some_and(|it| !it.predicates.is_empty())
    {
        return Err(syn::Error::new(generics.span(), "generics are unsupported"));
    }

    todo!()
}

#[derive(FromDeriveInput, Debug)]
#[darling(attributes(jsonrpc))]
struct Input {
    ident: Ident,
    data: ast::Data<util::Ignored, Field>,
    rename_all: Option<RenameRule>,
    deny_unknown_fields: bool,
    #[darling(rename = "crate")]
    krate: Option<ModulePath>,
}

struct RenameAll {
    serialize: RenameRule,
    deserialize: RenameRule,
}

#[derive(Debug)]
struct ModulePath {
    leading_colon: Option<Token![::]>,
    segments: Punctuated<Ident, Token![::]>,
}

impl Parse for ModulePath {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let syn::Path {
            leading_colon,
            segments,
        } = input.call(syn::Path::parse_mod_style)?;
        Ok(Self {
            leading_colon,
            segments: segments.into_iter().map(|it| it.ident).collect(),
        })
    }
}

impl ToTokens for ModulePath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            leading_colon,
            segments,
        } = self;
        leading_colon.to_tokens(tokens);
        segments.to_tokens(tokens);
    }
}

impl FromMeta for ModulePath {
    fn from_expr(expr: &syn::Expr) -> darling::Result<Self> {
        Ok(syn::parse2(expr.to_token_stream())?)
    }
}

#[derive(FromField, Debug)]
#[darling(attributes(jsonrpc))]
struct Field {
    ident: Option<Ident>,
    rename: Option<Rename>,
    #[darling(multiple)]
    alias: Vec<String>,
    #[darling(default)]
    optional: bool,
    #[darling(default)]
    rest: bool,
}

#[derive(Debug)]
struct Rename {
    serialize: Option<String>,
    deserialize: Option<String>,
}

impl FromMeta for Rename {
    fn from_list(items: &[ast::NestedMeta]) -> darling::Result<Self> {
        let mut errors = darling::Error::accumulator();
        let mut this = Rename {
            serialize: None,
            deserialize: None,
        };
        for item in items {
            match item {
                ast::NestedMeta::Meta(it) => match it {
                    syn::Meta::Path(it) => errors.push(darling::Error::unknown_field_path(it)),
                    syn::Meta::List(it) => {
                        errors.push(darling::Error::unknown_field_path(&it.path))
                    }
                    syn::Meta::NameValue(it) => {
                        match [
                            ("serialize", &mut this.serialize),
                            ("deserialize", &mut this.deserialize),
                        ]
                        .into_iter()
                        .find(|(ident, _)| it.path.is_ident(ident))
                        {
                            Some((_, dest)) => match dest {
                                Some(_) => {
                                    errors.push(darling::Error::duplicate_field_path(&it.path))
                                }
                                None => match &it.value {
                                    syn::Expr::Lit(syn::ExprLit {
                                        attrs: _,
                                        lit: syn::Lit::Str(it),
                                    }) => *dest = Some(it.value()),
                                    other => {
                                        errors.push(darling::Error::unexpected_expr_type(other))
                                    }
                                },
                            },
                            None => errors.push(darling::Error::unknown_field_path(&it.path)),
                        };
                    }
                },
                ast::NestedMeta::Lit(it) => errors.push(darling::Error::unexpected_lit_type(it)),
            }
        }
        errors.finish_with(this)
    }

    fn from_char(value: char) -> darling::Result<Self> {
        Self::from_string(&value.to_string())
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        Ok(Self {
            serialize: Some(value.into()),
            deserialize: Some(value.into()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let it = syn::parse_quote! {
            #[jsonrpc(crate = hello)]
            struct Foo {
                #[jsonrpc(rest, alias = "foo", alias = "bar")]
                #[jsonrpc(rename(deserialize = "aa", serialize = "bb"))]
                foo: String,
            }
        };
        dbg!(Input::from_derive_input(&it).unwrap());
    }
}
