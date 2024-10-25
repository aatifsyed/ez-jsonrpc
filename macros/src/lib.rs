use std::iter;

use darling::{ast, util, FromDeriveInput, FromField, FromMeta};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    ext::IdentExt as _,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    DeriveInput, Ident, Token,
};

#[proc_macro_derive(DeserializePositional, attributes(jsonrpc))]
pub fn deserialize_positional(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    expand_deserialize_positional(item)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(SerializePositional, attributes(jsonrpc))]
pub fn serialize_positional(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    expand_serialize_positional(item)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(DeserializeNamed, attributes(jsonrpc))]
pub fn deserialize_named(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    expand_deserialize_named(item)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_derive(SerializeNamed, attributes(jsonrpc))]
pub fn serialize_named(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    expand_serialize_named(item)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand_serialize_named(item: DeriveInput) -> syn::Result<TokenStream> {
    let Strukt {
        ident,
        fields,
        krate,
        ..
    } = Strukt::new(&item)?;
    let Exports {
        SerializeNamed,
        SerializeMap,
        Result,
        ..
    } = Exports::new(krate);
    let ser_fields = fields.iter().map(|Field { ident, rename, .. }| {
        let name = rename.clone().unwrap_or(ident.unraw().to_string());
        quote! {serializer.serialize_entry(&#name, &self.#ident)?;}
    });
    Ok(quote! {
        const _: () = {
            impl #SerializeNamed for #ident {
                fn ser_named<S: #SerializeMap>(&self, mut serializer: S) -> #Result<S::Ok, S::Error> {
                    #(#ser_fields)*
                    serializer.end()
                }
            }
        };
    })
}

fn expand_deserialize_named(item: DeriveInput) -> syn::Result<TokenStream> {
    let Strukt {
        ident,
        fields,
        deny_unknown_fields,
        krate,
    } = Strukt::new(&item)?;
    let Exports {
        DeserializeNamed,
        MapAccess,
        Deserialize,
        MapAccessDeserializer,
        Result,
        ..
    } = Exports::new(krate);
    let shim = syn::Ident::new(&format!("_{}", ident.unraw()), ident.span());
    let deny_unknown_fields = deny_unknown_fields.then_some(quote! {#[serde(deny_unknown_fields)]});
    let fields = fields.iter().map(
        |Field {
             ident,
             ty,
             rename,
             default,
         }| {
            let default = default.then_some(quote! {#[serde(default)]});
            let rename = rename.as_ref().map(|it| quote! {#[serde(rename = #it)]});
            quote! {
                #default
                #rename
                #ident: #ty,
            }
        },
    );
    let remote = ident.to_string();
    Ok(quote! {
        const _: () = {
            impl<'de> #DeserializeNamed<'de> for #ident {
                fn de_named<D: #MapAccess<'de>>(deserializer: D) -> #Result<Self, D::Error> {
                    #[derive(#Deserialize)]
                    #[serde(remote = #remote)]
                    #deny_unknown_fields
                    struct #shim {
                        #(#fields)*
                    }
                    #shim::deserialize(#MapAccessDeserializer::new(deserializer))
                }
            }
        };
    })
}

fn expand_deserialize_positional(item: DeriveInput) -> syn::Result<TokenStream> {
    let Strukt {
        ident,
        fields,
        deny_unknown_fields,
        krate,
    } = Strukt::new(&item)?;
    for ((ix, l), r) in iter::zip(fields.iter().enumerate(), fields.iter().skip(1)) {
        if l.default && !r.default {
            return Err(syn::Error::new(
                fields[ix].ident.span(),
                "#[default] fields must follow other fields",
            ));
        }
    }
    let Exports {
        DeserializePositional,
        de_Error,
        IgnoredAny,
        SeqAccess,
        Result,
        Err_,
        None_,
        Ok_,
        ..
    } = Exports::new(krate);
    let field_ctors = fields.iter().enumerate().map(
        |(
            ix,
            Field {
                ident, ty, default, ..
            },
        )| {
            let missing_required_parameter = format!("missing required parameter at index {ix}");
            match default {
                false => quote! {
                    #ident: deserializer.next_element::<#ty>()?
                            .ok_or_else(|| #de_Error::custom(#missing_required_parameter))?,
                },
                true => quote! {
                    #ident: deserializer.next_element::<#ty>()?.unwrap_or_default(),
                },
            }
        },
    );
    let deny_unknown_fields = deny_unknown_fields.then_some(quote! {
        let #Ok_(#None_) = deserializer.next_element::<#IgnoredAny>() else {
            return #Err_(
                #de_Error::custom("unknown trailing arguments")
            );
        };
    });
    Ok(quote! {
        const _: () = {
            impl<'de> #DeserializePositional<'de> for #ident {
                fn de_positional<D: #SeqAccess<'de>>(mut deserializer: D) -> #Result<Self, D::Error> {
                    let this = Self {
                        #(#field_ctors)*
                    };
                    #deny_unknown_fields;
                    #Ok_(this)
                }
            }
        };
    })
}

fn expand_serialize_positional(item: DeriveInput) -> syn::Result<TokenStream> {
    let Strukt {
        ident,
        krate,
        fields,
        ..
    } = Strukt::new(&item)?;
    let Exports {
        SerializePositional,
        SerializeSeq,
        Result,
        ..
    } = Exports::new(krate);
    let ser_fields = fields
        .iter()
        .map(|Field { ident, .. }| quote! { serializer.serialize_element(&self.#ident)?; });

    Ok(quote! {
        const _: () = {
            impl #SerializePositional for #ident {
                fn ser_positional<S: #SerializeSeq>(&self, mut serializer: S) -> #Result<S::Ok, S::Error> {
                    #(#ser_fields)*
                    serializer.end()
                }
            }
        };
    })
}

#[derive(Debug)]
struct Strukt {
    ident: Ident,
    fields: Vec<Field>,
    deny_unknown_fields: bool,
    krate: Option<ModulePath>,
}

macro_rules! exports {
    ($($path:path as $ident:ident);* $(;)?) => {
        #[allow(non_snake_case)]
        struct Exports {
            $(
                #[doc = stringify!($path)]
                $ident: TokenStream,
            )*
        }
        impl Exports {
            fn new(path_to_ez_jsonrpc: Option<ModulePath>) -> Self {
                let path_to_ez_jsonrpc = path_to_ez_jsonrpc.unwrap_or(parse_quote!(::ez_jsonrpc)).into_token_stream();
                Self {
                    $(
                        $ident: quote! { #path_to_ez_jsonrpc::__private::exports::$ident }
                    ),*
                }
            }
        }
    };
}

exports! {
    crate::params::DeserializeNamed as DeserializeNamed;
    crate::params::DeserializePositional as DeserializePositional;
    crate::params::SerializeNamed as SerializeNamed;
    crate::params::SerializePositional as SerializePositional;
    Err as Err_;
    None as None_;
    Ok as Ok_;
    Result as Result;
    serde::de::Error as de_Error;
    serde::de::IgnoredAny as IgnoredAny;
    serde::de::MapAccess as MapAccess;
    serde::de::SeqAccess as SeqAccess;
    serde::de::value::MapAccessDeserializer as MapAccessDeserializer;
    serde::ser::SerializeMap as SerializeMap;
    serde::ser::SerializeSeq as SerializeSeq;
    serde::Deserialize as Deserialize;
}

impl Strukt {
    fn new(input: &DeriveInput) -> darling::Result<Self> {
        #[derive(FromDeriveInput)]
        #[darling(attributes(jsonrpc))]
        struct _Strukt {
            ident: Ident,
            data: ast::Data<util::Ignored, Field>,
            #[darling(default)]
            deny_unknown_fields: bool,
            #[darling(rename = "crate")]
            krate: Option<ModulePath>,
        }
        if !input.generics.params.is_empty()
            || input
                .generics
                .where_clause
                .as_ref()
                .is_some_and(|it| !it.predicates.is_empty())
        {
            return Err(darling::Error::unsupported_shape("generics").with_span(&input.generics));
        }
        let _Strukt {
            ident,
            data,
            deny_unknown_fields,
            krate,
        } = FromDeriveInput::from_derive_input(input)?;
        Ok(Self {
            ident,
            fields: match data {
                ast::Data::Enum(_) => return Err(darling::Error::unsupported_shape("enum")),
                ast::Data::Struct(fields) => fields.fields,
            },
            deny_unknown_fields,
            krate,
        })
    }
}

#[derive(Debug)]
struct ModulePath {
    leading_colon: Option<Token![::]>,
    segments: Punctuated<Ident, Token![::]>,
}

impl Parse for ModulePath {
    fn parse(input: ParseStream) -> syn::Result<Self> {
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

#[derive(Debug)]
struct Field {
    ident: Ident,
    ty: syn::Type,
    rename: Option<String>,
    default: bool,
}

impl FromField for Field {
    fn from_field(field: &syn::Field) -> darling::Result<Self> {
        #[derive(FromField)]
        #[darling(attributes(jsonrpc))]
        struct _Field {
            ident: Option<Ident>,
            ty: syn::Type,
            rename: Option<String>,
            #[darling(default)]
            default: bool,
        }

        let _Field {
            ident,
            ty,
            rename,
            default,
        } = FromField::from_field(field)?;
        Ok(Self {
            ident: ident.ok_or_else(|| {
                darling::Error::unsupported_shape_with_expected(
                    "unnamed fields",
                    &"a struct with named fields",
                )
            })?,
            ty,
            rename,
            default,
        })
    }
}
