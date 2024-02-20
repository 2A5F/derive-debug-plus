#![doc = include_str!("../README.md")]

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma, Attribute, DataEnum,
    DataStruct, DeriveInput, Error, Expr, Fields, FieldsNamed, FieldsUnnamed, Ident, LitStr,
    Meta, Path, Token, Variant,
};

/// Derive macro generating an implementation of [`Debug`](std::fmt::Debug)
/// with more customization options that the normal [`Debug`] derive macro.
///
/// For detailed documentation see the [`mod-level docs`](self)
#[proc_macro_derive(Dbg, attributes(dbg))]
pub fn derive_debug(target: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(target as DeriveInput);
    derive_debug_impl(item).into()
}

fn derive_debug_impl(item: DeriveInput) -> TokenStream {
    let name = &item.ident;
    let (impl_generics, type_generics, where_clause) = &item.generics.split_for_impl();

    let options = match parse_options(&item.attrs, OptionsTarget::DeriveItem) {
        Ok(options) => options,
        Err(e) => return e.to_compile_error(),
    };

    let display_name = if let Some(alias) = options.alias {
        alias
    } else {
        let alias = name.to_string();
        syn::parse_quote_spanned! { name.span() => #alias }
    };

    let res = match &item.data {
        syn::Data::Struct(data) => derive_struct(&display_name, data),
        syn::Data::Enum(data) => derive_enum(data),
        syn::Data::Union(data) => Err(syn::Error::new_spanned(
            data.union_token,
            "#[derive(Dbg)] not supported on unions",
        )),
    };

    match res {
        Ok(res) => quote! {
            impl #impl_generics ::std::fmt::Debug for #name #type_generics #where_clause {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    #res
                }
            }
        },
        Err(e) => e.to_compile_error(),
    }
}

fn derive_struct(display_name: &Expr, data: &DataStruct) -> Result<TokenStream, syn::Error> {
    match &data.fields {
        Fields::Named(fields) => {
            let fields = derive_named_fields(fields, true)?;
            Ok(quote! {
                f.debug_struct(#display_name)
                    #fields
                    .finish()
            })
        }
        Fields::Unnamed(fields) => {
            let fields = derive_unnamed_fields(fields, true)?;
            Ok(quote! {
                f.debug_tuple(#display_name)
                    #fields
                    .finish()
            })
        }
        Fields::Unit => Ok(quote! {
            f.debug_struct(#display_name).finish()
        }),
    }
}

fn derive_enum(data: &DataEnum) -> Result<TokenStream, syn::Error> {
    if data.variants.is_empty() {
        return Ok(quote! {
            unsafe { ::core::hint::unreachable_unchecked() }
        });
    }

    let variants = derive_enum_variants(data.variants.iter())?;

    Ok(quote! {
        match self {
            #variants
        }
    })
}

fn derive_enum_variants<'a>(
    variants: impl Iterator<Item = &'a Variant>,
) -> Result<TokenStream, syn::Error> {
    let mut res = TokenStream::new();

    for variant in variants {
        let name = &variant.ident;

        let options = parse_options(&variant.attrs, OptionsTarget::EnumVariant)?;

        let display_name = if let Some(alias) = options.alias {
            alias
        } else {
            let alias = name.to_string();
            syn::parse_quote_spanned! { name.span() => #alias }
        };

        let derive_variant = match options.print_type {
            FieldPrintType::Normal => derive_variant(name, &display_name, &variant.fields)?,
            FieldPrintType::Skip => skip_variant(name, &display_name, &variant.fields)?,
            _ => return Err(syn::Error::new_spanned(variant, "Internal error")),
        };

        res.extend(derive_variant);
    }

    Ok(res)
}

fn derive_variant(
    name: &Ident,
    display_name: &Expr,
    fields: &Fields,
) -> Result<TokenStream, syn::Error> {
    let match_list = derive_match_list(fields)?;

    match fields {
        Fields::Named(fields) => {
            let fields = derive_named_fields(fields, false)?;
            Ok(quote! {
                Self::#name #match_list => f.debug_struct(#display_name) #fields .finish(),
            })
        }
        Fields::Unnamed(fields) => {
            let fields = derive_unnamed_fields(fields, false)?;
            Ok(quote! {
                Self::#name #match_list => f.debug_tuple(#display_name) #fields .finish(),
            })
        }
        Fields::Unit => Ok(quote! { Self::#name => write!(f, #display_name), }),
    }
}

fn skip_variant(
    name: &Ident,
    display_name: &Expr,
    fields: &Fields,
) -> Result<TokenStream, syn::Error> {
    match fields {
        Fields::Named(_) => {
            Ok(quote! { Self::#name{..} => f.debug_struct(#display_name).finish(), })
        }
        Fields::Unnamed(_) => {
            Ok(quote! { Self::#name(..) => f.debug_tuple(#display_name).finish(), })
        }
        Fields::Unit => Ok(quote! { Self::#name => write!(f, #display_name), }),
    }
}

fn derive_match_list(fields: &Fields) -> Result<TokenStream, syn::Error> {
    match fields {
        Fields::Named(fields) => {
            let mut res = TokenStream::new();
            for field in &fields.named {
                let name = field.ident.as_ref().unwrap();
                let options = parse_options(&field.attrs, OptionsTarget::NamedField)?;

                match options.print_type {
                    FieldPrintType::Skip => res.extend(quote! { #name: _, }),
                    _ => res.extend(quote! { #name, }),
                }
            }
            Ok(quote! { { #res } })
        }
        Fields::Unnamed(fields) => {
            let mut res = TokenStream::new();
            for (i, field) in fields.unnamed.iter().enumerate() {
                let name = format_ident!("field_{}", i);
                let options = parse_options(&field.attrs, OptionsTarget::UnnamedField)?;

                match options.print_type {
                    FieldPrintType::Skip => res.extend(quote! { _, }),
                    _ => res.extend(quote! { #name, }),
                }
            }
            Ok(quote! { (#res) })
        }
        Fields::Unit => Ok(quote! {}),
    }
}

fn derive_named_fields(fields: &FieldsNamed, use_self: bool) -> Result<TokenStream, syn::Error> {
    let mut res = TokenStream::new();

    for field in &fields.named {
        let name = field.ident.as_ref().unwrap();

        let options = parse_options(&field.attrs, OptionsTarget::NamedField)?;

        let name_str = if let Some(alias) = options.alias {
            alias
        } else {
            let alias = name.to_string();
            syn::parse_quote_spanned! { name.span() => #alias }
        };

        match options.print_type {
            FieldPrintType::Normal => {
                let field_ref = if use_self {
                    quote! { &self.#name }
                } else {
                    quote! { #name }
                };
                res.extend(quote! { .field(#name_str, #field_ref) });
            }
            FieldPrintType::Placeholder(placeholder) => {
                res.extend(quote! { .field(#name_str, &format_args!(#placeholder)) })
            }
            FieldPrintType::Format(fmt) => {
                let field_ref = if use_self {
                    quote! { self.#name }
                } else {
                    quote! { #name }
                };
                res.extend(quote! { .field(#name_str, &format_args!(#fmt, #field_ref)) })
            }
            FieldPrintType::Custom(formatter) => {
                let field_ref = if use_self {
                    quote! { &self.#name }
                } else {
                    quote! { #name }
                };
                res.extend(
                    quote! { .field(#name_str, &format_args!("{}", #formatter(#field_ref))) },
                )
            }
            FieldPrintType::Expr(expr) => res.extend(quote! { .field(#name_str, #expr) }),
            FieldPrintType::Skip => {}
        }
    }

    Ok(res)
}

fn derive_unnamed_fields(
    fields: &FieldsUnnamed,
    use_self: bool,
) -> Result<TokenStream, syn::Error> {
    let mut res = TokenStream::new();

    for (i, field) in fields.unnamed.iter().enumerate() {
        let options = parse_options(&field.attrs, OptionsTarget::UnnamedField)?;

        match options.print_type {
            FieldPrintType::Normal => {
                let field_ref = if use_self {
                    let index = syn::Index::from(i);
                    quote! { &self.#index }
                } else {
                    format_ident!("field_{}", i).to_token_stream()
                };
                res.extend(quote! { .field(#field_ref) });
            }
            FieldPrintType::Placeholder(placeholder) => {
                res.extend(quote! { .field(&format_args!(#placeholder)) })
            }
            FieldPrintType::Format(fmt) => {
                let field_ref = if use_self {
                    let index = syn::Index::from(i);
                    quote! { self.#index }
                } else {
                    format_ident!("field_{}", i).to_token_stream()
                };
                res.extend(quote! { .field(&format_args!(#fmt, #field_ref)) })
            }
            FieldPrintType::Custom(formatter) => {
                let field_ref = if use_self {
                    let index = syn::Index::from(i);
                    quote! { &self.#index }
                } else {
                    format_ident!("field_{}", i).to_token_stream()
                };
                res.extend(quote! { .field(&format_args!("{}", #formatter(#field_ref))) });
            }
            FieldPrintType::Expr(expr) => {
                res.extend(quote! { .field(#expr) });
            }
            FieldPrintType::Skip => {}
        }
    }

    Ok(res)
}

enum FieldPrintType {
    Normal,
    Placeholder(String),
    Skip,
    Format(LitStr),
    Custom(Path),
    Expr(Expr),
}

struct FieldOutputOptions {
    print_type: FieldPrintType,
    alias: Option<Expr>,
}

#[derive(PartialEq, Eq)]
enum OptionsTarget {
    DeriveItem,
    EnumVariant,
    NamedField,
    UnnamedField,
}

fn parse_options(
    attributes: &[Attribute],
    target: OptionsTarget,
) -> Result<FieldOutputOptions, syn::Error> {
    let mut res = FieldOutputOptions {
        print_type: FieldPrintType::Normal,
        alias: None,
    };

    for attrib in attributes {
        let meta = &attrib.meta;

        if !meta.path().is_ident("dbg") {
            continue;
        }

        let meta = if let Meta::List(m) = meta {
            m
        } else {
            return Err(syn::Error::new_spanned(
                meta,
                "invalid #[dbg(...)] attribute",
            ));
        };

        let options: OptionItems = syn::parse2(meta.tokens.clone())?;

        for option in options.options {
            match option.option {
                TheOption::Skip if target != OptionsTarget::DeriveItem => {
                    res.print_type = FieldPrintType::Skip
                }
                TheOption::Alias(alias) if target != OptionsTarget::UnnamedField => {
                    res.alias = Some(alias)
                }
                TheOption::Placeholder(placeholder)
                    if target == OptionsTarget::NamedField
                        || target == OptionsTarget::UnnamedField =>
                {
                    res.print_type = FieldPrintType::Placeholder(placeholder.value())
                }
                TheOption::Fmt(fmt)
                    if target == OptionsTarget::NamedField
                        || target == OptionsTarget::UnnamedField =>
                {
                    res.print_type = FieldPrintType::Format(fmt)
                }
                TheOption::Expr(expr) => res.print_type = FieldPrintType::Expr(expr),
                TheOption::Formatter(path)
                    if target == OptionsTarget::NamedField
                        || target == OptionsTarget::UnnamedField =>
                {
                    res.print_type = FieldPrintType::Custom(path)
                }
                _ => return Err(syn::Error::new_spanned(option.path, "invalid option")),
            }
        }
    }

    Ok(res)
}

struct OptionItems {
    pub options: Punctuated<OptionItem, Comma>,
}

impl syn::parse::Parse for OptionItems {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let options = Punctuated::<OptionItem, Comma>::parse_separated_nonempty(input)?;
        Ok(Self { options })
    }
}

struct OptionItem {
    pub path: Path,
    pub option: TheOption,
}

enum TheOption {
    Skip,
    Alias(Expr),
    Placeholder(LitStr),
    Fmt(LitStr),
    Expr(Expr),
    Formatter(Path),
}

impl syn::parse::Parse for OptionItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let path: Path = input.parse()?;

        if path.is_ident("skip") {
            return Ok(Self {
                path,
                option: TheOption::Skip,
            });
        }
        if path.is_ident("alias") {
            let _ = input.parse::<Token![=]>()?;
            let a = input.parse()?;
            return Ok(Self {
                path,
                option: TheOption::Alias(a),
            });
        }
        if path.is_ident("placeholder") {
            let _ = input.parse::<Token![=]>()?;
            let a = input.parse()?;
            return Ok(Self {
                path,
                option: TheOption::Placeholder(a),
            });
        }
        if path.is_ident("fmt") {
            let _ = input.parse::<Token![=]>()?;
            let a = input.parse()?;
            return Ok(Self {
                path,
                option: TheOption::Fmt(a),
            });
        }
        if path.is_ident("expr") {
            let _ = input.parse::<Token![=]>()?;
            let a = input.parse()?;
            return Ok(Self {
                path,
                option: TheOption::Expr(a),
            });
        }
        if path.is_ident("formatter") {
            let _ = input.parse::<Token![=]>()?;
            let a = input.parse()?;
            return Ok(Self {
                path,
                option: TheOption::Formatter(a),
            });
        }

        Err(Error::new(path.span(), "invalid option"))
    }
}
