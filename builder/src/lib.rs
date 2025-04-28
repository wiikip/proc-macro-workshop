
use std::collections::{HashMap, HashSet};

use proc_macro2::TokenStream as TokenStream2;
use proc_macro::{TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{parenthesized, parse_macro_input, Expr, Field, Fields, GenericArgument, Ident, Lit, LitStr, MetaList, MetaNameValue, PathArguments, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let _ = input;
    let derived_input = parse_macro_input!(input as syn::DeriveInput);
    let builders_attrs =  match match derived_input.data {
        syn::Data::Struct(ref s) => {
            parse_builders_attributes(&s.fields)
        }
        _ => unimplemented!()
    } {
        Ok(ba) => ba,
        Err(e) => return e.into()
    };

    let optional_attrs = match derived_input.data {
        syn::Data::Struct(ref s) => {
            s.fields.iter().filter_map(|f|{
                if match f.ty {
                    Type::Path(ref fp) => {
                        fp.path.segments.iter().any(|fps| {
                            if fps.ident == "Option" {
                                return true
                            }
                            return false
                        })},
                    _ => false
                }{
                    let inner_type = match f.ty {
                        Type::Path(ref fp) =>
                        {
                         match fp.path.segments.first().unwrap().arguments {
                            PathArguments::AngleBracketed(ref arg) => {
                                match arg.args.first().unwrap() {
                                    GenericArgument::Type(t) => t,
                                    _ => unreachable!()
                                }
                            }
                            _ => unreachable!()
                         }
                        }
                        _ => unreachable!()
                    };
                    return Some((f.ident.as_ref().unwrap(),inner_type))
                }
                None
            })
        }
        _ => unreachable!()
    };
    let attrs: Vec<TokenStream2> = match derived_input.data {
        syn::Data::Struct(ref s) => {
            s.fields.iter().map(|f|  {
                let field_name = f.ident.as_ref().unwrap();
                let field_type = &f.ty;
                if builders_attrs.contains_key(field_name) {
                    return quote! {
                        #field_name: #field_type
                    }
                }
                if optional_attrs.clone().any(|f| {
                    f.0 == field_name
                }) {
                    return quote! {
                        #field_name: #field_type
                    }
                }
                quote!{
                    #field_name: core::option::Option<#field_type>
                }
            }).collect()
        },
        _ => unimplemented!()
    };

    let initialized_attrs: Vec<TokenStream2> = match derived_input.data {
        syn::Data::Struct(ref s) => {
            s.fields.iter().map(|f|  {
                let field_name = f.ident.as_ref().unwrap();
                if builders_attrs.contains_key(field_name){
                    return quote! {
                        #field_name: Vec::new()
                    }
                }
                quote!{
                    #field_name: None
                }
            }).collect()
        },
        _ => unimplemented!()
    };


    let struct_name = format_ident!("{}", derived_input.ident);
    let builder_name = format_ident!("{}Builder", derived_input.ident);
    let builder_error_name = format_ident!("{}BuilderError", derived_input.ident);
    let setters = match derived_input.data {
        syn::Data::Struct(ref s) => {
            s.fields.iter().map(|f| {
                let field_name = f.ident.as_ref().unwrap();
                if builders_attrs.contains_key(field_name) {
                    let setter_name = builders_attrs.get(field_name).unwrap();
                    let field_type = get_inner_type(&f.ty);
                    return quote!{
                        fn #setter_name(&mut self, arg: #field_type) -> &mut Self {
                            self.#field_name.push(arg);
                            self
                        }
                    }
                }
                let field_type = optional_attrs.clone().find(|f| f.0 == field_name).map(|f| f.1).unwrap_or(&f.ty);
                quote! {
                    fn #field_name(&mut self, arg: #field_type) -> &mut Self{
                        self.#field_name = Some(arg);
                        self
                    }
                }
            })
        }
        _ => unimplemented!()
    };
    let check_attrs = match derived_input.data {
        syn::Data::Struct(ref s) => {
            s.fields.iter().filter_map(|f| {
                let field_name = f.ident.as_ref().unwrap();
                if optional_attrs.clone().any(|f| f.0 == field_name) {
                    return None
                }
                if builders_attrs.contains_key(field_name){
                    return None
                }
                Some(quote! {
                    if let None = self.#field_name {
                        return Err(#builder_error_name.into())
                    }
                })
            })
        }
        _ => unreachable!()
    };

    let map_attrs = match derived_input.data {
        syn::Data::Struct(ref s) => {
            s.fields.iter().map(|f| {
                let field_name = f.ident.as_ref().unwrap();
                if builders_attrs.contains_key(field_name){
                    return quote!{
                        #field_name: self.#field_name.clone()
                    }
                }
                if optional_attrs.clone().any(|f| f.0 == field_name) {
                    return quote! {
                        #field_name: self.#field_name.take()
                    }
                }
                quote! {
                    #field_name: self.#field_name.take().unwrap()
                }
            })
        }
        _ => unreachable!()
    };

    let t  = quote! {
        #[derive(Debug, Clone)]
        struct #builder_error_name;
        impl ::core::fmt::Display for #builder_error_name{
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                write!(f, "Builder Error")
            }
        }
        impl std::error::Error for #builder_error_name {}

        struct #builder_name {
            #(#attrs,)*
        }
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name{
                    #(#initialized_attrs,)*
                }
            }
        }
        impl #builder_name {
            fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                #(#check_attrs)*
                Ok(#struct_name{
                    #(#map_attrs,)*
                })
            }
            #(#setters)*
        }
    }.into();
    t
}

fn parse_builders_attributes(fields: &Fields) -> std::result::Result<HashMap<Ident, Ident>, TokenStream2>{
    let builder_fields = match fields {
        Fields::Named(fields_named) => {
            fields_named.named.iter().filter(|f| f.attrs.len() > 0 && f.attrs.iter().any( |fa| fa.path().segments[0].ident.to_string() == "builder"))
        }
        _ => unimplemented!("expected named fields")
    };

    builder_fields.into_iter().map(|f|{
        let attr = f.attrs.iter().find(|attr| attr.path().segments[0].ident.to_string() == "builder").unwrap();
        let meta = attr.parse_args().unwrap();
        match meta {
            syn::Meta::NameValue(l) => {
                if l.path.is_ident("each") {
                    return match l.value {
                        Expr::Lit(lit) => {
                            match lit.lit {
                                Lit::Str(s) => Ok((f.ident.clone().unwrap(), Ident::new(&s.value(), proc_macro2::Span::call_site()))),
                                _ => Err(syn::Error::new_spanned(lit , "expected lit to be string").to_compile_error())
                            }
                        },
                        _ => Err(syn::Error::new_spanned(l, "expected lit").to_compile_error())
                    }
                };
                Err(syn::Error::new_spanned(l.path, "expected each ident").to_compile_error())
            }
            _ => Err(syn::Error::new_spanned(meta, "expected list for builder").to_compile_error())
        }
    }).collect::<std::result::Result<HashMap<_,_>,_>>()

}

fn get_inner_type(t: &Type) -> &Type {
    let inner_type = match t {
        Type::Path(fp) =>
        {
         match fp.path.segments.first().unwrap().arguments {
            PathArguments::AngleBracketed(ref arg) => {
                match arg.args.first().unwrap() {
                    GenericArgument::Type(t) => t,
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
         }
        }
        _ => unreachable!()
    };
    inner_type
}