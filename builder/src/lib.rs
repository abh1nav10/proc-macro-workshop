use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Error, Ident};

fn is_option(ty: syn::Type) -> bool {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if path.segments.len() == 1 {
            if let std::option::Option::Some(syn::PathSegment { ident, .. }) = path.segments.first()
            {
                if ident == "Option" {
                    return true;
                }
            }
        }
    }
    false
}

fn extract_inner_type(ty: syn::Type) -> std::option::Option<syn::Ident> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if let std::option::Option::Some(syn::PathSegment {
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
            ..
        }) = path.segments.first()
        {
            if args.len() != 1 {
                return std::option::Option::None;
            }
            if let std::option::Option::Some(syn::GenericArgument::Type(syn::Type::Path(
                syn::TypePath { path, .. },
            ))) = args.first()
            {
                if let std::option::Option::Some(syn::PathSegment { ident, .. }) =
                    path.segments.first()
                {
                    return std::option::Option::Some(ident.clone());
                }
            }
        }
    }
    std::option::Option::None
}

enum ErrorVariant {
    Present(String),
    Fault(Error),
    Absent,
}

fn look_for_attribute(field: syn::Field) -> ErrorVariant {
    for attribute in field.attrs {
        let mut iter = if let syn::Meta::List(syn::MetaList { tokens, .. }) = attribute.meta {
            tokens.into_iter()
        } else {
            unimplemented!()
        };
        match iter.next() {
            std::option::Option::Some(proc_macro2::TokenTree::Ident(i)) => {
                if i != "each" {
                    let error = Error::new(i.span(), "expected `builder(each = '...')`");
                    return ErrorVariant::Fault(error);
                }
            }
            _ => panic!("Expected Ident, found something else!"),
        }
        match iter.next() {
            std::option::Option::Some(proc_macro2::TokenTree::Punct(p)) => {
                if p.as_char() != '=' {
                    return ErrorVariant::Fault(Error::new(
                        p.span(),
                        "Unexpected punctuation mark!",
                    ));
                }
            }
            _ => panic!("Expected Punct, found something else!"),
        }
        match iter.next() {
            std::option::Option::Some(proc_macro2::TokenTree::Literal(l)) => {
                if let syn::Lit::Str(s) = syn::Lit::new(l) {
                    let output = s.value();
                    return ErrorVariant::Present(output);
                }
            }
            _ => panic!("Expected Literal, found something else!"),
        }
    }
    ErrorVariant::Absent
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = input.data
    {
        named
    } else {
        unimplemented!();
    };

    let optionized_fields = fields.iter().map(|field| {
        let ty = &field.ty;
        let ident = &field.ident;
        if is_option(ty.clone()) {
            let output = quote! {
                #ident: #ty
            };
            return output;
        }
        quote! {
            #ident: std::option::Option<#ty>
        }
    });

    let initial_builder_state = fields.iter().map(|field| {
        let ident = &field.ident;
        quote! {
            #ident: std::option::Option::None
        }
    });

    let builder_methods = fields.iter().map(|field| {
        let ty = &field.ty;
        let ident = &field.ident;
        match look_for_attribute(field.clone()) {
            ErrorVariant::Present(a) => {
                let inner_type =
                    if let std::option::Option::Some(t) = extract_inner_type(ty.clone()) {
                        t
                    } else {
                        panic!("Consider giving an appropriate type to Vec")
                    };
                let name = syn::Ident::new(&a, Span::call_site().into());
                let output = quote! {
                    fn #name(&mut self, element: #inner_type) -> &mut Self {
                        if let std::option::Option::Some(ref mut p) = self.#ident {
                            p.push(element);
                        } else {
                            let vec = vec![element];
                            self.#ident = std::option::Option::Some(vec);
                        }
                        self
                    }
                };
                return output;
            }
            ErrorVariant::Fault(e) => return e.to_compile_error(),
            ErrorVariant::Absent => {}
        }
        if is_option(ty.clone()) {
            let inner_type =
                if let std::option::Option::Some(inner) = extract_inner_type(ty.clone()) {
                    inner
                } else {
                    panic!("Consider giving an appropriate type to Option!")
                };
            let output = quote! {
                fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            };
            return output;
        }
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        }
    });

    let final_map = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        if let ErrorVariant::Present(_) = look_for_attribute(field.clone()) {
            let output = quote! {
                #ident: self.#ident.clone().unwrap_or_else(std::vec::Vec::new)
            };
            return output;
        }
        if is_option(ty.clone()) {
            let output = quote! {
                #ident: self.#ident.clone()
            };
            return output;
        }
        quote! {
            #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " not set"))?
        }
    });

    let output = quote! {
        struct #builder_name {
            #(#optionized_fields,)*
        }

        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#initial_builder_state,)*
                }
            }
        }

        impl #builder_name {
            #(#builder_methods)*

            fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#final_map,)*
                })
            }
        }
    };
    TokenStream::from(output)
}
