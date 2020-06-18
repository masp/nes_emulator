use proc_macro::{TokenStream};
use syn::{parse_macro_input};
use quote::{quote};
use syn::export::{TokenStream2};

#[proc_macro_derive(FromString)]
pub fn from_string(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as syn::ItemEnum);
    let name = &input.ident;
    let vars = &input.variants;
    let var_names: Vec<TokenStream2> = vars.iter().map(|v| {
        let id = &v.ident;
        quote! { stringify!(#id) }
    }).collect();
    let var_ids: Vec<TokenStream2> = vars.iter().map(|v| {
        let id = &v.ident;
        quote! { #name::#id }
    }).collect();

    let result = quote! {
        impl #name {
            pub fn to_string(&self) -> &'static str {
                match self {
                    #( #var_ids => #var_names ),*
                }
            }

            pub fn from_string(s: &str) -> ::std::option::Option<#name> {
                match s {
                    #( #var_names => Some(#var_ids), )*
                    _ => None
                }
            }
        }
    };
    result.into()
}