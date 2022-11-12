use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, ItemFn, Signature};

#[proc_macro_attribute]
pub fn with_position(_: TokenStream, item: TokenStream) -> TokenStream {
    let ItemFn {
        attrs,
        vis,
        sig: Signature { ident, inputs, output, .. },
        block,
    } = parse_macro_input!(item as ItemFn);

    quote! {
        #(#attrs)*
        #vis fn #ident(#inputs, position: &Position) #output {
            #block
        }
    }
    .into()
}

#[proc_macro_derive(ErrorFormat)]
pub fn format_error(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, .. } = parse_macro_input!(input as DeriveInput);

    quote! {
        impl #ident {
            #[inline]
            pub fn new(message: &str, args: Vec<String>, position: &Position) -> Self {
                let mut message = message.to_string();

                args.iter().enumerate().for_each(|(i, arg)| {
                    message = message.replace(&format!("{{{}}}", i), arg);
                });

                Self { message, position: position.to_owned() }
            }
        }
    }
    .into()
}
