use quote::{quote};
use syn::parse::{Parse, ParseStream, Result};

use syn::{parse_macro_input, Ident, Token, Type, Visibility, parenthesized};

struct ArenaDeclaration {
    visibility: Visibility,
    name: Ident,
    tys: Vec<Type>,
}

impl Parse for ArenaDeclaration {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility: Visibility = input.parse()?;
        let _: Token![struct] = input.parse()?;
        let name: Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let tys = content.parse_terminated(Type::parse, Token![,])?;
        Ok(Self {
            visibility,
            name,
            tys: tys.into_iter().collect(),
        })
    }
}

#[proc_macro]
pub fn declare_arena(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ArenaDeclaration { visibility, name, tys } = parse_macro_input!(input as ArenaDeclaration);

    let mut types_stream = quote!();
    let mut type_inits_stream = quote!();
    let mut count_type_stream = quote!();
    let mut count_type_init_stream = quote!();
    let mut release_temps_stream = quote!();
    let mut impl_arena_stream = quote!();
    for (i, ty) in tys.iter().enumerate() {
        let i = syn::Index::from(i);
        types_stream.extend(quote!{ arena::FlatArena::<#ty>, });
        type_inits_stream.extend(quote!{ arena::FlatArena::<#ty>::new(), });
        count_type_stream.extend(quote!{ usize, });
        count_type_init_stream.extend(quote!{ self.#i.temps_count(), });
        release_temps_stream.extend(quote!{
            self.#i.release_temporaries(target_count.#i);
        });
        impl_arena_stream.extend( quote!{
            impl arena::TypedArena<#ty> for #name {
                fn alloc(
                    &mut self,
                    value: #ty
                ) -> arena::HandleId<#ty> { self.#i.alloc(value) }

                fn alloc_temp(
                    &mut self,
                    init: impl FnOnce() -> #ty,
                    reset: impl FnOnce(&mut #ty) -> ()
                ) -> HandleId<#ty> { self.#i.alloc_temp(init, reset) }

                fn dealloc(
                    &mut self,
                    handle: arena::HandleId<#ty>
                ) -> bool { self.#i.dealloc(handle) }

                fn get(
                    &self,
                    handle: arena::HandleId<#ty>
                ) -> Option<&#ty> { self.#i.get(handle) }

                fn get_mut(
                    &mut self,
                    handle: arena::HandleId<#ty>
                ) -> Option<&mut #ty> { self.#i.get_mut(handle) }
            }
        });
    }

    let expanded = quote! {
        #visibility struct #name ( #types_stream );
        impl #name {
            pub fn new() -> Self { Self(#type_inits_stream) }
        }

        impl arena::TempsCount for #name {
            type CountType = (#count_type_stream);
            fn temps_count(&self) -> Self::CountType { (#count_type_init_stream) }
        }

        impl arena::ReleaseTemps for #name {
            fn release_temporaries(&mut self, target_count: Self::CountType) {
                #release_temps_stream
            }
        }

        #impl_arena_stream
    };

    expanded.into()
}
