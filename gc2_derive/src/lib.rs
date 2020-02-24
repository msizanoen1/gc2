use proc_macro2::Span;
use quote::quote;
use syn::visit_mut::VisitMut;

fn gc_obj_derive(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    let body = s.each(|bi| {
        quote! {
            gc2::GcObj::visit(#bi, c)
        }
    });

    s.add_bounds(synstructure::AddBounds::None);

    s.gen_impl(quote! {
        extern crate gc2;

        gen unsafe impl gc2::GcObj for @Self {
            unsafe fn visit(&self, c: &mut gc2::Collector) {
                match self { #body }
            }
        }
    })
}

fn check_force(s: &mut synstructure::Structure, is_mut: bool) {
    let attrs = s.ast().attrs.clone();
    for attr in attrs {
        if let Some(id) = attr.path.get_ident() {
            if (id.to_string() == "gc_force_accessible" && !is_mut)
                || (id.to_string() == "gc_force_accessible_mut" && is_mut)
            {
                s.add_bounds(synstructure::AddBounds::Generics);
            }
        }
    }
}

fn gc_accessible_derive(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    check_force(&mut s, false);

    s.gen_impl(quote! {
        extern crate gc2;

        gen unsafe impl gc2::GcAccessible for @Self {}
    })
}

fn gc_accessible_derive_mut(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    check_force(&mut s, true);

    s.gen_impl(quote! {
        extern crate gc2;

        gen unsafe impl gc2::GcAccessibleMut for @Self {}
    })
}

struct TypeReplaceVisitor(bool);

impl VisitMut for TypeReplaceVisitor {
    fn visit_field_mut(&mut self, i: &mut syn::Field) {
        let ty = i.ty.clone();
        if !self.0 {
            i.ty = syn::parse_quote!(gc2::GcAccess<'__access, #ty>);
        } else {
            i.ty = syn::parse_quote!(gc2::GcAccessMut<'__access, #ty>);
        }
    }
}

fn gc_project_derive(s: synstructure::Structure) -> proc_macro2::TokenStream {
    let mut new_ast = s.ast().clone();
    syn::visit_mut::visit_derive_input_mut(&mut TypeReplaceVisitor(false), &mut new_ast);
    new_ast.ident = syn::Ident::new(&format!("{}Project", new_ast.ident), Span::call_site());
    let new_ident = new_ast.ident.clone();
    let params = new_ast.generics.params.clone();
    new_ast.generics.params = syn::parse_quote!('__access, #params);
    let new = synstructure::Structure::new(&new_ast);
    let body = s.each_variant(|v| {
        let name = v.ast().ident.clone();
        let bindings = v
            .bindings()
            .iter()
            .cloned()
            .map(|n| quote!(gc2::GcAccess::from_raw(#n)))
            .collect::<Vec<_>>();
        let new_v = new
            .variants()
            .iter()
            .find(|n| *n.ast().ident == name)
            .unwrap_or_else(|| new.variants().first().unwrap());
        new_v.construct(|_, i| &bindings[i])
    });
    let (impl_generics, ty_generics, where_clause) = s.ast().generics.split_for_impl();
    let name = s.ast().ident.clone();
    let new_generics = new_ast.generics.split_for_impl().1;
    quote! {
        #new_ast

        #[automatically_derived]
        impl #impl_generics #name #ty_generics #where_clause {
            fn project<'__access>(self: gc2::GcAccess<'__access, Self>) -> #new_ident #new_generics {
                unsafe {
                    match gc2::GcAccess::get_unchecked(self) { #body }
                }
            }
        }
    }
}

fn gc_project_derive_mut(mut s: synstructure::Structure) -> proc_macro2::TokenStream {
    let mut new_ast = s.ast().clone();
    syn::visit_mut::visit_derive_input_mut(&mut TypeReplaceVisitor(true), &mut new_ast);
    new_ast.ident = syn::Ident::new(&format!("{}ProjectMut", new_ast.ident), Span::call_site());
    let new_ident = new_ast.ident.clone();
    let params = new_ast.generics.params.clone();
    new_ast.generics.params = syn::parse_quote!('__access, #params);
    let new = synstructure::Structure::new(&new_ast);
    s.bind_with(|_| synstructure::BindStyle::RefMut);
    let body = s.each_variant(|v| {
        let name = v.ast().ident.clone();
        let bindings = v
            .bindings()
            .iter()
            .cloned()
            .map(|n| quote!(gc2::GcAccessMut::from_raw(#n)))
            .collect::<Vec<_>>();
        let new_v = new
            .variants()
            .iter()
            .find(|n| *n.ast().ident == name)
            .unwrap_or_else(|| new.variants().first().unwrap());
        new_v.construct(|_, i| &bindings[i])
    });
    let (impl_generics, ty_generics, where_clause) = s.ast().generics.split_for_impl();
    let name = s.ast().ident.clone();
    let new_generics = new_ast.generics.split_for_impl().1;
    quote! {
        #new_ast
        impl #impl_generics #name #ty_generics #where_clause {
            fn project_mut<'__access>(self: &mut gc2::GcAccessMut<'__access, Self>) -> #new_ident #new_generics {
                unsafe {
                    match gc2::GcAccessMut::get_mut_unchecked(self) { #body }
                }
            }
        }
    }
}

synstructure::decl_derive!([GcObj] => gc_obj_derive);
synstructure::decl_derive!([GcAccessible, attributes(gc_force_accessible)] => gc_accessible_derive);
synstructure::decl_derive!([GcAccessibleMut, attributes(gc_force_accessible_mut)] => gc_accessible_derive_mut);
synstructure::decl_derive!([GcProject] => gc_project_derive);
synstructure::decl_derive!([GcProjectMut] => gc_project_derive_mut);
