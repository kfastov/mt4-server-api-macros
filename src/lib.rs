extern crate proc_macro;
use std::fs;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{self, Token, Expr, parse::{Parse, ParseStream, Result}, Ident, ItemFn, parse_macro_input, Lit, LitStr, visit::Visit, ItemStruct, punctuated::Punctuated, FnArg, token::Comma};

#[derive(Debug)]
struct PluginInfo {
    name: String,
    copyright: String,
    version: u32, // TODO require explicit syntax vX.Y.Z
}

fn camel_to_snake(camel: &str) -> String {
    // change each uppercase letter to lowercase + _
    let mut snake = String::new();
    for (i, ch) in camel.chars().enumerate() {
        if ch.is_ascii_uppercase() {
            if i != 0 {
                snake.push('_');
            }
            snake.push(ch.to_ascii_lowercase())
        } else {
            snake.push(ch);
        }
    }
    snake
}

impl Parse for PluginInfo {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name: Option<String> = None;
        let mut copyright: Option<String> = None;
        let mut version: Option<u32> = None;
        for _ in 0..3 {
            let key: Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            let value: Expr = input.parse()?;
            // assert value is a string literal
            if let Expr::Lit(s) = value {
                if let Lit::Str(s) = s.lit {
                    // println!("key: {}, value: {}", &key, s.value());
                    match key.to_string().as_str() {
                        "name" => {name = Some(s.value())},
                        "copyright" => {copyright = Some(s.value())},
                        "version" => {
                            let mut ver: u32 = 0;
                            let mut mult = 100;
                            for c in s.value().chars() {
                                match c {
                                    n @ '0'..='9' => {
                                        let digit: u8 = n.to_string().parse().unwrap();
                                        ver += mult * digit as u32;
                                        mult /= 10;
                                    },
                                    'v' | '.' => (),
                                    _ => return Err(input.error("Failed to parse version"))
                                };
                            }
                            version = Some(ver)
                        },
                        _ => return Err(input.error("Wrong token")), // TODO correct spanning
                    }
                }
            };
        };
        if name.is_some() && copyright.is_some() && version.is_some() {
            Ok(PluginInfo {
                copyright: copyright.unwrap(),
                name: name.unwrap(),
                version: version.unwrap(), // TODO check version
            })
        } else {
            Err(input.error("Incomplete plugin info")) // TODO list missing fields
        }
    }
}

#[proc_macro]
pub fn plugin_info(input: TokenStream) -> TokenStream {
    let PluginInfo {name, copyright, version} = parse_macro_input!(input as PluginInfo);
    
    // TODO if we can use `use` in blocks at top level (not modules)
    quote! {
        use std::ffi::CString;
        
        static PLUGIN_NAME: &str = #name;
        static PLUGIN_COPYRIGHT: &str = #copyright;
        static PLUGIN_VERSION: u32 = #version;

        #[no_mangle]
        pub extern "stdcall" fn MtSrvAbout(info: *mut mt4_server_api::PluginInfo) {
            let info = unsafe { &mut(*info) };
            
            info.version = PLUGIN_VERSION;
            
            let c_name = CString::new(PLUGIN_NAME).unwrap();
            let name_bytes: &[i8] = unsafe {
                std::mem::transmute(c_name.as_bytes_with_nul())
            };
            info.name[..name_bytes.len()].copy_from_slice(name_bytes);
            
            let c_copyright = CString::new(PLUGIN_COPYRIGHT).unwrap();
            let copyright_bytes: &[i8] = unsafe {
                std::mem::transmute(c_copyright.as_bytes_with_nul())
            };
            info.copyright[..copyright_bytes.len()].copy_from_slice(copyright_bytes);
        }
    }.into()
}

struct HookArgs {
    hook_name: String,
}

impl Parse for HookArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = Ident::parse(&input)?;
        Ok(HookArgs {
            hook_name: ident.to_string()
        })
    }
}

impl HookArgs {
    // TODO load hook content dynamically from rust source file
    // TODO check that necessary hooks are defined
    fn hook_content(&self, handler_ident: &Ident) -> TokenStream2 {
        (match self.hook_name.as_str() {
            "startup" => quote!{
                #[no_mangle]
                pub extern "stdcall" fn MtSrvStartup(_server: *mut mt4_server_api::VTableHolder) -> i32 { // CServerInterface *server
                    // TODO check version
                    // let version = unsafe {((*((*_server).vtable)).Version)(std::mem::transmute(_server))};
                    // initialize global static server reference
                    unsafe {&(mt4_server_api::server_api).put(_server as *mut mt4_server_api::VTableHolder)};
                    // TODO give handler ability to abort plugin load
                    #handler_ident();
                    // if version >= 5 {1} else {0}
                    1
                }
            },
            "manager_command" => quote!{
                #[no_mangle]
                pub extern "stdcall" fn MtSrvManagerProtocol(
                    ip: mt4_server_api::ULONG,
                    us: *const mt4_server_api::UserInfo,
                    in_data: *const ::std::os::raw::c_uchar,
                    in_size: ::std::os::raw::c_int,
                    out_data: *mut *mut ::std::os::raw::c_uchar,
                    out_size: *mut ::std::os::raw::c_int,
                ) -> ::std::os::raw::c_int {
                    // convert user info to immutable reference
                    // let us = & unsafe {*us};
                    // convert in_data to immutable slice
                    let in_data: &[u8] = unsafe {
                        std::slice::from_raw_parts(in_data, in_size.try_into().unwrap())
                    };
                    // call handler (TODO check dealloc behavior)
                    // let result: Vec<u8> = // TODO Result<Option<Vec<u8>> or smth
                    let result: Option<Vec<u8>> = #handler_ident(in_data);
                    
                    match result {
                        Some(data) => {
                            // convert result and return successful result
                            let mut boxed_slice = data.into_boxed_slice();
                            let data_ptr = boxed_slice.as_mut_ptr();
                            let len = boxed_slice.len();
                            
                            std::mem::forget(boxed_slice);
                            
                            unsafe {
                                *out_data = data_ptr;
                                *out_size = len as i32;
                            }
                            // return successful execution code
                            1
                        },
                        None => {
                            0
                        }
                    }
                }
            },
            _ => panic!("Unsupported hook type")
        }).into()
    }
    // TODO method which takes signature and checks it
}

#[proc_macro_attribute]
pub fn hook(attr: TokenStream, input: TokenStream) -> TokenStream {
    // getting hook name
    let args = parse_macro_input!(attr as HookArgs);
    
    // getting fn name
    let handler = parse_macro_input!(input as ItemFn);
    let handler_ident = handler.sig.ident.clone();
    // TODO check signature
    
    // TODO use bindgen output
    let hook_def = args.hook_content(&handler_ident);
    
    (quote!{
        #hook_def
        #handler
    }).into()
}

#[derive(Debug)]
struct VtableArgs {
    path: String,
    class_name: String,
}

struct MethodCollector<'a> {
    args: &'a VtableArgs,
    methods: Vec<&'a syn::Signature>,
}

impl<'a> VtableArgs {
    fn collector(&'a self) -> MethodCollector<'a> {
        MethodCollector {
            args: self,
            methods: Vec::new(),
        }
    }
}

impl Parse for VtableArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let class_name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let path: LitStr = input.parse()?;
        
        // TODO expect end
        
        Ok(Self {
            class_name: class_name.to_string(),
            path: path.value(),
        })
    }
}

impl<'a> MethodCollector<'a> {
    fn should_add(&self, sign: &'a syn::Signature) -> bool {
        // TODO no clone
        sign.ident.to_string().starts_with(&self.args.class_name)
    }
    /// Generates owned TokenStream of function pointers
    // TODO maybe implement this as into()
    fn generate(&self) -> TokenStream2 {
        // iterate through methods
        // decompose each signature
        // emit (method_name: unsafe extern "stdcall" fn(inputs) -> output)
        let mut stream = TokenStream2::new();
        
        let token_iter = self.methods.iter()
        .map(|sig| {
            let ident = self.prettify_ident(&sig.ident);
            let inputs = &sig.inputs;
            let output = &sig.output;
            // TODO nicu idents
            quote! { #ident: unsafe extern "stdcall" fn(#inputs) #output, }
        });
        
        stream.extend(token_iter);
        stream
    }
    fn generate_wrappers(&self) -> TokenStream2 {
        // iterate through methods
        // decompose each signature
        // emit wrapper :D
        // 0: check type of fn call arguments
        let mut stream = TokenStream2::new();
        
        let token_iter = self.methods.iter()
        .map(|sig| {
            let ident = &sig.ident;
            let inputs = &sig.inputs;
            let inputs_without_this: Punctuated<FnArg, Comma> = {
                let inputs_owned = inputs.clone();
                // remove first element (which is `this`)
                inputs_owned.into_iter()
                    .skip(1)
                    .collect()
            };
            let input_args: Punctuated<Ident, Comma> = {
                let inputs_owned = inputs.clone();
                inputs_owned.into_iter()
                    .skip(1)
                    .map(|x| {
                        match x {
                            FnArg::Typed(pat_type) => {
                                match *pat_type.pat {
                                    syn::Pat::Ident(arg) => {
                                        arg.ident
                                    },
                                    _ => panic!("Unsupported pattern type")
                                }
                            },
                            _ => panic!("Unsupported arg")
                        }
                    })
                    .collect()
            };
            let output = &sig.output;
            // TODO pretty idents
            let ident = self.prettify_ident(ident);
            quote! {
                pub fn #ident( &self, #inputs_without_this ) #output {
                    unsafe {
                        let vtable_holder = &*self.this_ptr;
                        ((*(vtable_holder.vtable)).#ident)
                            (self.this_ptr as *mut ::std::os::raw::c_void, #input_args)
                    }
                }    
            }
        });
        
        stream.extend(token_iter);
        stream
    }
    fn prettify_ident(&self, ident: &Ident) -> Ident {
        // convert to string
        let name = ident.to_string();
        // remove classname part
        let class_name = &self.args.class_name;
        if name.starts_with(class_name) {
            let new_name = &name[class_name.len() + 1..]; // TODO this plus one is dirty
            let new_name = camel_to_snake(new_name);
            Ident::new(&new_name, ident.span())
        } else {
            ident.clone()
        }
    }
}

impl<'a> Visit<'a> for MethodCollector<'a> {
    fn visit_signature(&mut self, i: &'a syn::Signature) {
        if self.should_add(i) {
            self.methods.push(i);
        }
    }
}

/// vtable macro:
/// reads the file
/// looks for all method declarations (CServerInterface_*)
/// collects them and creates struct from them
#[proc_macro_attribute]
pub fn vtable(attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut args = parse_macro_input!(attr as VtableArgs);
    let buf = fs::read_to_string(&args.path).unwrap();
    let bindings = syn::parse_file(&buf).unwrap();
    let (methods, wrappers) = select_methods(&bindings, &mut args);
    let struct_def = parse_macro_input!(input as ItemStruct);
    let ident = struct_def.ident;
    let vis = struct_def.vis;
    
    quote! {
        #[repr(C)]
        #vis struct #ident {
            #methods
        }
        // TODO: rewrite everything after this line
        #[repr(C)]
        pub struct VTableHolder {
            vtable: *const #ident,
        }
        #[derive(Copy, Clone)]
        #vis struct Wrapper {
            // TODO move out of here and to separate macro
            this_ptr: *mut VTableHolder,
        }
        impl Wrapper {
            #wrappers
        }
    }.into()
}

fn select_methods(syn_file: &syn::File, args: &mut VtableArgs) -> (TokenStream2, TokenStream2) { // TODO return array of function signatures
    let mut collector = args.collector();
    collector.visit_file(syn_file);
    (collector.generate(), collector.generate_wrappers())
}

#[proc_macro_attribute]
pub fn vtable_wrapper(_attr: TokenStream, input: TokenStream) -> TokenStream {
    // parse attr args
    // check that we can apply attribute macros sequentially
    input
}

#[cfg(test)]
mod tests {
    use proc_macro2::Span;

    use super::*;
    #[test]
    fn test_camel_to_snake() {
        assert_eq!(camel_to_snake("FooBarBaz"), "foo_bar_baz".to_string())
    }
    #[test]
    fn test_prettify_ident() {
        let args = VtableArgs {
            class_name: "CClassName".into(),
            path: "n/a".into(),
        };
        let collector = MethodCollector {
            args: &args,
            methods: Vec::new(),
        };
        let code = quote!{CClassName_MethodName};
        // let ident = Ident::parse(code.into());
        // // let ident = Ident::new("CClassName_MethodName", Span::);
        // let ident = collector.prettify_ident(&ident);
        // assert_eq!(camel_to_snake("FooBarBaz"), "foo_bar_baz".to_string())
    }
    }