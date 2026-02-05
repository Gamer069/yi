pub mod druk;

pub use druk::STD_FUNCTIONS;

pub struct StdFunc {
	pub yi_name: &'static str,
	pub c_name: &'static str,
	pub params: &'static [Type],
	pub ret: Type,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
	I64,
	U64,

	I8,
	F64,

	Str,
	Bool,
	Void,
	// add types as we go on...
}

pub type YiStr = *const i8;

#[macro_export]
macro_rules! std {
    // Entry point - collect all function definitions
    (
        $(
            fn $c_name:ident ($($param_name:ident: $param_ty:tt),* $(,)?) $(-> $ret_ty:tt)? as $yi_name:literal
            $body:block
        )+
    ) => {
		use crate::StdFunc;
		use crate::Type;

        // Generate the functions
        $(
            std!(@func $c_name ($($param_name: $param_ty),*) $(-> $ret_ty)? $body);
        )+

        // Generate the metadata table
        pub static STD_FUNCTIONS: &[StdFunc] = &[
            $(
                StdFunc {
                    yi_name: $yi_name,
                    c_name: stringify!($c_name),
                    params: &[$(std!(@type $param_ty)),*],
                    ret: std!(@ret $($ret_ty)?),
                },
            )+
        ];
    };

    // Generate function with return type
    (@func $name:ident ($($param_name:ident: $param_ty:tt),*) -> $ret_ty:tt $body:block) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn $name($($param_name: $param_ty),*) -> $ret_ty $body
    };

    // Generate function without return type
    (@func $name:ident ($($param_name:ident: $param_ty:tt),*) $body:block) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn $name($($param_name: $param_ty),*) $body
    };

    // Helper: map Rust types to Type enum
    (@type i64) => { Type::I64 };
    (@type i8) => { Type::I8 };
    (@type bool) => { Type::Bool };
    (@type f64) => { Type::F64 };
    (@type YiStr) => { Type::Str };
    (@type *const i8) => { Type::Str };

    // Helper: handle optional return type
    (@ret) => { Type::Void };  // No return type specified
    (@ret i64) => { Type::I64 };
    (@ret i8) => { Type::I8 };
    (@ret f64) => { Type::F64 };
    (@ret ()) => { Type::Void };
}
