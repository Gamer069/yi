extern crate yi_std;

use cranelift::{module::{Linkage, Module as _}, prelude::{AbiParam, types}};
use yi_std::{STD_FUNCTIONS, StdFunc, Type as YiType};

use crate::{ir::IRGenerator, keywords::TypeKw};

impl IRGenerator {
	pub fn add_std_functions(&mut self) {
		for func in &*STD_FUNCTIONS {
			self.add_std_function_from_metadata(func);
		}
	}

	fn add_std_function_from_metadata(&mut self, func: &StdFunc) {
		let module = self.module.as_mut().expect("Модуль не ініціалізовано");
		let mut sig = module.make_signature();

		for &param_type in func.params {
			let cr_type = TypeKw::from_yi_std(param_type).cranelift();
			sig.params.push(AbiParam::new(cr_type));
		}

		if func.ret != YiType::Void {
			let ret_cr_type = TypeKw::from_yi_std(func.ret).cranelift();
			sig.returns.push(AbiParam::new(ret_cr_type));
		}

		let id = module
			.declare_function(func.c_name, Linkage::Import, &sig)
			.expect(&format!("Не війшло задекларувати функцію {}", func.yi_name));

		self.functions_to_id.insert(func.c_name.to_string(), id);
		self.functions_to_ret_type.insert(func.c_name.to_string(), func.ret);
	}
}

fn yi_type_to_cranelift(ty: YiType) -> cranelift::prelude::Type {
	match ty {
		YiType::I64 => types::I64,
		YiType::I8 => types::I8,
		YiType::F64 => types::F64,
		YiType::Str => types::I64,
		YiType::Void => types::I32,

		// fallback
		_ => types::I8
	}
}

pub fn translate_std_func_name<'a>(yi_name: &'a str, arg_types: &[YiType]) -> &'a str {
	STD_FUNCTIONS
		.iter()
		.find(|f| f.yi_name == yi_name && f.params == arg_types)
		.map(|f| f.c_name)
		.unwrap_or(yi_name)
}
