use cranelift::prelude::{types, FunctionBuilder, InstBuilder as _};

use crate::{keywords, parser::Statements};

use super::IRGenerator;

impl IRGenerator {
	pub fn gen_func(&mut self, id: String, args: Vec<(String, keywords::TypeKw)>, ret_ty: keywords::TypeKw, stmts: Vec<Statements>) {
		let mut builder_ctx = std::mem::take(&mut self.builder_ctx);

		let mut func = self.functions.remove(&id).unwrap();

		let mut func_builder = FunctionBuilder::new(&mut func, &mut builder_ctx);

		let entry_block = func_builder.create_block();
		func_builder.append_block_params_for_function_params(entry_block);
		func_builder.switch_to_block(entry_block);

		for (i, (arg_name, arg_ty)) in args.iter().enumerate() {
			// TODO: debug info
			let cr_ty = arg_ty.cranelift();
			let var = func_builder.declare_var(cr_ty);
			let val = func_builder.block_params(entry_block)[i];
			func_builder.def_var(var, val);
			self.cranelift_var_map.insert(arg_name.clone(), var.as_u32() as usize);
			self.cranelift_signedness_map.insert(arg_name.clone(), arg_ty.signedness());
			self.yi_type_map.insert(arg_name.clone(), arg_ty.std());
		}

		let mut terminated = false;

		for stmt in stmts {
			if terminated {
				continue;
			}

			if let Statements::Let(var_id, ty, expr) = stmt {
				self.gen_let(var_id, ty, expr, &mut func_builder);
			} else if let Statements::Assign(var_id, expr) = stmt {
				self.gen_assign(var_id, *expr, &mut func_builder);
			} else if let Statements::Expr(expr) = stmt {
				self.gen_expr(Some(*expr), &mut func_builder);
			} else if let Statements::Return(expr) = stmt {
				match self.gen_expr(Some(*expr), &mut func_builder) {
					Some((val, _yi_type, _signedness)) => {
						func_builder.ins().return_(&[val]);
					},
					None => {
						func_builder.ins().return_(&[]);
					}
				}
				terminated = true;
			}
		}

		if !terminated {
			if id == "старт" && ret_ty == keywords::TypeKw::Void {
				let zero = func_builder.ins().iconst(types::I32, 0);
				func_builder.ins().return_(&[zero]);
			} else if ret_ty == keywords::TypeKw::Void {
				func_builder.ins().return_(&[]);
			}
		}

		func_builder.seal_block(entry_block);

		if self.ir {
			println!("{}", func_builder.func.display());
			println!();
		}

		func_builder.finalize();

		self.functions.insert(id.clone(), func);

		self.builder_ctx = builder_ctx;
	}

}
