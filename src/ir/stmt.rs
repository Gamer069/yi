use cranelift::prelude::{EntityRef, FunctionBuilder, InstBuilder, Value, Variable};
use yi_std::Type as YiType;

use crate::{eeprintln, ir::{IRGenerator, Signedness}, keywords::TypeKw, parser::{Expr, Statements}};

impl IRGenerator {
	pub fn gen_let(&mut self, var_id: String, ty: Option<TypeKw>, expr: Option<Box<Expr>>, mut func_builder: &mut FunctionBuilder<'_>) {
		let expr = expr.map(|expr| *expr);
		let expr = self.gen_expr(expr.clone(), &mut func_builder);

		if let Some((val, yi_type, signedness)) = expr {
			let cr_ty = ty.as_ref().map(|ty| ty.cranelift()).unwrap_or(func_builder.func.dfg.value_type(val));

			let new_var = func_builder.declare_var(cr_ty);

			func_builder.def_var(new_var, val);

			self.cranelift_var_map.insert(var_id.clone(), new_var.as_u32() as usize);

			let final_signedness = ty.as_ref().map(|t| t.signedness()).unwrap_or(signedness);

			self.cranelift_signedness_map.insert(var_id.clone(), final_signedness);

			let final_yi_type = ty.as_ref().map(|t| t.std()).unwrap_or(yi_type);

			self.yi_type_map.insert(var_id, final_yi_type);
		}
	}

	pub fn gen_assign(&mut self, var_id: String, expr: Expr, mut func_builder: &mut FunctionBuilder<'_>) {
		let var = self.cranelift_var_map.get(&var_id).copied();

		let (val, _yi_type, _signedness) = self.gen_expr(Some(expr.clone()), &mut func_builder).expect("Немає значення для призначення змінної");

		if let Some(var) = var {
			let variable = Variable::new(var);
			func_builder.def_var(variable, val);
		} else {
			eeprintln!("Змінна {} не задеклерована", var_id);
			std::process::exit(-1);
		}
	}

	pub fn gen_statements_block(
		&mut self,
		stmts: Vec<Statements>,
		builder: &mut FunctionBuilder
	) -> Result<(Option<(Value, YiType, Signedness)>, bool), String> {
		let mut last_value = None;
		let mut terminated = false;

		for stmt in stmts {
			match stmt {
				Statements::Let(var_id, ty, expr) => {
					self.gen_let(var_id, ty, expr, builder);
				},
				Statements::Assign(var_id, expr) => {
					self.gen_assign(var_id, *expr, builder);
				},
				Statements::Expr(expr) => {
					last_value = self.gen_expr(Some(*expr), builder);
				},
				Statements::Return(expr) => {
					match self.gen_expr(Some(*expr), builder) {
						Some((val, _yi_type, _)) => {
							builder.ins().return_(&[val]);
						},
						None => {
							builder.ins().return_(&[]);
						}
					}
					terminated = true;
					break; // Stop processing statements
				},
				Statements::Func(_, _, _, _) => {
					eeprintln!("Вкладені функції ще не підтримуються");
					std::process::exit(-1);
				},
			}
		}

		Ok((last_value, terminated))
	}


}
