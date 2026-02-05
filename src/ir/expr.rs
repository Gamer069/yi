use cranelift::{codegen::ir::BlockArg, module::{DataDescription, Linkage, Module as _}, prelude::{EntityRef, FunctionBuilder, InstBuilder, Value, Variable, types}};
use yi_std::Type as YiType;

use crate::{eeprintln, ir::{IRGenerator, Signedness, stdlib, util}, parser::{BinOp, Expr}};

impl IRGenerator {
	pub fn gen_expr(&mut self, expr: Option<Expr>, mut builder: &mut FunctionBuilder) -> Option<(Value, YiType, Signedness)> {
		match expr {
			Some(Expr::U64(val)) => {
				Some((builder.ins().iconst(types::I64, val as i64), YiType::U64, Signedness::Unsigned))
			},
			Some(Expr::F64(val)) => {
				Some((builder.ins().f64const(val), YiType::F64, Signedness::Signed))
			},
			Some(Expr::I64(val)) => {
				Some((builder.ins().iconst(types::I64, val), YiType::I64, Signedness::Signed))
			},
			Some(Expr::Str(val)) => {
				let data_id = if let Some(data_id_usize) = self.string_literals.get(&val) {
					cranelift::module::DataId::from_u32(*data_id_usize as u32)
				} else {
					// strings are complicated
					let module = self.module.as_mut().unwrap();

					let mut bytes = val.clone().into_bytes();
					bytes.push(0x00);

					let name = format!("str{}", self.string_literal_count);
					self.string_literal_count += 1;
					let data_id = module
						.declare_data(&name, Linkage::Local, false, false)
						.unwrap();

					let mut data_desc = DataDescription::new();
					data_desc.define(bytes.into_boxed_slice());

					let _ = module.define_data(data_id, &data_desc).inspect_err(|err| {
						eeprintln!("Була помилка при визначанні данних для строки: {}", err);
					});
					self.string_literals.insert(val.clone(), data_id.as_u32() as usize);
					data_id
				};

				let module = self.module.as_mut().unwrap();
				let data_ref = module.declare_data_in_func(data_id, &mut builder.func);
				let ptr_val = builder.ins().symbol_value(types::I64, data_ref);

				Some((ptr_val, YiType::Str, Signedness::Signed))
			},
			Some(Expr::Bool(val)) => {
				if val {
					Some((builder.ins().iconst(types::I8, 1), YiType::Bool, Signedness::Signed))
				} else {
					Some((builder.ins().iconst(types::I8, 0), YiType::Bool, Signedness::Signed))
				}
			},
			Some(Expr::Id(id)) => {
				let index = self.cranelift_var_map.get(&id);
				if let Some(index) = index {
					Some((builder.use_var(Variable::new(*index)), self.yi_type_map.get(&id).unwrap().clone(), self.cranelift_signedness_map.get(&id).unwrap().clone()))
				} else {
					eeprintln!("{} не визначено", id);
					std::process::exit(-1);
				}
			},
			Some(Expr::Call(id, args)) => {
				let mut i = 0;
				let val_args: Vec<_> = args.iter().map(|arg| {
					i += 1;
					self.gen_expr(Some(arg.clone()), &mut builder)
						.expect(&format!("Аргумент {} до функції '{}' є порожнім (мовинен бути значенням)", i+1, id))
				}).collect();

				let arg_types: Vec<_> = val_args.iter().map(|(_val, yi_type, _signedness)| *yi_type).collect();

				let actual_name = stdlib::translate_std_func_name(&id, &arg_types);
				let func_id = self.functions_to_id.get(actual_name).copied();
				let ret_type = self.functions_to_ret_type.get(actual_name).copied();

				if let Some(func_id) = func_id && let Some(ret_type) = ret_type {
					let module = self.module.as_mut().unwrap();
					let func_ref = module.declare_func_in_func(func_id, &mut builder.func);
					let val_args_pure: Vec<_> = val_args.iter().map(|(val, ..)| *val).collect();

					let call_inst = builder.ins().call(func_ref, &val_args_pure);
					let res = builder.inst_results(call_inst);

					if res.len() > 0 {
						return Some((res[0], ret_type, Signedness::Signed))
					} else {
						return None
					}
				}

				eeprintln!("Функція `{}` не-задеклерована", actual_name);
				std::process::exit(-1);
			}
			Some(Expr::BinOp(left, op, right)) => {
				let (left_val, _yi_type, left_signedness) = self.gen_expr(Some(*left), builder).expect(&format!("Операція {:#?} не може бути виконана з порожнім лівим операндом.", op));
				let (right_val, _yi_type, right_signedness) = self.gen_expr(Some(*right), builder).expect(&format!("Операція {:#?} не може бути виконана з порожнім правим операндом.", op));

				let left_ty = builder.func.dfg.value_type(left_val);
				let right_ty = builder.func.dfg.value_type(right_val);

				if util::type_rank(left_ty).is_some() && util::type_rank(right_ty).is_some() {
					Some(self.op(left_val, left_signedness, op, right_val, right_signedness, builder))
				} else {
					eeprintln!("Не вийшло порівняти, лівий операнд не такого самого типу як правий операнд");
					std::process::exit(-1);
				}
			},

			Some(Expr::If(cond, then_block, else_block)) => {
				let (cond_val, _cond_yi_ty, _cond_signedness) = self.gen_expr(Some(*cond), builder)
					.expect("Якщо не може бути ніщо");

				let then_block_cr = builder.create_block();
				let else_block_cr = builder.create_block();
				let merge_block_cr = builder.create_block();

				// Branch on condition from current block
				builder.ins().brif(cond_val, then_block_cr, &[], else_block_cr, &[]);

				// === Generate THEN block ===
				builder.switch_to_block(then_block_cr);
				let (then_val, then_terminated) = self.gen_statements_block(then_block, builder)
					.expect("Не вийшло скомпілювати блок інструкцій в `якщо`");

				// Add jump to merge if not terminated
				if !then_terminated {
					if let Some((val, _yi_type, _)) = then_val {
						let ty = builder.func.dfg.value_type(val);
						builder.append_block_param(merge_block_cr, ty);
						builder.ins().jump(merge_block_cr, &[BlockArg::Value(val)]);
					} else {
						builder.ins().jump(merge_block_cr, &[]);
					}
				}
				// Now we can seal then block since we're done with it
				builder.seal_block(then_block_cr);

				// === Generate ELSE block ===
				builder.switch_to_block(else_block_cr);
				let (else_val, else_terminated) = self.gen_statements_block(else_block, builder)
					.expect("Не вийшло скомпілювати блок інструкцій в `інакше`");

				// Add jump to merge if not terminated
				if !else_terminated {
					match (then_val, else_val) {
						(Some((_then_v, ..)), Some((else_v, yi_type, sign))) => {
							// Both produce values - merge already has param from then block
							builder.ins().jump(merge_block_cr, &[BlockArg::Value(else_v)]);

							// Seal blocks and switch to merge
							builder.seal_block(else_block_cr);
							builder.switch_to_block(merge_block_cr);
							builder.seal_block(merge_block_cr);

							return Some((builder.block_params(merge_block_cr)[0], yi_type, sign));
						},
						(None, None) => {
							// Neither produces values
							builder.ins().jump(merge_block_cr, &[]);

							builder.seal_block(else_block_cr);
							builder.switch_to_block(merge_block_cr);
							builder.seal_block(merge_block_cr);

							return None;
						},
						(Some((then_v, _yi_type, sign)), None) if then_terminated => {
							// Only else is active, then terminated
							builder.ins().jump(merge_block_cr, &[]);

							builder.seal_block(else_block_cr);
							builder.switch_to_block(merge_block_cr);
							builder.seal_block(merge_block_cr);

							return None;
						},
						(None, Some((else_v, _yi_type, sign))) if !then_terminated => {
							// Type mismatch
							eeprintln!("Гілки умови `якщо` повинні повертати однакові типи");
							std::process::exit(-1);
						},
						_ => {
							eeprintln!("Гілки умови `якщо` повинні повертати однакові типи");
							std::process::exit(-1);
						}
					}
				} else if then_terminated {
					// Both terminated - no merge needed
					builder.seal_block(else_block_cr);
					builder.seal_block(merge_block_cr);
					return None;
				} else {
					// else terminated, then didn't - merge already has jump from then
					builder.seal_block(else_block_cr);
					builder.switch_to_block(merge_block_cr);
					builder.seal_block(merge_block_cr);

					if let Some((_, yi_type, sign)) = then_val {
						return Some((builder.block_params(merge_block_cr)[0], yi_type, sign));
					} else {
						return None;
					}
				}
			},

			// support both
			Some(Expr::Void) => None,
			None => None,

			Some(Expr::Arg(..)) => {
				eeprintln!("Щось сталось не так - АСД має інвалідний для нього вираз");
				std::process::exit(-1);
			}
		}
	}

	pub fn op(
		&self,
		left: Value,
		left_signedness: Signedness,
		op: BinOp,
		right: Value,
		right_signedness: Signedness,
		builder: &mut FunctionBuilder,
	) -> (Value, YiType, Signedness) {
		let left_ty = builder.func.dfg.value_type(left);
		let right_ty = builder.func.dfg.value_type(right);

		let target_ty = util::highest_type(left_ty, right_ty).unwrap();

		let op_signedness = if left_signedness.is_signed() || right_signedness.is_signed() {
			Signedness::Signed
		} else {
			Signedness::Unsigned
		};

		let left_cast = util::cast(left, left_ty, left_signedness, target_ty, op_signedness, builder);
		let right_cast = util::cast(right, right_ty, right_signedness, target_ty, op_signedness, builder);

		let val = match op {
			BinOp::Add => {
				if target_ty.is_int() {
					builder.ins().iadd(left_cast, right_cast)
				} else {
					builder.ins().fadd(left_cast, right_cast)
				}
			}
			BinOp::Sub => {
				if target_ty.is_int() {
					builder.ins().isub(left_cast, right_cast)
				} else {
					builder.ins().fsub(left_cast, right_cast)
				}
			}
			BinOp::Mul => {
				if target_ty.is_int() {
					builder.ins().imul(left_cast, right_cast)
				} else {
					builder.ins().fmul(left_cast, right_cast)
				}
			}
			BinOp::Div => {
				if target_ty.is_int() {
					if op_signedness.is_signed() {
						builder.ins().sdiv(left_cast, right_cast)
					} else {
						builder.ins().udiv(left_cast, right_cast)
					}
				} else {
					builder.ins().fdiv(left_cast, right_cast)
				}
			}
			BinOp::EqEq => {
				if target_ty.is_int() {
					builder.ins().icmp(
						cranelift::codegen::ir::condcodes::IntCC::Equal,
						left_cast,
						right_cast,
					)
				} else {
					builder.ins().fcmp(
						cranelift::codegen::ir::condcodes::FloatCC::Equal,
						left_cast,
						right_cast,
					)
				}
			}
			BinOp::NotEq => {
				if target_ty.is_int() {
					builder.ins().icmp(
						cranelift::codegen::ir::condcodes::IntCC::NotEqual,
						left_cast,
						right_cast,
					)
				} else {
					builder.ins().fcmp(
						cranelift::codegen::ir::condcodes::FloatCC::NotEqual,
						left_cast,
						right_cast,
					)
				}
			}
		};

		let (result_yi_type, result_signedness) = match op {
			BinOp::EqEq | BinOp::NotEq => (YiType::Bool, Signedness::Unsigned),
			_ => {
				let signedness = if target_ty.is_int() {
					op_signedness
				} else {
					Signedness::Unsigned
				};
				(util::yi_type_from_cranelift(target_ty), signedness)
			}
		};

		(val, result_yi_type, result_signedness)
	}


}
