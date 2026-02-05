use cranelift::{codegen::ir::BlockArg, module::{DataDescription, Linkage, Module as _}, prelude::{EntityRef, FunctionBuilder, InstBuilder, Value, Variable, types}};
use yi_std::Type as YiType;

use crate::{eeprintln, ir::{IRGenerator, Signedness, stdlib, util}, parser::{BinOp, Expr, IfExpr, UnaryOp}};

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
				let func_id = self.functions_to_id.get(&actual_name).copied();
				let ret_type = self.functions_to_ret_type.get(&actual_name).copied();

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

			Some(Expr::If(IfExpr { cond, then: then_block, r#else: else_block }, elifs)) => {
				let (cond_val, _cond_yi_ty, _cond_signedness) = self.gen_expr(Some(*cond), builder)
					.expect("Якщо не може бути ніщо");

				let then_block_cr = builder.create_block();
				let else_or_elif_block_cr = builder.create_block();
				let merge_block_cr = builder.create_block();

				// Branch on condition from current block
				builder.ins().brif(cond_val, then_block_cr, &[], else_or_elif_block_cr, &[]);

				// === Generate THEN block ===
				builder.switch_to_block(then_block_cr);
				builder.seal_block(then_block_cr);

				let (then_val, then_terminated) = self.gen_statements_block(then_block, builder)
					.expect("Не вийшло скомпілювати блок інструкцій в `якщо`");

				// Track if we added a block parameter
				let mut merge_has_param = false;

				// Add jump to merge if not terminated
				if !then_terminated {
					if let Some((val, _yi_type, _)) = then_val {
						let ty = builder.func.dfg.value_type(val);
						builder.append_block_param(merge_block_cr, ty);
						merge_has_param = true;
						builder.ins().jump(merge_block_cr, &[BlockArg::Value(val)]);
					} else {
						builder.ins().jump(merge_block_cr, &[]);
					}
				}

				// === Generate ELSE-IF chain or final ELSE block ===
				builder.switch_to_block(else_or_elif_block_cr);
				builder.seal_block(else_or_elif_block_cr);

				let (else_val, else_terminated) = if !elifs.is_empty() {
					// Recursively build nested if-else from elif chain
					let nested_if_expr = Expr::If(
						IfExpr {
							cond: elifs[0].cond.clone(),
							then: elifs[0].then.clone(),
							r#else: if elifs.len() > 1 {
								// More elifs remain - leave else empty for recursion
								vec![]
							} else {
								// Last elif - use the final else block
								else_block.clone()
							},
						},
						if elifs.len() > 1 {
							// Pass remaining elifs to next recursion level
							elifs[1..].to_vec()
						} else {
							vec![]
						}
					);

					// Generate the nested elif chain
					let result = self.gen_expr(Some(nested_if_expr), builder);
					// Check termination: if the current block has a terminator instruction
					let current_block = builder.current_block().unwrap();
					let terminated = builder.func.layout.last_inst(current_block)
						.map(|inst| builder.func.dfg.insts[inst].opcode().is_terminator())
						.unwrap_or(false);
					(result, terminated)
				} else {
					// No elifs - just generate the final else block
					self.gen_statements_block(else_block, builder)
						.expect("Не вийшло скомпілювати блок інструкцій в `інакше`")
				};

				// Add jump to merge from else/elif if not terminated
				if !else_terminated {
					match (merge_has_param, else_val) {
						(true, Some((else_v, _, _))) => {
							builder.ins().jump(merge_block_cr, &[BlockArg::Value(else_v)]);
						},
						(false, None) => {
							builder.ins().jump(merge_block_cr, &[]);
						},
						(true, None) => {
							eeprintln!("Гілки умови `якщо` повинні повертати однакові типи");
							std::process::exit(-1);
						},
						(false, Some(_)) => {
							eeprintln!("Гілки умови `якщо` повинні повертати однакові типи");
							std::process::exit(-1);
						}
					}
				}

				// === Finalize merge block ===
				builder.switch_to_block(merge_block_cr);
				builder.seal_block(merge_block_cr);

				// Return appropriate value based on what was produced
				if then_terminated && else_terminated {
					// Both paths terminated
					return None;
				}

				if merge_has_param {
					let params = builder.block_params(merge_block_cr);
					if params.len() > 0 {
						// Return with type from whichever branch has it
						if let Some((_, yi_type, sign)) = then_val {
							return Some((params[0], yi_type, sign));
						} else if let Some((_, yi_type, sign)) = else_val {
							return Some((params[0], yi_type, sign));
						}
					}
				}

				// No value produced
				None
			},

			Some(Expr::UnaryOp(expr, UnaryOp::Not)) => {
				let (val, yi_type, _signedness) = self.gen_expr(Some(*expr), builder)
					.expect("Унарний оператор «не» не може бути порожнім");

				if yi_type != YiType::Bool {
					eeprintln!("Унарний оператор «не» не може бути порожнім");
					std::process::exit(-1);
				}

				let one = builder.ins().iconst(types::I8, 1);
				let new_val = builder.ins().bxor(val, one);
				Some((new_val, YiType::Bool, Signedness::Unsigned))
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
			BinOp::Mod => {
				if target_ty.is_int() {
					if op_signedness.is_signed() {
						builder.ins().srem(left_cast, right_cast)
					} else {
						builder.ins().urem(left_cast, right_cast)
					}
				} else {
					eeprintln!("Неможливо визначити остачу з не-числовим значенням.");
					std::process::exit(-1);
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
