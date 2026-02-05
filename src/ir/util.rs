use cranelift::prelude::{FunctionBuilder, InstBuilder as _, Type, Value, types};
use yi_std::Type as YiType;

use crate::{eeprintln, ir::Signedness};

pub fn cast(val: Value, from: Type, from_signedness: Signedness, to: Type, to_signedness: Signedness, builder: &mut FunctionBuilder) -> Value {
	if from == to {
		val
	} else if from.is_int() && to.is_int() {
		if from_signedness.is_signed() {
			builder.ins().sextend(to, val)
		} else {
			builder.ins().uextend(to, val)
		}
	} else if from.is_int() && to.is_float() {
		if from_signedness.is_signed() {
			builder.ins().fcvt_from_sint(to, val)
		} else {
			builder.ins().fcvt_from_uint(to, val)
		}
	} else if from.is_float() && to.is_int() {
		if to_signedness.is_signed() {
			builder.ins().fcvt_to_sint(to, val)
		} else {
			builder.ins().fcvt_to_uint(to, val)
		}
	} else if from.is_float() && to.is_float() {
		let from_type_mass = from.lane_bits() * from.lane_count();
		let to_type_mass = to.lane_bits() * to.lane_count();
		if from_type_mass > to_type_mass {
			builder.ins().fpromote(to, val)
		} else {
			builder.ins().fdemote(to, val)
		}
	} else {
		eeprintln!("Не можна закастувати з {:?} до {:?}", from, to);
		std::process::exit(-1);
	}
}

pub fn type_rank(ty: Type) -> Option<u8> {
	if ty == types::I8  { Some(1) }
	else if ty == types::I16 { Some(2) }
	else if ty == types::I32 { Some(3) }
	else if ty == types::I64 { Some(4) }
	else if ty == types::F32 { Some(5) }
	else if ty == types::F64 { Some(6) }
	else { None } // not a numeric type
}

pub fn highest_type(a: Type, b: Type) -> Option<Type> {
	match (type_rank(a), type_rank(b)) {
		(Some(ra), Some(rb)) => {
			if ra >= rb { Some(a) } else { Some(b) }
		},
		_ => None,
	}
}

pub fn yi_type_from_cranelift(ty: cranelift::prelude::types::Type) -> YiType {
	use cranelift::prelude::types::*;
	match ty {
		I8 | I16 | I32 | I64 => YiType::I64,   // or U64 if unsigned? depends on context
		F32 | F64 => YiType::F64,
		_ => YiType::Void,
	}
}

