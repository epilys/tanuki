// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright Manos Pitsidianakis

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]

use arbitrary_int::traits::Integer;

macro_rules! bitmask {
    ( $off: expr, $len: expr ) => {
        ((1 << $len) - 1) << $off
    };
}

macro_rules! setbits {
    ( $var: expr, $off: expr, $len: expr, $val: expr ) => {
        ($var & !bitmask!($off, $len)) | (($val << $off) & bitmask!($off, $len))
    };
}

pub struct STRBr_A64 {
    pub _const_0: arbitrary_int::u2,
    pub _const_1: arbitrary_int::u6,
    pub _const_2: arbitrary_int::u2,
    pub _const_3: arbitrary_int::u1,
    pub rm: arbitrary_int::u5,
    pub _const_4: arbitrary_int::u3,
    pub shft: arbitrary_int::u1,
    pub _const_5: arbitrary_int::u2,
    pub rn: arbitrary_int::u5,
    pub rt: arbitrary_int::u5,
}

impl STRBr_A64 {
    pub const fn new() -> Self {
        Self {
            _const_0: unsafe { <arbitrary_int::u2>::new_unchecked(0b00) },
            _const_1: unsafe { <arbitrary_int::u6>::new_unchecked(0b111000) },
            _const_2: unsafe { <arbitrary_int::u2>::new_unchecked(0b00) },
            _const_3: unsafe { <arbitrary_int::u1>::new_unchecked(0b1) },
            rm: unsafe { <arbitrary_int::u5>::new_unchecked(0) },
            _const_4: unsafe { <arbitrary_int::u3>::new_unchecked(0b011) },
            shft: unsafe { <arbitrary_int::u1>::new_unchecked(0) },
            _const_5: unsafe { <arbitrary_int::u2>::new_unchecked(0b10) },
            rn: unsafe { <arbitrary_int::u5>::new_unchecked(0) },
            rt: unsafe { <arbitrary_int::u5>::new_unchecked(0) },
        }
    }
    pub fn encode(&self) -> u32 {
        let mut accum = 0_u32;
        {
            let val = self._const_0.as_u32();
            accum = setbits!(accum, 30u8, 2usize, val);
        };
        {
            let val = self._const_1.as_u32();
            accum = setbits!(accum, 24u8, 6usize, val);
        };
        {
            let val = self._const_2.as_u32();
            accum = setbits!(accum, 22u8, 2usize, val);
        };
        {
            let val = self._const_3.as_u32();
            accum = setbits!(accum, 21u8, 1usize, val);
        };
        {
            let val = self.rm.as_u32();
            accum = setbits!(accum, 16u8, 5u8, val);
        };
        {
            let val = self._const_4.as_u32();
            accum = setbits!(accum, 13u8, 3usize, val);
        };
        {
            let val = self.shft.as_u32();
            accum = setbits!(accum, 12u8, 1u8, val);
        };
        {
            let val = self._const_5.as_u32();
            accum = setbits!(accum, 10u8, 2usize, val);
        };
        {
            let val = self.rn.as_u32();
            accum = setbits!(accum, 5u8, 5u8, val);
        };
        {
            let val = self.rt.as_u32();
            accum = setbits!(accum, 0u8, 5u8, val);
        }
        accum
    }
}

impl<'a> arbitrary::Arbitrary<'a> for STRBr_A64 {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let mut ret = Self::new();
        u.arbitrary_loop(None, None, |u| {
            let rm: arbitrary_int::u5 = u.arbitrary()?;
            let shft: arbitrary_int::u1 = u.arbitrary()?;
            let rn: arbitrary_int::u5 = u.arbitrary()?;
            let rt: arbitrary_int::u5 = u.arbitrary()?;
            if !(rn.as_u32() != 31.as_u32()
                && rn.as_u32() != rt.as_u32()
                && rm.as_u32() != rt.as_u32()
                && rn.as_u32() != rm.as_u32())
            {
                return Ok(std::ops::ControlFlow::Continue(()));
            }
            ret.rm = rm;
            ret.shft = shft;
            ret.rn = rn;
            ret.rt = rt;
            Ok(std::ops::ControlFlow::Break(()))
        })?;
        Ok(ret)
    }
}

#[test]
fn test_disas() {
    use arbitrary::{Arbitrary, Unstructured};

    let mut u = Unstructured::new(&[1, 2, 3]);

    let bic = STRBr_A64::arbitrary(&mut u).unwrap();
    assert_eq!(bic.encode(), 0x0038206800);

    // for ins in bad64::disasm(
    //     &unsafe { std::mem::transmute::<[u32; 1], [u8; 4]>(instructions) },
    //     0,
    // ) {
    //     eprintln!("ins: {ins:?}");
    // }
}
