// This file was created by gen_ConcatReg.py. If you want to modify this file,
// please modify gen_ConcatReg.py instead. If you need a wider concatReg
// function, change the value of n in gen_ConcatReg.py and run it again.

// The Bluespec provided BuildVector.bsv provides another example of
// constructing a function that takes a variable number of arguments

// Typeclass for creating _concatReg with a variable number of arguments.
typeclass ConcatReg#(type r, numeric type n1, numeric type n2)
  dependencies ((r,n1) determines n2, (r,n2) determines n1);
  // dependencies (r determines (n1,n2));
  function r _concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2);
endtypeclass
// Base case
instance ConcatReg#(Reg#(Bit#(n3)), n1, n2) provisos (Add#(n1, n2, n3));
  function Reg#(Bit#(TAdd#(n1,n2))) _concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2);
    return (interface Reg;
        method Bit#(TAdd#(n1,n2)) _read = {r1._read, r2._read};
        method Action _write(Bit#(TAdd#(n1,n2)) x);
          r1._write(truncateLSB(x));
          r2._write(truncate(x));
        endmethod
      endinterface);
  endfunction
endinstance
// Recursion
instance ConcatReg#(function r f(Reg#(Bit#(n3)) r3), n1, n2) provisos (ConcatReg#(r, TAdd#(n1, n2), n3));
  function function r f(Reg#(Bit#(n3)) r3) _concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2);
    return _concatReg(interface Reg;
        method Bit#(TAdd#(n1,n2)) _read = {r1._read, r2._read};
        method Action _write(Bit#(TAdd#(n1,n2)) x);
          r1._write(truncateLSB(x));
          r2._write(truncate(x));
        endmethod
      endinterface);
  endfunction
endinstance

// Wrapper function for users. This can take a variable number of arguments.
// You will need to use asReg() for the third argument and beyond.
function r concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2) provisos(ConcatReg#(r, n1, n2));
  return _concatReg(asReg(r1),asReg(r2));
endfunction

// Automatically generated macros with a set number of registers.
// These don't require asReg when used.
function Reg#(Bit#(n)) concatReg2(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2
    ) provisos (
      Add#(n1,n2,n)
    );
  return concatReg(asReg(r1),asReg(r2));
endfunction

function Reg#(Bit#(n)) concatReg3(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3
    ) provisos (
      Add#(TAdd#(n1,n2),n3,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3));
endfunction

function Reg#(Bit#(n)) concatReg4(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4
    ) provisos (
      Add#(TAdd#(TAdd#(n1,n2),n3),n4,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4));
endfunction

function Reg#(Bit#(n)) concatReg5(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5));
endfunction

function Reg#(Bit#(n)) concatReg6(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6));
endfunction

function Reg#(Bit#(n)) concatReg7(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7));
endfunction

function Reg#(Bit#(n)) concatReg8(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8));
endfunction

function Reg#(Bit#(n)) concatReg9(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9));
endfunction

function Reg#(Bit#(n)) concatReg10(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10));
endfunction

function Reg#(Bit#(n)) concatReg11(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11));
endfunction

function Reg#(Bit#(n)) concatReg12(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12));
endfunction

function Reg#(Bit#(n)) concatReg13(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13));
endfunction

function Reg#(Bit#(n)) concatReg14(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14));
endfunction

