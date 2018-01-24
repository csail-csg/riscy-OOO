#!/usr/bin/python3

# Max number of registers to concat
n = 24

# Top of the generated BSV file
top_of_file = """// This file was created by gen_ConcatReg.py. If you want to modify this file,
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
// These don't require asReg when used."""

# Function to print concatRegN
def print_concatRegN(n):
    print("function Reg#(Bit#(n)) concatReg" + str(n) + "(")
    for i in range(1,n+1):
        print("      Reg#(Bit#(n" + str(i) + ")) r" + str(i) + ("," if i != n else ""))
    print("    ) provisos (")
    # print("      Add#( TAdd#( TAdd#( n1 ,n2) ,n3) ,n4,n)\n");
    print("      Add#(", end="")
    for i in range(1,n-2+1):
        print("TAdd#(", end="")
    for i in range(1,n+1):
        print("n" + str(i) + (")" if (i != 1 and i != n) else "") + ",", end="")
    print("n)")
    print("    );")
    print("  return concatReg(", end="")
    for i in range(1,n+1):
        print("asReg(r" + str(i) + ")" + ("," if i != n else ""), end="")
    print(");")
    print("endfunction")


if __name__ == "__main__":
    print(top_of_file)
    for i in range(2,n+1):
        print_concatRegN(i)
        print("")
