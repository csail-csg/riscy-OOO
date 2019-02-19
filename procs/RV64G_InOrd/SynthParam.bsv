
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import DefaultValue::*;
import Types::*;
import ProcTypes::*;
import HasSpecBits::*;
import ReservationStationEhr::*;

// ALU exe num = superscalar size
// FPU exe num = superscalar size / 2
typedef SupSize AluExeNum;
typedef TDiv#(SupSize, 2) FpuMulDivExeNum;

// Phy RFile
// write: Alu < FpuMulDiv < Mem
// read: Alu, FpuMulDiv, Mem
typedef TAdd#(1, TAdd#(FpuMulDivExeNum, AluExeNum)) RFileWrPortNum;
typedef TAdd#(1, TAdd#(FpuMulDivExeNum, AluExeNum)) RFileRdPortNum;

// ports for writing rf and sb
// ordering: Alu < FpuMulDiv < Mem
function Integer aluWrPort(Integer i) = i;
function Integer fpuMulDivWrPort(Integer i) = valueof(AluExeNum) + i;
Integer memWrPort = valueof(FpuMulDivExeNum) + valueof(AluExeNum);

// ports for reading rf and sb, ordering doesn't matter
function Integer aluRdPort(Integer i) = i;
function Integer fpuMulDivRdPort(Integer i) = valueof(AluExeNum) + i;
Integer memRdPort = valueof(FpuMulDivExeNum) + valueof(AluExeNum);

// ports for correct spec, ordering doesn't matter
typedef AluExeNum CorrectSpecPortNum;
function Integer finishAluCorrectSpecPort(Integer i) = i;

// ports for manual conflict with wrong spec, ordering doesn't matter
typedef FpuMulDivExeNum ConflictWrongSpecPortNum;
function Integer finishFpuMulDivConflictWrongSpecPort(Integer i) = i;
