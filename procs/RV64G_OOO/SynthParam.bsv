
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
typedef SupSize AluExeNum;

// Phy RFile
// write: Alu < FpuMulDiv < Mem
// read: Alu, FpuMulDiv, Mem
typedef TAdd#(2, AluExeNum) RFileWrPortNum;
typedef TAdd#(2, AluExeNum) RFileRdPortNum;

// sb lazy lookup num: same as RFile read, becaues all pipelines recv bypass
typedef RFileRdPortNum SbLazyLookupPortNum;

// ports for writing conservative elements
// i.e. write rf & set conservative sb & wake up rs in conservative pipeline (i.e. not recv bypass)
// ordering: LrScAmo < Alu < FpuMulDiv < Ld
typedef RFileWrPortNum WrConsPortNum;
function Integer aluWrConsPort(Integer i) = i;
Integer fpuMulDivWrConsPort = valueof(AluExeNum);
Integer memWrConsPort = 1 + valueof(AluExeNum);

// ports for writing aggressive elements
// i.e. set aggressive sb & wake up rs in aggressive pipeline (i.e. recv bypass)
// Mem pipeline has two places: mem resp and forwarding. Since the rule
// containing forwarding calls lsq.issue, and the rule containing mem resp
// calls lsq.wakeupLd, we order forward before mem.
// Ordering: Alu < FpuMulDiv < Forward < Mem
typedef TAdd#(3, AluExeNum) WrAggrPortNum;
function Integer aluWrAggrPort(Integer i) = i;
Integer fpuMulDivWrAggrPort = valueof(AluExeNum);
Integer forwardWrAggrPort = 1 + valueof(AluExeNum);
Integer memWrAggrPort = 2 + valueof(AluExeNum);

// ports for read rf & lazy lookup in sb, ordering doesn't matter
function Integer aluRdPort(Integer i) = i;
Integer fpuMulDivRdPort = valueof(AluExeNum);
Integer memRdPort = 1 + valueof(AluExeNum);

// ports for correct spec, ordering doesn't matter
typedef TAdd#(2, AluExeNum) CorrectSpecPortNum;
function Integer finishAluCorrectSpecPort(Integer i) = i;
Integer deqLSQCorrectSpecPort = valueof(AluExeNum);
Integer finishMemCorrectSpecPort = 1 + valueof(AluExeNum);

// ports for manual conflict with wrong spec, ordering doesn't matter
typedef 2 ConflictWrongSpecPortNum;
Integer finishFpuMulDivConflictWrongSpecPort = 0;
Integer lsqFirstConflictWrongSpecPort = 1;
