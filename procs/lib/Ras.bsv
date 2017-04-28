
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

`include "ProcConfig.bsv"
import Types::*;
import ProcTypes::*;
import RegFile::*;
import Vector::*;
import Ehr::*;

interface ReturnAddrStack;
    interface Vector#(`sizeSup,function Action pushAddress(Addr a)) pushAddress;
    interface Vector#(`sizeSup,Action) popAddress;
    interface Vector#(`sizeSup,Addr) firstAddress;
endinterface

// Local RAS Typedefs SHOULD BE A POWER OF TWO.
typedef 8 RasEntries;
typedef Bit#(TLog#(RasEntries)) RasIndex;

//(* synthesize *)
module mkRas( ReturnAddrStack );
    Vector#( RasEntries, Ehr#(`sizeSup, Addr) ) stack <- replicateM(mkEhr(0));
    Vector#(`sizeSup, function Action pushAddress(Addr a)) _pushAddress = newVector;
    Vector#(`sizeSup, Action) _popAddress = newVector;
    Vector#(`sizeSup, Addr) _firstAddress = newVector;
    Vector#(`sizeSup, Ehr#(2,Maybe#(Addr))) willPush <- replicateM(mkEhr(tagged Invalid));
    Vector#(`sizeSup, Ehr#(2,Bool)) willPop <- replicateM(mkEhr(False));

    Vector#(`sizeSup,Integer) indexes = genVector;
    // head points past valid data
    // to gracefully overflow, head is allowed to overflow to 0 and overwrite the oldest data
    Ehr#(`sizeSup,RasIndex) head <- mkEhr(0);

    RasIndex max_head = fromInteger(valueOf(RasEntries)-1);

    function Action pushAddressF(Integer i, Addr a);
	return(action
	       willPush[i][0] <= tagged Valid a;
	       endaction);
    endfunction

    function Action popAddressF(Integer i);
	return(action
	   willPop[i][0] <= True;
	   endaction);
    endfunction

    function Addr firstAddressF(Integer i);
	   return (stack[head[0]][0]);
    endfunction

    rule canonicalize;
	RasIndex newhead = head[0];
	for(Integer i=0; i < `sizeSup ; i = i+1) begin
	    case (willPush[i][1]) matches
    		tagged Invalid : noAction;
    		tagged Valid .e :
    		    begin
    			newhead = (newhead == max_head) ? 0 : newhead + 1;
    			stack[newhead][i] <= e;
    			willPush[i][1] <= tagged Invalid;
    		    end
    	    endcase
	    if (willPop[i][1]) begin
    	        newhead = (newhead == 0) ? max_head : newhead - 1;
    		willPop[i][1] <= False;
    	    end
	end
        head[0] <= newhead;
    endrule

    _pushAddress = map(pushAddressF,indexes);
    _popAddress = map(popAddressF,indexes);
    _firstAddress = map(firstAddressF,indexes);
    interface Vector pushAddress = _pushAddress;
    interface Vector popAddress = _popAddress;
    interface Vector firstAddress = _firstAddress;
endmodule
