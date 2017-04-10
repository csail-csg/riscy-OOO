import Vector::*;
import Ehr::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import SetAssocTlb::*;

// L2 4KB page set assoc TLB

typedef 4 L2TlbWayNum;
typedef 9 LogL2TlbSetNum;
typedef Bit#(TLog#(L2TlbWayNum)) L2TlbWay;

// use true LRU: index 0 --MRU, index way-1 -- LRU
typedef Vector#(L2TlbWayNum, L2TlbWay) L2TlbRepInfo;
L2TlbRepInfo l2TlbRepInfoInitVal = genWith(fromInteger);

function L2TlbWay getL2TlbRepWay(L2TlbRepInfo repInfo, Vector#(L2TlbWayNum, Bool) invalid);
    // if there are invalid entries, LRU entry must be invalid
    return repInfo[valueof(L2TlbWayNum) - 1];
endfunction

function L2TlbRepInfo updateL2TlbRepInfo(L2TlbRepInfo repInfo, L2TlbWay way);
    L2TlbRepInfo newInfo = repInfo;
    newInfo[0] = way; // MRU
    Bool findWay = False;
    for(Integer i = 1; i < valueof(L2TlbWayNum); i = i+1) begin
        findWay = findWay || (repInfo[i-1] == way);
        if(!findWay) begin
            newInfo[i] = repInfo[i-1];
        end
    end
    return newInfo;
endfunction

typedef SetAssocTlbResp#(L2TlbWay) L2SetAssocTlbResp;

typedef SetAssocTlb#(L2TlbWayNum, LogL2TlbSetNum, L2TlbRepInfo) L2SetAssocTlb;

// cannot synthesize becaue a method guard depends on input
module mkL2SetAssocTlb(L2SetAssocTlb);
    let m <- mkSetAssocTlb(l2TlbRepInfoInitVal, getL2TlbRepWay, updateL2TlbRepInfo);
    return m;
endmodule
