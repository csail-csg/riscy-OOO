
interface BootRomRequest;
    // Boot rom is 8B aligned, index is in terms of 8B
    method Action initReq(Bit#(16) index, Bit#(64) v, Bool last);
endinterface

interface BootRomIndication;
    // signal init done
    method Action initDone;
endinterface
