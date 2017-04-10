typedef enum {
    TrapCommit, // trapped inst still need to commit renaming
    NonTrapCommit // non-trapped inst cannot commit renaming
} RenameError deriving(Bits, Eq, FShow);

interface RenameDebugIndication;
    method Action renameErr(
        Bit#(8) core, RenameError err, Bit#(64) pc, Bit#(5) iType,
        Bool isException, Bool isInterrupt, Bit#(4) trapVal,
        Bit#(16) specBits, Bool specTagValid, Bit#(4) specTag
    );
endinterface
