#include "SimBase.h"
#include "Port.h"

struct RenamedInst {
    // All src operands that depends on an older inst forms a linked list.
    // This is a node in the list
    struct SrcPendNode {
        RenamedInst *self; // inst of this src operand
        SrcPendNode *next;
    };

    static const int src_num = 3;
    // min time when all src ready; changed when each src becomes ready
    uint64_t src_ready_time;
    // whether each src is depending on some old inst
    SrcPendNode src_pend[src_num];
    // number of pending srcs
    int src_pend_num;

    uint8_t dst_reg;
    // list of src operands of younger insts that depend on me
    SrcPendNode *first_dep;
    SrcPendNode *last_dep;

    Port *dispatch_port;
    int latency_to_use; // latency from dispatch to used by younger inst
    int latency_to_wb; // latency from dispatch to set ROB as commitable

    bool executed; // can commit

    bool end_sim; // special inst to end simulation

    RenamedInst() :
        src_ready_time(0),
        src_pend_num(0),
        dst_reg(0),
        first_dep(NULL),
        last_dep(NULL),
        dispatch_port(NULL),
        latency_to_use(0), 
        latency_to_wb(0),
        executed(false)
    {
        for(int i = 0; i < src_num; i++) {
            src_pend[i].self = this;
            src_pend[i].next = NULL;
        }
    }
};

class SimBW : public SimBase {
public:
    static SimBW* create(int argc, char **argv, std::vector<std::string> &htif_args);
    SimBW(int lat_load_to_use, uint64_t forward_insts, uint64_t sim_insts, const char *file);
    virtual ~SimBW() {}
    virtual void run();

protected:
    static void usage(char *prog);

    void fast_forward();
    void sim();
    void dump_stats();

    // action to take in each cycle
    bool commit(); // return true if simulation should END
    void rename();
    // helper to rename 1 inst
    void rename_inst(const traced_inst_t &trace, RenamedInst *inst);

    // callbacks
    void dispatch(RenamedInst* inst);
    void writeback(RenamedInst* inst); // set ROB as commitable

    // parameters
    static const int rob_size = 64;
    static const int commit_bw = 2;
    static const int rename_bw = 2;
    static const int dispatch_mem_bw = 1;
    static const int dispatch_alu_bw = 2;
    static const int writeback_bw = 3;
    const int latency_load_to_use; // load dispatch to dependent inst dispatch
    static const int latency_load_to_wb = 6;
    static const int latency_non_load_to_use = 1;
    static const int latency_non_load_to_wb = 4;

    // rob
    std::vector<RenamedInst> rob;
    int rob_enq_idx;
    int rob_deq_idx;
    // helpers for ROB
    int inline incr_rob_idx(int idx) {
        return idx == rob_size ? 0 : idx + 1;
    }
    bool inline rob_full() { return incr_rob_idx(rob_enq_idx) == rob_deq_idx; }
    bool inline rob_empty() { return rob_enq_idx == rob_deq_idx; }

    // rename table
    // rt_busy != null means that the src is not ready, and incoming inst
    // should make itself depend on busy. Otherwise, rt_ready gives the time
    // when the source operand is ready.
    static const int reg_num = 64;
    RenamedInst* rt_busy[reg_num];
    uint64_t rt_ready[reg_num];

    // ports
    Port dispatch_mem_port;
    Port dispatch_alu_port;
    Port writeback_port;

    // helper to schedule callback on port
    void inline schedule_dispatch(RenamedInst *inst, uint64_t time) {
        inst->dispatch_port->schedule(time, std::bind(&SimBW::dispatch, this, inst));
    }
    void inline schedule_writeback(RenamedInst *inst, uint64_t time) {
        writeback_port.schedule(time, std::bind(&SimBW::writeback, this, inst));
    }

    // check load or store
    static void inline is_mem(uint32_t inst, bool &load, bool &store) {
        uint32_t opcode = (inst >> 2) & 0x01F;
        load  = opcode == 0 || opcode == 1; // Load/LoadFp
        store = opcode == 8 || opcode == 9; // Store/StoreFp
    }

    // forward/sim params
    const uint64_t forward_inst_num;
    const uint64_t sim_inst_num;

    // performance stats
    uint64_t inst_count;
    uint64_t load_count;
    time_t real_time;

    std::string out_file;
};
