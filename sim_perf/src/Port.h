#pragma once

#include <stdint.h>
#include <utility>
#include <vector>
#include <queue>
#include <functional>
#include "SimBase.h"

// simulate a port with limited bandwidth to perform callback
// events

class Port {
public:
    Port(int bw) : bandwidth(bw) {}
    ~Port() {}

    // schedule new event at future
    typedef std::function<void()> Callback;
    void schedule(uint64_t time, const Callback &call) {
        event_queue.push(Event(time, call));
    }

    // process events in queue that are ready at current time
    void process() {
        uint64_t cur_time = SimBase::cur_cycle();
        for(int i = 0; i < bandwidth && !event_queue.empty(); i++) {
            // When processing a callback, we first pop it out, because the
            // callback may (though unlikely) insert a new event with a smaller
            // timestamp to this event queue
            Event event = event_queue.top();
            if(event.first <= cur_time) {
                event_queue.pop();
                event.second(); // callback
            } else {
                break; // no ready event
            }
        }
    }

protected:
    // event queue
    typedef std::pair<uint64_t, Callback> Event; // time + callback
    struct CompareEvent {
        bool operator()(const Event &a, const Event &b) {
            // a has less priority than b when a's time is larger
            return a.first > b.first;
        }
    };
    typedef std::priority_queue<Event, std::vector<Event>, CompareEvent> EventQueue;
    EventQueue event_queue;

    const int bandwidth; // limited processing bandwidth
};
