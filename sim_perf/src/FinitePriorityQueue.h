#pragma once

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <functional>
#include <queue>
#include "spike/common.h"

// The number of scheduled events in the system has a upper bound, so we can
// allocate fixed amount of memory, avoiding dynamic memory allocation.

template<class T, class Compare = std::less<T> >
class FinitePriorityQueue {
public:
    FinitePriorityQueue(size_t capacity, Compare compare = Compare()) :
        queue(compare, Container(capacity)) 
    {
    }

    ~FinitePriorityQueue() {}

    inline void push(const T& x) { queue.push(x); }

    inline void pop() { queue.pop(); }

    inline const T& top() const { return queue.top(); }

    inline bool empty() const { return queue.empty(); }

private:
    class Container {
    public:
        typedef T value_type;
        typedef T& reference;
        typedef const T& const_reference;
        typedef T* iterator;
        typedef const T* const_iterator;
        typedef std::ptrdiff_t difference_type;
        typedef size_t size_type;

        Container() : data(NULL), max_size(0), cur_size(0) {}

        Container(size_t capacity) : data(NULL), max_size(capacity), cur_size(0) {
            data = new T[max_size];
        }

        Container(const Container& x) : data(NULL), max_size(x.max_size), cur_size(x.cur_size) {
            data = new T[max_size];
            for(size_t i = 0; i < cur_size; i++) {
                data[i] = x.data[i];
            }
        }

        Container& operator = (const Container& x) {
            fprintf(stderr, "[BoundPriorityQueue::Container:=] not implemented\n");
            exit(1);
        }

        ~Container() {
            delete []data;
            data = NULL;
            cur_size = 0;
        }

        bool empty() const { return cur_size == 0; }

        size_t size() const { return cur_size; }

        T& front() { return data[0]; }
        const T& front() const { return data[0]; }

        void push_back(const T& x) {
            if(unlikely(cur_size >= max_size)) {
                fprintf(stderr, "[BoundPriorityQueue::Container::push_back] overflow\n");
                exit(1);
            }
            data[cur_size] = x;
            cur_size++;
        }

        void pop_back() {
            if(unlikely(cur_size == 0)) {
                fprintf(stderr, "[BoundPriorityQueue::Container::pop_back] underflow\n");
                exit(1);
            }
            cur_size--;
        }

        T* begin() { return data; }
        const T* begin() const { return data; }
        const T* cbegin() const { return data; }

        T* end() { return data + cur_size; }
        const T* end() const { return data + cur_size; }
        const T* cend() const { return data + cur_size; }

    private:
        T* data;
        const size_t max_size;
        size_t cur_size; // current size
    };

    typedef std::priority_queue<T, Container, Compare> PriorityQueue;
    PriorityQueue queue;
};
