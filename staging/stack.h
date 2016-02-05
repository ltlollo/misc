#ifndef STACK_H
#define STACK_H

#include <atomic>
#include <mutex>

struct Ele {
    using Key = std::uint64_t;
    using Val = struct { std::uint64_t f, s; };
    using Data = struct { Key key; Val value; };
    using Res = struct { bool err = true; Data data; };
    std::atomic<Key> key;
    Val value;
};

constexpr unsigned suffix(const Ele::Key f, const Ele::Key s) {
    unsigned i = 0;
    Ele::Key v = f^s;
    for (; i < 64; ++i) {
        if (((v>>i)&1)) {
            break;
        }
    }
    return i;
}

constexpr unsigned prefix(const Ele::Key f, const Ele::Key s) {
    unsigned i = 0;
    Ele::Key v = f^s;
    for (; i < 64; ++i) {
        if ((v>>(63-i))&1) {
            break;
        }
    }
    return i;
}

struct Stack {
    using Guard = std::lock_guard<std::mutex>;
    static constexpr auto relax = std::memory_order_relaxed;
    static constexpr auto consume = std::memory_order_consume;
    static constexpr auto acquire = std::memory_order_acquire;

    static constexpr unsigned backup = 3;
    Ele::Key id;
    Ele* init;
    unsigned size;
    std::atomic<Ele*> curr;
    std::mutex m;

    bool compare(const Ele::Key f, const Ele::Key s) {
        return prefix(id, f) > prefix(id, s);
    }
    bool insert(const Ele::Key key, const Ele::Val& value) {
        Ele* it;
        /* this might miss the oportunity of insertion of better match due to
         * current node failing. relying on backups not failing for delayed
         * self-heal
         */
        if ((it = curr.load(consume)) != nullptr
            && it > init + backup
            && compare(it->key.load(acquire), key)) {
            return false;
        }
        Guard lock(m);
        it = curr.load(relax);
        if (it == nullptr) {
            init->key.store(key, relax);
            init->value = value;
            curr.store(init, relax);
            return true;
        }
        if (compare(it->key, key)) {
            return false;
        }
        if (++it == init + size) {
            return false;
        }
        it->key.store(key, relax);
        init->value = value;
        curr.store(it, relax);
        return true;
    }
    bool remove(const Ele::Key& key) {
        /* removal is rare, no need to avoid lock */
        Guard lock(m);
        Ele* it = curr.load(relax);
        if (it == nullptr) {
            return false;
        }
        if (it->key.load(relax) == key) {
            if (it == init) {
                curr.store(nullptr, relax);
            } else {
                curr.store(it-1, relax);
            }
            return true;
        }
        return false;
    }
    Ele::Res front() {
        Ele::Res res = {};
        Ele* it;
        {
            Guard lock(m);
            if ((it = curr.load(relax)) != nullptr) {
                res.data.key = it->key.load(relax);
                res.data.value = it->value;
            }
        }
        res.err = (it == nullptr);
        return res;
    }
};

struct Cache {
    Ele::Key id;
    Ele buf[64/2*(64+1)];
    Stack lines[64];
    Cache(Ele::Key id) : id{id} {
        Ele* it = buf;
        for(unsigned i = 0; i < 64; ++i) {
            auto& line = lines[i];
            line.init = it;
            line.size = size(i);
            line.curr = nullptr;
            line.id = id^(1ull<<(63-i));
            it += size(i);
        }
    }
    static constexpr unsigned size(const unsigned pos) {
        return 64-pos;
    }
    int line(Ele::Key key) {
        auto res = prefix(key, id);
        return (res == 64) ? -1 : res;
    }
    Ele::Res request(const Ele::Data& who, const unsigned what) {
        if (who.key == id) {
            return Ele::Res{};
        }
        lines[line(who.key)].insert(who.key, who.value);
        return lines[what].front();
    }
};

#endif // STACK_H
