#ifndef STACK_H
#define STACK_H

#include <atomic>
#include <mutex>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>

template<typename T>
struct Option {
    uint8_t err = true;
    T data;
};

template<typename T, typename Err>
struct Result {
    Err err;
    T data;
};

struct Ele {
    using Key = std::uint64_t;
    using Val = sockaddr_storage;
    using Data = struct { Key key; Val value; };
    std::atomic<Key> key;
    Val value;
};

bool operator==(const Ele::Val& f, const Ele::Val& s) {
    return memcmp(&f, &s, sizeof(Ele::Val));
}

enum class SearchErr {
    Ok = 0,
    None = 1,
    Self = 2,
};
enum class GetErr {
    Ok = 0,
    None = 1,
    Broken = 2,
};

Result<Ele::Data, GetErr> get(const Ele::Data& , unsigned) {
    Result<Ele::Data, GetErr> res = {GetErr::None, {}};
    return res;
}

constexpr unsigned prefix(const Ele::Key f, const Ele::Key s) {
    auto v = f^s;
    if (v == 0) {
        return 64;
    }
    unsigned i = 0, range = 32;
    do {
        if (v>>range) {
            v >>= range;
        } else {
            i += range;
        }
    } while(range /= 2);
    return i;
}

constexpr unsigned suffix(const Ele::Key f, const Ele::Key s) {
    auto v = f^s;
    if (v == 0) {
        return 64;
    }
    unsigned i = 0, range = 32;
    do {
        if (v<<range) {
            v <<= range;
        } else {
            i += range;
        }
    } while (range /= 2);
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
        Ele* it;
        if ((it = curr.load(consume)) == nullptr
            || it->key.load(acquire) != key) {
            return false;
        }
        Guard lock(m);
        if ((it = curr.load(relax)) == nullptr) {
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
    bool remove(const Ele::Data& ele) {
        Ele* it;
        if ((it = curr.load(consume)) == nullptr
            || it->key.load(acquire) != ele.key) {
            return false;
        }
        Guard lock(m);
        if ((it = curr.load(relax)) == nullptr) {
            return false;
        }
        if (it->key.load(relax) == ele.key
            && it->value == ele.value) {
            if (it == init) {
                curr.store(nullptr, relax);
            } else {
                curr.store(it-1, relax);
            }
            return true;
        }
        return false;
    }
    auto front() {
        Option<Ele::Data> res = {};
        Ele* it;
        if ((it = curr.load(consume)) == nullptr) {
            return res;
        }
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
    Option<Ele::Data> request(const Ele::Data& who, const unsigned what) {
        Option<Ele::Data> res = {};
        if (who.key == id) {
            return res;
        }
        insert(who);
        return res = lines[what].front();
    }
    Result<Ele::Val, SearchErr> search(Ele::Key key) {
        if (id == key) {
            return {SearchErr::Self, {}};
        }
        auto ele = lines[line(key)].front();
        if (ele.err) {
            return {SearchErr::None, {}};
        }
        if (ele.data.key == key) {
            return {SearchErr::Ok, ele.data.value};
        }
        Result<Ele::Data, GetErr> resp;
        do {
            resp = get(ele.data, prefix(resp.data.key, key));
            if (resp.err == GetErr::Ok) {
                insert(ele.data = resp.data);
            } else {
                ele.err = true;
                if (resp.err == GetErr::Broken) {
                    remove(ele.data);
                };
                break;
            }
        } while (64-prefix(ele.data.key, key));
        if (!ele.err) {
            return {SearchErr::Ok, ele.data.value};
        } else {
            return {SearchErr::None, {}};
        }
    }
    bool insert(const Ele::Data& ele) {
        return lines[line(ele.key)].insert(ele.key, ele.value);
    }
    bool remove(const Ele::Key& k) {
        return lines[line(k)].remove(k);
    }
    bool remove(const Ele::Data& ele) {
        return lines[line(ele.key)].remove(ele);
    }
};

#endif // STACK_H
