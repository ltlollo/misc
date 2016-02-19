#pragma once

#include <cstddef>
#include <stdlib.h>
#include <string.h>

namespace mtl {

template<typename T> concept bool Reloc   = requires(T d) {
    { reloc(d)      } -> void
};
template<typename T> concept bool Del     = requires(T d) {
    { del(d)        } -> void
};
template<typename T> concept bool Init    = requires(T d) {
    { init(d)       } -> void
};
template<typename T> concept bool At      = requires(T cont, size_t i) {
    { cont[i]       } -> auto
};

template<typename T> concept bool NoReloc = !Reloc<T>;
template<typename T> concept bool NoDel   = !Del<T>;
template<typename T> concept bool NoInit  = !Init<T>;
template<typename T> concept bool NoAt    = !At<T>;

template<typename T> concept bool Cont    = requires(T c, size_t i) {
    { c.size        } -> size_t;
    { make(c, i, c[0]) } -> bool;
    requires At<T>;
};

template<typename T> concept bool DnCont  = requires(T c, const size_t i) {
    { c.reserved    } -> size_t;
    { reserve(c, i) } -> bool;
    requires Cont<T>;
};

namespace mem {

template<NoReloc T> T* ualloc(const size_t n) {
    return (T*)malloc(n * sizeof(T));
}
template<NoReloc T> T* zalloc(const size_t n) {
    return (T*)calloc(n, sizeof(T));
}
template<NoReloc T> T* ralloc(T* raw, const size_t, const size_t n) {
    return (T*)realloc(raw, n * sizeof(T));
}
template<NoReloc T> void cpy(T* dst, T* src, const size_t size) {
    memcpy(dst, src, size * sizeof(T));
}
template<Reloc T> void cpy(T* dst, T* src, const size_t size) {
    memcpy(dst, src, size * sizeof(T));
    for (size_t i = 0; i < size; ++i) {
        reloc(dst[i]);
    }
}
template<typename T> bool iszero(const T& ele) {
    char zero[sizeof(T)] = {0};
    return bcmp(&zero, &ele, sizeof(T)) == 0;
}
template<Reloc T> T* ralloc(T* raw, const size_t os, const size_t ns) {
    T *res;
    if ((res = mem::ralloc(raw, ns)) == nullptr) {
        return res;
    }
    for (size_t i = os; i < ns; ++i) {
        reloc(res[i]);
    }
    return res;
}
template<typename T> T *ealloc(const T& ele, const size_t size) {
    if (mem::iszero(ele))  {
        return mem::zalloc<T>(size);
    }
    T *res;
    if ((res = mem::ualloc<T>(size)) == nullptr) {
        return res;
    }
    for (size_t i = 0; i < size; ++i) {
        res[i] = ele;
    }
    return res;
}
template<typename T> void ezero(T* raw, const size_t size) {
    memset(raw, 0, sizeof(T) * size);
}

}

template<typename T> struct Vec {
    size_t size     = 0;
    size_t reserved = 0;
    T*     data     = nullptr;
    T& operator[](size_t i) { return data[i]; }
};
bool make(Vec<NoReloc>& vec, const size_t size, const NoReloc& ele) {
    if ((vec.data = mem::ealloc(ele, size)) == nullptr) {
        return false;
    }
    vec.size = size;
    vec.reserved = size;
    return true;
}
bool reserve(Vec<auto>& vec, const size_t ns) {
    auto res = mem::ralloc(vec.data, vec.size, ns);
    if (res == nullptr) {
        return false;
    }
    vec.data = res;
    vec.reserved = ns;
    return true;
}
template<Init T> bool make(Vec<T>& vec, const size_t size) {
    if ((vec.data = mem::ualloc<T>(size)) == nullptr) {
        return false;
    }
    for (size_t i = 0; i < size; ++i) {
        init(vec[i]);
    }
    vec.size = size;
    vec.reserved = size;
    return true;
}
DnCont{T} bool join(T& dst, T& src) {
    size_t ns = dst.size + src.size;
    if (ns > dst.reserved) {
        if (reserve(dst, ns) == false) {
            return false;
        }
    }
    mem::cpy(dst.data + dst.size, src.data, src.size);
    src.size = 0;
    dst.size = ns;
    return true;
}
DnCont{T} bool push(T& dst, const auto& ele, size_t initn = 1) {
    size_t ns;
    if (dst.size < dst.reserved) {
        dst[dst.size++] = ele;
        return true;
    }
    ns = dst.reserved ? 2 * dst.reserved : initn;
    if (reserve(dst, ns) == false) {
        return false;
    }
    dst[dst.size++] = ele;
    return true;
}
DnCont{T} bool pusham(T& dst, const auto& ele, size_t initn = 1) {
    size_t ns;
    if (dst.size < dst.reserved) {
        dst[dst.size++] = ele;
        return true;
    }
    ns = dst.reserved ?  dst.reserved + dst.reserved/2 : initn;
    if (reserve(dst, ns) == false) {
        return false;
    }
    dst[dst.size++] = ele;
    return true;
}
void del(Vec<Del>& vec) {
    for (size_t i = 0; i < vec.size; ++i) {
        del(vec[i]);
    }
    free(vec.data);
}
void del(Vec<NoDel>& vec) {
    free(vec.data);
}
void init(Vec<auto>& vec) {
    vec.size        = 0;
    vec.reserved    = 0;
    vec.data        = nullptr;
}

template<typename T, size_t N = 16> struct MuVec {
    size_t size     = 0;
    size_t reserved = N;
    T*     data     = mem;
    T      mem[N]   = {0};
    T& operator[](size_t i) { return data[i]; }
};
template<size_t N> void init(MuVec<auto, N>& vec) {
    vec.size        = 0;
    vec.reserved    = N;
    vec.data        = vec.mem;
    vec.mem         = {0};
}
template<size_t N>
bool make(MuVec<NoReloc, N>& vec, const size_t size, const NoReloc& ele) {
    if (size <= N) {
        for (size_t i = 0; i< N; ++i) {
            vec[i] = ele;
        }
        vec.size = size;
        return true;
    }
    if ((vec.data = mem::ealloc(ele, size)) == nullptr) {
        return false;
    }
    vec.size = size;
    vec.reserved = size;
    return true;
}
template<size_t N> void reloc(MuVec<auto, N>& vec) {
    if (vec.size < N) {
        vec.data = vec.mem;
    }
}
template<size_t N> void del(MuVec<Del, N>& vec) {
    for (size_t i = 0; i < vec.size; ++i) {
        del(vec[i]);
    }
    if (vec.size > N) {
        free(vec.data);
    }
}
template<size_t N> void del(MuVec<NoDel, N>& vec) {
    if (vec.size > N) {
        free(vec.data);
    }
}
template<typename T, size_t N>
bool reserve(MuVec<T, N>& vec, const size_t ns) {
    T *res;
    if (vec.size < N) {
        if ((res = mem::ualloc<T>(ns)) != nullptr) {
            mem::cpy(res, vec.data, vec.size);
        }
    } else {
        res = mem::ralloc(vec.data, vec.size, ns);
    }
    if (res == nullptr) {
        return false;
    }
    vec.data = res;
    vec.reserved = ns;
    return true;
}

template<typename T> struct FixVec {
    size_t size     = 0;
    T*     data     = nullptr;
    T& operator[](size_t i) { return data[i]; }
};
bool make(FixVec<NoReloc>& vec, const size_t size, const NoReloc& ele) {
    if ((vec.data = mem::ealloc(ele, size)) == nullptr) {
        return false;
    }
    vec.size = size;
    return true;
}
template<Init T> bool make(FixVec<T>& vec, const size_t size) {
    if ((vec.data = mem::ualloc<T>(size)) == nullptr) {
        return false;
    }
    for (size_t i = 0; i < size; ++i) {
        init(vec[i]);
    }
    vec.size = size;
    return true;
}
void del(FixVec<Del>& vec) {
    for (size_t i = 0; i < vec.size; ++i) {
        del(vec[i]);
    }
    free(vec.data);
}
void del(FixVec<NoDel>& vec) {
    free(vec.data);
}
void init(FixVec<auto>& vec) {
    vec.size        = 0;
    vec.data        = nullptr;
}

}
