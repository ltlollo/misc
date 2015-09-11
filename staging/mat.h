#ifndef MAT_H
#define MAT_H

#include <cstdlib>
#include <new>
#include <utility>
#include <type_traits>

template<typename D=size_t>
struct Bound {
    D wl, wr, ht, hb;
};

template<typename T, typename D=size_t>
struct MatView {
    D width, height;
    T* data;
    template<typename F> void for_each(F&& f) {
        for (size_t i = 0; i < size_t(height)*width; ++i) {
            f(data+i);
        }
    }
    template<typename F, typename S> void for_each(F&& f, const Bound<S>& b) {
        using U = std::make_unsigned_t<S>;
        T* ptr = (*this)[b.wl]+b.ht;
        U wlen = b.wr-b.wl;
        U hlen = b.hb-b.ht;
        for (U i = 0; i < hlen; ++i) {
            for (U j = 0; j < wlen; ++j) {
                f(ptr+i*width+j);
            }
        }
    }
    template<typename F, typename S>
    void for_each(F&& f, const Bound<S>& b, size_t mask) {
        using U = std::make_unsigned_t<S>;
        T* ptr = (*this)[b.wl]+b.ht;
        U wlen = b.wr-b.wl;
        U hlen = b.hb-b.ht;
        for (U i = 0; i < hlen; ++i) {
            for (U j = 0; j < wlen; ++j) {
                if ((mask>>(j+wlen*i))&1) {
                    f(ptr+i*width+j);
                }
            }
        }
    }
    template<typename S> T* operator[](const S i) noexcept {
        return data+i*width;
    }
};

template<typename D=size_t>
struct Pos {
    D row, col;
};

template<typename T, typename D=size_t>
struct Mat {
private:
    D width, height;
    T* data{nullptr};
public:
    Mat(D width, D height) :
        width{width},
        height{height},
        data{new T[width*height]} {
    }
    Mat(Mat<T, D>&& rhs) noexcept : width{rhs.width}, height{rhs.height} {
        std::swap(data, rhs.data);
    }
    Mat<T, D>& operator=(Mat<T, D>&& rhs) noexcept {
        std::swap(width,  rhs.width);
        std::swap(height, rhs.height);
        std::swap(data,   rhs.data);
        return *this;
    }
    Mat(const Mat<T, D>&) = delete;
    Mat<T, D>& operator=(const Mat<T, D>&) = delete;
    template<typename F> void for_each(F&& f) {
        MatView<T, D>{ width, height, data }.for_each(f);
    }
    template<typename F> void for_each(F&& f, const Bound<D>& b) {
        MatView<T, D>{ width, height, data }.for_each(f, b);
    }
    T* operator[](const D row) noexcept {
        return data+row*width;
    }
    MatView<T, D> view(D row, D col) noexcept {
        return MatView<T, D>{ width, height, data+size_t(width)*row+col };
    }
    MatView<T, D> operator[](const Pos<D>& ele) noexcept {
        return view(ele.row, ele.col);
    }
    ~Mat() noexcept {
        delete[] data;
    }
};

#endif // MAT_H
