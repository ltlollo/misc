#ifndef MAT_H
#define MAT_H

#include <cstdlib>
#include <new>
#include <utility>

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
    template<typename F> void for_each(F&& f, const Bound<D> b) {
        for (auto i = b.ht; i < height-b.hb; ++i) {
            for (auto j = b.wl; j < width-b.wr; ++j) {
                f(data+i*width+j);
            }
        }
    }
    T* operator[](const D i) noexcept {
        return data[i*width];
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
    template<typename F> void for_each(F&& f) {
        MatView<T, D>{ width, height, data }.for_each(f);
    }
    template<typename F> void for_each(F&& f, const Bound<D>& b) {
        MatView<T, D>{ width, height, data }.for_each(f, b);
    }
    T* operator[](const D row) noexcept {
        return data+row*width;
    }
    MatView<T, D>& view(D row, D col) noexcept {
        return MatView<T>{ data+row*width+col, width, height };
    }
    MatView<T, D>& operator[](const Pos<D>& ele) noexcept {
        return view(ele.row, ele.col);
    }
    ~Mat() noexcept {
        delete[] data;
    }
};

#endif // MAT_H
