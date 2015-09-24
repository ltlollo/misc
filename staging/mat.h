#ifndef MAT_H
#define MAT_H

#include <cstdlib>
#include <new>
#include <utility>
#include <type_traits>

/* Scope
 * small matrix `Mat<T, S>` class to help ease different 2D iteration
 * strategies, where D is `T`.
 * `T` is the type of the contained data (, `S` determines how the dimentions
 * are stored and must be able to represent width*height, deault: size_t).
 *
 * Example(code)
 *
 * int i = 0;
 * auto m = Mat<int>(100, 100);
 * m.for_each([&](auto ep){ *ep = i++; });
 * auto v = m[Pos<>{2, 40}];
 * v.for_each([](auto e){ cout << *e << ' '; }, Bound<int>{-2, 2, -2, 2});
 * cout << endl;
 * // or
 * for (int i = -2; i < 2; ++i) {
 *     for (int j = -2; j < 2; ++j) { cout << v[i][j] << '\t'; }
 *     cout << endl;
 * }
 */

template<typename D=size_t>
struct Bound {
    D wl, wr, ht, hb;
};
template<typename D=size_t>
struct Erode {
    D wl, wr, ht, hb;
};

template<typename T, typename D=size_t>
struct MatView {
    D width, height;
    T* data;
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

template<typename D=size_t> struct Pos { D row, col; };
//template<typename D=long> struct WrapVert { D value; };
//template<typename D=long> struct WrapHoriz { D value; };

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
        for (size_t i = 0; i < width*height; ++i) {
            f(data+i);
        }
    }
    template<typename F> void for_each(F&& f, const Erode<D> b) {
        for (auto i = b.ht; i < height-b.hb; ++i) {
            for (auto j = b.wl; j < width-b.wr; ++j) {
                f(data+i*width+j);
            }
        }
    }
    T* operator[](const D row) noexcept {
        return data+width*row;
    }
    //template<typename S> T* operator[](const WrapVert<S> row) noexcept {
    //    return data+width*((height+row.value)%height);
    //}
    MatView<T, D> view(D row, D col) noexcept {
        return MatView<T, D>{ width, height, data+width*row+col };
    }
    MatView<T, D> operator[](const Pos<D>& ele) noexcept {
        return view(ele.row, ele.col);
    }
    MatView<T, D> view(T* ele) noexcept {
        return MatView<T, D>{ width, height, ele };
    }
    ~Mat() noexcept {
        ::operator delete[](data, width*height);
    }
};

template<typename T, typename D, typename S>
constexpr auto view(T* data, D width, D height, S row, S col) noexcept {
        return MatView<T, D>{ width, height, data+width*row+col };
}

template<typename T, typename D, typename S>
constexpr auto view(T* data, D width, D height, const Pos<S> ele) noexcept {
        return view(ele.row, ele.col);
}

template<typename D, typename S>
constexpr D wrap(const D dim, const S val) noexcept {
    return (dim+val)%dim;
}


#endif // MAT_H
