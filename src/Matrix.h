// from http://stackoverflow.com/a/2076668/3961092

#ifndef Matrix_h
#define Matrix_h

#include <vector>
#include <iomanip>

template <class T>
class Matrix{
public:
    Matrix(size_t rows, size_t cols);
    Matrix(T fill, size_t rows, size_t cols);
    T& operator()(size_t i, size_t j);
    T operator()(size_t i, size_t j) const;
    size_t nrow() const;
    size_t ncol() const;

    std::vector<T> vprod(std::vector<T> x) const;
    std::vector<T> col(size_t j) const;
    void print() const;

private:
    size_t mRows;
    size_t mCols;
    std::vector<T> mData;
};

template <class T>
Matrix<T>::Matrix(size_t rows, size_t cols)
: mRows(rows),
  mCols(cols),
  mData(rows * cols)
{
}

template <class T>
Matrix<T>::Matrix(T fill, size_t rows, size_t cols)
: mRows(rows),
  mCols(cols),
  mData(rows * cols)
{
    std::fill(mData.begin(), mData.end(), fill);
}

template <class T>
T& Matrix<T>::operator()(size_t i, size_t j){
    return mData[j * mRows + i];
}

template <class T>
T Matrix<T>::operator()(size_t i, size_t j) const{
    return mData[j * mRows + i];
}

template <class T>
std::vector<T> Matrix<T>::col(size_t j) const{
    typedef typename std::vector<T>::const_iterator const_iterator;
    const_iterator first = mData.begin() + j * mRows;
    const_iterator last = mData.begin() + (j+1) * mRows;
    std::vector<T> colj(first, last);
    
    return colj;
}

template <class T>
size_t Matrix<T>::nrow() const{
    return mRows;
}

template <class T>
size_t Matrix<T>::ncol() const{
    return mCols;
}

template <class T>
std::vector<T> Matrix<T>::vprod(std::vector<T> x) const{
    // Matrix A = nxp
    // vector x = px1
    auto n = nrow();
    auto p = ncol();
    std::vector<T> y(n);

    for(auto i = 0; i < n; ++i){
        T innerprod = 0.;
        for(auto j = 0; j < p; ++j){
            innerprod += operator()(i, j) * x[j];
        }
        y[i] = innerprod;
    }

    return y;
}

template <class T>
void Matrix<T>::print() const{
    // printf ("floats: %4.2f %+.0e %E \n", 3.1416, 3.1416, 3.1416);
    for (unsigned i = 0; i < nrow(); ++i){
        for (unsigned j = 0; j < ncol(); ++j){
            printf("%8.4f ", operator()(i, j));
            // std::cout << std::setw(6) << std::setprecision(3) << operator()(i, j);
        }
        std::cout << std::endl;
    }

    return;    
}

#endif
