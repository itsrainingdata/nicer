
#ifndef linalg_h
#define linalg_h

#include <math.h>
#include "Matrix.h"

template <class T>
T mean(std::vector<T> x){
    T sum = std::accumulate(x.begin(), x.end(), 0.0);
    T avg = sum / x.size();

    return avg;
}

template <class T>
T innerprod(std::vector<T> x, std::vector<T> y){
    // T ip = 0;
    // for(auto i = 0; i < x.size(); ++i){
    //     ip += x[i] * y[i];
    // }
    T ip = std::inner_product(x.begin(), x.end(), y.begin(), 0.0);

    return ip;
}

template <class T>
T vnorm(std::vector<T> x){
    T ip = innerprod(x, x);

    return sqrt(ip);
}

template <class T>
T matinnerprod(Matrix<T> x, size_t col1, size_t col2){
    T ip = 0;
    for(auto i = 0; i < x.nrow(); ++i){
        ip += x(i, col1) * x(i, col2);
    }

    return ip;
}

template <class T>
Matrix<T> gram(Matrix<T> x){
    Matrix<T> grammat(x.ncol(), x.ncol());
    for(size_t i = 0; i < x.ncol(); ++i){
        for(size_t j = 0; j < x.ncol(); ++j){
            grammat(i, j) = matinnerprod(x, i, j);
        }
    }

    return grammat;
}

#endif
