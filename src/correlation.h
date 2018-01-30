//
//  correlation.h
//  ccdr2
//
//  Created by Bryon Aragam on 12/20/16.
//  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
//

#ifndef correlation_h
#define correlation_h

#include "Matrix.h"

//------------------------------------------------------------------------------/
//   HELPER FUNCTIONS FOR CORRELATION DATA
//------------------------------------------------------------------------------/

//
// 
//

// utilities
template <typename T> std::vector<size_t> order(const std::vector<T> &v);
std::vector<std::string> reorder(const std::vector<std::string> x, const std::vector<size_t> ord);

// uses Matrix.h
template<class T> Matrix<T> cor_vector_to_Matrix(const std::vector<T>& cors, unsigned int ncol);
template<class T> std::vector<T> max_entry_by_column(Matrix<T> &cors);
template<class T> std::vector<size_t> getNodeOrder(Matrix<T> cors);
template<class T> void printSymmetricMatrix(Matrix<T> m);

// from http://stackoverflow.com/a/12399290/3961092
template <typename T>
std::vector<size_t> order(const std::vector<T> &v){

  // initialize original index locations
  std::vector<size_t> idx(v.size());
  iota(idx.begin(), idx.end(), 0);

  // sort indexes based on comparing values in v
  sort(idx.begin(), idx.end(),
       [&v](size_t i1, size_t i2) {return v[i1] > v[i2];});

  return idx;
}

std::vector<std::string> reorder(const std::vector<std::string> x, const std::vector<size_t> ord){
    std::vector<std::string> out(ord.size());
    for(auto i = 0; i < ord.size(); ++i){
        out[i] = x[ord[i]];
    }

    return out;
}

// --- MATRIX.H ------------------------------------------------------------------------
template<class T>
Matrix<T> cor_vector_to_Matrix(const std::vector<T>& cors, unsigned int ncol){

    Matrix<T> cormat(ncol, ncol);
    for(unsigned j = 0; j < ncol; ++j) // columns first
        for(unsigned i = 0; i <= j; ++i){
            cormat(i, j) = cors[(i + j*(j+1)/2)];
            cormat(j, i) = cors[(i + j*(j+1)/2)];
        }
    return cormat;
}

template<class T>
std::vector<T> max_entry_by_column(Matrix<T> &cors){
    std::vector<T> maxcor(cors.ncol());
    for(auto j = 0; j < cors.ncol(); ++j){
        maxcor[j] = 0;
        for(auto i = 0; i < cors.nrow(); ++i){
            if(i != j){
                T thiscor = fabs(cors(i, j));
                if(thiscor > maxcor[j]){
                    maxcor[j] = thiscor;
                }
            }
        }
    }
    
    return maxcor;
}

template<class T>
std::vector<size_t> getNodeOrder(Matrix<T> cors){
    auto maxcor = max_entry_by_column(cors);
    return order(maxcor);
}

template<class T>
void printSymmetricMatrix(Matrix<T> m){
    // printf ("floats: %4.2f %+.0e %E \n", 3.1416, 3.1416, 3.1416);
    for (unsigned i = 0; i < m.nrow(); ++i){
        for (unsigned j = 0; j < m.ncol(); ++j){
            printf("%6.3f ", m(i, j));
        }
        std::cout << std::endl;
    }

    return;    
}

#endif
