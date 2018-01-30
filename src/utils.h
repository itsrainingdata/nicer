//
//  utils.h
//
//  Created by Bryon Aragam (local) on 1/2/17.
//  Copyright (c) 2014-2017 Bryon Aragam (local). All rights reserved.
//

#ifndef utils_h
#define utils_h

#include <numeric>
#include <algorithm>
#include <vector>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <unordered_map>
#include <map>

template <class S, class T>
void printMap(std::map<S, T> m){
    std::cout << "m = ";
    for (const auto &p : m) {
        std::cout << "[" << p.first << ":" << p.second << "]";
    }
}

template <class S, class T>
void printMap(std::unordered_map<S, T> m){
    std::cout << "m = ";
    for (const auto &p : m) {
        std::cout << "[" << p.first << ":" << p.second << "]";
    }
}

template <class T>
void printVector(std::vector<T> x){
    std::cout << "(" << x[0];
    for(auto i = 1; i < x.size(); ++i){
        std::cout << ", " << x[i];
    }
    std::cout << ")\n";
}

template <class T>
std::string strVector(std::vector<T> x){
    std::ostringstream vec_out;
    int field_width = 10;
    // vec_out << std::setw(field_width) << std::setprecision(4);
    vec_out << std::setprecision(4);

    vec_out << "(" << x[0];
    for(auto i = 1; i < x.size(); ++i){
        vec_out << ", " << x[i];
    }
    vec_out << ")";

    return vec_out.str();
}

std::vector<size_t> range(size_t min, size_t max){
    std::vector<size_t> out(max-min+1);
    std::iota(std::begin(out), std::end(out), min);

    return out;
}

std::vector<size_t> range(size_t n){
    return range(1, n);
}

std::vector<size_t> range0(size_t n){
    return range(0, n);
}

template<class T>
std::vector<T> rep(T val, size_t n){
    std::vector<T> out(n);
    std::fill(std::begin(out), std::end(out), val);

    return out;
}

#endif
