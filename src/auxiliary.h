//
//  auxiliary.h
//  ccdr_proj
//
//  Created by Nick Austin on 3/20/14.
//  Copyright (c) 2014 Nick Austin. All rights reserved.
//

#ifndef auxiliary_h
#define auxiliary_h

#include <random>

#include "SparseMatrix.h"

//------------------------------------------------------------------------------/
//   auxiliary FUNCTIONS FOR CCDR CODEBASE
//------------------------------------------------------------------------------/

//
// Various random functions that are needed in the main CCDr code. All of these functions are SOLELY
//  for testing in a C++ development environment (e.g Xcode), outside of R.
//
// NOTE: The sign function, which is a critical function for the algorithm, used to be defined here,
//       but this dependency has been moved to penalty.h to decouple the C++ test code from the
//       critical code.
//

double sign(double x);                          // usual sign function

//bool nonzero(double z);                         // determine if a number is nonzero or not

int rand_int(int max_int);                      // generate a random integer between 1 and max_int

SparseMatrix randomSBM(int pp,             //
                            int nnz,            // generate a random SparseMatrix
                            double coef);       //

std::vector<double> random_unif(int len);       // generate a random Uniform[0,1] vector of length len

std::vector<double> lambdaGrid(double maxlam,   //
                               double minlam,   // generate a grid of lambdas based on the usual log-scale
                               int nlam);       //

// 
// sign
//
// 4/2/15: MOVED TO PENALTY.H TO ELIMINATE R PACKAGE DEPENDENCY ON THIS HEADER FILE

//
// rand_int
//
//   Generate a random number between 1 and max_int
//
int rand_int(int max_int){
    return (rand() % max_int + 1);
}

//
// randomSBM
//
//   Generate a random object from the class SparseMatrix by simply adding random elements
//
//   NOTE: This function does NOT necessarily return a DAG
//
SparseMatrix randomSBM(int pp,
                            int nnz,
                            double coef){

    #ifdef _DEBUG_ON_
        FILE_LOG(logDEBUG2) << "Function call: randomSBM";
    #endif
    
    SparseMatrix final = SparseMatrix(pp);
    
    for(int k = 0; k < nnz; ++k){
        // generate a random row
        int row = 1;
        while(row < 2) row = rand_int(pp); // row must be > 1 for acyclicity, so we keep generating new random ints until we get one > 1
        
        // generate a random column (must be < row in order to guarantee acyclicity)
        int col = rand_int(row - 1);
        
        row--; col--; // re-index since we're using C++
        
        final.addBlock(row, col, coef, 0);
    }
    
    return final;
    
}

//
// random_unif
//
//   Generate a random integer between 1 and max_int (nothing fancy here)
//
std::vector<double> random_unif(int len){
    std::vector<double> c(len, 0);
    
    std::default_random_engine generator;
    std::uniform_real_distribution<double> distribution(0.0, 1.0);
    
    for(int i = 0; i < len; ++i){
        double x = distribution(generator);
        c[i] = x;
    }
    
    return c;
}

//
// lambdaGrid
//
//   Generate a sequence / grid of lambdas on the standard log-scale for the CCDr algorithm. The scale is determined
//     by the maximum and minimum desired values and the length of the desired grid: nlam values are chosen, decreasing
//     on a log-scale, in the interval [minlam, maxlam] (inclusive)
//
//   NOTE: The typical use of this function uses the choices maxlam = sqrt(n) and minlam = rlam*maxlam, where rlam is some
//          ratio (typically around 0.001-0.1). These defaults are NOT enforced by this function and need to be inserted
//          via the calling code.
// 
//
std::vector<double> lambdaGrid(double maxlam, double minlam, int nlam){
    double fmlam = maxlam / minlam;             // naming convention is consistent with legacy R code
    double delta = log(fmlam) / (nlam - 1.0);   // the difference between successive lambda values is based on a log-scale
    
    std::vector<double> lambda_grid(nlam, 0);
    lambda_grid[0] = maxlam;
    for(int i = 1; i < nlam; ++i){
        lambda_grid[i] = log(lambda_grid[i-1]) - delta;
        lambda_grid[i] = exp(lambda_grid[i]);
    }
    
    #ifdef _DEBUG_ON_
        // the final value should be = minlam, if it isn't something went wrong in the calculations
        if(fabs(lambda_grid[nlam - 1] - minlam) > 1e-6){
            OUTPUT << "Whoa lambdaGrid didn't work! last = " << lambda_grid[nlam - 1] << " minlam = " << minlam << std::endl;
            FILE_LOG(logERROR) << "Whoa lambdaGrid didn't work! last = " << lambda_grid[nlam - 1] << " minlam = " << minlam;
        }
    #endif
    
    return lambda_grid;
}

#endif
