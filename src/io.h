
//
//  io.h
//  ccdr_proj
//
//  Created by Nick Austin on 3/28/14.
//  Copyright (c) 2014 Nick Austin. All rights reserved.
//

#ifndef io_h
#define io_h

#include <vector>
#include <iostream>
#include <fstream>
#include <string>

//------------------------------------------------------------------------------/
//   FILE I/O FUNCTIONS FOR CCDR CODEBASE
//------------------------------------------------------------------------------/

//
// Special functions needed to read in data that is written by R to test the algorithm in a C++ development
//   enivornment (e.g. Xcode). These functions simply read in a text file in order, ignoring any special structure.
//   By default, a whitespace character or line break is the delimiting character. 
// 
// Because I didn't feel like templating the code, there are two separate functions for reading in ints and doubles.
//

// 
// Get the user's home directory: The code assumes that all files are located in .../<USER_NAME>/Dropbox/...
//   This is mainly intended to allow the code to work portably between office and home.
//
const std::string HOME_DIR(getenv("HOME"));

//
// read_double
//
//   Read in a file containing doubles using default delimiting criteria (space, line break).
//
std::vector<double> read_double(std::string file_name){
    std::vector<double> output;
    
    std::ifstream fin;
    fin.open(file_name);
    
    double x;
    while(fin >> x){
        output.push_back(x);
    }
    
    fin.close();
    
    return output;
}

//
// read_int
//
//   Read in a file containing integers using default delimiting criteria (space, line break).
//
std::vector<int> read_int(std::string file_name){
    std::vector<int> output;
    
    std::ifstream fin;
    fin.open(file_name);
    
    int x;
    while(fin >> x){
        output.push_back(x);
    }
    
    fin.close();
    
    return output;
}

//
// read_cors
//
//   A wrapper for read_double() that uses the hard-coded file location for the test correlation data, which is located in 
//     .../ccdr_test/TEST_CORS.csv
//
std::vector<double> read_cors(){
//    char FILE_NAME[] = "/Dropbox/PhD Research/Programming Projects/CD Algorithm Code/Rcpp Reboot/TEST_CORS.csv"; // file to read in
//    char input_file_path[50];
//    strcpy(input_file_path, HOME_DIR);      // create a string to store the location of user's HOME directory
//    strcat(input_file_path, FILE_NAME);     // append the destination directory/filename to HOME

    std::string input_file_path = HOME_DIR + "/Desktop/ccdr_generate/TEST_CORS.csv";
//    std::string input_file(input_file_path);
    
    return read_double(input_file_path);
}

//
// read_params
//
//   A wrapper for read_int() that uses the hard-coded file location for the test parameter data, which is located in 
//     .../ccdr_test/TEST_PARAMS.csv
//
std::vector<int> read_params(){
//    char FILE_NAME[] = "/Dropbox/PhD Research/Programming Projects/ccdr_test/TEST_PARAMS.csv"; // file to read in
//    char input_file_path[50];
//    strcpy(input_file_path, getenv("HOME"));      // create a string to store the location of user's HOME directory
//    strcat(input_file_path, FILE_NAME);     // append the destination directory/filename to HOME
    
    std::string input_file_path = HOME_DIR + "/Desktop/ccdr_generate/TEST_PARAMS.csv";
//    std::string input_file(input_file_path);
    
    return read_int(input_file_path);
}

std::vector<int> read_moral(){
    std::string input_file_path = HOME_DIR + "/Desktop/ccdr_generate/TEST_MORAL.csv";
    
    return read_int(input_file_path);
}

#endif
