//
//  CCDrAlgorithm.h
//  ccdr_proj
//
//  Created by Bryon Aragam on 3/20/14.
//  Copyright (c) 2014-2015 Bryon Aragam. All rights reserved.
//

#ifndef BlockList_h
#define BlockList_h

#include <vector>
#include <math.h>
#include <algorithm>

//------------------------------------------------------------------------------/
//   BLOCKLIST CLASS
//------------------------------------------------------------------------------/

class BlockList{

public:
    // user-defined input
    unsigned int numBlocks;  // number of blocks in list
    unsigned int nodes; // number of nodes associated with the list

    //
    // Constructors
    //
    BlockList();
    BlockList(std::vector<std::vector<int>> in_blocks);
    BlockList(std::vector<std::vector<int>> in_blocks, unsigned int in_nodes);

    //
    // Member functions
    //
    std::vector<int> getBlock(unsigned int k) const;
    unsigned int size() const;
    void shuffle();

private:
    std::vector<std::vector<int>> blocks;  // [k][l], k<=size, l<=2
};

BlockList::BlockList(){
    std::vector<std::vector<int>> empty;
    blocks = empty;
    numBlocks = empty.size();
    nodes = 0;
}

BlockList::BlockList(std::vector<std::vector<int>> in_blocks){
    //
    // Probably add some consistency checks here
    //

    blocks = in_blocks;
    numBlocks = in_blocks.size();
    nodes = 0;
}

BlockList::BlockList(std::vector<std::vector<int>> in_blocks, unsigned int in_nodes){
    //
    // Probably add some consistency checks here
    //

    blocks = in_blocks;
    numBlocks = in_blocks.size();
    nodes = in_nodes;
}

std::vector<int> BlockList::getBlock(unsigned int k) const{
    return blocks[k]; // 2-dimensional vector
}

unsigned int BlockList::size() const{
    return numBlocks;
}

void BlockList::shuffle(){
    std::random_shuffle(blocks.begin(), blocks.end());
};

#endif
