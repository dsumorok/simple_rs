// Copyright (c) 2015, Daniel Sumorok
// All rights reserved.

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:

// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <random>
#include "SimpleRS.hh"

static const std::size_t Q = 256;
static const std::size_t n = 254;
static const std::size_t k = 204;
static const std::size_t iter = 1000000;

typedef SimpleRS<Q, n, k> rs_t;

int main() {
  rs_t rsTest;
  rs_t::gfIWord_t encoderInput;
  rs_t::gfCWord_t encoderOutput;
  rs_t::gfCWord_t decoderInput;
  rs_t::gfCWord_t decoderOutput;
  std::size_t i;
  std::size_t j;
  int successCount = 0;
  int failCount = 0;
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<unsigned int> genVal(0, rs_t::getQ()-1);
  std::uniform_int_distribution<unsigned int> genErrVal(1, rs_t::getQ()-1);
  std::uniform_int_distribution<unsigned int> genThresh(1, rs_t::gett()-1);
  std::uniform_int_distribution<unsigned int> genErr(0, n);
  const size_t t = rs_t::gett();
  std::size_t *errCounts;

  errCounts = new std::size_t[t+1];
  memset(errCounts, 0, sizeof(std::size_t) * (t+1) );

  for(i=0; i<iter; ++i) {
    // Generate Test Input Vector
    for(j=0; j<k; ++j) {
      encoderInput[j] = genVal(gen);
    }

    // Encode Test Input Vector
    rsTest.encode(encoderInput, encoderOutput);

    // Threshold that determines if an error should be introduced
    unsigned int thresh = genThresh(gen);
    std::size_t actualErrCount = 0;

    // Initialize decoder input with encoder output
    memcpy(decoderInput, encoderOutput, sizeof(rs_t::gfEl_t) * n);

    for(j=0; j<n; ++j) {
      if(genErr(gen) < thresh) {
	
	// Introduce error at index j
	rs_t::gfEl_t errVal = genErrVal(gen);
	decoderInput[j] ^= errVal;

	// Make sure the total number of errors does not exceed the
	// maximum number of errors that can be corrected
	++actualErrCount;
	if(actualErrCount == rs_t::gett()) {
	  break;
	}
      }
    }
    errCounts[actualErrCount] += 1;
    
    // Initialize decoder output with decoder input
    memcpy(decoderOutput, decoderInput, sizeof(rs_t::gfEl_t) * n);

    // Correct errors
    rsTest.decode(decoderOutput);

    // Verify that errors were corrected
    if(memcmp(encoderOutput, decoderOutput, n) == 0) {
      ++successCount;
    } else {
      ++failCount;
    }

    if((i%10000) == 9999) {
      printf("SuccessCount = %d, failCount = %d\n",
	     successCount, failCount);
    }
  }

  for(i=0; i<=rs_t::gett(); ++i) {
    printf("Count for %3zd errors: %zd\n",
	   i, errCounts[i]);
  }
  
  if((i%10000) != 0) {
    printf("SuccessCount = %d, failCount = %d\n",
	   successCount, failCount);
  }

  delete [] errCounts;
  
  return 0;
}
