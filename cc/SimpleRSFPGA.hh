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

#ifndef SIMPLERSFPGA_HH
#define SIMPLERSFPGA_HH

#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "SimpleRS.hh"

template<std::size_t Q,
         std::size_t n,
         std::size_t k,
         typename _gfEl_t = uint8_t>
class SimpleRSFPGA : public SimpleRS<Q,n,k> {
public:
  typedef _gfEl_t gfEl_t;
  typedef gfEl_t gfIWord_t[k];
  typedef gfEl_t gfCWord_t[n];
  int m_encoderFd;
  int m_decoderFd;

private:
  typedef gfEl_t gfPoly_t[n];
  gfEl_t m_logTable[Q];

public:

  virtual bool decode(gfEl_t *codeWords, size_t blockCount) {
    size_t dataLen = n * blockCount;

    do {
      if(write(m_decoderFd, codeWords, dataLen) < (ssize_t)dataLen) {
        fprintf(stderr, "Failed to write to driver\n");
        break;
      }

      if(read(m_decoderFd, codeWords, dataLen) < (ssize_t)dataLen) {
        fprintf(stderr, "Failed to read from driver\n");
        break;
      }

      return true;
    } while(false);

    return false;
  }

  virtual bool decode(gfEl_t *codeWord) {
    return decode(codeWord, 1);
  }

  virtual bool encode(const gfEl_t *informationWords,
                      gfEl_t *codeWords, size_t blockCount) {
    size_t dataLenIn = k * blockCount;
    size_t dataLenOut = n * blockCount;

    do {
      if(write(m_encoderFd, informationWords, dataLenIn) <
         (ssize_t)dataLenIn) {
        fprintf(stderr, "Failed to write to driver\n");
        break;
      }

      if(read(m_encoderFd, codeWords, dataLenOut) <
         (ssize_t)dataLenOut) {
        fprintf(stderr, "Failed to read from driver\n");
        break;
      }

      return true;
    } while(false);

    return false;
  }

  virtual bool encode(const gfEl_t *informationWord,
		      gfEl_t *codeWord) {
    return encode(informationWord, codeWord, 1);
  }


  SimpleRSFPGA() : m_encoderFd(-1), m_decoderFd(-1) {
    const char *encoderFName = "/dev/rsencoder";
    const char *decoderFName = "/dev/rsdecoder";

    m_encoderFd = open(encoderFName, O_RDWR);
    if(m_encoderFd < 0) {
      fprintf(stderr, "Failed to open %s: %s\n",
              encoderFName, strerror(errno));
    }

    m_decoderFd = open(decoderFName, O_RDWR);
    if(m_decoderFd < 0) {
      fprintf(stderr, "Failed to open %s: %s\n",
              decoderFName, strerror(errno));
    }

  }
};

#endif
