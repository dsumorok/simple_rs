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

#ifndef SIMPLERS_HH
#define SIMPLERS_HH

#include <random>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <cstring>

template<std::size_t Q,
         std::size_t n,
         std::size_t k,
         typename _gfEl_t = uint8_t>
class SimpleRS {
public:
  typedef _gfEl_t gfEl_t;

private:
  typedef gfEl_t gfPoly_t[n];

  static const std::size_t m_t = (n-k) / 2;
  uint32_t m_primativePoly;
  gfEl_t m_powTable[Q];
  gfEl_t m_logTable[Q];
  gfEl_t UStorage[2*m_t+1];
  gfEl_t VStorage[2*m_t+1];
  gfEl_t WStorage[2*m_t+1];
  gfEl_t XStorage[2*m_t+1];
  gfPoly_t m_generator;
  bool m_valid;

  inline gfEl_t divEl(const gfEl_t a, const gfEl_t b) const {
    int logA;
    int logInvB;

    if(a == 0) {
      return 0;
    }

    if(b == 0) {
      return 0;
    }

    logA    = m_logTable[a];
    logInvB = Q - m_logTable[b] - 1;

    return m_powTable[(logA + logInvB) % (Q-1)];
  }

  inline gfEl_t multEl(gfEl_t a, gfEl_t b) const {
    int logA;
    int logB;

    if(a == 0) {
      return 0;
    }

    if(b == 0) {
      return 0;
    }

    logA = m_logTable[a];
    logB = m_logTable[b];

    return m_powTable[(logA + logB) % (Q-1)];
  }

  static uint32_t modGF2Poly(uint32_t a, uint32_t b) {
    int c = 1;
    int i;

    if(b == 0) {
      fprintf(stderr, "%s: Division by zero.\n", __func__);
      return 0;
    }

    while(!(b & 0x00000100)) {
      ++c;
      b <<= 1;
    }

    for(i=0; i<c; ++i) {
      if(a & 0x00000100) {
        a ^= b;
      }
      a <<= 1;
    }

    return a >> c;

  };

  template <typename a_t, typename b_t, typename r_t>
  bool multGFPoly(const a_t &a,
                  const b_t &b,
                  r_t &result) const {
    const std::size_t aLen = sizeof(a) / sizeof(a[0]);
    const std::size_t bLen = sizeof(b) / sizeof(b[0]);
    const std::size_t rLen = sizeof(result) / sizeof(result[0]);
    std::size_t i, j;
    std::size_t indexA = aLen, indexB = bLen;
    std::size_t sLen, lLen;
    const gfEl_t *shorter;
    const gfEl_t *longer;
    std::size_t indexR;

    for(i=0; i<aLen; ++i) {
      if(a[i] != 0) {
        indexA = i;
        break;
      }
    }

    for(i=0; i<bLen; ++i) {
      if(b[i] != 0) {
        indexB = i;
        break;
      }
    }

    memset(result, 0, sizeof(result));

    if((indexA == aLen) || (indexB == bLen)) {
      return true;
    }

    if((aLen - indexA) < (bLen - indexB)) {
      shorter = &a[indexA];
      longer  = &b[indexB];
      sLen = aLen - indexA;
      lLen = bLen - indexB;
    } else {
      shorter = &b[indexB];
      longer  = &a[indexA];
      sLen = bLen - indexB;
      lLen = aLen - indexA;
    }

    if((sLen + lLen - 1U) > rLen) {
      return false;
    }

    indexR = rLen-1;

    for(i=0; i < sLen; ++i) {
      gfEl_t el = 0;

      for(j=0; j<=i; ++j) {
        size_t jj = lLen - i -1;
        el ^= multEl(longer[jj + j], shorter[sLen - j - 1]);
      }
      result[indexR--] = el;
    }

    for(; i < lLen; ++i) {
      gfEl_t el = 0;

      for(j=0; j<sLen; ++j) {
        size_t jj = lLen - i - 1;
        el ^= multEl(longer[jj + j], shorter[sLen - j - 1]);
      }
      result[indexR--] = el;
    }

    for(i=0; i<(sLen-1); ++i) {
      gfEl_t el = 0;

      for(j=0; j<(sLen-i-1); ++j) {
        el ^= multEl(longer[sLen-2-i-j], shorter[j]);
      }
      result[indexR--] = el;
    }

    return true;
  }

  template <typename a_t, typename b_t, typename r_t>
  bool modGFPoly(const a_t &a,
                 const b_t &b,
                 r_t &result) const {
    const std::size_t aLen = sizeof(a) / sizeof(a[0]);
    const std::size_t bLen = sizeof(b) / sizeof(b[0]);
    const std::size_t rLen = sizeof(result) / sizeof(result[0]);
    std::size_t i, j;
    std::size_t indexA = aLen, indexB = bLen;

    if(rLen < aLen) {
      return false;
    }

    memset(result, 0, sizeof(result));

    for(i=0; i<aLen; ++i) {
      if((indexA > i) && (a[i] != 0)) {
        indexA = i;
      }
      result[i + rLen - aLen] = a[i];
    }

    for(i=0; i<bLen; ++i) {
      if((indexB > i) && (b[i] != 0)) {
        indexB = i;
      }
    }

    if(indexB == bLen) {
      fprintf(stderr, "%s: Division by zero.\n", __func__);
      return false;
    }

    if(b[indexB] != 1) {
      fprintf(stderr, "%s: b is not normalized\n", __func__);
      return false;
    }

    while(indexA <= indexB) {
      int kk = indexB - indexA;

      for(j=(bLen-1); j>=indexB; --j) {
        result[j - kk] ^= multEl(result[indexA], b[j]);
      }
      result[indexA] = 0;

      ++indexA;
    }

    return true;
  }

  bool divGFPoly(const gfPoly_t &a,
                 const gfPoly_t &b,
                 gfPoly_t &result) const {
    int i, j;
    int indexA = -1, indexB = -1;
    gfEl_t divRes;
    gfPoly_t divWork;

    for(i=(Q-1); i>=0; --i) {
      if((indexA < 0) && (a[i] != 0)) {
        indexA = i;
      }

      if((indexB < 0) && (b[i] != 0)) {
        indexB = i;
      }

      divWork[i] = a[i];
      result[i] = 0;
    }

    if(indexB < 0) {
      fprintf(stderr, "%s: Division by zero.\n", __func__);
      return false;
    }

    divRes = divEl(divWork[indexA], b[indexB]);
    while(indexA >= indexB) {
      int kk = indexA - indexB;

      divRes = divEl(divWork[indexA], b[indexB]);
      for(j=0; j<indexA; ++j) {
        divWork[kk + j] = divWork[kk + j] ^
          multEl(divRes, b[j]);
      }

      divWork[indexA] = 0;
      result[kk] = divRes;

      --indexA;
    }

    return true;
  }

  uint32_t findPrimitivePoly(int degree) const {
    uint32_t poly1;
    uint32_t poly2;
    uint32_t gen;
    uint32_t lowerBound;
    uint32_t upperBound;
    int count;

    lowerBound = 1 << degree;
    upperBound = 1 << (degree + 1);

    for(poly1 = lowerBound; poly1<upperBound; ++poly1) {
      /* Check if poly1 is prime */
      for(poly2=2; poly2<poly1; ++poly2) {
        if(modGF2Poly(poly1, poly2) == 0) {
          break;
        }
      }

      if(poly2 == poly1) {
        gen = 2;
        count = 2;
        while(gen != 1) {
          ++count;
          gen = modGF2Poly(gen << 1, poly1);
        }

        if(count == (1 << degree)) {
          return poly1;
        }
      }
    }

    return 0;
  };

  void genGF() {
    std::size_t i;
    uint32_t el;

    el = 1;

    m_logTable[0] = 0;

    for(i=1; i<Q; ++i) {
      el = modGF2Poly(el, m_primativePoly);
      m_powTable[i-1] = el;
      m_logTable[el] = i-1;

      el = el << 1;
    }

    m_logTable[0]   = Q-1;
    m_powTable[Q-1] = 0;
  }

  void initCode() {
    uint32_t tmp;
    gfEl_t root[2] = {0};
    gfPoly_t genCopy = {0};
    std::size_t i;
    uint32_t m;
    const uint32_t q = 2;
    gfEl_t t1[4] = {1,2,3,4};
    gfEl_t t2[7] = {1,2,3,4,5,6,7};

    multGFPoly(t1, t2, m_generator);

    m = 0;
    tmp = 1;
    while(tmp < Q) {
      tmp *= q;
      ++m;
    }

    m_primativePoly = findPrimitivePoly(m);

    genGF();
    root[0] = 1;
    memset(m_generator, 0, sizeof(gfPoly_t));
    m_generator[n-1] = 1;

    for(i=0; i<(2 * m_t); ++i) {
      memcpy(genCopy, m_generator, sizeof(gfPoly_t));
      root[1] = m_powTable[i+1];
      multGFPoly(root, genCopy, m_generator);
    }
  };

  bool validateConstants() {
    if(Q > 65535) {
      return false;
    }

    if(Q < 2) {
      return false;
    }

    if(n < 3) {
      return false;
    }

    if(n >= Q) {
      return false;
    }

    if(k < 0) {
      return false;
    }

    if(k >= n) {
      return false;
    }

    if((n - k) & 0x01) {
      return false;
    }

    return true;
  }

  template<typename T = gfPoly_t>
  gfEl_t evalPoly(const T &poly, gfEl_t val,
                  std::size_t polyLen = sizeof(T) / sizeof(gfEl_t)) const {
    //const std::size_t polyLen = sizeof(poly) / sizeof(poly[0]);
    gfEl_t result = 0;
    std::size_t i;

    for(i=0; i<polyLen; ++i) {
      result = multEl(val, result) ^ poly[i];
    }

    return result;
  }

  void mea(const gfEl_t *S,
           gfEl_t el[m_t+1], gfEl_t ee[m_t]) {
    std::size_t i;
    std::size_t j;
    int sigma = -1;
    gfEl_t *U = UStorage;
    gfEl_t *V = VStorage;
    gfEl_t *X = XStorage;
    gfEl_t *W = WStorage;
    gfEl_t *uu = U;
    gfEl_t *vv = V;
    gfEl_t *xx = X;
    gfEl_t *ww = W;
    gfEl_t a1, a2;
    int shift = 0;

    /* Initialization */
    memset(U, 0, sizeof(UStorage));
    memset(X, 0, sizeof(XStorage));
    memset(W, 0, sizeof(WStorage));

    U[1] = 1;
    memcpy(V+1, S, sizeof(gfEl_t)*2*m_t);
    V[0] = 0;
    X[2*m_t] = 1;

    for(i=0; i<(2*m_t); ++i) {
      if((V[1] != 0) && (sigma < 0)) {
        sigma = -sigma - 1;
        uu = V;
        vv = U;
        xx = W;
        ww = X;
      } else {
        sigma = sigma - 1;
        uu = U;
        vv = V;
        xx = X;
        ww = W;
      }

      a1 = uu[1];
      a2 = vv[1];

      for(j=0; j<(2*m_t); ++j) {
        vv[j] = multEl(a1, vv[j+1]) ^
          multEl(a2, uu[j+1]);

        xx[j] = multEl(a1, xx[j+1]) ^
          multEl(a2, ww[j+1]);
      }
      vv[2*m_t] = 0;
      xx[2*m_t] = 0;

      V = vv;
      U = uu;
      X = xx;
      W = ww;
    }

    shift = m_t;
    for(i=0; i<(m_t); ++i) {
      if(X[m_t-i] != 0) {
        shift = i;
        break;
      }
    }

    for(i=shift; i<m_t; ++i) {
      el[i] = X[i-shift];
      ee[i] = V[i-shift+1];
    }
    el[i] = X[i-shift];
  };

  void correctErrors(const gfEl_t el[m_t+1],
                     const gfEl_t ee[m_t],
                     gfEl_t *codeWord) {
    gfEl_t d_el[m_t];
    std::size_t i;

    memset(d_el, 0, sizeof(d_el));

    for(i=0; i < (m_t+1)/2; ++i) {
      d_el[m_t-i*2-1] = el[m_t-i*2-1];
    }

    for(i=0; i < n; ++i) {
      gfEl_t xVal;
      gfEl_t val;

      if(i == 0) {
        xVal = 1;
      } else {
        xVal = m_powTable[Q-i-1];
      }

      val = evalPoly(el, xVal, m_t+1);

      if(val == 0) {
        gfEl_t num, denom;
        gfEl_t errVal;

        num = evalPoly(ee, xVal, m_t);
        denom = evalPoly(d_el, xVal, m_t);
        errVal = divEl(num, denom);

        codeWord[n-i-1] ^= errVal;
      }
    }
  }

public:
  virtual bool decode(gfEl_t *codeWord, size_t blockCount) {
    for(size_t i=0; i<blockCount; ++i) {
      if(!decode(&codeWord[i * n])) {
        return false;
      }
    }

    return true;
  }

  virtual bool decode(gfEl_t *codeWord) {
    gfEl_t S[2*m_t];
    std::size_t i;
    gfEl_t errorLocator[m_t+1];
    gfEl_t errorEvaluator[m_t];
    gfEl_t cWord[n];

    if(!m_valid) {
      return false;
    }

    for(size_t i=0; i<n; ++i) {
      cWord[i] = codeWord[i];
    }

    memset(errorLocator, 0, sizeof(errorLocator));
    memset(errorEvaluator, 0, sizeof(errorEvaluator));

    for(i=0; i<(2*m_t); ++i) {
      S[2*m_t-i-1] = evalPoly(cWord, m_powTable[i+1]);
    }

    mea(S, errorLocator, errorEvaluator);

    correctErrors(errorLocator, errorEvaluator,
                  codeWord);

    return true;
  }

  virtual bool encode(const gfEl_t *informationWords,
                      gfEl_t *codeWords, size_t blockCount) {

    for(size_t i=0; i<blockCount; ++i) {
      if(!encode(&informationWords[i * k],
                 &codeWords[i * n])) {
        return false;
      }
    }

    return true;
  }

  virtual bool encode(const gfEl_t *informationWord,
                      gfEl_t *codeWord) {
    std::size_t i;
    gfPoly_t iWord = {0};
    gfPoly_t cWord = {0};

    if(!m_valid) {
      fprintf(stderr, "not valid\n");
      return false;
    }

    for(i=0; i<k; ++i) {
      iWord[i] = informationWord[i];
    }

    modGFPoly(iWord, m_generator, cWord);

    for(i=0; i<k; ++i) {
      codeWord[i] = iWord[i];
    }

    for(i=k; i<n; ++i) {
      codeWord[i] = cWord[i];
    }

    return true;
  }

  static std::size_t getQ() {
    return Q;
  }

  static std::size_t getn() {
    return n;
  }

  static std::size_t getk() {
    return k;
  }

  static std::size_t gett() {
    return (n-k) / 2;
  }

  void printGFPolyLog(const gfPoly_t &poly) const {
    int i;

    for(i=(Q-1); i>=0; --i) {
      if(poly[i] == 0) {
        printf("ZZ ");
      } else {
        printf("%02X ", m_logTable[ poly[i] ]);
      }
    }
    printf("\n");
  };

  template<typename T = gfPoly_t>
  static void printGFPoly(const T &poly) {
    const std::size_t polyLen = sizeof(poly) / sizeof(poly[0]);
    printGFPoly(poly, polyLen);
  }

  static void printGFPoly(const gfEl_t *poly, size_t polyLen) {
    std::size_t i = 0;
    std::size_t c = 0;

    if(polyLen == 0) {
      return;
    }

    printf("{");
    for(; i<(polyLen-1); i++) {
      if(n == 64) {
        printf("%02o ", poly[i]);
      } else {
        printf("%02X ", poly[i]);
      }
      if( ((c++) % 16) == 15) {
        printf("\n ");
      }
    }

    if(n == 64) {
      printf("%02o}\n", poly[i]);
    } else {
      printf("%02X}\n", poly[i]);
    }
  }

  SimpleRS() {
    m_valid = validateConstants();
    if(!m_valid) {
      printf("Invalid\n");
      return;
    }

    initCode();

  };

  bool test(std::size_t groupSize, std::size_t iter) {
    gfEl_t *encoderInput;
    gfEl_t *encoderOutput;
    gfEl_t *decoderInput;
    gfEl_t *decoderOutput;
    std::size_t i;
    std::size_t j;
    std::size_t g;
    int successCount = 0;
    int failCount = 0;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<unsigned int> genVal(0, getQ()-1);
    std::uniform_int_distribution<unsigned int> genErrVal(1, getQ()-1);
    std::uniform_int_distribution<unsigned int> genThresh(1, gett()-1);
    std::uniform_int_distribution<unsigned int> genErr(0, n);
    const size_t t = gett();
    std::size_t *errCounts;


    encoderInput  = new gfEl_t[k * groupSize];
    encoderOutput = new gfEl_t[n * groupSize];
    decoderInput  = new gfEl_t[n * groupSize];
    decoderOutput = new gfEl_t[n * groupSize];

    errCounts = new std::size_t[t+1];
    memset(errCounts, 0, sizeof(std::size_t) * (t+1) );

    for(i=0; i<iter; i += groupSize) {
      for(g=0; g<groupSize; ++g) {
        // Generate Test Input Vector
        for(j=0; j<k; ++j) {
          encoderInput[j + g*k] = genVal(gen);
        }
      }

      // Encode Test Input Vector
      if(!encode(encoderInput, encoderOutput, groupSize)) {
        fprintf(stderr, "Encode Failed!\n");
        return 1;
      }

      // Initialize decoder input with encoder output
      memcpy(decoderInput, encoderOutput,
             sizeof(gfEl_t) * n * groupSize);

      for(g=0; g<groupSize; ++g) {
        // Threshold that determines if an error should be introduced
        unsigned int thresh = genThresh(gen);
        std::size_t actualErrCount = 0;

        for(j=0; j<n; ++j) {
          if(genErr(gen) < thresh) {

            // Introduce error at index j
            gfEl_t errVal = genErrVal(gen);
            decoderInput[j + n*g] ^= errVal;

            // Make sure the total number of errors does not exceed the
            // maximum number of errors that can be corrected
            ++actualErrCount;
            if(actualErrCount == t) {
              break;
            }
          }
        }
        errCounts[actualErrCount] += 1;
      }

      // Initialize decoder output with decoder input
      memcpy(decoderOutput, decoderInput,
             sizeof(gfEl_t) * n * groupSize);

      // Correct errors
      decode(decoderOutput, groupSize);

      for(g=0; g<groupSize; ++g) {
        // Verify that errors were corrected
        if(memcmp(&encoderOutput[n*g], &decoderOutput[n*g], n) == 0) {
          ++successCount;
        } else {
          ++failCount;
        }
      }

      if((i%10000) == (10000 - groupSize)) {
        printf("SuccessCount = %d, failCount = %d\n",
               successCount, failCount);
      }
    }

    for(i=0; i<=t; ++i) {
      printf("Count for %3zd errors: %zd\n",
             i, errCounts[i]);
    }

    if((i%10000) != 0) {
      printf("SuccessCount = %d, failCount = %d\n",
             successCount, failCount);
    }

    delete [] errCounts;
    delete [] encoderInput;
    delete [] encoderOutput;
    delete [] decoderInput;
    delete [] decoderOutput;

    return true;
  }
};
#endif
