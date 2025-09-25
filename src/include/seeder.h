#define STRICT_R_HEADERS
#pragma once
#ifndef SEEDER
#define SEEDER

// Cross-platform thread-local storage
#if defined(_MSC_VER) || defined(__MINGW32__)
#define THREAD_LOCAL __declspec(thread)
#elif defined(__GNUC__) || defined(__clang__)
#define THREAD_LOCAL thread_local
#else
#define THREAD_LOCAL // Fallback for unsupported compilers
#endif

//#include <Rcpp.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <mutex>
#include <thread>
#include <vector>
#include <nlopt.hpp>
#include <iostream>
#ifndef NO_OMP
#include <omp.h>  
#endif

class Seeder {
private:
  static int globalSeed;              // Global seed shared by all instances
  static std::mutex seedMutex;        // Thread safety for seed setting
  THREAD_LOCAL static gsl_rng *rng;   // Thread-local RNG
  const gsl_rng_type *T = gsl_rng_mt19937;
  int max_threads = 1;
  std::vector<gsl_rng*> rngs;

public:
  // Constructor - uses global seed
  Seeder() {
    std::cout<<"Creating new Seeder with global seed: "<< globalSeed<<std::endl;
    initializeThreadRNG();
  }
  
  // Static method to set global seed (call once, affects all instances)
  static void setGlobalSeed(int seed) {
    std::lock_guard<std::mutex> lock(seedMutex);
    globalSeed = seed;
    nlopt_srand(seed);
    std::cout<<"Global seed set to: "<< seed<<std::endl;
  }
  
  // Static method to get current global seed
  static int getGlobalSeed() {
    std::lock_guard<std::mutex> lock(seedMutex);
    return globalSeed;
  }
  
private:
  void initializeThreadRNG() {
    if (!rng) {
      rng = gsl_rng_alloc(T);
      #ifndef NO_OMP
      int thread_num = omp_get_thread_num();
      gsl_rng_set(rng, globalSeed + thread_num);
      std::cout<<"Thread "<<thread_num<<" RNG initialized with seed: "<<(globalSeed + thread_num)<<std::endl;
      #else
      gsl_rng_set(rng, globalSeed);
      std::cout<<"Main thread RNG initialized with seed: "<<globalSeed<<std::endl;
      #endif
    }
  }
  
  // Copy constructor and assignment operator can be enabled if needed
  // For now, keeping them deleted for safety
  Seeder(Seeder const &) = delete;
  Seeder &operator=(Seeder const &) = delete;

  void reset_max_threads(int threads) {
  #ifndef NO_OMP
    if(max_threads < threads) {
      max_threads = threads;
      // Thread-local RNGs are automatically initialized when accessed
      // No need to pre-allocate since each thread creates its own
    }
  #endif
  }

public:
  // Destructor - public so objects can be properly destroyed
  ~Seeder() {
    std::cout<<"Destroying Seeder and cleaning up thread-local RNG"<<std::endl;
    // Thread-local RNG cleanup is automatic when thread ends
    // No need for explicit cleanup in destructor
  }
  // Get thread-specific RNG - automatically initializes if needed
  gsl_rng* get_thread_rng() {
    if (!rng) {
      initializeThreadRNG();
    }
    return rng;
  }

  double get_uniform() {
    gsl_rng* current_rng = get_thread_rng();
    return gsl_rng_uniform(current_rng);
  }

  double get_gaussian_ziggurat() {
    gsl_rng* current_rng = get_thread_rng();
    return gsl_ran_gaussian_ziggurat(current_rng, 1.0);
  }

  double get_ran_flat() {
    gsl_rng* current_rng = get_thread_rng();
    return gsl_ran_flat(current_rng, -1, 1);
  }
};

#endif
