#include <seeder.h>

// Static member definitions
int Seeder::globalSeed = 12;  // Default seed
std::mutex Seeder::seedMutex;
THREAD_LOCAL gsl_rng* Seeder::rng = nullptr;
