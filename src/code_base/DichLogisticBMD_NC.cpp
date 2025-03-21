#include "DichLogisticBMD_NC.h"

#ifdef R_COMPILATION
// necessary things to run in R
#include <RcppGSL.h>
// necessary things to run in R
// #ifdef ToxicR_DEBUG
// #pragma GCC diagnostic push
// #pragma GCC diagnostic ignored "-Wignored-attributes"
// #include <RcppEigen.h>
// #pragma GCC diagnostic pop
// #else
#include <RcppEigen.h>
// #endif
#else
#include <Eigen/Dense>
#endif

double LOGISTIC_BMD_EXTRA_NC_INEQUALITY(Eigen::MatrixXd theta, void *data) {
  logistic_inequality *M = (logistic_inequality *)data;
  double inequality = M->inequality;
  double BMD = M->BMD;
  double BMR = M->BMR;
  bool geq = M->geq;

  double a = LOGISTIC_A(theta(0, 0));
  double Z = LOGISTIC_EXTRA_Z(a, BMR); // note BMD is a placeholder
  Z = Z / BMD;
  double rV = 0.0;
  rV = (geq) ? inequality - Z : Z - inequality;
  return rV;
}

double LOGISTIC_BMD_ADDED_NC_INEQUALITY(Eigen::MatrixXd theta, void *data) {
  logistic_inequality *M = (logistic_inequality *)data;
  double inequality = M->inequality;
  double BMD = M->BMD;
  double BMR = M->BMR;
  bool geq = M->geq;

  double a = LOGISTIC_A(theta(0, 0));
  double Z = LOGISTIC_ADDED_Z(a, BMR);
  Z = pow(Z, a) / pow(BMD, a);
  double rV = 0.0;

  rV = (geq) ? inequality - Z : Z - inequality;

  return rV;
}
