#pragma once

#ifndef lognormal_IEXP_aerts_NCH
#define lognormal_IEXP_aerts_NCH
#include "log_likelihoods.h"
#include "lognormalModels.h"
#ifdef R_COMPILATION
// necessary things to run in R
#ifdef ToxicR_DEBUG
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wignored-attributes"
#include <RcppEigen.h>
#pragma GCC diagnostic pop
#else
#include <RcppEigen.h>
#endif
#include <RcppGSL.h>
#else
#include <Eigen/Dense>

#endif

#include <gsl/gsl_randist.h>

/*class lognormalIEXP_aerts_BMD_NC : public LL
 * This class defines a normal log-likelihood where
 * the Inverse-EXP model (Aerts version) is the model that is fit to the data.
 * NC stands for no covariates, for future extentions that
 */

/*implements the basic model model Y = X*/

class lognormalIEXP_aerts_BMD_NC : public lognormalLLModel {
public:
  lognormalIEXP_aerts_BMD_NC(Eigen::MatrixXd tY, Eigen::MatrixXd tX, bool SS,
                             int junk)
      : lognormalLLModel(tY, tX, SS){
            // if it is a sufficient statistics model
        };

  lognormalIEXP_aerts_BMD_NC(Eigen::MatrixXd tY, Eigen::MatrixXd tX, bool SS,
                             bool CV, int junk)
      : lognormalLLModel(tY, tX, SS){
            // if it is a sufficient statistics model
        };

  virtual Eigen::MatrixXd mean(Eigen::MatrixXd theta) { return mean(theta, X); }
  lognormalIEXP_aerts_BMD_NC(){

  };

  virtual cont_model mean_type() { return cont_model::invexp_aerts; }

  lognormalIEXP_aerts_BMD_NC(lognormalIEXP_aerts_BMD_NC &M) {
    sufficient_statistics = M.sufficient_statistics;
    Y = M.Y;
    X = M.X;
  };

  int nParms() { return 5; }

  virtual int parameter_to_remove(contbmd TYPE);
  virtual int type_of_profile(contbmd TYPE);

  //	virtual Eigen::MatrixXd mmean(Eigen::MatrixXd theta,Eigen::MatrixXd x){
  //		double a = theta(0,0);
  //		double b = theta(1,0);
  //		double c  = theta(2,0);
  //		double d = theta(3,0);
  //
  //		Eigen::MatrixXd rV = a*(1.0 + (c - 1.0)*(exp(-1.0*b * pow(x.array(),
  //-1.0*d))));
  //
  //		return exp(log(rV.array()) + 0.5 * exp(theta(4,0)));
  //	}
  // Inverse-Exponential
  virtual Eigen::MatrixXd mean(Eigen::MatrixXd theta, Eigen::MatrixXd x) {
    double a = theta(0, 0);
    double b = theta(1, 0);
    double c = theta(2, 0);
    double d = theta(3, 0);

    Eigen::MatrixXd rV =
        a * (1.0 + (c - 1.0) * (exp(-1.0 * b * pow(x.array(), -1.0 * d))));

    // return log(rV.array()) + 0.5 * exp(theta(4,0));
    return rV;
  }

  // return true if it is a increasing function

  // BASIC BMD Computation absolute to hybrid
  double bmd_absolute(Eigen::MatrixXd theta, double BMRF, bool isIncreasing);
  double bmd_stdev(Eigen::MatrixXd theta, double BMRF, bool isIncreasing);
  double bmd_reldev(Eigen::MatrixXd theta, double BMRF, bool isIncreasing);
  double bmd_point(Eigen::MatrixXd theta, double BMRF, bool isIncreasing);
  double bmd_extra(Eigen::MatrixXd theta, double BMRF, bool isIncreasing);
  double bmd_hybrid_extra(Eigen::MatrixXd theta, double BMRF, bool isIncreasing,
                          double BPROB);

  // BASIC BMD Computation absolute to hybrid
  double bmd_absolute_bound(Eigen::MatrixXd theta, double BMD, double BMRF,
                            bool isIncreasing);
  double bmd_stdev_bound(Eigen::MatrixXd theta, double BMD, double BMRF,
                         bool isIncreasing);
  double bmd_reldev_bound(Eigen::MatrixXd theta, double BMD, double BMRF,
                          bool isIncreasing);
  double bmd_point_bound(Eigen::MatrixXd theta, double BMD, double BMRF,
                         bool isIncreasing);
  double bmd_extra_bound(Eigen::MatrixXd theta, double BMD, double BMRF,
                         bool isIncreasing);
  double bmd_hybrid_extra_bound(Eigen::MatrixXd theta, double BMD, double BMRF,
                                bool isIncreasing, double TAIL_PROB);

  ///
  /*Eigen::MatrixXd bmd_start_extra_hybrid(Eigen::MatrixXd theta, double BMD,
                                                                                  double BMRF, bool isIncreasing,
                                                                                  double TAIL_PROB);*/
  //
  virtual double bmd_start_absolute(unsigned n, const double *b, double *grad,
                                    void *data);
  virtual std::vector<double> bmd_start_absolute_clean(std::vector<double> x,
                                                       double BMRF, double BMD,
                                                       bool isIncreasing);

  //
  /*Eigen::MatrixXd bmd_start_stdev(Eigen::MatrixXd theta, double BMD,
                                                                  double BMRF,
  bool isIncreasing, double TAIL_PROB);
  //*/
  virtual double bmd_start_reldev(unsigned n, const double *b, double *grad,
                                  void *data);

  virtual std::vector<double> bmd_start_reldev_clean(std::vector<double> x,
                                                     double BMRF, double BMD,
                                                     bool isIncreasing);

  /////////////////////////////////////////////////////////////////////////
  // standard deviation starting value code
  //
  /////////////////////////////////////////////////////////////////////////
  virtual double bmd_start_stddev(unsigned n, const double *b, double *grad,
                                  void *data);

  virtual std::vector<double> bmd_start_stddev_clean(std::vector<double> x,
                                                     double BMRF, double BMD,
                                                     bool isIncreasing);

  /////////////////////////////////////////////////////////////////////////
  // standard deviation starting value code
  /////////////////////////////////////////////////////////////////////////

  virtual double bmd_start_extra(unsigned n, const double *b, double *grad,
                                 void *data);

  virtual std::vector<double> bmd_start_extra_clean(std::vector<double> x,
                                                    double BMRF, double BMD,
                                                    bool isIncreasing);

  /////////////////////////////////////////////////////////
  virtual double bmd_start_point(unsigned n, const double *b, double *grad,
                                 void *data);

  virtual std::vector<double> bmd_start_point_clean(std::vector<double> x,
                                                    double BMRF, double BMD,
                                                    bool isIncreasing);

  /////////////////////////////////////////////////////////

  virtual double bmd_start_hybrid_extra(unsigned n, const double *b,
                                        double *grad, void *data);

  std::vector<double> bmd_start_hybrid_extra_clean(std::vector<double> x,
                                                   double BMRF, double BMD,
                                                   bool isIncreasing,
                                                   double tail_prob);

  //	virtual int type_of_profile(contbmd TYPE);
  // double negLogLikelihood(Eigen::MatrixXd theta);

private:
  // add BMD specific stuff here
};

#endif
