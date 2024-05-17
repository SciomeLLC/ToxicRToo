#pragma once

#ifndef normal_IGAMMA_aerts_NCH
#define normal_IGAMMA_aerts_NCH
#include "normalModels.h"
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

#include <gsl/gsl_cdf.h>
#include <gsl/gsl_randist.h>

/*class normalIGAMMA_aerts_BMD_NC : public LL
 * This class defines a normal log-likelihood where
 * the Inverse Gamma model is the model that is fit to the data.
 * NC stands for no covariates, for future extentions that
 */

/*implements the basic model model Y = X*/

class normalIGAMMA_aerts_BMD_NC : public normalLLModel {
public:
  normalIGAMMA_aerts_BMD_NC(Eigen::MatrixXd tY, Eigen::MatrixXd tX, bool SS,
                            bool CV, int junk)
      : normalLLModel(tY, tX, SS, CV){
            // if it is a sufficient statistics model
        };

  virtual Eigen::MatrixXd mean(Eigen::MatrixXd theta) { return mean(theta, X); }
  normalIGAMMA_aerts_BMD_NC(){

  };

  virtual cont_model mean_type() { return cont_model::invgamma_aerts; }

  normalIGAMMA_aerts_BMD_NC(normalIGAMMA_aerts_BMD_NC &M) {
    sufficient_statistics = M.sufficient_statistics;
    Y = M.Y;
    X = M.X;
    constant_variance = M.constant_variance;
  };

  int nParms() {
    if (isConstVar()) {
      return 6; // Hill regression + constant variance
    } else {
      return 7; // Hill regression + variance proportional to mean
    };
  }

  virtual int parameter_to_remove(contbmd TYPE);

  // Inverse-Gamma
  virtual Eigen::MatrixXd mean(Eigen::MatrixXd theta, Eigen::MatrixXd x) {
    double a = theta(0, 0);
    double b = theta(1, 0);
    double c = theta(2, 0);
    double d = theta(3, 0);
    double xi = theta(4, 0);

    Eigen::MatrixXd rV = x;
    rV = rV.unaryExpr([&a, &b, &c, &d, &xi](double xx) {
      //			if(xx == 0){
      //				return a;
      //			}
      return a *
             (1.0 + (c - 1.0) * (gsl_cdf_gamma_Q(pow(xx, -d), xi, 1.0 / b)));
    });

    // Eigen::MatrixXd rV = a*(1 + (c - 1)*(gsl_cdf_gamma_Q(pow(x.array(),
    // -1.0*d), xi, b)));

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
  double negLogLikelihood(Eigen::MatrixXd theta);

private:
  // add BMD specific stuff here
};

#endif
