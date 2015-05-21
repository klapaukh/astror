/*
 Copyright (C) 2010  Andrew Cotter and Roman Klapaukh

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */



#include <stdexcept>

#include <Rcpp.h>

#include "squish_luminance.h"
#include "solve_poisson.h"


using namespace Rcpp;

//' Squishes the contrast in an input luminance matrix. 
//' A great deal of tweaking of the
//' "alpha", "beta", "delta" and "theta" arguments is usually required before one
//' gets pleasing results.
//'
//' The HDR contrast processing is performed using Andrew Cotter's implementation of:
//' Raanan Fattal, Dani Lischinski and Michael Werman. "Gradient Domain 
//' High Dynamic Range Compression\". Proceedings of the 29th annual
//' conference on computer graphics and interactive techniques. Pages 249-256. 2002.
//' The alpha, beta and saturation parameters are analagous to those described in
//' the paper, except that in this implementation, alpha is a proportion of the
//' average gradient magnitude, and beta is what would be 1-beta in the paper.
//' Gamma is the maximum gradient attenuation factor at a given level, which is
//' needed in order to cope with small gradients. The paper isn't clear on whether
//' the average gradient magnitudes used for the alpha scaling should be the
//' overall averages, or the average only over the current level in the pyramid. The author 
//' uses a convex combination of these two averages, mediated by the theta
//' parameter, where 0 chooses the overall average, and 1 the average on the current pyramid level.
//' 
//'
//' @param alpha controls the point at which contrast-magnification gives way
//'        to contrast-attenuation
//' @param beta controls the overall extent to which
//'        contrast-squishing is performed
//' @param theta Increasing theta causes large-scale
//'        (spatially) contrast differences to be given greater importance, relative to
//'        small-scale differences. 
//' @param delta Changing delta seldom has any great effect. 
//' @param Epsilon is
//'        the termination threshold--increasing it will reduce the quality of the
//'        solution, but improve runtime, while decreasing it will have the opposite
//'        effect. One warning: if epsilon is too small, then the solution may never
//'        converge, and you'll need to abort this program.
//' @export
// [[Rcpp::export]]
NumericMatrix gradientDomainHDRCompression(NumericMatrix extractedLuminance, double alpha, double beta, double delta, double theta, double epsilon, double saturation){

  alpha = 0.1;
  beta = 0.1;
  delta = 1.1;
  theta = 0;
  epsilon = 0.0001;
  saturation = 1;

  int rows = extractedLuminance.nrow();
  int columns = extractedLuminance.ncol();

  SharedArray<float> luminance(rows * columns);

  //copy values from extractedLuminance to luminance
  for(int i =0; i < rows * columns ; i++){
    luminance[i] = extractedLuminance[i];
  }


  try{
    SquishLuminance(luminance, rows, columns, epsilon, alpha, beta, delta, theta);
  } catch (std::exception& exception) {
    stop(exception.what());
  }

  NumericMatrix result(rows,columns);

  //copy luminance to result;
  for(int i =0; i < rows*columns; i++){
    result[i]  = luminance[i];
  }


  return result;

}

