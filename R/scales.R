#' Log scale the values. 
#'
#' The applies a log scale to all the values after shifting them all up [or
#' down] such that they the lowest value is about 1.01. This ensures that the
#' logarithm behaves in a sensible fashion, and can be applied repeatedly.
#'
#' @param values the values [or matrix of values] to log scale
#' @export
slog <- function(values){
  m <- min(values);
  return(log(values - (1.01 - m)));
}



#' This is a function to extract log luminance values from the a set of RGB
#' channels for use with the gradient domain high dynamic range compression
#' algorithm. 
#'
#' @param red The red channel
#' @param green The green channel
#' @param blue The blue channel
#' @export
extractLuminance <- function(red, green, blue) {
  if(!( all(dim(red) == dim(green)) & all(dim(green) == dim(blue)))){
    stop("red, green, and blue must all be the same size")
  }
  if(any(missing(red),missing(green), missing(blue)){
     stop("A red, green, and blue must all be provided")
  }

  #If any values are below zero, shift all the numbers up so zero is the least
  #value
  red = red - min(0,red) 
  green = green - min(0,green)
  blue = blue - min(0,blue)

  #This gives us the log of the luminance which we need of the compression
  intensity = slog((red + green + blue) / 3)

  return(intensity)
}

#' Function to safely log the lumininance values for processing 
#'
#' @param values vector of luminence values to compress for processing
#' @export
logLuminance <- slog;

#' Function to apply the new luminance values to the original image.
#'
#' @param red The red channel
#' @param green The green channel
#' @param blue The blue channel
#' @param luminance The new luminance values
#' @param Saturation constant (must be > 0)
#' @export
luminanceToColour <- function(red, green, blue, luminance, saturation) {
  if (saturation <= 0){
    stop("saturation must be positive");
  }
        
  intensity = extractLuminance(red,green,blue);

  red = red - min(0,red) 
  green = green - min(0,green)
  blue = blue - min(0,blue)

  newRed   = saturation * slog(red   / intensity) + luminance;
  newGreen = saturation * slog(green / intensity) + luminance;
  newblue  = saturation * slog(blue  / intensity) + luminance;

  max = max(newRed,newGreen,newBlue)

  newRed   = exp(newRed   - maximum);
  newGreen = exp(newGreen - maximum);
  newBlue  = exp(newBlue  - maximum);

  return(list(red = newRed, green = newGreen, blue = newBlue));
}


#' Convert modified luminance values to the range [0,1] so they can be used to
#' actually draw an image.
#'
#' @param luminance luminance values to re-exponentiate and make valid for
#' use in image creation.
#' @export
luminanceToGray <- function(luminance) {
  maximum = max(luminance)
  result = exp(result - maximum)
  return(result);
}


#' astror is a package for scaling fits images into a range displayable by a
#' standard monitor.
#' 
#' @import Rcpp
#' @useDynLib astror
#' @importFrom Rcpp sourceCpp
#' @docType package
#' @name astror
NULL



