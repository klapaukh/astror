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
  return(log(values + (1.2 - m)));
}

#' Linearly scale values to fit in the range 0-1.
#'
#' This applies a linear scale to all of the values in the range such that 
#' after the transformation all values are in the range [0,1]. 
#'
#' @param values the values [matrix or vector] to scale
#' @export
linearScale <- function(values){
  x <- values - min(values) #Set base to Zero
  x <- x / max(x) #Set max to 1
  return(x)
}

#' Geometrically scale a vector or matrix of values to fit in the range [0,1]
#'
#' Applies a geometric scaling function to ensure that all values are within
#' the range [0,1]. The smallest value will always be mapped to 0, and the largest
#' to 1. 
#'
#' @param values the values [vector or matrix] to scale
#' @export
geometricScale <- function(values){
  values <- values - min(values) 
  base   <- (1/2)^(1/max(values))
  values <- base^(-values) - 1 
  return (values)  
}

#' Clamp the lowest values to a percentile
#'
#' @param values the values to remove the bottom tail from
#' @param fraction the fraction of values to clamp up
#' @export
clampBelowPercentile <- function(values,fraction=0.01){
  cutOff <- quantile(values,fraction)
  min = min(values[values > cutOff])
  values[values <= cutOff] = min
  return(values)
}

#' Clamp the highest values to a percentile
#'
#' @param values the values to remove the top tail from
#' @param fraction the fraction of values to clamp down
#' @export
clampAbovePercentile <- function(values,fraction=0.99){
  cutOff <- quantile(values,fraction)
  max = max(values[values < cutOff])
  values[values >= cutOff] = max
  return(values)
}

#' Clamp the lowest and highest values.
#'
#' @param values The values to remove the top and bottom from
#' @param below Percentile below which to clamp up
#' @param above Percentile above which to camp down
#' @export
clampPercentile <- function(values, below=0.01, above=0.99){
  cutOff <- quantile(values, c(below, above))

  top = max(values[values < cutOff[2] ])
  bottom = min(values[values > cutOff[1]])

  values[ values >= cutOff[2]] = top
  values[ values <= cutOff[1]] = bottom 

  return(values)
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
  if(any(missing(red),missing(green), missing(blue))){
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
luminanceToColour <- function(red, green, blue, luminance, saturation=1) {
  if (saturation <= 0){
    stop("saturation must be positive");
  }

  red   = red   + 0.1 - min(0,red) 
  green = green + 0.1 - min(0,green)
  blue  = blue  + 0.1 - min(0,blue)
       
  intensity = (red + green + blue) / 3;
 
  red   = saturation * slog(red   / intensity) + luminance;
  green = saturation * slog(green / intensity) + luminance;
  blue  = saturation * slog(blue  / intensity) + luminance;

  max = max(red,green,blue)

  red   = exp(red   - max);
  green = exp(green - max);
  blue  = exp(blue  - max);

  return(array(c(red, green, blue), c(dim(red),3)));
}


#' Convert modified luminance values to the range [0,1] so they can be used to
#' actually draw an image.
#'
#' @param luminance luminance values to re-exponentiate and make valid for
#' use in image creation.
#' @export
luminanceToGray <- function(luminance) {
  maximum = max(luminance)
  result = exp(luminance - maximum)
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



