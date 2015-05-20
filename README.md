# astror

This is an R library that aims to provide some helpful routines for displaying radio telescope images.
The library aims to primarily provide some easy interfaces to some simple scaling routines.

This library uses code from [Andrew Cotter's HDR Tools library](http://ttic.uchicago.edu/~cotter/projects/hdr_tools/) to do gradient domain high dymanic range compression.

##Example usage

```{R} 
library(FITSio)
library(astror)

fits <- readFits("a.fits")
imageData = fits$imDat

plot.new()
rasterImage(slog(imageData))
```

