# mpoly 1.1.2

## New features

*   `coef()` allows you to extract the coefficients of an mpoly object (#25, 
    thanks @ggrothendieck).
    
*   `deriv()` now allows you to not bring the power down as a multiplier.

*   `normalize_coefficients()` now allows you normalize term coefficients 
    according to an input norm, defaulting to the Euclidean norm.
    
*   `as.function()` methods are now exported.

*   `mpoly` objects now get a plot method (2 variables only).



# mpoly 1.1.1

## New features

*   `as.function()` is now "vectorized" in a consistent way for both `mpoly` and 
    `mpolyList` objects. See examples in `?as.function.mpoly` for details.

## Minor improvements and fixes

*   __mpoly__ now is extensively tested, and many bugs have been found and fixed.
*   `bernsteinApprox()` is now deprecated in favor of `bernstein_approx()`.
*   `bezierFunction()` is now deprecated in favor of `bezier_function()`.
*   More arithmetic is available for `mpolyList` objects, such as single 
    constant times mpolyList.
*   `lissajous()` now allows users to create lissajous polynomials.
