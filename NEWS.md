# mpoly 1.1.1

## New features

*   `as.function()` is now "vectorized" in a consistent way for both `mpoly` and 
    `mpolyList` objects. See exampels in `?as.function.mpoly` for details.

## Minor improvements and fixes

*   __mpoly__ now is extensively tested, and many bugs have been found and fixed.
*   `bernsteinApprox()` is now deprecated in favor of `bernstein_approx()`.
*   `bezierFunction()` is now deprecatd in favor of `bezier_function()`.
*   More arithmetic is available for `mpolyList` objects, such as single 
    constant times mpolyList.
*   `lissajous()` now allows users to create lissajous polynomials.
