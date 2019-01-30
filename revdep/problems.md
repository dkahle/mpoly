# algstat

Version: 0.0.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    hierarchical: no visible global function definition for ‘terms’
    hierarchical: no visible global function definition for ‘loglin’
    hierarchical: no visible global function definition for ‘dmultinom’
    markov: no visible global function definition for ‘download.file’
    metropolis: no visible global function definition for ‘runif’
    mpolyListToMat : <anonymous>: no visible global function definition for
      ‘runif’
    polyOptim: no visible global function definition for ‘deriv’
    print.hierarchical: no visible binding for global variable ‘sd’
    spectral : summarize: no visible global function definition for ‘sd’
    subsets : <anonymous>: no visible global function definition for
      ‘combn’
    Undefined global functions or variables:
      combn deriv dmultinom download.file loglin runif sd terms
    Consider adding
      importFrom("stats", "deriv", "dmultinom", "loglin", "runif", "sd",
                 "terms")
      importFrom("utils", "combn", "download.file")
    to your NAMESPACE file.
    ```

# mvp

Version: 1.0-2

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/aac.R’ failed.
    Last 13 lines of output:
      
      Attaching package: 'mvp'
      
      The following object is masked from 'package:base':
      
          drop
      
      > 
      > 
      > P <- as.mvp("1+x+x^3*z+4x^2*y+ 4*z^4")^3
      Error in is_ok_mvp(vars, powers, coeffs) : <text>:1:12: unexpected symbol
      1: 1+x+x^3*z+4x
                     ^
      Calls: as.mvp ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'mvp'
    
    The following object is masked from 'package:base':
    
        drop
    
    Quitting from lines 81-84 (mvp.Rmd) 
    Error: processing vignette 'mvp.Rmd' failed with diagnostics:
    <text>:1:2: unexpected symbol
    1: 3x
         ^
    Execution halted
    ```

# symmoments

Version: 1.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    integrate.polynomial : f: no visible global function definition for
      ‘dmvnorm’
    Undefined global functions or variables:
      dmvnorm
    ```

