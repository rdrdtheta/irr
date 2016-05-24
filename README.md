IRR Function
================

This is a function that solves for an IRR that has identical outputs to the function in Excel

Load in Functions
-----------------

``` r
# net present value that discounts your cash flow vector by a chosen interest rate 
# (period 0 is defaulted to not be discounted)
npv <- function(i, cf, t=seq(along=cf) - 1) sum(cf/(1+i)^t)

# input your cash flow vector and your irr guess (default at 0.01)
irr <- function(cf, p = 0.01) {
  tryCatch(
    return(nlm(function(p){npv(p, cf)^2}, p = p)$estimate)
    , error = function(e) return(0)
  )
}
```

Example Code
------------

Using this function goes as follows:

``` r
cash_flow <- c(-5000, 100, 200, 300, 300, 600, 700, 1400, 1300, 1200, 300, 100)

irr(cf = cash_flow, p = 0.01)
```

    ## [1] 0.03922821
