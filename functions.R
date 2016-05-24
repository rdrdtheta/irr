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