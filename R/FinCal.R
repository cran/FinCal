#' Estimate present value (pv) of a single sum
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param fv future value
#' @seealso \code{\link{pv}}
#' @export
#' @examples
#' pv.simple(0.07,10,100)
#' pv.simple(r=0.03,n=3,fv=1000)
pv.simple <- function(r,n,fv){
  return((fv/(1+r)^n)*(-1))
}

#' Estimate future value (fv) of a single sum
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param pv present value
#' @export
#' @examples
#' fv.simple(0.08,10,-300)
#' fv.simple(r=0.04,n=20,pv=-50000)
fv.simple <- function(r,n,pv){
  return((pv * (1 + r)^n)*(-1))
}

#' Estimate present value (pv) of an annuity
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param pmt period payment
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @export
#' @examples
#' pv.annuity(0.03,12,1000)
#' pv.annuity(r=0.0425,n=3,pmt=30000)
pv.annuity <- function(r, n, pmt, type=0) {
  if(type != 0 && type !=1){
    print("Error: type should be 0 or 1!")
  }else{
  pv = (pmt / r * (1 - 1 / (1 + r)^n))*(1 + r)^type * (-1)
  return(pv)
  }
}

#' Estimate future value of an annuity
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param pmt period payment
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @export
#' @examples
#' fv.annuity(0.03,12,-1000)
#' fv.annuity(r=0.03,n=12,pmt=-1000,type=1)
fv.annuity <- function(r,n,pmt,type=0) {
  if(type != 0 && type !=1){
    print("Error: type should be 0 or 1!")
  }else{
  fv = (pmt / r * ((1 + r)^n - 1))*(1+r)^type * (-1)
  return(fv)
  }
}

#' Estimate present value of a perpetuity
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param g growth rate of perpetuity
#' @param pmt period payment
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @export
#' @examples
#' pv.perpetuity(r=0.1,pmt=1000,g=0.02) 
#' pv.perpetuity(r=0.1,pmt=1000,type=1) 
#' pv.perpetuity(r=0.1,pmt=1000) 
pv.perpetuity <- function(r, pmt, g=0, type=0){
  if(type != 0 && type !=1){
    print("Error: type should be 0 or 1!")
  }else{
    if(g >= r){
      print("Error: g is not smaller than r!")
    }else{
      pv <- (pmt / (r - g)) * ((1 + r)^type) * (-1)
      return(pv) 
    }
  }
}

#' Estimate present value (pv)
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param fv future value
#' @param pmt period payment
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @export
#' @examples
#' pv(0.07,10,1000,10)
#' pv(r=0.05,n=20,fv=1000,pmt=10,type=1)
pv <- function(r,n,fv=0,pmt=0,type=0){
  if(type != 0 && type !=1){
    print("Error: type should be 0 or 1!")
  }else{
    pv <- pv.simple(r,n,fv) + pv.annuity(r,n,pmt,type)
    return(pv)
  }
}

#' Estimate future value (fv)
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param pv present value
#' @param pmt period payment
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @export
#' @examples
#' fv(0.07,10,1000,10)
fv <- function(r,n,pv=0,pmt=0,type=0){
  if(type != 0 && type !=1){
    print("Error: type should be 0 or 1!")
  }else{
    return(fv.simple(r,n,pv) + fv.annuity(r,n,pmt,type))
  }
}

#' Estimate period payment
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param pv present value
#' @param fv future value
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @export
#' @examples
#' pmt(0.08,10,-1000,10)
#' pmt(r=0.08,n=10,pv=-1000,fv=0)
#' pmt(0.08,10,-1000,10,1)
pmt <- function(r,n,pv,fv,type=0){
  if(type != 0 && type !=1){
    print("Error: type should be 0 or 1!")
  }else{
  pmt <- (pv+fv/(1+r)^n)*r/(1-1/(1+r)^n) * (-1) * (1+r)^(-1 * type)
  return(pmt)
  }
}

#' Estimate the number of periods
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param pv present value
#' @param fv future value
#' @param pmt period payment
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @export
#' @examples
#' n.period(0.1,-10000,60000000,-50000,0)
#' n.period(r=0.1,pv=-10000,fv=60000000,pmt=-50000,type=1)
n.period <- function(r,pv,fv,pmt,type=0){
  if(type != 0 && type !=1){
    print("Error: type should be 0 or 1!")
  }else{
  n <- log(-1 * (fv*r-pmt* (1+r)^type)/(pv*r+pmt* (1+r)^type))/log(1+r)
  return(n)
  }
}

#' Convert a given norminal rate to a continuous compounded rate 
#'
#' @param r norminal rate
#' @param m number of times compounded each year
#' @export
#' @examples
#' r.continuous(0.03,4)
r.continuous <- function(r,m) {
  return(m*log(1+r/m))
}

#' Convert a given continuous compounded rate to a norminal rate
#'
#' @param rc continuous compounded rate
#' @param m number of desired times compounded each year
#' @export
#' @examples
#' r.norminal(0.03,1)
#' r.norminal(0.03,4)
r.norminal <- function(rc,m) {
  return(m*(exp(rc/m)-1))
}

