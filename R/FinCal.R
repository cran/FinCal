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
#' @seealso \code{\link{fv}}
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
#' @param pmt payment per period
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{pv}}
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
#' @param pmt payment per period
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{fv}}
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
#' @param pmt payment per period
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{r.perpetuity}}
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
#' @param pmt payment per period
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{pv.simple}}
#' @seealso \code{\link{pv.annuity}}
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
#' @param pmt payment per period
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{fv.simple}}
#' @seealso \code{\link{fv.annuity}}
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

#' Computing the future value of an uneven cash flow series
#'
#' @param r stated annual rate
#' @param cf uneven cash flow
#' @seealso \code{\link{fv.simple}}
#' @export
#' @examples
#' fv.uneven(r=0.1, cf=c(-1000, -500, 0, 4000, 3500, 2000))
fv.uneven <- function(r,cf){
  m <- length(cf)
  sum <- 0
  for(i in 1:m){
    n <- m - i
    sum <- sum + fv.simple(r,n,cf[i])
  }
  return(sum)
}

#' Computing the present value of an uneven cash flow series
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param cf uneven cash flow
#' @seealso \code{\link{pv.simple}}
#' @seealso \code{\link{npv}}
#' @export
#' @examples
#' pv.uneven(r=0.1, cf=c(-1000, -500, 0, 4000, 3500, 2000))
pv.uneven <- function(r,cf){
  n <- length(cf)
  sum <- 0
  for(i in 1:n){
    sum <- sum + pv.simple(r,i,cf[i])
  }
  return(sum)
}

#' Estimate period payment
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param n number of periods
#' @param pv present value
#' @param fv future value
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{pv}}
#' @seealso \code{\link{fv}}
#' @seealso \code{\link{n.period}}
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
#' @param pmt payment per period
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{pv}}
#' @seealso \code{\link{fv}}
#' @seealso \code{\link{pmt}}
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

#' Computing the rate of return for each period
#'
#' @param n number of periods
#' @param pv present value
#' @param fv future value
#' @param pmt payment per period
#' @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
#' @seealso \code{\link{fv.simple}}
#' @seealso \code{\link{fv.annuity}}
#' @export
#' @examples
#' discount.rate(n=5,pv=0,fv=600,pmt=-100,type=0)
discount.rate <- function(n,pv,fv,pmt,type=0){
r <- seq(0,1,by=0.0001)
m <- length(r)

if (fv.simple(r[2],n,pv) + fv.annuity(r[2],n,pmt,type) < fv){
flag <- 1
}else{
flag <- 0
}

for(i in 2:m){
sumfv <- fv.simple(r[i],n,pv) + fv.annuity(r[i],n,pmt,type)
error <- abs(sumfv-fv)

	if(flag == 1){
		if(error < 0.001 || sumfv > fv){
		break
		}
	}else{
		if(error < 0.001 || sumfv < fv){
		break
		}
	}
}
return(r[i])
}

#' Computing NPV, the PV of the cash flows less the initial (time = 0) outlay
#'
#' @param r discount rate, or the interest rate at which the amount will be compounded each period
#' @param cf cash flow,the first cash flow is the initial outlay
#' @seealso \code{\link{pv.simple}}
#' @seealso \code{\link{pv.uneven}}
#' @export
#' @examples
#' npv(r=0.12, cf=c(-5, 1.6, 2.4, 2.8))
npv <- function(r,cf){
  n <- length(cf)
  subcf <- cf[2:n]
  return(-1 * pv.uneven(r, subcf) + cf[1])
}

#' Computing IRR, the internal rate of return
#' 
#' @param cf cash flow,the first cash flow is the initial outlay
#' @seealso \code{\link{pv.uneven}}
#' @export
#' @examples
#' irr(cf=c(-5, 1.6, 2.4, 2.8))
irr <- function(cf){
  r <- seq(0,1,by=0.0001)
  m <- length(r) 
  n <- length(cf)
  subcf <- cf[2:n]
  for(i in 2:m){
    npv <- -1 * pv.uneven(r[i], subcf) + cf[1]
    if(abs(npv) < 0.0001 || npv < 0){
      break
    }
  }
  return(r[i])
}

#' Rate of return for a perpetuity
#'
#' @param pmt payment per period
#' @param pv present value
#' @seealso \code{\link{pv.perpetuity}}
#' @export
#' @examples
#' r.perpetuity(pmt=4.5,pv=-75) 
r.perpetuity <- function(pmt, pv){
  return(-1 * pmt / pv) 
}

#' Convert stated annual rate to the effective annual rate
#'
#' @param r stated annual rate
#' @param m number of compounding periods per year
#' @seealso \code{\link{ear.continuous}}
#' @seealso \code{\link{hpr2ear}}
#' @seealso \code{\link{ear2bey}}
#' @seealso \code{\link{ear2hpr}}
#' @export
#' @examples
#' ear(r=0.12,m=12)
#' ear(0.04,365)
ear <- function(r,m){
  return((1 + r / m)^m - 1)
}

#' Convert holding period return to the effective annual rate
#'
#' @param hpr holding period return
#' @param t number of days remaining until maturity
#' @seealso \code{\link{ear}}
#' @seealso \code{\link{hpr}}
#' @seealso \code{\link{ear2hpr}}
#' @export
#' @examples
#' hpr2ear(hpr=0.015228,t=120)
hpr2ear <- function(hpr,t){
  return((1 + hpr)^(365/t) - 1)
}

#' Convert stated annual rate to the effective annual rate with continuous compounding
#'
#' @param r stated annual rate
#' @seealso \code{\link{ear}}
#' @export
#' @examples
#' ear.continuous(r=0.1)
#' ear.continuous(0.03)
ear.continuous <- function(r){
  return(exp(r) - 1)
}

#' Convert a given norminal rate to a continuous compounded rate 
#'
#' @param r norminal rate
#' @param m number of times compounded each year
#' @seealso \code{\link{r.norminal}}
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
#' @seealso \code{\link{r.continuous}}
#' @export
#' @examples
#' r.norminal(0.03,1)
#' r.norminal(0.03,4)
r.norminal <- function(rc,m) {
  return(m*(exp(rc/m)-1))
}

#' Computing HPR, the holding period return
#' 
#' @param ev ending value
#' @param bv beginning value
#' @param cfr cash flow received
#' @seealso \code{\link{twrr}}
#' @seealso \code{\link{hpr2ear}}
#' @seealso \code{\link{hpr2mmy}}
#' @export
#' @examples
#' hpr(ev=33,bv=30,cfr=0.5)
hpr <- function(ev,bv,cfr=0){
  return((ev-bv+cfr)/bv)
}

#' Computing HPR, the holding period return
#' 
#' @param mmy money market yield
#' @param t number of days remaining until maturity
#' @seealso \code{\link{bdy2mmy}}
#' @seealso \code{\link{hpr2mmy}}
#' @seealso \code{\link{hpr}}
#' @export
#' @examples
#' mmy2hpr(mmy=0.04898,t=150)
mmy2hpr <- function(mmy, t){
  return(mmy * t / 360)
}

#' Computing HPR, the holding period return
#' 
#' @param ear effective annual rate
#' @param t number of days remaining until maturity
#' @seealso \code{\link{hpr2ear}}
#' @seealso \code{\link{ear}}
#' @seealso \code{\link{hpr}}
#' @export
#' @examples
#' ear2hpr(ear=0.05039,t=150)
ear2hpr <- function(ear, t){
  return((1 + ear)^(t/365) - 1)
}

#' Computing TWRR, the time-weighted rate of return
#' 
#' @param ev ordered ending value list 
#' @param bv ordered beginning value list
#' @param cfr ordered cash flow received list
#' @seealso \code{\link{hpr}}
#' @export
#' @examples
#' twrr(ev=c(120,260),bv=c(100,240),cfr=c(2,4))
twrr <- function(ev,bv,cfr){
  r=length(ev)
  s=length(bv)
  t=length(cfr)
  wr=1
  if(r != s || r != t || s != t){
    stop("Different number of values!")
  }else{
    for(i in 1:r){
      wr = wr * (hpr(ev[i],bv[i],cfr[i]) + 1)
    }
    return(wr^(1/r) - 1)
  }
}

#' Computing bank discount yield (BDY) for a T-bill
#' 
#' @param d the dollar discount, which is equal to the difference between the face value of the bill and the purchase price 
#' @param f the face value (par value) of the bill
#' @param t number of days remaining until maturity
#' @seealso \code{\link{bdy2mmy}}
#' @export
#' @examples
#' bdy(d=1500,f=100000,t=120)
bdy <- function(d,f,t){
  return(360 * d / f / t)
}

#' Computing money market yield (MMY) for a T-bill
#' 
#' @param hpr holding period return
#' @param t number of days remaining until maturity
#' @seealso \code{\link{hpr}}
#' @seealso \code{\link{mmy2hpr}}
#' @export
#' @examples
#' hpr2mmy(hpr=0.01523,t=120)
hpr2mmy <- function(hpr,t){
  return(360 * hpr / t)
}

#' Computing money market yield (MMY) for a T-bill
#' 
#' @param bdy bank discount yield
#' @param t number of days remaining until maturity
#' @seealso \code{\link{bdy}}
#' @export
#' @examples
#' bdy2mmy(bdy=0.045,t=120)
bdy2mmy <- function(bdy,t){
  return(360 * bdy /(360 - t * bdy))
}

#' bond-equivalent yield (BEY), 2 x the semiannual discount rate
#' 
#' @param hpr holding period return
#' @param t number of month remaining until maturity
#' @seealso \code{\link{hpr}}
#' @export
#' @examples
#' hpr2bey(hpr=0.02,t=3)
hpr2bey <- function(hpr,t){
  return(((1 + hpr)^(6/t) -1)*2)
}

#' bond-equivalent yield (BEY), 2 x the semiannual discount rate
#' 
#' @param ear effective annual rate
#' @seealso \code{\link{ear}}
#' @export
#' @examples
#' ear2bey(ear=0.08)
ear2bey <- function(ear){
  return(((1 + ear)^0.5 - 1)*2)
}



