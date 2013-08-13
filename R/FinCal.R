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
#' @seealso \code{\link{r.norminal}}
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
#' @seealso \code{\link{ear.continuous}}
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

#' Weighted mean as a portfolio return
#' 
#' @param r returns of the individual assets in the portfolio
#' @param w corresponding weights associated with each of the individual assets
#' @export
#' @examples
#' wpr(r=c(0.12, 0.07, 0.03),w=c(0.5,0.4,0.1))
wpr <- function(r,w){
  if(sum(w) != 1){
    warning("sum of weights is NOT equal to 1!")
    return(sum(r*w))
  }else{
    return(sum(r*w))
  }
}

#' Geometric mean return
#' 
#' @param r returns over multiple periods
#' @export
#' @examples
#' geometric.mean(r=c(-0.0934, 0.2345, 0.0892))
geometric.mean <- function(r){
  rs <- r + 1
  return(prod(rs)^(1/length(rs))-1)
}

#' harmonic mean, average price
#' @param p price over multiple periods
#' @export
#' @examples
#' harmonic.mean(p=c(8,9,10))
harmonic.mean <- function(p){
  return(1/mean(1/p))
}

#' Computing Coefficient of variation
#'
#' @param sd standard deviation
#' @param avg average value
#' @seealso \code{\link{Sharpe.ratio}}
#' @export
#' @examples
#' coefficient.variation(sd=0.15,avg=0.39)
coefficient.variation <- function(sd, avg){
  return(sd/avg)
}

#' Computing Sharpe Ratio
#'
#' @param rp portfolio return
#' @param rf risk-free return
#' @param sd standard deviation of portfolio retwns
#' @seealso \code{\link{coefficient.variation}}
#' @seealso \code{\link{SFRatio}}
#' @export
#' @examples
#' Sharpe.ratio(rp=0.038,rf=0.015,sd=0.07)
Sharpe.ratio <- function(rp,rf,sd){
  return((rp-rf)/sd)
}

#' Computing Roy's safety-first ratio
#' 
#' @param rp portfolio return
#' @param rl threshold level return
#' @param sd standard deviation of portfolio retwns
#' @seealso \code{\link{Sharpe.ratio}}
#' @export
#' @examples
#' SFRatio(rp=0.09,rl=0.03,sd=0.12)
SFRatio <- function(rp, rl ,sd){
  return((rp-rl)/sd)
}

#' Computing Sampling error
#' 
#' @param sm sample mean
#' @param mu population mean
#' @export
#' @examples
#' sampling.error(sm=0.45, mu=0.5)
sampling.error <- function(sm,mu){
  return(sm-mu)
}

#' Download stock prices from Yahoo Finance (open, high, low, close, volume, adjusted)
#' 
#' @param symbol symbol of stock, e.g. AAPL, GOOG, SPX
#' @param start start date, e.g., 2013-07-31
#' @param end end date, e.g., 2013-08-06
#' @param freq time interval, e.g., d:daily, w:weekly, m:monthly
#' @seealso \code{\link{get.ohlcs.yahoo}}
#' @seealso \code{\link{get.ohlc.google}}
#' @export
#' @examples
#' get.ohlc.yahoo(symbol="AAPL")
#' @examples
#' get.ohlc.yahoo(symbol="AAPL",start="2013-08-01",freq="d")
#' @examples
#' get.ohlc.yahoo(symbol="AAPL",start="2013-07-01",end="2013-08-01",freq="w")
get.ohlc.yahoo <- function(symbol,start="firstDay",end="today",freq="d"){
  if(start == "firstDay"){
      URL=paste("http://ichart.finance.yahoo.com/table.csv?s=", symbol, sep="")
      dat <- read.csv(URL)
      dat=dat[order(dat$Date),]
  }else{
    temp <- strsplit(start,"-")
    a=sprintf('%.2d',as.integer(temp[[1]][2])-1)
    b=temp[[1]][3]
    c=temp[[1]][1]
    if(end != "today"){
      temp <- strsplit(end,"-")
      d=sprintf('%.2d',as.integer(temp[[1]][2])-1)
      e=temp[[1]][3]
      f=temp[[1]][1]
    }else{
      end=as.character(Sys.Date())
      temp <- strsplit(end,"-")
      d=sprintf('%.2d',as.integer(temp[[1]][2])-1)
      e=temp[[1]][3]
      f=temp[[1]][1]
    }
        URL=paste("http://ichart.finance.yahoo.com/table.csv?s=",symbol,"&a=",a,"&b=",b,"&c=",c,"&d=",d,"&e=",e,"&f=",f,"&g=",freq, sep="")
        dat <- read.csv(URL)
        dat=dat[order(dat$Date),]
  } 
colnames(dat) <- c("date", "open", "high", "low", "close", "volume", "adjusted")
return(dat)
}

#' Download stock prices from Google Finance (open, high, low, close, volume)
#' 
#' @param symbol symbol of stock, e.g. AAPL, GOOG, SPX
#' @param start start date, e.g., 2013-07-31
#' @param end end date, e.g., 2013-08-06
#' @seealso \code{\link{get.ohlc.yahoo}}
#' @seealso \code{\link{get.ohlcs.google}}
#' @export
#' @examples
#' get.ohlc.google(symbol="AAPL")
#' @examples
#' get.ohlc.google(symbol="AAPL",start="2013-08-01")
#' @examples
#' get.ohlc.google(symbol="AAPL",start="2013-07-01",end="2013-08-01")
get.ohlc.google <- function(symbol,start="2013-01-01",end="today"){
  require(RCurl)
    temp <- strsplit(start,"-")
    a=month.abb[as.numeric(temp[[1]][2])]
    b=temp[[1]][3]
    c=temp[[1]][1]
    if(end != "today"){
      temp <- strsplit(end,"-")
      d=month.abb[as.numeric(temp[[1]][2])]
      e=temp[[1]][3]
      f=temp[[1]][1]
    }else{
      end=as.character(Sys.Date())
      temp <- strsplit(end,"-")
      d=month.abb[as.numeric(temp[[1]][2])]
      e=temp[[1]][3]
      f=temp[[1]][1]
    }
    URL=paste("https://www.google.com/finance/historical?q=",symbol,"&output=csv","&startdate=",a,"+",b,"+",c,"&enddate=",d,"+",e,"+",f, sep="")
    myCsv <- getURL(URL, ssl.verifypeer = FALSE)
    dat <- read.csv(textConnection(myCsv))
    colnames(dat) <- c("date", "open", "high", "low", "close", "volume")
    dates=as.character(dat$date)
    for(i in 1:length(dates)){
      tempdates=strsplit(dates[i],"-")
      tempdates[[1]][2]=match(tempdates[[1]][2],month.abb)
      tempd=paste(tempdates[[1]][1],tempdates[[1]][2],tempdates[[1]][3],sep="-")
      dates[i]=format(as.Date(tempd,"%d-%m-%y"),"%Y-%m-%d")
    }
    dat$date=dates
    dat=dat[order(dat$date),]
    return(dat)
}

#' Batch download stock prices from Yahoo Finance (open, high, low, close, volume, adjusted)
#' 
#' @param symbols symbols of stock, e.g. AAPL, GOOG, SPX
#' @param start start date, e.g., 2013-07-31
#' @param end end date, e.g., 2013-08-06
#' @param freq time interval, e.g., d:daily, w:weekly, m:monthly
#' @seealso \code{\link{get.ohlc.yahoo}}
#' @seealso \code{\link{get.ohlcs.google}}
#' @export
#' @examples
#' get.ohlcs.yahoo(symbols=c("AAPL","GOOG","SPY"),freq="d")
#' @examples
#' get.ohlcs.yahoo(symbols=c("AAPL","GOOG","SPY"),start="2013-01-01",freq="m")
get.ohlcs.yahoo <- function(symbols,start="firstDay",end="today",freq="d"){
  n <- length(symbols)
  ohlc=list()
  if(start == "firstDay"){
      for(i in 1:n){
        URL=paste("http://ichart.finance.yahoo.com/table.csv?s=", symbols[i], sep="")
        dat <- read.csv(URL)
        dat=dat[order(dat$Date),]
        colnames(dat) <- c("date", "open", "high", "low", "close", "volume", "adjusted")
        ohlc[[symbols[i]]]=dat
      }
  }else{
    temp <- strsplit(start,"-")
    a=sprintf('%.2d',as.integer(temp[[1]][2])-1)
    b=temp[[1]][3]
    c=temp[[1]][1]
    if(end != "today"){
      temp <- strsplit(end,"-")
      d=sprintf('%.2d',as.integer(temp[[1]][2])-1)
      e=temp[[1]][3]
      f=temp[[1]][1]
    }else{
      end=as.character(Sys.Date())
      temp <- strsplit(end,"-")
      d=sprintf('%.2d',as.integer(temp[[1]][2])-1)
      e=temp[[1]][3]
      f=temp[[1]][1]
    }
    
    for(i in 1:n){
      URL=paste("http://ichart.finance.yahoo.com/table.csv?s=",symbols[i],"&a=",a,"&b=",b,"&c=",c,"&d=",d,"&e=",e,"&f=",f,"&g=",freq, sep="")
      dat <- read.csv(URL)
      dat=dat[order(dat$Date),]
      colnames(dat) <- c("date", "open", "high", "low", "close", "volume", "adjusted")
      ohlc[[symbols[i]]]=dat
    }
  } 
  return(ohlc)
}

#' Batch download stock prices from Google Finance (open, high, low, close, volume)
#' 
#' @param symbols symbols of stock, e.g. AAPL, GOOG, SPX
#' @param start start date, e.g., 2013-07-31
#' @param end end date, e.g., 2013-08-06
#' @seealso \code{\link{get.ohlc.google}}
#' @seealso \code{\link{get.ohlcs.yahoo}}
#' @export
#' @examples
#' get.ohlcs.google(symbols=c("AAPL","GOOG","SPY"))
#' @examples
#' get.ohlcs.google(symbols=c("AAPL","GOOG","SPY"),start="2013-01-01")
#' @examples
#' get.ohlcs.google(symbols=c("AAPL","GOOG","SPY"),start="2013-01-01",end="2013-07-31")
get.ohlcs.google <- function(symbols,start="2013-01-01",end="today"){
  require(RCurl)
  n <- length(symbols)
  ohlc=list()
  temp <- strsplit(start,"-")
  a=month.abb[as.numeric(temp[[1]][2])]
  b=temp[[1]][3]
  c=temp[[1]][1]
  if(end != "today"){
    temp <- strsplit(end,"-")
    d=month.abb[as.numeric(temp[[1]][2])]
    e=temp[[1]][3]
    f=temp[[1]][1]
  }else{
    end=as.character(Sys.Date())
    temp <- strsplit(end,"-")
    d=month.abb[as.numeric(temp[[1]][2])]
    e=temp[[1]][3]
    f=temp[[1]][1]
  }
  for(i in 1:n){
    URL=paste("https://www.google.com/finance/historical?q=",symbols[i],"&output=csv","&startdate=",a,"+",b,"+",c,"&enddate=",d,"+",e,"+",f, sep="")
    myCsv <- getURL(URL, ssl.verifypeer = FALSE)
    dat <- read.csv(textConnection(myCsv))
    colnames(dat) <- c("date", "open", "high", "low", "close", "volume")
    dates=as.character(dat$date)
    for(j in 1:length(dates)){
      tempdates=strsplit(dates[j],"-")
      tempdates[[1]][2]=match(tempdates[[1]][2],month.abb)
      tempd=paste(tempdates[[1]][1],tempdates[[1]][2],tempdates[[1]][3],sep="-")
      dates[j]=format(as.Date(tempd,"%d-%m-%y"),"%Y-%m-%d")
    }
    dat$date=dates
    dat=dat[order(dat$date),]
    ohlc[[symbols[i]]]=dat
  }
  return(ohlc)
}

#' Technical analysts - Line charts: show prices for each period as a continuous line
#' 
#' @param ohlc output from get.ohlc.yahoo or get.ohlc.google
#' @param y y coordinates: close, open, high, low or adjusted
#' @param main an overall title for the plot
#' @param ... Arguments to be passed to methods
#' @seealso \code{\link{get.ohlc.yahoo}}
#' @seealso \code{\link{get.ohlc.google}}
#' @export
#' @examples
#' google <- get.ohlc.yahoo("GOOG"); lineChart(google)
#' @examples
#' apple <- get.ohlc.google("AAPL"); lineChart(apple)
lineChart <- function(ohlc,y="close",main="",...){
  options(warn=-1)
  require(ggplot2)
  require(scales)
  ohlc$date <- as.Date(ohlc$date,"%Y-%m-%d")
  myBreak=date.breaks(ohlc)
  if(y == "close"){
    ggplot(ohlc, aes_string(x='date',y='close')) + geom_line() + labs( title =main ) + labs(x="") + 
      theme(axis.text.x=element_text(angle=90)) + 
      scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
  }else if(y == "open"){
    ggplot(ohlc, aes_string(x='date',y='open')) + geom_line() + labs( title =main ) + labs(x="") + 
      theme(axis.text.x=element_text(angle=90)) + 
      scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
  }else if(y == "high"){
    ggplot(ohlc, aes_string(x='date',y='high')) + geom_line() + labs( title =main ) + labs(x="") + 
      theme(axis.text.x=element_text(angle=90)) + 
      scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
  }else if(y == "low"){
    ggplot(ohlc, aes_string(x='date','y=low')) + geom_line() + labs( title =main ) + labs(x="") + 
      theme(axis.text.x=element_text(angle=90)) + 
      scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
  }else if(y == "adjusted"){
    ggplot(ohlc, aes_string(x='date',y='adjusted')) + geom_line() + labs( title =main ) + labs(x="") + 
      theme(axis.text.x=element_text(angle=90)) + 
      scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
  }
}

#' Technical analysts - Candlestick chart: show prices for each period as a continuous line. The box is clear if the closing price is higher than the opening price, or filled red if the closing is lower than the opening price.
#' 
#' @param ohlc output from get.ohlc.yahoo or get.ohlc.google
#' @param start start date to plot, if not specified, all date in ohlc will be included
#' @param end end date to plot
#' @param main an overall title for the plot
#' @param ... Arguments to be passed to methods
#' @seealso \code{\link{get.ohlc.yahoo}}
#' @seealso \code{\link{get.ohlc.google}}
#' @export
#' @examples
#' google <- get.ohlc.yahoo("GOOG",start="2013-07-01",end="2013-08-01"); candlestickChart(google)
#' @examples
#' apple <- get.ohlc.google("AAPL",start="2013-07-01",end="2013-08-01"); candlestickChart(apple)
candlestickChart <- function(ohlc, start=NULL, end=NULL, main="", ...){
  options(warn=-1)
  require(ggplot2)
  require(scales)
  date <- as.Date(ohlc$date)
  open <- as.vector(ohlc$open)
  high <- as.vector(ohlc$high)
  low <- as.vector(ohlc$low)
  close <- as.vector(ohlc$close)
  
  xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)
   
  xSubset$candleLower <- pmin(xSubset$open, xSubset$close)
  xSubset$candleMiddle <- NA
  xSubset$candleUpper <- pmax(xSubset$open, xSubset$close)
  xSubset$fill <- 'red'
  xSubset$fill[xSubset$open < xSubset$close] = ''
  
  if(!is.null(start) & !is.null(end)){
    start=as.Date(start)
    end=as.Date(end)
    xSubset <-subset(xSubset, xSubset$date > start & xSubset$date < end) 
  }
  myBreak=date.breaks(xSubset)
  g <- ggplot(xSubset, aes_string(x='date', lower='candleLower', middle='candleMiddle', upper='candleUpper', ymin='low', ymax='high',na.rm=TRUE)) +
    geom_boxplot(stat='identity', aes_string(group='date', fill='fill')) + theme_bw() + 
    scale_fill_manual(name = "", values = c("white", "red")) + labs( title =main ) + labs(x="") +
    theme(legend.position="none") + theme(axis.text.x=element_text(angle=90)) +
    scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
  return(g)
}

#' Technical analysts - Volume charts: show each period's volume as a vertical line
#' 
#' @param ohlc output from get.ohlc.yahoo or get.ohlc.google
#' @param main an overall title for the plot
#' @param ... Arguments to be passed to methods
#' @seealso \code{\link{get.ohlc.yahoo}}
#' @seealso \code{\link{get.ohlc.google}}
#' @export
#' @examples
#' google <- get.ohlc.yahoo("GOOG"); volumeChart(google)
#' @examples
#' apple <- get.ohlc.google("AAPL"); volumeChart(apple)
volumeChart <- function(ohlc,main="",...){
  options(warn=-1)
  require(ggplot2)
  require(scales)
  ohlc$date <- as.Date(ohlc$date,"%Y-%m-%d")
  myBreak=date.breaks(ohlc)
  g=ggplot(ohlc, aes_string(x='date',y='volume')) + geom_bar(stat="identity") + labs( title =main ) + labs(x="") + 
    theme(axis.text.x=element_text(angle=90)) + 
    scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
  return(g)
}

#' Deciding width (days or weeks) for date_breaks in plotting
#' 
#' @param ohlc output from get.ohlc.yahoo or get.ohlc.google
#' @export
#' @examples
#' google <- get.ohlc.yahoo("GOOG"); date.breaks(google)
date.breaks <- function(ohlc){
  n=length(ohlc$date)
  myBreak="1 day"
  if(n<30){
    myBreak="1 day"
  }else if(n < 90){
    myBreak="3 days"
  }else if(n < 140){
    myBreak="1 week"
  }else{
    m=n/140
    myBreak=paste(m, "weeks", sep=" ")
  }
  return(myBreak)
}

#' Technical analysts - Line charts: show prices for each period as a continuous line for multiple stocks
#' 
#' @param ohlcs output from get.ohlc.yahoo.mult or get.ohlc.google.mult
#' @param y y coordinates: Close, Open, High, Low or Adj.Close
#' @param main an overall title for the plot
#' @param ... Arguments to be passed to methods
#' @seealso \code{\link{get.ohlcs.yahoo}}
#' @seealso \code{\link{get.ohlcs.google}}
#' @seealso \code{\link{lineChart}}
#' @export
#' @examples
#' googapple <- get.ohlcs.yahoo(c("GOOG","AAPL"),start="2013-01-01"); lineChartMult(googapple)
#' @examples
#' googapple <- get.ohlcs.google(c("GOOG","AAPL"),start="2013-01-01"); lineChartMult(googapple)
lineChartMult <- function(ohlcs,y="close",main="",...){
  options(warn=-1)
  require(ggplot2)
  require(scales)
  require("reshape2")
  sname=names(ohlcs)
  n=length(sname)
  Date=ohlcs[[sname[1]]]$date <- as.Date(ohlcs[[sname[1]]]$date,"%Y-%m-%d")
  myBreak=date.breaks(ohlcs[[sname[1]]])
  z=data.frame(Date)
  colname=c("Date")
  for(i in 1:n){
    colname=append(colname,sname[i])
    if(y == "close"){
      z[,i+1] = ohlcs[[sname[i]]]$close
    }else if(y == "ppen"){
      z[,i+1] = ohlcs[[sname[i]]]$open
    }else if(y == "high"){
      z[,i+1] = ohlcs[[sname[i]]]$high
    }else if(y == "low"){
      z[,i+1] = ohlcs[[sname[i]]]$low
    }else if(y == "adjusted"){
      z[,i+1] = ohlcs[[sname[i]]]$adjusted
    }
  }
  colnames(z) <- colname
  pdf <- melt(z, id="Date")
  ggplot(data=pdf, aes_string(x='Date', y='value', colour='variable')) + geom_line() +
    labs( title =main ) + labs(x="") + scale_y_continuous(name=y) +
    theme(axis.text.x=element_text(angle=90)) + theme(legend.title=element_blank()) +
    scale_x_date(labels = date_format("%Y-%m-%d"),breaks=date_breaks(myBreak))
}

