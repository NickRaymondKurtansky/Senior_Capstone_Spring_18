#BCRP code:

#################################################################################################
#Best constant rebalanced portfolio 
#### roxygen2 comments ################################################
#
#' Best Constant Rebalanced Portfolio Algorithm (BCRP)
#' 
#' computes the best constant rebalanced portfolio, i.e., the constant 
#' rebalanced portfolio achieving the highest wealth in hindsight.
#' 
#' @param returns Matrix of price relatives, i.e. the ratio of the closing
#'                (opening) price today and the day before (use function 
#'                \code{get_price_relatives} to calculate from asset prices).
#' @param method The method used to calculate BCRP. "\code{rand}" generates 
#'               random CRPs to find BCRP. By default the number of random 
#'               portfolios is "\code{samplings=1000}". "\code{approx}" limits 
#'               the set of CRPs to the portfolios of the form \eqn{b=(b_1, 1-b_1)}, 
#'               where \eqn{b_1} runs from 0 to 1 in steps of length "\code{step=0.05}".
#' @param ... further arguments (\code{samplings}, \code{step}) dependend 
#'            on the "\code{method}" argument.
#' 
#' @return Object of class OLP containing
#'         \item{Alg}{Name of the Algorithm}
#'         \item{Names}{vector of asset names in the portfolio}
#'         \item{Weights}{calculated portfolio weights as a vector}
#'         \item{Wealth}{wealth achieved by the portfolio as a vector}
#'         \item{mu}{exponential growth rate}
#'         \item{APY}{annual percantage yield (252 trading days)}
#'         \item{sigma}{standard deviation of exponential growth rate}
#'         \item{ASTDV}{annualized standard deviation (252 trading days)}
#'         \item{MDD}{maximum draw down (downside risk)}
#'         \item{SR}{Sharpe ratio}
#'         \item{CR}{Calmar ratio}
#'         see also \code{\link{print.OLP}}, \code{\link{plot.OLP}}
#'        
#' @note The print method for \code{OLP} objects prints only a short summary.
#' 
#' @details For the "\code{approx}" method the calculation may require very much 
#'          memory dependend on the number of assets and the "\code{step}" argument. 
#'          If an error occurs due to memory problems the "\code{rand}" method may work.
#'          
#' @references          
#' Ishijima 2001, Numerical Methods for Universal Portfolios
#' \url{http://www.business.uts.edu.au/qfrc/conferences/qmf2001/Ishijima_H.pdf}
#' 
#' @examples 
#' # load data
#' data(NYSE)
#' # select stocks
#' returns = cbind(comme=NYSE$comme, kinar=NYSE$kinar)
#' 
#' # compute BCRP
#' BCRP_rnd = alg_BCRP(returns, method="rand", samplings=1000); BCRP_rnd
#' BCRP_approx = alg_BCRP(returns, method="approx", step=0.05); BCRP_approx
#' plot(BCRP_rnd, BCRP_approx)
#' 
#' @export
#' 
#########################################################################
alg_BCRP <- function(returns, method="rand", ...){
  alg <- "BCRP"
  x   <- as.matrix(returns)
  
  # additional arguments
  addargs <- list(...)
  
  
  # verify 'method' arguement
  
  if(method=="approx"){
    # check 'step' argument
    if(hasArg(step)){
      portfolios_weights <- gen_sample_portfolios(n_assets=ncol(x), step=addargs$step)
    } 
    else{
      portfolios_weights <- gen_sample_portfolios(n_assets=ncol(x))
    } 
  }
  
  else if(method=="rand"){
    # check 'samplings' argument
    if(hasArg(samplings)){
      portfolios_weights <- gen_rand_portfolios(addargs$samplings, n_assets=ncol(x))
    } 
    else{
      portfolios_weights <- gen_rand_portfolios(n_portfolios=1000, n_assets=ncol(x))
    }
  } 
  
  else{
    stop("Choose proper method.")
  } 
  
  
  # calculate terminal wealth of sample portfolios
  portfolios_wealth <- vector(length=nrow(portfolios_weights))
  for( i in 1:nrow(portfolios_weights) ){
    portfolios_wealth[i] <- h_get_wealth_CRP(x, portfolios_weights[i,])[nrow(x)]
  }
  
  
  # Find max CRP-Value at time T
  BCRP_index <- which.max(portfolios_wealth)
  # BCRP weights
  b <- portfolios_weights[BCRP_index,]
  b <- matrix( rep(b, nrow(x)), nrow=nrow(x), ncol=ncol(x), byrow=TRUE)
  
  # Wealth
  S <- get_wealth(x, b)
  
  # create OLP object
  ret <- h_create_OLP_obj(alg, x, b, S)
  return(ret)
}
#################################################################################################


#################################################################################################
#  Price Relatives fucntion
# --- FUNCTION PriceRel ---------------------------------------
#
# Usage:    get_price_relatives(prices)
# Purpose:  calculates the price relatives (ratio of the asset 
#           price at t and the asset price at t-1)
# Input:    Object (Vector, Matrix, zoo) of Asset-Prices
# Output:   Matrix of price relatives
#
# -------------------------------------------------------------


#### roxygen2 comments ################################################
#
#' Get price relatives
#' 
#' calculates the price relatives of according asset prices; that is the 
#' closing (opening) price at time t divided by the closing (opening) 
#' price at time t-1.
#' 
#' @param prices Matrix of asset prices, where each column represents
#'               an asset.
#' 
#' @return Matrix of price relatives
#'  
#' @examples 
#' # load stock prices, for more information see quantmod-package
#' library(quantmod)
#' getSymbols("SPY", src="yahoo")
#' # closing prices
#' prices <- Cl(SPY)
#' # get price relatives
#' get_price_relatives(prices)
#' 
#' @export
#' 
#########################################################################
get_price_relatives <- function(prices){
  if(is.vector(prices)){
    returns_log <- diff(log(prices))
    price_relatives <- exp(returns_log)
  }
  else
  {
    #log-returns:
    returns_log <- apply(log(prices), 2, diff)
    # price relatives
    price_relatives <- apply(returns_log, 2, exp)
  }
  
  return(price_relatives)
}
#################################################################################################

# --- FUNCTION gen_sample_portfolios ---------------------------------------
#
# Usage:    gen_sample_portfolios(n_assets, step=0.05)
# Purpose:  generates all possible portfolios of the form b = (b_1, b_2, ... b_m),
#           where 'b_m - b_m-1 = step', 0 < step < 1
# Input:    n_assets  --> number of assets within each portfolio, integer
#           step      --> specifies the limiting set of CRP portfolios of the form b=(b_1,     
#                       1-b_1)}, where b_1 runs from 0 to 1 in steps of length 
#                       "step=0.05"
# Output:   Matrix with portfolio weights; each row represents a portfolio
#
# Dependencies: 'gtools'-package (function 'permutations()')
#
#-------------------------------------------------------------------------


#### roxygen2 comments ################################################
#
#' Generate sample portfolios
#' 
#' generates all possible portfolios of the form \eqn{b = (b_1, b_2, ... b_m)}, 
#' where \eqn{b_i - b_i-1 = step} and, \eqn{0 < step < 1}.
#' 
#' @param n_assets number of assets within each portfolio
#' @param step step length
#' 
#' @return Matrix with portfolio weights; each row represents a portfolio
#'       
#' @note 
#' can lead to memory problems for \code{n_assets > 5}. Alternatively
#' use \code{gen_rand_portfolios}.
#' 
#' @examples 
#' gen_sample_portfolios(2, step=0.05) 
#' 
#' @export
#' 
#########################################################################
gen_sample_portfolios <- function(n_assets, step=0.05){
  #make sure that sequence can end up 1
  step_internal <- 1/round(1/step)
  
  c1 <- seq(0,1, by=step_internal)
  tmp <- gtools::permutations(length(c1), n_assets ,c1, repeats.allowed=TRUE)
  portfolios <- tmp[which(rowSums(tmp)==1),]
  return(portfolios)
}

#################################################################################################


#################################################################################################

#### roxygen2 comments ################################################
#
#' Generate random portfolios
#' 
#' generates uniformly distributed random portfolios based on Algorithm 3
#' of Ishijima's 'Numerical Methods for Universal Portfolios' (see references)
#' 
#' @param n_portfolios number of portfolios to be generated
#' @param n_assets number of assets within each portfolio
#' 
#' @return Matrix with portfolio weights; each row represents a portfolio
#'  
#' @references 
#' Ishijima 2001, Numerical Methods for Universal Portfolios
#' \url{http://www.business.uts.edu.au/qfrc/conferences/qmf2001/Ishijima_H.pdf}
#'       
#' @examples 
#' gen_rand_portfolios(10, 3)
#' 
#' @export
#' 
#########################################################################
gen_rand_portfolios <- function(n_portfolios, n_assets){
  generate_portfolio <- function(n_assets){
    #Step 1:
    x <- rgamma(n=n_assets, shape=1)
    #Step 2:
    b <- x/sum(x)
    b[n_assets] <- 1-sum(b[1:(n_assets-1)])
    return(b)
  }  
  return(t(replicate(n_portfolios, generate_portfolio(n_assets))))
}

#################################################################################################


#################################################################################################

# --- Helper function h_get_wealth_CRP ------------------------
#
# Usage:    .Wealth.CRP(Returns, weights)
# Purpose:  Wealth of Constantly Rebalanced Portfolios
# Input:    returns --> Matrix; relative Returns, that is the Ratio of the 
#                       Return today and the day before
#           weights --> Vector (for CRP) or Matrix
# Output:   Vector of CRP Wealth
#
# ---------------------------------------------------------
h_get_wealth_CRP <- function(returns, weights){
  w_returns <- matrix(nrow=dim(returns)[1], ncol=length(weights))
  for(i in 1:length(weights)){
    w_returns[,i] <- returns[,i] * weights[i]
  }
  p_returns <- rowSums(w_returns)
  S_CRP <- cumprod(p_returns)
  return(S_CRP)
}

#################################################################################################


#################################################################################################


#### roxygen2 comments ################################################
#
#' Get portfolio wealth
#' 
#' calculates the achieved cumulative wealth of a steadily rebalanced
#' portfolio
#' 
#' @param returns Matrix of price relatives, i.e. the ratio of the closing
#'                (opening) price today and the day before (use function 
#'                \code{get_price_relatives} to calculate from asset prices).
#' @param weights vector or matrix containing portfolio weights.
#' 
#' @return vector of the portfolio's cumulative wealth
#' 
#' @examples 
#' # load data
#' data(NYSE)
#' # select stocks
#' x = cbind(comme=NYSE$comme, kinar=NYSE$kinar)
#' # specify portfolio weights
#' b = c(0.05, 0.05)
#' # calculate wealth
#' W = get_wealth(x, b); W
#'  
#' @export
#' 
#########################################################################
get_wealth <- function(returns, weights){
  x <- as.matrix(returns)
  if(is.vector(weights)){
    b <- matrix(rep(weights, nrow(x)), nrow=nrow(x), byrow=TRUE)
  }
  else{
    b <- weights  
  }
  S <- cumprod(rowSums(b*x))
  S <- c(1, S)
  return(S)
}

#################################################################################################


#################################################################################################

# --- Helper function h_create_OLP_obj ------------------------
#
# Usage:    h_create_OLP_obj(alg, returns, weights, wealth)
# Purpose:  creates an object of class OLP
# Input:    alg     --> Name of the algorithm
#           returns --> Matrix; relative Returns, that is the Ratio of the 
#                       Return today and the day before
#           weights --> Vector or Matrix
#           wealth  --> wealth of algorithm
# Output:   object of class OLP
#
# Note: Performance measures are calculated according to 
#       Dochow, Leppek, Schmidt: A framework for automated performance evaluation of portfolio selection algorithms, 2005
# ---------------------------------------------------------
h_create_OLP_obj = function(alg, returns, weights, wealth){
  #gr = get_growth_rate(wealth)
  x  = get_price_relatives(wealth) 
  # log-return
  r  = log(x)
  
  # Calculate Performance Measures
  
  # exponential growth rate
  mu = mean(r)
  
  #### annual percentage yield (APY) ####
  # annualization of exponential growth rate
  y = length(wealth) / 252
  APY = tail(wealth, n=1)^(1/y) - 1
  
  # standard deviation of the exponential growth rate
  sigma = sd(r)
  
  # annualized standard deviation (ASTDV)
  ASTDV = sigma * sqrt(252)
  
  # MDD (maximum draw down)
  # DD (draw down; measures the decline from a historical peak in the cumulative wealth at time t)
  DD = sapply(1:length(wealth), function(t){ 
    m = max( wealth[1:t] );
    max( 0, m - wealth[t] ) / m 
  } )
  # MDD (maximum draw down)
  MDD = max(DD)
  
  # Sharpe ratio (SR)
  SR = APY / ASTDV
  
  # Calmar ratio (CR)
  CR = APY / MDD
  
  # create return of function
  ret <- list(Alg=alg,
              Names       = colnames(returns), 
              Weights     = weights, 
              Wealth      = wealth,
              #GrowthRate  = gr,
              mu          = mu,
              APY         = APY,
              sigma       = sigma,
              ASTDV       = ASTDV,
              MDD         = MDD,
              SR          = SR,
              CR          = CR)
  class(ret) = "OLP"
  return(ret)  
}
#################################################################################################


#################################################################################################


#load any necessary libraries
library(dplyr)
library(ggplot2)
library(boot)

#load the stock data we built specifically for the purpose of inputting into the algorithm
load('Inputs.RData')
finance <- (Inputs$Finance)
health <- (Inputs$Healthcare)
trans <- (Inputs$Transportation)
tech <- (Inputs$Technology)
energy <- (Inputs$Energy)

fin.rand.BCRP <- alg_BCRP(get_price_relatives(finance), method="rand", samplings=1000)
hel.rand.BCRP <- alg_BCRP(get_price_relatives(health), method="rand", samplings=1000)
tra.rand.BCRP <- alg_BCRP(get_price_relatives(trans), method="rand", samplings=1000)
tec.rand.BCRP <- alg_BCRP(get_price_relatives(tech), method="rand", samplings=1000)
enr.rand.BCRP <- alg_BCRP(get_price_relatives(energy), method="rand", samplings=1000)


fin.rand.BCRP$Weights[1,]