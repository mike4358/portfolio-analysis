# attach library
library(quantmod)

# clear environment
rm(list=ls())

# get latest ARKK holdings file
# unfortunately, i haven't been able to find a history of these
holdings <- utils::read.csv(
  "https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_INNOVATION_ETF_ARKK_HOLDINGS.csv",
  header = TRUE,
  sep = ",")

# clean up
colnames(holdings) <- trimws(gsub("\\.", " ", colnames(holdings)))
holdings <- holdings[!is.na(holdings$weight),]

# report sum of weights
sum(holdings$weight)

# extract
all_tickers <- holdings$ticker
all_weights <- holdings$weight

# create environment to load data into
data_env <- new.env()

# get data
# done in loop for ticker handling and error isolation
# also, getSymbols throttles requests for more than 5 tickers at a time
tickers <- c()
weights <- c()
for (i in seq(1, length(all_tickers))) {
  ticker <- all_tickers[i]
  if(ticker != "") {
    ticker <- trimws(strsplit(ticker, " ")[[1]][1])
    print(paste("getting data for", ticker))
    try({
      quantmod::getSymbols(ticker, env = data_env)
      tickers <- c(tickers, ticker)
      weights <- c(weights, all_weights[i] / 100)
    })
  }
}

# report modified/failed tickers
setdiff(all_tickers, tickers)
#quantmod::barChart(data_env[["TSLA"]])

# calculate returns, merge, and create data.frame (eapply loops over all
# objects in an environment, applies a function, and returns a list)
# retrieved from https://stackoverflow.com/questions/24377590/getsymbols-downloading-data-for-multiple-symbols-and-calculate-returns
returns <- eapply(data_env, function(s) ROC(quantmod::Ad(s), type = "discrete"))
returns_df <- as.data.frame(do.call(merge, returns))

# adjust column names are re-order columns
colnames(returns_df) <- gsub(".Adjusted", "", colnames(returns_df))
returns_df <- returns_df[,tickers]

# restrict to only rolling 3 years
first_valid_idx <- min(which(!is.na(rowSums(returns_df))))
last_idx <- dim(returns_df)[1]
first_idx <- last_idx - 504 + 1
last_idx - first_idx  # report window size
returns_windowed <- returns_df[first_idx:last_idx,]

# calculate covariance matrix
cov_mat <- stats::cov(returns_windowed, use = "pairwise.complete.obs")

# calculate portfolio standard deviation
port_std_dev <- sqrt(c(weights %*% cov_mat %*% weights))

# calculate marginal contribution to risk
# taken from https://faculty.washington.edu/ezivot/econ424/riskbudgetingslides.pdf
mcr <- cov_mat %*% weights / port_std_dev

# calculate contribution to risk
cr <- weights * mcr

# calculate percent contribution to risk
pcr <- cr / port_std_dev

# plot pcr
barplot(pcr[,1], main = "percent contribution to risk", ylim = c(0, 0.15), las = 2, cex.names=.5)

# plot weights
barplot(weights, main = "weights", ylim = c(0, 0.1), names.arg = rownames(pcr), las = 2, cex.names = .5)
