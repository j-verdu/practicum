setwd("~/Desktop/FNA/DataMinr/")

counts <- read.csv("ticker_counts.csv", header = TRUE, row.names = 1)
returns <- read.csv("dataminr_returns.csv", header = TRUE, row.names = 1)

# Limit to those days common to both data files
common.days <- intersect(row.names(counts), row.names(returns))
counts <- counts[row.names(counts) %in% common.days == TRUE, ]
returns <- returns[row.names(returns) %in% common.days == TRUE, ]

# Limit to tickers common to both data files (some don't have returns data)
common.tickers <- intersect(names(counts), names(returns))
counts <- counts[, names(counts) %in% common.tickers == TRUE]
returns <- returns[, names(returns) %in% common.tickers == TRUE]

# Read in summary data that identifies # of tweets per day and whether
# that number of tweets was an outlier
summaries <- read.csv("ticker_summaries.csv", header = TRUE, row.names = 1)
summaries <- summaries[, names(summaries) %in% common.tickers == TRUE]

##############################################################################

# Test whether absolute returns are higher on days where number of tweets
# was an outlier
returns.norm <- returns[, 1][counts[, 1] <= summaries[1, 1]]
returns.outlier <- returns[, 1][counts[, 1] > summaries[1, 1]]
for (i in 2:ncol(returns)) {
  returns.norm <- c(returns.norm, returns[, i][counts[, i] <= summaries[1, i]])
  returns.outlier <- c(returns.outlier,
                       returns[, i][counts[, i] > summaries[1, i]])
}
t.test(abs(returns.norm), abs(returns.outlier), na.action = "na.omit", 
       var.equal = FALSE)

# Outliers vs. not outliers (next day)
id.outlier <- which(counts[, 1] > summaries[1, 1]) - 1
id.outlier <- id.outlier[id.outlier > 0]
returns.outlier <- returns[id.outlier, 1]
id.norm <- setdiff(1:nrow(counts), id.outlier)
returns.norm <- returns[id.norm, 1]
for (i in 2:ncol(returns)) {
  id.outlier <- which(counts[, i] > summaries[1, i]) - 1
  id.outlier <- id.outlier[id.outlier > 0]
  returns.outlier <- c(returns.outlier, returns[id.outlier, i])
  id.norm <- setdiff(1:nrow(counts), id.outlier)
  returns.norm <- c(returns.norm, returns[id.norm, i])
}
t.test(abs(returns.norm), abs(returns.outlier), na.action = "na.omit", 
       var.equal = FALSE)


# Outliers vs. not outliers (two days)
id.outlier <- which(counts[, 1] > summaries[1, 1]) - 2
id.outlier <- id.outlier[id.outlier > 0]
returns.outlier <- returns[id.outlier, 1]
id.norm <- setdiff(1:nrow(counts), id.outlier)
returns.norm <- returns[id.norm, 1]
for (i in 2:ncol(returns)) {
  id.outlier <- which(counts[, i] > summaries[1, i]) - 2
  id.outlier <- id.outlier[id.outlier > 0]
  returns.outlier <- c(returns.outlier, returns[id.outlier, i])
  id.norm <- setdiff(1:nrow(counts), id.outlier)
  returns.norm <- c(returns.norm, returns[id.norm, i])
}
t.test(abs(returns.norm), abs(returns.outlier), na.action = "na.omit", 
       var.equal = FALSE)


# Outliers vs. not outliers (three days)
id.outlier <- which(counts[, 1] > summaries[1, 1]) - 3
id.outlier <- id.outlier[id.outlier > 0]
returns.outlier <- returns[id.outlier, 1]
id.norm <- setdiff(1:nrow(counts), id.outlier)
returns.norm <- returns[id.norm, 1]
for (i in 2:ncol(returns)) {
  id.outlier <- which(counts[, i] > summaries[1, i]) - 3
  id.outlier <- id.outlier[id.outlier > 0]
  returns.outlier <- c(returns.outlier, returns[id.outlier, i])
  id.norm <- setdiff(1:nrow(counts), id.outlier)
  returns.norm <- c(returns.norm, returns[id.norm, i])
}
t.test(abs(returns.norm), abs(returns.outlier), na.action = "na.omit", 
       var.equal = FALSE)

### Note: even though 90th percentiles were used for norm vs. outlier, the
### outliers only correspond to about 2% of the data.  That is because 
### many assets have nearly all zeros and a few ones, so only the ones get
### counted as outlier days and these are less than 10% of the days for those
### assets.

#############################################################################
#############################################################################

# Test whether absolute returns are higher on days where there were any
# tweets vs. no tweets
returns.none <- returns[, 1][counts[, 1] == 0]
returns.some <- returns[, 1][counts[, 1] > 0]
for (i in 2:ncol(returns)) {
  returns.none <- c(returns.none, returns[, i][counts[, i] == 0])
  returns.some <- c(returns.some, returns[, i][counts[, i] > 0])
}

t.test(abs(returns.none), abs(returns.some), na.action = "na.omit", 
       var.equal = FALSE)

# Any tweets vs. no tweets (next day)
id.some <- which(counts[, 1] > 0) + 1
id.some <- id.some[id.some <= nrow(counts)]
returns.some <- returns[id.some, 1]
id.none <- setdiff(1:nrow(counts), id.some)
returns.none <- returns[id.none, 1]
for (i in 2:ncol(returns)) {
  id.some <- which(counts[, i] > 0) + 1
  id.some <- id.some[id.some <= nrow(counts)]
  returns.some <- c(returns.some, returns[id.some, i])
  id.none <- setdiff(1:nrow(counts), id.some)
  returns.none <- c(returns.none, returns[id.none, i])
}
t.test(abs(returns.none), abs(returns.some), na.action = "na.omit", 
       var.equal = FALSE)

# Any tweets vs. no tweets (2 days)
id.some <- which(counts[, 1] > 0) + 1
id.some <- id.some[id.some <= nrow(counts)]
returns.some <- returns[id.some, 1]
id.none <- setdiff(1:nrow(counts), id.some)
returns.none <- returns[id.none, 1]
for (i in 2:ncol(returns)) {
  id.some <- which(counts[, i] > 0) + 2
  id.some <- id.some[id.some <= nrow(counts)]
  returns.some <- c(returns.some, returns[id.some, i])
  id.none <- setdiff(1:nrow(counts), id.some)
  returns.none <- c(returns.none, returns[id.none, i])
}
t.test(abs(returns.none), abs(returns.some), na.action = "na.omit", 
       var.equal = FALSE)

