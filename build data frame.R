library(quantmod)
library(dplyr)

##############################################
#Build out data set for each industry

#Finance symbols
finance <- c("BMA", "MSL", "VLY", "INTL", "FNCB", "WAFD", "WRB", "WSBF", "CRVL", "CIA")

#Healthcare symobls
healthcare <- c("AKRX", "RMTI", "ANIK", "RELV", "DGX", "PMD", "ATRS", "ALQA", "OGEN", "SGEN")

#Transportation symbols
transportation <- c("GWR", "JBLU", "LUV", "SAIA", "LSTR", "KNX", "ESEA", "ODFL", "CVTI", "FDX")

#Technology symbols
technology <- c("SLP", "STX", "AAPL", "MANH", "RBCN", "VMW", "FORM", "SINA", "GHM", "OSIS")

#Energy symbols
energy <- c("OIS", "CVX", "AREX", "XOM", "PAA", "AHGP", "GE", "CPST", "MRO", "WLL")

industries <- list(finance, healthcare, transportation, technology, energy)
names(industries) <- c("Finance", "Healthcare", "Transportation", "Technology", "Energy")

Finance <- data.frame(matrix(NA, nrow=120, ncol=10))
names(Finance) <- finance
Healthcare <- data.frame(matrix(NA, nrow=120, ncol=10))
names(Healthcare) <- healthcare
Transportation <- data.frame(matrix(NA, nrow=120, ncol=10))
names(Transportation) <- transportation
Technology <- data.frame(matrix(NA, nrow=120, ncol=10))
names(Technology) <- technology
Energy <- data.frame(matrix(NA, nrow=120, ncol=10))
names(Energy) <- energy
Inputs <- list(Finance, Healthcare, Transportation, Technology, Energy)
names(Inputs) <- c("Finance", "Healthcare", "Transportation", "Technology", "Energy")

Symbol <- c(NA)
Industry <- c(NA)
Open <- c(NA)
Date <- c(NA)
Master <- data.frame(Symbol, Industry, Open, Date)
for(i in 1:5){
  i.ind <- names(industries)[i]
  for(j in 1:10){
    j.symbol <- industries[[i]][j]
    getSymbols(j.symbol, from = "2008-01-03", to = "2018-01-03")
    stock <- data.frame(eval(parse(text = j.symbol)))
    rows <- row.names(stock)
    stock$Date <- rows
    colnames(stock)[1] <- "Open"
    stock$Symbol <- rep(j.symbol, nrow(stock))
    stock$Industry <- rep(i.ind, nrow(stock))
    stock <- stock[,c(8, 9, 1, 7)]
    rownames(stock) <- 1:nrow(stock)
    
    subset.rows <- seq(from=1, to=nrow(stock), by=21)
    stock <- stock[subset.rows,]
    
    Master <- rbind(Master, stock)
    Inputs[[i]][j] <- stock$Open
  }
}
Master <- Master[-1,]

save(Master, file="Master_Stocks.RData")
write.csv(Master, file="Master_Stocks.csv")
save(Inputs, file="Inputs.RData")
write.csv(Inputs, file="Inputs.csv")

write.csv(Master, file="Master_Stocks.csv")

