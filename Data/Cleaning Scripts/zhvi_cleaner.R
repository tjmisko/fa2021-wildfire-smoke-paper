library(dplyr)
library(ggplot2)
library(usmap)

#import data
setwd("/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/Raw\ Data")
zhvi <- read.csv("County_zhvi.csv")

# fix fips codes, get full fips as identifier, place in proper location
addzero <- Vectorize(function(string, n.out){
    ell <- nchar(string)
    if (ell < n.out){
        string <- paste(c(rep("0", n.out - ell), string), collapse = "")
    }
    return(string)
}, "string")
zhvi$StateCodeFIPS <- addzero(as.character(zhvi$StateCodeFIPS), 2)
zhvi$MunicipalCodeFIPS <- addzero(as.character(zhvi$MunicipalCodeFIPS), 3)
zhvi$fips <- paste0(zhvi$StateCodeFIPS, zhvi$MunicipalCodeFIPS)
zhvi <- select(zhvi, c(1:9, 317, 10:316))

# fix column data structure so that each row contains one observation
county_list <- list()
for (r in 1:nrow(zhvi)){
    date <- matrix(rownames(t(zhvi[r,-1:-10])), nrow = 307)
    zhvi.score <- matrix(as.character(zhvi[r,-1:-10]))
    identifiers <- data.frame(matrix(rep(as.vector(as.character(zhvi[r, 1:10])), 307), 
                                     nrow = 307, ncol = 10, byrow = T))
    county_list[[r]] <- data.frame(cbind(identifiers, date, zhvi.score))

}
zhvi_clean <- bind_rows(county_list)
colnames(zhvi_clean)[1:10] <- colnames(zhvi)[1:10]

#expand date data
zhvi_clean$year <- substr(zhvi_clean$date,2,5)
zhvi_clean$month <- substr(zhvi_clean$date,7,8)
zhvi_clean$day <- substr(zhvi_clean$date, 10, 11)

#associate unique period number to each date string
time_period <- Vectorize(function(datestring){
    year <- as.numeric(substr(datestring,2,5)) - 1996
    month <- as.numeric(substr(datestring,7,8)) - 1
    return(12*year + month)
}, "datestring")
zhvi_clean$period <- time_period(zhvi_clean$date)

write.csv(zhvi_clean, "/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/Clean\ Data/zhvi_clean.csv")
