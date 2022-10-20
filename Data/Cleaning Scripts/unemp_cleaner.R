library(dplyr)
library(ggplot2)
library(usmap)
library(readxl)
library(splines)

setwd("/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/Raw\ Data/Employment")
files <- list.files(path = getwd(), full.names = T)

lst <- list()
counter <- 1
for (f in files){
    df <- data.frame(read_xlsx(f)[-(1:5),-c(1,4,6:9)])
    colnames(df) <- c("fips_state", "fips_county", "year", "unemployment")
    lst[[counter]] <- df
    counter <- counter + 1
}
u <- bind_rows(lst)
u$fips <- paste(u$fips_state, u$fips_county, sep = "")
u$unemployment <- round(as.numeric(u$unemployment), 5)
u$fips_state <- as.numeric(u$fips_state)
head(u)
u <- filter(u, !(fips_state %in% c(2,4,6,8,15,16,30,32,35,41,49,53,56,72, NA)))

lst <- list()
counter <- 1
for (f in unique(u$fips)){
    df <- filter(u, fips == f)
    s <- interpSpline((as.numeric(df$year) - 2010)*12 + 1, 
                      as.numeric(df$unemployment))
    out <- data.frame(month = 1:120, 
                      unemp = predict(s, 1:120)$y, 
                      fips = rep(f, 120))
    lst[[counter]] <- out
    counter <- counter + 1
    print(paste("Splining fips code", f))
}

u_month <- bind_rows(lst)

u_month$period <- u_month$month - 5
u_month <- filter(u_month, period > 0)

write.csv(u_month, "/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/Clean\ Data/unemp_clean.csv")



