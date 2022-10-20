library(usmap)
library(data.table)
library(ggplot2)
library(dplyr)
library(gganimate)
library(transformr)

# import data: this a few minutes because the csv is so big!
setwd("/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data")
smoke <- read.csv("Raw\ Data/HMS_USpopBG_2010_2019.csv")
smoke <- filter(smoke, STATEFP < 60)
smoke <- filter(smoke, !(STATEFP == 46 & COUNTYFP == 113))
smoke <- filter(smoke, !(STATEFP == 02 & COUNTYFP == 270))
smoke <- filter(smoke, !(STATEFP == 51 & COUNTYFP == 515))
pop2010 <- read.csv("Raw\ Data/co-est2019-alldata.csv")
pop2010 <- select(pop2010, c(4,5,9))
colnames(pop2010)[3] <- "p2010"
pop2010 <- filter(pop2010, STATE <= 60, COUNTY != 0)
pop2010.lookup <- data.table(pop2010)
setkey(pop2010.lookup, "STATE", "COUNTY")


# aggregate by day
smoke_date <- smoke %>% group_by(date, STATEFP, COUNTYFP) %>% 
    summarize(mu.light = mean(light, na.rm = T),
              mu.medium = mean(medium, na.rm = T),
              mu.heavy = mean(heavy, na.rm = T),
              pop.exp = sum(POPULATION))
get_2010_pop <- Vectorize(function(state, county, dt){
    return(pop2010.lookup[STATE == state & COUNTY == county, as.numeric(p2010)])
}, c("state", "county"))

# add date categories:
smoke_date$date <- as.character(smoke_date$date)
smoke_date$year <- substr(smoke_date$date, 1, 4)
smoke_date$month <- substr(smoke_date$date, 5, 6)
smoke_date$pop.cen <- get_2010_pop(smoke_date$STATEFP,
                                   smoke_date$COUNTYFP,
                                   pop2010)
smoke_date <- filter(smoke_date, !(STATEFP == 51 & COUNTYFP == 515))
smoke_date <- filter(smoke_date, !(STATEFP == 46 & COUNTYFP == 113))
smoke_date <- filter(smoke_date, !(STATEFP == 02 & COUNTYFP == 270))

smoke_date$pop.cen <- unlist(smoke_date$pop.cen)

# find bad index values
View(smoke_date[unlist(lapply(smoke_date$pop.cen, identical, y = numeric(0))),])

# compute population weights
smoke_date$weight <- smoke_date$pop.exp/smoke_date$pop.cen

smoke_date$wmu.light <- smoke_date$weight*smoke_date$mu.light
smoke_date$wmu.medium <- smoke_date$weight*smoke_date$mu.medium
smoke_date$wmu.heavy <- smoke_date$weight*smoke_date$mu.heavy

smoke_date$wmu.light[is.nan(smoke_date$wmu.light)] <- 0
smoke_date$wmu.medium[is.nan(smoke_date$wmu.medium)] <- 0
smoke_date$wmu.heavy[is.nan(smoke_date$wmu.heavy)] <- 0

write.csv(smoke_date, "Clean\ Data/smoke_date_clean.csv")
smoke_date <- read.csv("Clean\ Data/smoke_date_clean.csv")
head(smoke_date)

#smoke score: this is an arbitrary step (check robustness later)
smoke_date$smoke_score <- 5*smoke_date$wmu.light +
                          15*smoke_date$wmu.medium + 
                          25*smoke_date$wmu.heavy
class(quantile(smoke_date$smoke_score, seq(0,1,by=0.01)))

smoke_month <- smoke_date %>% group_by(month, year, STATEFP, COUNTYFP) %>%
    summarize(n.light = sum(wmu.light, na.rm = T),
              n.medium = sum(wmu.medium, na.rm = T),
              n.heavy = sum(wmu.heavy, na.rm = T),
              n.score = sum(smoke_score))
smoke_month <- data.frame(smoke_month)

getfips <- Vectorize(function(s, c){
    fips <- as.character(s*1000 + c)
    if (nchar(fips) == 4){
        return(paste("0", fips, sep = ""))
    } else {
        return(fips)
    }
}, c("s", "c"))


smoke_month$fips <- getfips(smoke_month$STATEFP, smoke_month$COUNTYFP)
smoke_month <- smoke_month %>% select(c(2, 1, 9, 3:8)) %>% arrange(year)

zero.lst <- list()
u.fips <- sort(unique(smoke_month$fips))
counter <- 1
for (y in unique(smoke_month$year)){
    for (m in unique(smoke_month$m)){
        zero.lst[[counter]] <- data.frame(year = rep(y, length(u.fips)),
                                          month = rep(m, length(u.fips)),
                                          fips = u.fips)
        counter <- counter + 1
    }
}


smoke_month_wzero <- bind_rows(zero.lst)
smoke_month$rstring <- paste(as.character(smoke_month$year*100 +
                                          smoke_month$month),
                             smoke_month$fips, sep = "")
smoke_month_wzero$rowstring <- paste(as.character(smoke_month_wzero$year*100 +
                                                  smoke_month_wzero$month),
                                     smoke_month_wzero$fips, sep = "")
matches <- as.numeric()
for (r in 1:nrow(smoke_month)){
    matches[r] <- which(smoke_month_wzero$rowstring == smoke_month$rstring[r])
    print(paste("matching row", r))
}
smoke_month$matches <- matches
smoke_month

zero.lst2 <- list()
a <- cbind(smoke_month_wzero[1,-4], 
           t(rep(NA, 2)), 
           t(rep(0,4)), 
           smoke_month_wzero[1,4], 
           NA)
colnames(a) <- colnames(smoke_month)
a
colnames(smoke_month_wzero)
colnames(smoke_month)
for (r in 1:nrow(smoke_month_wzero)){
    df <- filter(smoke_month, matches == r)
    if (nrow(df) == 1){
        zero.lst2[[r]] <- df
        print(paste("Filling row", as.character(r)))
    } else {
        empty <- cbind(smoke_month_wzero[r,-4], 
                       t(rep(NA, 2)), 
                       t(rep(0, 4)), 
                       smoke_month_wzero[r,4],
                       NA)
        colnames(empty) <- colnames(df)
        zero.lst2[[r]] <- empty
        print(paste("Adding zeros to row", as.character(r)))
    }
}
smoke_final <- bind_rows(zero.lst2)
head(smoke_final)
smoke_final$STATEFP <- as.numeric(substr(smoke_final$fips, 1, 2))
smoke_final$COUNTYFP <- as.numeric(substr(smoke_final$fips, 3, 5))
smoke_final
smoke_final <- select(smoke_final, c(-11))

getperiod <- Vectorize(function(y, m){
    if (y == 2010){
        return(m - 5)
    } else {
        return((y - 2010)*12 + m - 5)
    }
}, c("y", "m"))
smoke_final$period <- getperiod(smoke_final$year, smoke_final$month)
write.csv(smoke_final, "Clean\ Data/smoke_month_clean.csv")
# compute county coverage by day

