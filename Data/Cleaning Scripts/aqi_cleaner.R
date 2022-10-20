library(ggplot2)
library(dplyr)
library(usmap)

zhvi <- read.csv("County_zhvi.csv")
aqi_datalist <- list()
for (year in seq(1996, 2020)){
    filename <- paste0("AQI_data/annual_aqi_by_county_",as.character(year),".csv")
    dat <- read.csv(filename)
    aqi_datalist[[year - 1995]] <- dat
}
aqi_data <- do.call(rbind, aqi_datalist)
write.csv(aqi_data, "aqi_data.csv")

# data availability, AQI
head(aqi_data)
data_avail <- aqi_data %>% group_by(Year) %>% 
    summarize(q50 = median(Days.with.AQI),
              q25 = quantile(Days.with.AQI, 0.25),
              q75 = quantile(Days.with.AQI, 0.75),
              q10 = quantile(Days.with.AQI, 0.10))
ggplot(data_avail, aes(x = Year)) + geom_col(aes(y = q75), fill = "darkblue") + 
    geom_col(aes(y = q50)) + geom_col(aes(y = q25), fill = "red") + 
    geom_col(aes(y = q10), fill = "darkred") + ylab("Days with Recorded AQI") +
    labs(title = "Data Availability Analysis for AQI Data")

avail_plot <- ggplot(data_prob, aes(x = unique(Year)))
for (state in unique(data_prob$State)[]){
    avail_plot <- avail_plot + 
        geom_line(data = filter(data_prob, State == state), aes(y = dataavail))
}
avail_plot + geom_hline(aes(yintercept = 250), col = "red")

#counties which have 
bad.counties <- unique(select(filter(aqi_data, Bad.Prop > 0.10), County)$County)
bad_aqi <- filter(aqi_data, County %in% bad.counties)

ggplot(, aes(x = Year)) + 
    geom_line(aes(y = Bad.Prop))

# set up fips function to identify regions
get_abbr <- function(s){
    abbr <- read.csv("fips-codes-master/state_fips_master.csv")
    return(filter(abbr, grepl(s, long_name, fixed = T))$state_abbr)
}

get_fips <- Vectorize(function(s, c){
    dat <- read.csv("fips-codes-master/state_and_county_fips_master.csv")
    return(filter(dat, (grepl(c, name, fixed = T) & grepl(get_abbr(s), state, fixed = T)))$fips)
}, c("s", "c"))

get_fips("Pennsylvania", "Berks")
is.na(mapply(get_fips, aqi_data$State, aqi_data$County))

aqi_data$State
