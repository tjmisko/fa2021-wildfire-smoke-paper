library(usmap)
library(ggplot2)
library(dplyr)
library(gganimate)
library(transformr)

#read in clean data: 2875/3006 counties in the dataset
setwd("/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data")
zhvi <- read.csv("Clean\ Data/zhvi_clean.csv",
                 colClasses = c("fips" = "character",
                                "StateCodeFIPS" = "character",
                                "MunicipalCodeFIPS" = "character"))
# visualizing coverage by starting year
cg_1996 <- zhvi %>% group_by(fips) %>%
    summarize(mu = mean(zhvi.score),
              sd = sd(zhvi.score),
              cg = 1 - sum(is.na(zhvi.score))/307)
cg_2000 <- zhvi %>% filter(year >= 2000) %>% group_by(fips) %>%
    summarize(mu = mean(zhvi.score),
              sd = sd(zhvi.score),
              cg = 1 - sum(is.na(zhvi.score))/307)
cg_2005 <- zhvi %>% filter(year >= 2005) %>% group_by(fips) %>%
    summarize(mu = mean(zhvi.score),
              sd = sd(zhvi.score),
              cg = 1 - sum(is.na(zhvi.score))/307)
cg_2010 <- zhvi %>% filter(year >= 2010) %>% group_by(fips) %>%
    summarize(mu = mean(zhvi.score),
              sd = sd(zhvi.score),
              cg = 1 - sum(is.na(zhvi.score))/307)

# plot proportion of dataset with x proportion coverage
cg_percent <- Vectorize(function(x, df){sum(df$cg >= x)/length(df$cg)}, "x")
ggplot(data.frame(xx = seq(0,1,b=0.01),
                  cg1996 = cg_percent(seq(0,1,b=0.01), cg_1996),
                  cg2000 = cg_percent(seq(0,1,b=0.01), cg_2000),
                  cg2005 = cg_percent(seq(0,1,b=0.01), cg_2005),
                  cg2010 = cg_percent(seq(0,1,b=0.01), cg_2010))) + 
    geom_line(aes(x = xx, y = cg1996, color = "1996")) +
    geom_line(aes(x = xx, y = cg2000, color = "2000")) +
    geom_line(aes(x = xx, y = cg2005, color = "2005")) +
    geom_line(aes(x = xx, y = cg2010, color = "2010")) +
    labs(title = "ZHVI Data Coverage Proportion, Starting Year to 2021",
         col = "Starting Year") +
    xlab("Coverage Proportion (1 - Proportion of NA Values)") +
    ylab("Proportion of Counties with Coverage Level x") +
    ylim(0,1.005)


#geographic coverage check: pull out only the counties with full coverage 2010-2021
zhvi_fcg <- zhvi %>%
    filter(!as.logical(zhvi$fips %in% filter(cg_2010, cg < 1)$fips)) %>%
    filter(year >= 2010)
zhvi_fcg$period <- zhvi_fcg$period - min(zhvi_fcg$period)

plot_usmap(regions = "counties", fill = "blue",
           include = unique(zhvi_fcg$fips)) +
    labs(title = "Counties with Full ZHVI Timeseries, 2010 to 2021",
         subtitle = "Full Coverage for 2512 out of 3006 US Counties")


# explore evolution of zhvi over time
#plt <- plot_usmap(regions = "counties", 
                #data = filter(zhvi_fcg, year <= 2011), 
                #values = "zhvi.score",
           #include = unique(zhvi_fcg$fips)) +
    #scale_fill_continuous(low = "white", high = "red", name = "ZHVI", 
                          #label = scales::comma) + 
    #theme(legend.position = "right") +
    #transition_time(period)
#anim <- animate(plt, nframes = 24, fps = 4)

# state mean evolution by year
state_means <- zhvi_fcg %>% group_by(StateCodeFIPS, period) %>%
    summarize(mu = mean(zhvi.score))
us_means <- zhvi_fcg %>% group_by(period) %>%
    summarize(usmu = mean(zhvi.score),
              q10 = quantile(zhvi.score, 0.1),
              q90 = quantile(zhvi.score, 0.9))
    
plt.means <- ggplot(NULL, aes(x = period)) 
for (statefips in unique(state_means$StateCodeFIPS)){
    plt.means <- plt.means +
        geom_line(data = filter(state_means, StateCodeFIPS == statefips),
                  aes(y = mu), col = "lightgrey")
}
plt.means + 
    geom_line(data = us_means, aes(y = usmu), col = "darkblue") +
    geom_line(data = us_means, aes(y = q10), col = "darkblue", linetype = "dashed") + 
    geom_line(data = us_means, aes(y = q90), col = "darkblue", linetype = "dashed") +
    labs(title = "Monthly State Mean ZHVI, 2010 to 2021",
         subtitle = "80% of State Means Lie Between the Dashed Lines") + 
    xlab("Month (January 2010 == 0)") +
    ylab("Mean ZHVI")
