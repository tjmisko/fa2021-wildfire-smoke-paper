library(dplyr)
library(ggplot2)
library(data.table)
library(stargazer)
library(biglm)
library(usmap)

# import data, set types
setwd("/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/")
main <- read.csv("Clean\ Data/main.csv", colClasses = c("fips"="character"))

main$fips <- as.factor(main$fips)
main$period <- as.factor(main$period)
main$n.score <- as.numeric(main$n.score)
main$unemp <- as.numeric(main$unemp)
main$zhvi.score <- as.numeric(main$zhvi.score)
main$density <- as.numeric(main$density)
main <- select(main, -1)
# period 60 cutoff
smoke_t <- main %>% group_by(period) %>% summarize(m.l = mean(n.light),
                                                   m.m = mean(n.medium),
                                                   m.h = mean(n.heavy),
                                                   m.s = mean(n.score))

smoke_means <- ggplot(data = smoke_t, aes(x = period)) + geom_line(aes(y = 5*m.l)) + 
    geom_line(aes(y = 15*m.m), col = "dark blue") + 
    geom_line(aes(y = 25*m.h), col = "dark red") +
    geom_line(aes(y = m.s), linetype = "dashed") + 
    xlab("Period (Months Beginning in ") +
    ylab("Cumulative Monthly Exposure") + 
    geom_vline(aes(xintercept = 43), col = "blue", alpha = 0.15) +
    geom_vline(aes(xintercept = 48), col = "blue", alpha = 0.15) +
    geom_vline(aes(xintercept = 55), col = "blue", alpha = 0.55) +
    geom_vline(aes(xintercept = 60), col = "blue", alpha = 0.15) +
    geom_vline(aes(xintercept = 67), col = "blue", alpha = 0.15) +
    labs(title = "County Mean Smoke Exposure by Month") +
    theme(rect = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"))
ggsave(smoke_means,            # Save transparent png file
       filename = "smoke_means.png",
       bg = "transparent")
# function which record treatment status based on the smoke score differential
set.status <- Vectorize(function(value, control.threshold, treatment.threshold){
    if (abs(value) < control.threshold){
        return("control")
    } else if (value > treatment.threshold){
        return("positive treatment")
    } else if (value < -treatment.threshold){
        return("negative treatment")
    } else {
        return("boundary")
    }
}, "value")


# perform the analysis for t* = 55
main$period <- as.factor(main$period)
main$logZHVI <- log(main$zhvi.score)

colnames(main)[40]
main <- select(main, c(1:4,41,5:40))
data.pch43 <- select(main, -c(26:41))
data.pch48 <- select(main, -c(22:25,30:41))
data.pch55 <- select(main, -c(22:29,34:41))
data.pch60 <- select(main, -c(22:33,38:41))
data.pch67 <- select(main, -c(22:37))

# get status at different threshold values
data.pch43$status.k5.t25 <- as.factor(set.status(data.pch43$m.s.pch43, 0.05, 0.25))
data.pch43$status.k5.t50 <- as.factor(set.status(data.pch43$m.s.pch43, 0.05, 0.50))
data.pch43$status.k10.t25 <- as.factor(set.status(data.pch43$m.s.pch43, 0.10, 0.25))
data.pch43$status.k10.t50 <- as.factor(set.status(data.pch43$m.s.pch43, 0.10, 0.50))

data.pch48$status.k5.t25 <- as.factor(set.status(data.pch48$m.s.pch48, 0.05, 0.25))
data.pch48$status.k5.t50 <- as.factor(set.status(data.pch48$m.s.pch48, 0.05, 0.50))
data.pch48$status.k10.t25 <- as.factor(set.status(data.pch48$m.s.pch48, 0.10, 0.25))
data.pch48$status.k10.t50 <- as.factor(set.status(data.pch48$m.s.pch48, 0.10, 0.50))

data.pch55$status.k5.t25 <- as.factor(set.status(data.pch55$m.s.pch55, 0.05, 0.25))
data.pch55$status.k5.t50 <- as.factor(set.status(data.pch55$m.s.pch55, 0.05, 0.50))
data.pch55$status.k10.t25 <- as.factor(set.status(data.pch55$m.s.pch55, 0.10, 0.25))
data.pch55$status.k10.t50 <- as.factor(set.status(data.pch55$m.s.pch55, 0.10, 0.50))

data.pch60$status.k5.t25 <- as.factor(set.status(data.pch60$m.s.pch60, 0.05, 0.25))
data.pch60$status.k5.t50 <- as.factor(set.status(data.pch60$m.s.pch60, 0.05, 0.50))
data.pch60$status.k10.t25 <- as.factor(set.status(data.pch60$m.s.pch60, 0.10, 0.25))
data.pch60$status.k10.t50 <- as.factor(set.status(data.pch60$m.s.pch60, 0.10, 0.50))

data.pch67$status.k5.t25 <- as.factor(set.status(data.pch67$m.s.pch67, 0.05, 0.25))
data.pch67$status.k5.t50 <- as.factor(set.status(data.pch67$m.s.pch67, 0.05, 0.50))
data.pch67$status.k10.t25 <- as.factor(set.status(data.pch67$m.s.pch67, 0.10, 0.25))
data.pch67$status.k10.t50 <- as.factor(set.status(data.pch67$m.s.pch67, 0.10, 0.50))

# regression OLS
reg.OLS <- lm(data = data.pch67,
                 zhvi.score ~ n.score + unemp + density + period)
reg.OLS.log <- lm(data = data.pch60,
                     logZHVI ~ n.score + unemp + density + period)
reg.OLS.pch <- lm(data = data.pch60,
                  zhvi.score ~ m.s.pch60 + unemp + density + period)
reg.OLS.pch.log <- lm(data = data.pch60,
                      logZHVI ~ m.s.pch60 + unemp + density + period)

summary(reg.OLS.pch)
head(data.pch60)
reg.OLS.pch.log
stargazer(reg.OLS, reg.OLS.log, reg.OLS.pch, reg.OLS.pch.log)


# regression - treatment/control
nrow(filter(data.pch43, status.k5.t25 %in% c("positive treatment", "control")))
data.pch43$treat.k5.t25 <- as.numeric(data.pch43$status.k5.t25 == "positive treatment")*as.numeric(data.pch43$post43)
reg.pch43.k5.t25 <- biglm(data = filter(data.pch43, status.k5.t25 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t25 + unemp + period + fips)
data.pch43$treat.k5.t50 <- as.numeric(data.pch43$status.k5.t50 == "positive treatment")*as.numeric(data.pch43$post43)
reg.pch43.k5.t50 <- biglm(data = filter(data.pch43, status.k5.t50 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t50 + unemp + period + fips)
data.pch43$treat.k10.t25 <- as.numeric(data.pch43$status.k10.t25 == "positive treatment")*as.numeric(data.pch43$post43)
reg.pch43.k10.t25 <- biglm(data = filter(data.pch43, status.k10.t25 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k10.t25 + unemp + period + fips)
data.pch43$treat.k10.t50 <- as.numeric(data.pch43$status.k10.t50 == "positive treatment")*as.numeric(data.pch43$post43)
reg.pch43.k10.t50 <- biglm(data = filter(data.pch43, status.k10.t50 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t50 + unemp + period + fips)

data.pch48$treat.k5.t25 <- as.numeric(data.pch48$status.k5.t25 == "positive treatment")*as.numeric(data.pch48$post48)
reg.pch48.k5.t25 <- biglm(data = filter(data.pch48, status.k5.t25 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t25 + unemp + period + fips)
data.pch48$treat.k5.t50 <- as.numeric(data.pch48$status.k5.t50 == "positive treatment")*as.numeric(data.pch48$post48)
reg.pch48.k5.t50 <- biglm(data = filter(data.pch48, status.k5.t50 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t50 + unemp + period + fips)
data.pch48$treat.k10.t25 <- as.numeric(data.pch48$status.k10.t25 == "positive treatment")*as.numeric(data.pch48$post48)
reg.pch48.k10.t25 <- biglm(data = filter(data.pch48, status.k10.t25 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t25 + unemp + period + fips)
data.pch48$treat.k10.t50 <- as.numeric(data.pch48$status.k10.t50 == "positive treatment")*as.numeric(data.pch48$post48)
reg.pch48.k10.t50 <- biglm(data = filter(data.pch48, status.k10.t50 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t50 + unemp + period + fips)


data.pch55$treat.k5.t25 <- as.numeric(data.pch55$status.k5.t25 == "positive treatment")*as.numeric(data.pch55$post55)
reg.pch55.k5.t25 <- biglm(data = filter(data.pch55, status.k5.t25 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t25 + unemp + period + fips)
data.pch55$treat.k5.t50 <- as.numeric(data.pch55$status.k5.t50 == "positive treatment")*as.numeric(data.pch55$post55)
reg.pch55.k5.t50 <- biglm(data = filter(data.pch55, status.k5.t50 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t50 + unemp + period + fips)
data.pch55$treat.k10.t25 <- as.numeric(data.pch55$status.k10.t25 == "positive treatment")*as.numeric(data.pch55$post55)
reg.pch55.k10.t25 <- biglm(data = filter(data.pch55, status.k10.t25 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t25 + unemp + period + fips)
data.pch55$treat.k10.t50 <- as.numeric(data.pch55$status.k10.t50 == "positive treatment")*as.numeric(data.pch55$post55)
reg.pch55.k10.t50 <- biglm(data = filter(data.pch55, status.k10.t50 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t50 + unemp + period + fips)


data.pch60$treat.k5.t25 <- as.numeric(data.pch60$status.k5.t25 == "positive treatment")*as.numeric(data.pch60$post60)
reg.pch60.k5.t25 <- biglm(data = filter(data.pch60, status.k5.t25 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t25 + unemp + period + fips)
data.pch60$treat.k5.t50 <- as.numeric(data.pch60$status.k5.t50 == "positive treatment")*as.numeric(data.pch60$post60)
reg.pch60.k5.t50 <- biglm(data = filter(data.pch60, status.k5.t50 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t50 + unemp + period + fips)
data.pch60$treat.k10.t25 <- as.numeric(data.pch60$status.k10.t25 == "positive treatment")*as.numeric(data.pch60$post60)
reg.pch60.k10.t25 <- biglm(data = filter(data.pch60, status.k10.t25 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t25 + unemp + period + fips)
data.pch60$treat.k10.t50 <- as.numeric(data.pch60$status.k10.t50 == "positive treatment")*as.numeric(data.pch60$post60)
reg.pch60.k10.t50 <- biglm(data = filter(data.pch60, status.k10.t50 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t50 + unemp + period + fips)

data.pch67$treat.k5.t25 <- as.numeric(data.pch67$status.k5.t25 == "positive treatment")*as.numeric(data.pch67$post67)
reg.pch67.k5.t25 <- biglm(data = filter(data.pch67, status.k5.t25 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t25 + unemp + period + fips)
data.pch67$treat.k5.t50 <- as.numeric(data.pch67$status.k5.t50 == "positive treatment")*as.numeric(data.pch67$post67)
reg.pch67.k5.t50 <- biglm(data = filter(data.pch67, status.k5.t50 %in% c("positive treatment", "control")),
                          zhvi.score ~ treat.k5.t50 + unemp + period + fips)
data.pch67$treat.k10.t25 <- as.numeric(data.pch67$status.k10.t25 == "positive treatment")*as.numeric(data.pch67$post67)
reg.pch67.k10.t25 <- biglm(data = filter(data.pch67, status.k10.t25 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t25 + unemp + period + fips)
data.pch67$treat.k10.t50 <- as.numeric(data.pch67$status.k10.t50 == "positive treatment")*as.numeric(data.pch67$post67)
reg.pch67.k10.t50 <- biglm(data = filter(data.pch67, status.k10.t50 %in% c("positive treatment", "control")),
                           zhvi.score ~ treat.k10.t50 + unemp + period + fips)



data.pch55$treat.k5.t50 <- as.numeric(data.pch55$status.k5.t50 == "positive treatment")*as.numeric(data.pch55$post55)
reg.pch55 <- biglm(data = filter(data.pch55, status.k5.t50 == "positive treatment" |
                                   status.k5.t50 == "control"),
                 zhvi.score ~ treat.k5.t50 + unemp + density + period + fips)
summary(reg.pch55)


data.pch55$treat.k5.t50 <- as.numeric(data.pch55$status.k5.t50 == "positive treatment")*as.numeric(data.pch60$post60)


# regression - buckets
data.pch60$bucket <- as.factor(floor(data.pch60$m.s.pch60*10))
data.pch60$`bucket-7` <- data.pch60$bucket == -7 & data.pch60$post60
data.pch60$`bucket-6` <- data.pch60$bucket == -6 & data.pch60$post60
data.pch60$`bucket-5` <- data.pch60$bucket == -5 & data.pch60$post60
data.pch60$`bucket-4` <- data.pch60$bucket == -4 & data.pch60$post60
data.pch60$`bucket-3` <- data.pch60$bucket == -3 & data.pch60$post60
data.pch60$`bucket-2` <- data.pch60$bucket == -2 & data.pch60$post60
data.pch60$`bucket-1` <- data.pch60$bucket == -1 & data.pch60$post60
data.pch60$`bucket0` <- data.pch60$bucket == 0 & data.pch60$post60
data.pch60$`bucket1` <- data.pch60$bucket == 1 & data.pch60$post60
data.pch60$`bucket2` <- data.pch60$bucket == 2 & data.pch60$post60
data.pch60$`bucket3` <- data.pch60$bucket == 3 & data.pch60$post60
data.pch60$`bucket4` <- data.pch60$bucket == 4 & data.pch60$post60
data.pch60$`bucket5` <- data.pch60$bucket == 5 & data.pch60$post60
data.pch60$`bucket6` <- data.pch60$bucket == 6 & data.pch60$post60
data.pch60$`bucket7` <- data.pch60$bucket == 7 & data.pch60$post60
data.pch60$`bucket8` <- data.pch60$bucket == 8 & data.pch60$post60
data.pch60$`bucket9` <- data.pch60$bucket == 9 & data.pch60$post60
data.pch60$`bucket10` <- data.pch60$bucket == 10 & data.pch60$post60
data.pch60$`bucket11` <- data.pch60$bucket == 11 & data.pch60$post60
data.pch60$`bucket12` <- data.pch60$bucket == 12 & data.pch60$post60
data.pch60$`bucket13` <- data.pch60$bucket == 13 & data.pch60$post60
data.pch60$`bucket14` <- data.pch60$bucket == 14 & data.pch60$post60
data.pch60$`bucket15` <- data.pch60$bucket == 15 & data.pch60$post60
reg.buckets.2 <- biglm(data = data.pch60,
                     zhvi.score ~ `bucket-7` + `bucket-6` + `bucket-5` + `bucket-4` +
                         `bucket-3` + `bucket-2` + `bucket-1` + `bucket0` + bucket1 +
                         bucket2 + bucket3 + bucket4 + bucket5 + bucket6 + bucket7 +
                         bucket8 + bucket9 + bucket10 + bucket11 + bucket12 +
                         bucket13 + bucket14 + bucket15 + unemp + density + period)
summary(reg.buckets.2)

# bigger buckets
data.pch60$bbucket <- as.factor(floor(data.pch60$m.s.pch60*5))
data.pch60$`bbucket-4` <- data.pch60$bbucket == -4 & data.pch60$post60
data.pch60$`bbucket-3` <- data.pch60$bbucket == -3 & data.pch60$post60
data.pch60$`bbucket-2` <- data.pch60$bbucket == -2 & data.pch60$post60
data.pch60$`bbucket-1` <- data.pch60$bbucket == -1 & data.pch60$post60
data.pch60$`bbucket0` <- data.pch60$bbucket == 0 & data.pch60$post60
data.pch60$`bbucket1` <- data.pch60$bbucket == 1 & data.pch60$post60
data.pch60$`bbucket2` <- data.pch60$bbucket == 2 & data.pch60$post60
data.pch60$`bbucket3` <- data.pch60$bbucket == 3 & data.pch60$post60
data.pch60$`bbucket4` <- data.pch60$bbucket == 4 & data.pch60$post60
data.pch60$`bbucket5` <- data.pch60$bbucket == 5 & data.pch60$post60
data.pch60$`bbucket6` <- data.pch60$bbucket == 6 & data.pch60$post60
data.pch60$`bbucket7` <- data.pch60$bbucket == 7 & data.pch60$post60

reg.buckets.3 <- biglm(data = data.pch60,
                       zhvi.score ~ `bbucket-4` +
                           `bbucket-3` + `bbucket-2` + `bbucket-1` + `bbucket0` + `bbucket1` +
                           bbucket2 + bbucket3 + bbucket4 + bbucket5 + bbucket6 + bbucket7 + 
                           unemp + density + period)

summary(reg.buckets.3)

# visualize the buckets
map.bucket.data <- data.pch60 %>% group_by(fips) %>% 
    summarize(bucket = first(bucket))
plot_usmap("counties", data = map.bucket.data, values = "bucket",
           include = unique(map.bucket.data$fips)) + 
    labs(title = "Visualization of 10% Buckets in Change for Multiyear Mean Smoke Exposure",
         subtitle = "Twenty One Bucket Model")
map.bucket.data2 <- data.pch60 %>% group_by(fips) %>% 
    summarize(bbucket = first(bbucket))
plot_usmap("counties", data = map.bucket.data2, values = "bbucket",
           include = unique(map.bucket.data2$fips)) +
    labs(title = "Visualization of 20% Buckets in Change for Multiyear Mean Smoke Exposure",
         subtitle = "Twelve Bucket Model")

# parallel trends for buckets
parallel.plt.data <- data.pch60 %>% group_by(bucket, period) %>% summarize(m = mean(zhvi.score))
parallel.plt.buckets <- ggplot(NULL) + labs(title = "Parallel Trends Across Buckets") +
    xlab("Period") + ylab("Mean ZHVI")
for (b in unique(data.pch60$bucket)){
    parallel.plt.buckets <- parallel.plt.buckets + 
        geom_line(data = filter(parallel.plt.data, bucket == b), 
                  aes(x = period, y = m, group = F))
}
parallel.plt.buckets

ggplot(data = parallel.plt.data, aes(x = period, y = m, group = bucket)) + geom_line() +
    labs(title = "Parallel Trends Across Buckets",
         subtitle = "Assumption Not Satisfied for All of the Buckets") +
    xlab("Period") + ylab("Mean ZHVI")
sens.data.pch60 <- data.frame(upper = seq(0.15,0.65, by = 0.05))
coef.vector <- c()
thresholds.sens.pch60 <- data.frame(data.pch60)


for (u in sens.data.pch60$upper){
    print(u)
    df <- cbind(thresholds.sens.pch60,
                s = as.factor(set.status(thresholds.sens.pch60$m.s.pch60, 0.05, u)))
    df <- filter(df, (s == "positive treatment") | (s == "control"))
    df$treatment <- as.numeric(df$s == "positive treatment" & df$post60)
    reg.out <- biglm(data = df, 
                     zhvi.score ~ treatment + unemp + period + fips)
    coef.vector <- c(coef.vector, coef(reg.out)[2])
}
sens.data.pch60$coef <- coef.vector
coef.vector

ggplot(data = sens.data.pch60, aes(x = upper, y = coef)) + geom_line() +
    labs(title = "Sensitivity of Changes in Upper Threshold")

plt.pch60.status <- data.pch60 %>% group_by(fips) %>% 
    summarize(status.k10.t50 = first(status.k10.t50))
#plot maps
plot_usmap("counties", data = plt.pch60.status, values = "status.k10.t50",
           include = unique(plt.pch60.status$fips)) + 
    labs(title = "Treatment/Control Status",
         subtitle = "Control Bound = 10%, Treatment Bound = 50%")
plot_usmap("counties", data = plt.pch55.status, values = "status.k5.t50",
           include = unique(plt.pch55.status$fips))
plot_usmap("counties", data = plt.pch55.status, values = "status.k10.t25",
           include = unique(plt.pch55.status$fips))
plot_usmap("counties", data = plt.pch55.status, values = "status.k10.t50",
           include = unique(plt.pch55.status$fips))



# parallel trends plot 
plt.means <- data.pch55.reg %>% group_by(status.k5.t50, period) %>% 
    summarize(m.z = mean(zhvi.score))
unique(plt.means$status.k5.t50)
filter(plt.means, status.k5.t50 == "positive treatment")
plt.means$period <- as.numeric(plt.means$period)

parallel <- ggplot(NULL, aes(x = period, y = m.z)) + 
    geom_vline(aes(xintercept = 55), linetype = "dashed") +
    geom_line(data = filter(plt.means, status.k5.t50 == "positive treatment"),
              aes(col = "treated")) +
    geom_line(data = filter(plt.means, status.k5.t50 == "control"),
              aes(col = "control"))

parallel

