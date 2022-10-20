library(dplyr)
library(ggplot2)
library(data.table)
library(stargazer)
library(usmap)

setwd("/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/")
smoke <- read.csv("Clean\ Data/smoke_month_clean.csv",
                  colClasses = ("fips" = "character"))
smoke <- filter(smoke, !(STATEFP %in% c(2,4,6,8,15,16,30,32,35,41,49,53,56)))
smoke$month <- as.integer(smoke$month)
unemp <- read.csv("Clean\ Data/unemp_clean.csv", 
                  colClasses = c("fips" = "character",
                                 "period" = "numeric"))[,-1]
density <- read.csv("Raw\ Data/Housing\ Characteristics/density.csv",
                    colClasses = ("GEOID" = "character"))
density <- select(density, c(3,16))
plot_usmap("counties", include = unique(smoke$fips))
zhvi <- read.csv("Clean\ Data/zhvi_clean.csv",
                 colClasses = ("fips" = "character"))
zhvi$month <- as.integer(zhvi$month)
cg_2010 <- zhvi %>% filter(year >= 2010) %>% group_by(fips) %>%
    summarize(mu = mean(zhvi.score),
              sd = sd(zhvi.score),
              cg = 1 - sum(is.na(zhvi.score))/307)
zhvi <- zhvi %>%
    filter(!as.logical(zhvi$fips %in% filter(cg_2010, cg < 1)$fips)) %>%
    filter(year >= 2010)

zhvi <- filter(zhvi, (fips %in% unique(smoke$fips)), year %in% 2010:2019)
typeof(zhvi$month)
zhvi$month
smoke <- filter(smoke, fips %in% unique(zhvi$fips))

zhvi <- select(zhvi, c(-1, -2, -4, -7, -5, -8, -9, -10,-12))
zhvi <- select(zhvi, c(5, 6, 3, 4, 2, 1))

zhvi <- arrange(zhvi, fips, year, month)
smoke <- arrange(select(smoke, -1), fips, year, month)
data <- select(cbind(zhvi, smoke), c(-7, -8, -9, - 16))
data <- filter(data, period > 0)

unemp <- arrange(filter(unemp, fips %in% unique(data$fips)), fips, period)


data$unemp <- unemp$unemp

colnames(density) <- c("fips", "density")
density <- filter(density, fips %in% unique(data$fips))
density.lookup <- data.table(density)
setkey(density.lookup, "fips")

get.density <- Vectorize(function(f, dt){
    print(f)
    return(dt[fips == f, 2])
}, "f")

data$density <- unlist(get.density(data$fips, density.lookup))
data$density <- as.numeric(unlist(data$density))
View(data)
data$zhvi.score <- as.numeric(data$zhvi.score)

# determine treatment groups for different choices of treatment: wildfire 
# season begins in may/june, and I also have calendar year beginning dates

data$post43 <- as.numeric(data$period) > 43
data$post48 <- as.numeric(data$period) > 48
data$post55 <- as.numeric(data$period) > 55
data$post60 <- as.numeric(data$period) > 60
data$post67 <- as.numeric(data$period) > 67


data$n.light <- as.numeric(data$n.light)
data$n.medium <- as.numeric(data$n.medium)
data$n.heavy <- as.numeric(data$n.heavy)
data$n.score <- as.numeric(data$n.score)

mean43 <- data %>% group_by(fips, post43) %>% 
    summarize(m.l = mean(n.light), 
              m.m = mean(n.medium), 
              m.h = mean(n.heavy),
              m.score = mean(n.score))
mean48 <- data %>% group_by(fips, post48) %>% 
    summarize(m.l = mean(n.light), 
              m.m = mean(n.medium), 
              m.h = mean(n.heavy),
              m.score = mean(n.score))
mean55 <- data %>% group_by(fips, post55) %>% 
    summarize(m.l = mean(n.light), 
              m.m = mean(n.medium), 
              m.h = mean(n.heavy),
              m.score = mean(n.score))
mean60 <- data %>% group_by(fips, post60) %>% 
    summarize(m.l = mean(n.light), 
              m.m = mean(n.medium), 
              m.h = mean(n.heavy),
              m.score = mean(n.score))
mean67 <- data %>% group_by(fips, post67) %>% 
    summarize(m.l = mean(n.light), 
              m.m = mean(n.medium), 
              m.h = mean(n.heavy),
              m.score = mean(n.score))

mean43 <- data.table(mean43)
mean48 <- data.table(mean48)
mean55 <- data.table(mean55)
mean60 <- data.table(mean60)
mean67 <- data.table(mean67)

setkey(mean43, "fips")
setkey(mean48, "fips")
setkey(mean55, "fips")
setkey(mean60, "fips")
setkey(mean67, "fips")

changelst <- list()
counter <- 1
for (f in unique(mean43$fips)){
    print(paste("Computing Means for FIPS Code", f))
    out1.43 <- mean43[fips == f & post43 == F, c(1,3:6)]
    out2.43 <- mean43[fips == f & post43 == T, c(1,3:6)]
    out1.48 <- mean48[fips == f & post48 == F, c(1,3:6)]
    out2.48 <- mean48[fips == f & post48 == T, c(1,3:6)]
    out1.55 <- mean55[fips == f & post55 == F, c(1,3:6)]
    out2.55 <- mean55[fips == f & post55 == T, c(1,3:6)]
    out1.60 <- mean60[fips == f & post60 == F, c(1,3:6)]
    out2.60 <- mean60[fips == f & post60 == T, c(1,3:6)]
    out1.67 <- mean67[fips == f & post67 == F, c(1,3:6)]
    out2.67 <- mean67[fips == f & post67 == T, c(1,3:6)]
    colnames(out1.43) <- c("fips",
                        "m.l.pre43",
                        "m.m.pre43",
                        "m.h.pre43",
                        "m.s.pre43")
    colnames(out2.43) <- c("fips",
                        "m.l.post43",
                        "m.m.post43",
                        "m.h.post43",
                        "m.s.post43")
    colnames(out1.48) <- c("fips",
                           "m.l.pre48",
                           "m.m.pre48",
                           "m.h.pre48",
                           "m.s.pre48")
    colnames(out2.48) <- c("fips",
                           "m.l.post48",
                           "m.m.post48",
                           "m.h.post48",
                           "m.s.post48")
    colnames(out1.55) <- c("fips",
                           "m.l.pre55",
                           "m.m.pre55",
                           "m.h.pre55",
                           "m.s.pre55")
    colnames(out2.55) <- c("fips",
                           "m.l.post55",
                           "m.m.post55",
                           "m.h.post55",
                           "m.s.post55")
    colnames(out1.60) <- c("fips",
                           "m.l.pre60",
                           "m.m.pre60",
                           "m.h.pre60",
                           "m.s.pre60")
    colnames(out2.60) <- c("fips",
                           "m.l.post60",
                           "m.m.post60",
                           "m.h.post60",
                           "m.s.post60")
    colnames(out1.67) <- c("fips",
                           "m.l.pre67",
                           "m.m.pre67",
                           "m.h.pre67",
                           "m.s.pre67")
    colnames(out2.67) <- c("fips",
                           "m.l.post67",
                           "m.m.post67",
                           "m.h.post67",
                           "m.s.post67")
    changelst[[counter]] <- cbind(out1.43[,1:5], out2.43[,2:5], 
                                  out1.48[,2:5], out2.48[,2:5], 
                                  out1.55[,2:5], out2.55[,2:5],
                                  out1.60[,2:5], out2.60[,2:5],
                                  out1.67[,2:5], out2.67[,2:5])
    counter <- counter + 1
}
changes <- bind_rows(changelst)
changes <- select(changes, c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41))
head(changes)
changes$m.s.delta43 <- changes$m.s.post43 - changes$m.s.pre43
changes$m.s.delta48 <- changes$m.s.post48 - changes$m.s.pre48
changes$m.s.delta55 <- changes$m.s.post55 - changes$m.s.pre55
changes$m.s.delta60 <- changes$m.s.post60 - changes$m.s.pre60
changes$m.s.delta67 <- changes$m.s.post67 - changes$m.s.pre67

changes$m.s.pch43 <- (changes$m.s.post43 - changes$m.s.pre43)/changes$m.s.pre43
changes$m.s.pch48 <- (changes$m.s.post48 - changes$m.s.pre48)/changes$m.s.pre48
changes$m.s.pch55 <- (changes$m.s.post55 - changes$m.s.pre55)/changes$m.s.pre55
changes$m.s.pch60 <- (changes$m.s.post60 - changes$m.s.pre60)/changes$m.s.pre60
changes$m.s.pch67 <- (changes$m.s.post67 - changes$m.s.pre67)/changes$m.s.pre67

changes.lookup <- data.table(changes)
setkey(changes.lookup, "fips")
input.df <- data.frame(fips = data$fips,
                       period = data$period,
                       "m.s.pre43" = 0, 
                       "m.s.post43" = 0, 
                       "m.s.delta43"= 0, 
                       "m.s.pch43"= 0,
                       "m.s.pre48"= 0, 
                       "m.s.post48"= 0, 
                       "m.s.delta48"= 0, 
                       "m.s.pch48"= 0,
                       "m.s.pre55"= 0, 
                       "m.s.post55"= 0, 
                       "m.s.delta55"= 0, 
                       "m.s.pch55"= 0,
                       "m.s.pre60"= 0, 
                       "m.s.post60"= 0, 
                       "m.s.delta60"= 0, 
                       "m.s.pch60"= 0,
                       "m.s.pre67"= 0, 
                       "m.s.post67"= 0, 
                       "m.s.delta67"= 0, 
                       "m.s.pch67"= 0)
input.df <- as.matrix(input.df)

for (r in 1:(nrow(input.df)/115)){
    print(paste("row", r))
    df <- matrix(rep(as.matrix(select(changes.lookup[fips == input.df[115*(r-1) + 1, 1]], 
                                      c(2,3,12,17,4,5,13,18,6,7,14,19,8,9,15,20,10,11,16,21))), 
               115), byrow = T, ncol = 20)
    input.df[(115*(r-1)+1):(115*(r-1) + 115), 3:22] <- df
}

data <- cbind(data, data.frame(input.df[,c(-1,-2)]))
data

write.csv(data, "/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/Clean\ Data/main.csv")
