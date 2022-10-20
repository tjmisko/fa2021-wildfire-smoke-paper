library(dplyr)
library(ggplot2)
library(usmap)

setwd("/Users/tristanmisko/Documents/Berkeley/ECON/191/2021/Data/")
smoke <- read.csv("Clean\ Data/smoke_month_clean.csv",
                  colClasses = c("fips" = "character",
                                 "n.light" = "numeric",
                                 "n.medium" = "numeric",
                                 "n.heavy" = "numeric"))[,-1]
smoke <- filter(smoke, period > 0)

smoke_y <- smoke %>% group_by(STATEFP, year) %>%
    summarize(m.light.y = mean(n.light),
              m.medium.y = mean(n.medium),
              m.heavy.y = mean(n.heavy),
              m.score.y = mean(n.score))

smoke_y <- filter(smoke_y, !(STATEFP %in% c(2,4,6,8,15,16,30,32,35,41,49,53,56)))

plt.score <- ggplot(NULL, aes(x = year, y = m.score.y)) + ylab("Mean Medium Smoke Days") +
    xlab("Year") + scale_x_continuous(breaks = c(2010:2019)) + 
    labs(title = "Evolution of Smoke Score at the State Level")
for (statefp in unique(smoke_y$STATEFP)){
    plt.score <- plt.score + geom_line(data = filter(smoke_y, STATEFP == statefp))
}
plt.score


# find treated groups
smoke_y$l.treated <- smoke_y$m.light.y > 3
smoke_y$m.treated <- smoke_y$m.medium.y > 1
smoke_y$h.treated <- smoke_y$m.heavy.y > 0.3
treatment_status <- smoke_y %>% group_by(STATEFP) %>%
    summarize(l.treat = any(l.treated),
              m.treat = any(m.treated),
              h.treat = any(h.treated))
det.treat.l <- Vectorize(function(fips, df){
    return(filter(df, STATEFP == fips)$l.treat)
}, "fips")
det.treat.m <- Vectorize(function(fips, df){
    return(filter(df, STATEFP == fips)$m.treat)
}, "fips")
det.treat.h <- Vectorize(function(fips, df){
    return(filter(df, STATEFP == fips)$h.treat)
}, "fips")
smoke_y$l.treated <- det.treat.l(smoke_y$STATEFP, treatment_status)
smoke_y$m.treated <- det.treat.m(smoke_y$STATEFP, treatment_status)
smoke_y$h.treated <- det.treat.h(smoke_y$STATEFP, treatment_status)

p.light <- ggplot(NULL, aes(x = year, y = m.light.y)) + ylab("Mean Light Smoke Days") +
    xlab("Year") + scale_x_continuous(breaks = c(2010:2019)) + 
    labs(title = "Evolution of Light Smoke Days at the State Level")
for (statefp in unique(smoke_y$STATEFP)){
    p.light <- p.light + geom_line(data = filter(smoke_y, STATEFP == statefp),
                                   aes(col = l.treated))
}
p.light

p.med <- ggplot(NULL, aes(x = year, y = m.medium.y)) + ylab("Mean Medium Smoke Days") +
    xlab("Year") + scale_x_continuous(breaks = c(2010:2019)) + 
    labs(title = "Evolution of Medium Smoke Days at the State Level")
for (statefp in unique(smoke_y$STATEFP)){
    p.med <- p.med + geom_line(data = filter(smoke_y, STATEFP == statefp),
                               aes(col = m.treated))
}
p.med


p.heavy <- ggplot(NULL, aes(x = year, y = m.heavy.y)) + ylab("Mean Heavy Smoke Days") +
    xlab("Year") + scale_x_continuous(breaks = c(2010:2019)) + 
    labs(title = "Evolution of Heavy Smoke Days at the State Level")
for (statefp in unique(smoke_y$STATEFP)){
    p.heavy <- p.heavy + geom_line(data = filter(smoke_y, STATEFP == statefp),
                                   aes(col = h.treated))
}
p.heavy

smoke.means.l <- smoke_y %>% group_by(year, l.treated) %>%
    summarize(l.mu = mean(m.light.y))
q.light <- ggplot(NULL, aes(x = year, y = l.mu)) + 
    geom_line(data = filter(smoke.means.l, l.treated == FALSE), aes(col = l.treated)) +
    geom_line(data = filter(smoke.means.l, l.treated == TRUE), aes(col = l.treated))
smoke.means.m <- smoke_y %>% group_by(year, m.treated) %>%
    summarize(m.mu = mean(m.medium.y))
q.medium <- ggplot(NULL, aes(x = year, y = m.mu)) + 
    geom_line(data = filter(smoke.means.m, m.treated == FALSE), aes(col = m.treated)) +
    geom_line(data = filter(smoke.means.m, m.treated == TRUE), aes(col = m.treated))
smoke.means.h <- smoke_y %>% group_by(year, h.treated) %>%
    summarize(h.mu = mean(m.heavy.y))
q.heavy <- ggplot(NULL, aes(x = year, y = h.mu)) + 
    geom_vline(aes(xintercept = 2014.5), linetype = "dashed", col = "grey") +
    geom_line(data = filter(smoke.means.h, h.treated == FALSE), aes(col = h.treated)) +
    geom_line(data = filter(smoke.means.h, h.treated == TRUE), aes(col = h.treated)) +
    scale_x_continuous(breaks = c(2010:2019))

q.light
q.medium
q.heavy


# determine treated counties I want the counties where the percent change in the
# average number of smoke 

smoke$post2014 <- smoke$year > 2014
det_treatment <- smoke %>% group_by(fips, post2014) %>% 
    summarize(m.l = mean(n.light), 
              m.m = mean(n.medium), 
              m.h = mean(n.heavy),
              m.score = mean(n.score))
det_treatment %>% group_by(fips)

changelst <- list()
counter <- 1
for (f in unique(det_treatment$fips)){
    df <- filter(det_treatment, fips == f) 
    out1 <- select(filter(df, !post2014), c(1, 3:6))
    out2 <- select(filter(df, post2014), c(1, 3:6))
    colnames(out1) <- c("fips",
                        "m.l.pre2014",
                        "m.m.pre2014",
                        "m.h.pre2014",
                        "m.score.pre2014")
    colnames(out2) <- c("fips",
                        "m.l.post2014",
                        "m.m.post2014",
                        "m.h.post2014",
                        "m.score.post2014")
    changelst[[counter]] <- cbind(out1, out2[,2:5])
    counter <- counter + 1
}

changes <- bind_rows(changelst)
changes$STATEFP <- as.numeric(substr(changes$fips, 1, 2))
changes <- filter(changes, !(STATEFP %in% c(2,4,6,8,15,16,30,32,35,41,49,53,56)))
changes$l.delta <- changes$m.l.post2014 - changes$m.l.pre2014
changes$m.delta <- changes$m.m.post2014 - changes$m.m.pre2014
changes$h.delta <- changes$m.h.post2014 - changes$m.h.pre2014
changes$s.delta <- changes$m.score.post2014 - changes$m.score.pre2014
changes$s.treat.positive <- changes$s.delta > 5
changes$s.treat.negative <- changes$s.delta < -5
changes$s.control <- abs(changes$s.delta) < 2

get_status <- Vectorize(function(t.p, t.n, c){
    if (t.p){
        return("treated.positive")
    } else if (t.n){
        return("treated.negative")
    } else if (c){
        return("control")
    } else {
        return("boundary")
    }
}, c("t.p", "t.n", "c"))
changes$s.status <- get_status(changes$s.treat.positive, 
                               changes$s.treat.negative, 
                               changes$s.control)
plt.change.s <- ggplot(NULL) +
    scale_x_continuous(breaks = c(0,1)) + ylab("Mean Days of Smoke Score") +
    xlab("Pre-2014 to Post 2014") + ylim(c(0, 80)) + xlim(c(-0.1,1.1))
sub.s <- sample(changes$fips, 200, replace = F)
for (f in sub.s){
    df <- filter(changes, fips == f)
    plt.change.s <- plt.change.s +
        geom_segment(data = df,
                     aes(x = 0, xend = 1, 
                         y = m.score.pre2014, yend = m.score.post2014,
                         col = s.status),
                     alpha = 0.75)
}
plt.change.s + labs(title = "Subsample of ")


p <- plot_usmap(regions = "counties", data = changes, values = "s.status",
           include = unique(changes$fips)) +
    scale_fill_manual(values = c("red", "darkgreen", "blue", "yellow"), 
                      name = "Treatment") +
    theme(legend.position = "right") + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA),  
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA)) +
    labs(title = "Treatment Status by County",
         subtitle = "Status Determined by Difference in Pre-2014 and Post-2014 Mean Smoke Score")

