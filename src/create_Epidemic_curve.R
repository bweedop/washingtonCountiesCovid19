# Script to plot epidemic curve
## traditional and time-lapse map


# libraries

my.packages <- c("readxl", "dplyr", "magrittr", "usmap", "ggplot2", "sf", "tidyr", "viridis", "animation", "spdep", "purrr", "rsatscan")

lapply(my.packages, library, character.only = TRUE)



# import data

population <- read_xlsx("./data/rawData/PopulationData/co-est2019-annres-53.xlsx", skip = 3, n_max = 40)

case <- read_xlsx("./data/rawData/COVID-19CaseData/WA_COVID19_Cases_Hospitalizations_Deaths.xlsx")

counties <- st_read("./data/rawData/Spatial/county10/county10.shp")

# Simple feature collection with 39 features and 39 fields
# geometry type:  POLYGON
# dimension:      XY
# bbox:           xmin: 579552.7 ymin: 81835.08 xmax: 2551107 ymax: 1355593
# projected CRS:  NAD83(HARN) / Washington South (ftUS)





# clean

population$county <- sapply(population[,1], function(x){gsub("^[\\.]([^,]+)[,].*", "\\1", x)})

population %<>% select(county, pop2019 = `2019`) %>% 
  filter(!county%in%c("Washington"))

counties %<>% select(county = NAMELSAD10, geometry)

case %<>% select(county = County, week = WeekStartDate, total.cases = TotalCases) %>% 
  mutate(week = as.Date(week, format = "%Y-%m-%d")) %>%
  filter(!county%in%c("Unassigned"))






# summary

summary(population)
summary(counties)
summary(case)

case.summary <- case %>% 
                  group_by(county) %>% 
                  summarise(n.obs = n(), 
                            min.case = min(total.cases), 
                            max.case = max(total.cases), 
                            cum.cases = sum(total.cases), 
                            peak.date = week[which.max(total.cases)]) %>% 
                  full_join(population, by="county") %>%
                  mutate(stdzd.cum.cases = cum.cases / pop2019 * 100000) %>%
                  arrange(desc(cum.cases))
  
  


# get weekly, add missings = 0

shell.df <- expand.grid(county = unique(case$county), 
                        week = unique(case$week))


covid.weekly <- left_join(shell.df, case, by=c("county", "week")) %>% 
                  mutate(total.cases = ifelse(is.na(total.cases), 0, total.cases))





# plot


## simple epidemic curve

wa.covid <- covid.weekly %>% 
              group_by(week) %>% 
              summarise(n.cases = sum(total.cases))


plot(n.cases~week, data=wa.covid, type = "o")


par(mar=c(5.1,4.1,4.1,2.1))




date.labels <- seq(min(wa.covid$week), max(wa.covid$week), length.out = 19)

at.points <- seq(0.5, 54.5, by=1)[which(wa.covid$week%in%date.labels)]


par(mar=c(5.1,4.1,4.1,2.1))


# png(filename = "./output/covid_incidence/epidemic_curve.png", height = 5, width = 5, units = "in", res = 300, pointsize = 12, family = "sans")
barplot(n.cases~week, data=wa.covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F)
axis(1, at=at.points, tick=T, labels = F)
text(x = at.points-2, y = par("usr")[3]-5, labels = paste(format(date.labels, "%d %b"),' '), srt = 45, pos = 1, xpd = TRUE,cex=0.7, offset = 1.15)
axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7)
title(main = "Total COVID-19 Cases in Washington, USA \n January 2020 - January 2021", xlab = "Week", ylab = "Cases", cex.lab = 0.9)
# dev.off()


# plot(n.cases~week, data=wa.covid, type = "h", axes=F, xlab='', ylab='')
# axis(1, at=date.labels, tick=T, labels = F)
# text(x = date.labels-10, y = par("usr")[3]-5, labels = paste(format(date.labels, "%d %b"),' '), srt = 45, pos = 1, xpd = TRUE,cex=.7, offset = 1.15)
# 






# plot weekly case counts for each county on map, make animated gif

weeks <- unique(covid.weekly$week)
weeks <- weeks[order(weeks)]


my.breaks <- c(0, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, 75, 100, 200, 500, 1000, 1500, 2000, 3000, 5000, 6000)
my.palette <- viridis(length(my.breaks)-1)





# saveGIF(
# 
# 
#   for(i in 1:length(weeks)){
#   par(mar=c(5.1,4.1,4.1,2.1))
#   temp <- left_join(counties, covid.weekly[which(covid.weekly$week==weeks[i]),], by = "county")
#   
#   plot(temp['total.cases'], 
#        pal = my.palette, 
#        breaks = my.breaks, 
#        main = paste0("\n Weekly Total COVID-19 Cases \n ", format(weeks[i], "%d %b")), 
#        family = "sans", ps = 12)
#   }, 
#   
#   movie.name = "C:/Users/Cody Dailey/Documents/Github Projects/washingtonCountiesCovid19/output/covid_incidence/cases_by_week_gif.gif", 
#   ani.interval = 0.5, 
#   ani.width = 1500, 
#   ani.height = 1500, 
#   ani.res = 300
# 
# )





# heat map for correlations among counties

covid.weekly.wide.by.county <- covid.weekly[order(covid.weekly$county, covid.weekly$week),] %>% 
  pivot_wider(names_from = "county", values_from = "total.cases") 


names(covid.weekly.wide.by.county) <- gsub(" County", "", names(covid.weekly.wide.by.county))

cor.mat.by.county <- cor(covid.weekly.wide.by.county[,-1])

# png(filename = "./output/covid_incidence/epidemic_curve_correlations.png", height = 7, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
heatmap(cor.mat.by.county, Rowv=NA, Colv=NA, col = viridis(15))
# dev.off()


# png(filename = "./output/covid_incidence/epidemic_curve_correlations_clustered.png", height = 7, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
heatmap(cor.mat.by.county, Rowv=NA, col = viridis(15))
# dev.off()


rates.of.change <- covid.weekly.wide.by.county
rates.of.change[,2:40] <- rbind(rates.of.change[-1,2:40] / rates.of.change[-nrow(rates.of.change),2:40], rep(NA, length(2:40)))

rates.of.change %<>% mutate_at(2:40, function(x){ifelse(is.infinite(x) | is.nan(x), NA, x)})




roc.cor.mat <- cor(rates.of.change[,2:40], use = "pairwise.complete.obs")


roc.cor.mat95 <- roc.cor.mat
roc.cor.mat95[which(roc.cor.mat95<0.95)] <- 0

roc.cor.mat9 <- roc.cor.mat
roc.cor.mat9[which(roc.cor.mat9<0.9)] <- 0

roc.cor.mat8 <- roc.cor.mat
roc.cor.mat8[which(roc.cor.mat8<0.8)] <- 0

roc.cor.mat6 <- roc.cor.mat
roc.cor.mat6[which(roc.cor.mat6<0.6)] <- 0

roc.cor.mat4 <- roc.cor.mat
roc.cor.mat4[which(roc.cor.mat4<0.4)] <- 0

roc.cor.mat0 <- roc.cor.mat
roc.cor.mat0[which(roc.cor.mat0<0)] <- 0

roc.cor.mat.neg <- roc.cor.mat
roc.cor.mat.neg[which(roc.cor.mat.neg>=0)] <- 0
roc.cor.mat.neg <- abs(roc.cor.mat.neg)


centroids <- st_centroid(st_geometry(counties))

neighbors.list <- poly2nb(counties)



plot(counties$geometry, border = "grey")
plot(centroids, add=T)
plot(neighbors.list, centroids , add=T)




nb.matrix <- nb2mat(neighbors.list, style = "B")



nb.matrix.weighted8 <- nb.matrix*roc.cor.mat8
nb.matrix.weighted6 <- nb.matrix*roc.cor.mat6
nb.matrix.weighted4 <- nb.matrix*roc.cor.mat4
nb.matrix.weighted0 <- nb.matrix*roc.cor.mat0
nb.matrix.weighted.neg <- nb.matrix*roc.cor.mat.neg

nb.weighted8 <- mat2listw(nb.matrix.weighted8)
nb.weighted6 <- mat2listw(nb.matrix.weighted6)
nb.weighted4 <- mat2listw(nb.matrix.weighted4)
nb.weighted0 <- mat2listw(nb.matrix.weighted0)
nb.weighted.neg <- mat2listw(nb.matrix.weighted.neg)


# png(filename = "./output/covid_incidence/rates_of_change_correlation_neighbors.png", height = 7, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
plot(counties$geometry, border = "grey")
plot(centroids, add=T)
# plot(nb.weighted.neg, st_coordinates(centroids), lwd = 1, lty = 1, col="red", points = F, add=T)
plot(nb.weighted0, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(nb.weighted4, st_coordinates(centroids), lwd = 4, points = F, add=T)
plot(nb.weighted6, st_coordinates(centroids), lwd = 8, lty = 1, points = F, add=T)
plot(nb.weighted8, st_coordinates(centroids), lwd = 16, points = F, add=T)
legend("bottomleft", lwd=c(1,4,8,16), legend = as.expression(lapply(c(0.0,0.4,0.6,0.8), function(x) bquote(rho>.(x)))), cex = 0.6, y.intersp = 2)
# dev.off()


diag(roc.cor.mat0) <- 0
diag(roc.cor.mat4) <- 0
diag(roc.cor.mat6) <- 0
diag(roc.cor.mat8) <- 0
diag(roc.cor.mat9) <- 0
diag(roc.cor.mat95) <- 0


roc.cor.lw0 <- mat2listw(roc.cor.mat0)
roc.cor.lw4 <- mat2listw(roc.cor.mat4)
roc.cor.lw6 <- mat2listw(roc.cor.mat6)
roc.cor.lw8 <- mat2listw(roc.cor.mat8)
roc.cor.lw9 <- mat2listw(roc.cor.mat9)
roc.cor.lw95 <- mat2listw(roc.cor.mat95)



plot(counties$geometry, border = "grey")
plot(centroids, add=T)
# plot(roc.cor.lw0, st_coordinates(centroids), add=T)
# plot(roc.cor.lw4, st_coordinates(centroids), add=T)
# plot(roc.cor.lw6, st_coordinates(centroids), lwd = 1, add=T)
plot(roc.cor.lw8, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(roc.cor.lw9, st_coordinates(centroids), lwd = 4, points = F, add=T)
plot(roc.cor.lw95, st_coordinates(centroids), lwd = 16, points = F, add=T)





which(roc.cor.mat95!=0 & lower.tri(roc.cor.mat95), arr.ind = T)








# spatial autocorrelation
## morans i for each week

morans.i.results <- list()

for(i in 2:ncol(covid.weekly.wide)){
  morans.i.results[[i-1]] <- moran.test(as.matrix(covid.weekly.wide[,i]), nb2listw(neighbors.list))
}

morans.i.results <- data.frame(week = covid.weekly.wide.by.county$week, morans.i = sapply(morans.i.results, pluck, "statistic"), p = sapply(morans.i.results, pluck, "p.value"))




# satscan

write.table(covid.weekly%>%mutate(county=gsub(" ", "", county)), file = "./data/processedData/covid_weekly.txt", quote = F, row.names = F)


wa.coords <- cbind(county = gsub(" ", "", counties$county), st_coordinates(st_centroid(st_geometry(counties))))[,c(1,3,2)]

write.table(wa.coords, file = "./data/processedData/wa_coords.txt", quote = F, row.names = F)

write.table(population%>%mutate(county=gsub(" ", "", county)), file = "./data/processedData/wa_pop.txt", quote = F, row.names = F)






high.risk.cluster <- c("Benton County", "Franklin County", "Walla Walla County", "Yakima County",
                       "Klickitat County", "Grant County", "Adams County", "Columbia County",
                       "Kittitas County", "Garfield County", "Douglas County", "Whitman County",
                       "Lincoln County", "Asotin County", "Skamania County", "Chelan County",
                       "Pierce County", "Spokane County", "Lewis County")

low.risk.cluster <- c("Jefferson County", "Clallam County", "Mason County", "Kitsap County",
                      "Grays Harbor County", "Island County", "San Juan County", "Thurston County",
                      "King County")


clusters.map <- counties
clusters.map$risk <- ifelse(clusters.map$county%in%high.risk.cluster, "High Risk", ifelse(clusters.map$county%in%low.risk.cluster, "Low Risk", "Neutral"))

plot(clusters.map$geometry, col=as.factor(clusters.map$risk))
legend("bottomleft", fill = c(1:3), legend = c("High Risk", "Low Risk", "Neutral"))


# coordinates(spTransform(centroids.mp, CRS("+proj=longlat +datum=WGS84")))




covid.weekly.list <- list()

for(i in 1:length(weeks)){
  covid.weekly.list[[i]] <- covid.weekly[which(covid.weekly$week==weeks[i]),] %>% mutate(county=gsub(" ", "", county))
}

temporary.directory <- tempdir()


write.geo(wa.coords, temporary.directory,"WA")
write.pop(population%>%mutate(county=gsub(" ", "", county)), temporary.directory,"WA")


for(i in 1:length(weeks)){
  write.cas(covid.weekly.list[[i]], temporary.directory, paste0("WA", i))
}

satscan.wa <- list()


for(i in 1:length(weeks)){

invisible(ss.options(reset=TRUE))

ss.options(list(CaseFile=paste0("WA",i,".cas"),StartDate=format(weeks[i], "%Y/%m/%d"),EndDate=format(weeks[i], "%Y/%m/%d"), PrecisionCaseTimes=3, 
                PopulationFile="WA.pop",
                CoordinatesFile="WA.geo", CoordinatesType=0, AnalysisType=1, ModelType=0, ScanAreas=3, TimeAggregationUnits=3, TimeAggregationLength=1))
ss.options(c("NonCompactnessPenalty=0", "ReportGiniClusters=n", "LogRunToHistoryFile=n"))

write.ss.prm(temporary.directory,"WA_param")


satscan.wa[[i]] <- satscan(temporary.directory,"WA_param")

}

# sp::plot(NYCfever$shapeclust)


hist(unlist(NYCfever$llr), main="Monte Carlo")

# Let's draw a line for the clusters in the observed data
abline(v=NYCfever$col[,c("TEST_STAT")], col = "red")













# heat map for correlations among weeks

covid.weekly.wide <- covid.weekly[order(covid.weekly$county, covid.weekly$week),] %>%
  group_by(county) %>%
  mutate(week.number = seq_along(county)) %>%
  select(-week) %>%
  pivot_wider(names_prefix = "week", names_from = "week.number", values_from = "total.cases")

cor.mat.by.week <- cor(covid.weekly.wide[,-1])

# png(filename = "./output/covid_incidence/weekly_cases_correlations.png", height = 7, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
heatmap(cor.mat.by.week, Rowv=NA, Colv=NA, col = viridis(50))
# dev.off()









# star models


## first order neighbors
### solve calculates total number of neighbors and then reciprocal within matrix along diagonal 39*39
### multiplies to nb.matrix to take binary nb.matrix and replace with fraction 39*39
### multiplies to cases data (wide by week, 39*55) to yield 39*55 matrix with avg cases of neighbors at given timepoint
nb1.avg.cases <- solve(diag(apply(nb.matrix, 1, sum)))%*%nb.matrix%*%as.matrix(covid.weekly.wide[,2:56])
nb1.cases <- nb.matrix%*%as.matrix(covid.weekly.wide[,2:56])


## second order neighbors
nb2.matrix <- nb.matrix%*%nb.matrix
diag(nb2.matrix) <- 0
nb2.matrix[which(nb2.matrix>0)] <- 1

nb2.avg.cases <- solve(diag(apply(nb2.matrix, 1, sum)))%*%nb2.matrix%*%as.matrix(covid.weekly.wide[,2:56])
nb2.cases <- nb2.matrix%*%as.matrix(covid.weekly.wide[,2:56])


## time lagged case counts, stacked to column vector
### time lag 1 = t1
t1.lag.cases <- covid.weekly.wide[,2:55] %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
### time lag 2 = t2
t2.lag.cases <- covid.weekly.wide[,2:54] %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
### time lag 3 = t3
t3.lag.cases <- covid.weekly.wide[,2:53] %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()

## case counts stacked to column vector, length to match time lags
y1.cases <- covid.weekly.wide[,3:56] %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
y2.cases <- covid.weekly.wide[,4:56] %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
y3.cases <- covid.weekly.wide[,5:56] %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()


## spatial lag 1, aka first order neighbors, lagged at t=1,2,3
s1.t1.lag.cases <- nb1.avg.cases[,2:55] %>% as.data.frame() %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
s1.t2.lag.cases <- nb1.avg.cases[,2:54] %>% as.data.frame() %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
s1.t3.lag.cases <- nb1.avg.cases[,2:53] %>% as.data.frame() %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()

s1.t1.lag.acases <- nb1.cases[,2:55] %>% as.data.frame() %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
s1.t2.lag.acases <- nb1.cases[,2:54] %>% as.data.frame() %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
s1.t3.lag.acases <- nb1.cases[,2:53] %>% as.data.frame() %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()


## spatial lag 2, aka second order neighbors, lagged at t=1,2,3
s2.t1.lag.cases <- nb2.avg.cases[,2:55] %>% as.data.frame() %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
s2.t2.lag.cases <- nb2.avg.cases[,2:54] %>% as.data.frame() %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
s2.t3.lag.cases <- nb2.avg.cases[,2:53] %>% as.data.frame() %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()

s2.t1.lag.acases <- nb2.cases[,2:55] %>% as.data.frame() %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
s2.t2.lag.acases <- nb2.cases[,2:54] %>% as.data.frame() %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
s2.t3.lag.acases <- nb2.cases[,2:53] %>% as.data.frame() %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()




## try to incorporate rates of change into the model
### use raw rates, sub 1 for NAs

roc.wide.by.week <- t(rates.of.change[,-1])
roc.wide.by.week <- ifelse(is.na(roc.wide.by.week), 1, roc.wide.by.week)

t1.lag.roc <- roc.wide.by.week[,1:54] %>% as.data.frame() %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
t2.lag.roc <- roc.wide.by.week[,1:53] %>% as.data.frame() %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
t3.lag.roc <- roc.wide.by.week[,1:52] %>% as.data.frame() %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()



rates.of.change.sign <- covid.weekly.wide.by.county
rates.of.change.sign[,2:40] <- rbind(ifelse(rates.of.change[-1,2:40] == rates.of.change[-nrow(rates.of.change),2:40], 0, 
                                            ifelse(rates.of.change[-1,2:40] > rates.of.change[-nrow(rates.of.change),2:40], 1, 
                                                   ifelse(rates.of.change[-1,2:40] < rates.of.change[-nrow(rates.of.change),2:40], -1, NA))), rep(NA, length(2:40)))

roc.wide.by.week.sign <- t(rates.of.change.sign[,-1])
roc.wide.by.week.sign <- ifelse(is.na(roc.wide.by.week.sign), 0, roc.wide.by.week.sign)

t1.lag.roc.sign <- roc.wide.by.week.sign[,1:54] %>% as.data.frame() %>% pivot_longer(1:54) %>% select(value) %>% as.matrix()
t2.lag.roc.sign <- roc.wide.by.week.sign[,1:53] %>% as.data.frame() %>% pivot_longer(1:53) %>% select(value) %>% as.matrix()
t3.lag.roc.sign <- roc.wide.by.week.sign[,1:52] %>% as.data.frame() %>% pivot_longer(1:52) %>% select(value) %>% as.matrix()








## modeling

### lag time 1 and space 1
#### lm
star.lag.t1.s1 <- lm(y1.cases~t1.lag.cases+s1.t1.lag.cases)
summary(star.lag.t1.s1)

#### glm with log link
star.lag.t1.s1 <- glm(y1.cases~t1.lag.cases+s1.t1.lag.cases, family="poisson")
summary(star.lag.t1.s1)

#### negative binomial
summary(MASS::glm.nb(y1.cases~t1.lag.cases+s1.t1.lag.cases))
summary(MASS::glm.nb(y1.cases~t1.lag.cases+s1.t1.lag.acases))





### lag time 2 and space 1
#### lm
star.lag.t2.s1 <- lm(y2.cases~t2.lag.cases+s1.t2.lag.cases)
summary(star.lag.t2.s1)

#### glm with log link
star.lag.t2.s1 <- glm(y2.cases~t2.lag.cases+s1.t2.lag.cases, family="poisson")
summary(star.lag.t2.s1)

#### negative binomial
summary(MASS::glm.nb(y2.cases~t2.lag.cases+s1.t2.lag.cases))





### lag time 3 and space 1
#### lm
star.lag.t3.s1 <- lm(y3.cases~t3.lag.cases+s1.t3.lag.cases)
summary(star.lag.t3.s1)

#### glm with log link
star.lag.t3.s1 <- glm(y3.cases~t3.lag.cases+s1.t3.lag.cases, family="poisson")
summary(star.lag.t3.s1)

#### negative binomial
summary(MASS::glm.nb(y3.cases~t3.lag.cases+s1.t3.lag.cases))







### lag time 1 and space 2
#### lm
star.lag.t1.s2 <- lm(y1.cases~t1.lag.cases+s2.t1.lag.cases)
summary(star.lag.t1.s2)

#### glm with log link
star.lag.t1.s2 <- glm(y1.cases~t1.lag.cases+s2.t1.lag.cases, family="poisson")
summary(star.lag.t1.s2)

#### negative binomial
summary(MASS::glm.nb(y1.cases~t1.lag.cases+s2.t1.lag.cases))





### lag time 2 and space 2
#### lm
star.lag.t2.s2 <- lm(y2.cases~t2.lag.cases+s2.t2.lag.cases)
summary(star.lag.t2.s2)

#### glm with log link
star.lag.t2.s2 <- glm(y2.cases~t2.lag.cases+s2.t2.lag.cases, family="poisson")
summary(star.lag.t2.s2)

#### negative binomial
summary(MASS::glm.nb(y2.cases~t2.lag.cases+s2.t2.lag.cases))





### lag time 3 and space 2
#### lm
star.lag.t3.s2 <- lm(y3.cases~t3.lag.cases+s2.t3.lag.cases)
summary(star.lag.t3.s2)

#### glm with log link
star.lag.t3.s2 <- glm(y3.cases~t3.lag.cases+s2.t3.lag.cases, family="poisson")
summary(star.lag.t3.s2)

#### negative binomial
summary(MASS::glm.nb(y3.cases~t3.lag.cases+s2.t3.lag.cases))

















t1.xmat <- kronecker(diag(54), rep(1,39))[,1:53]
t2.xmat <- kronecker(diag(53), rep(1,39))[,1:52]
t3.xmat <- kronecker(diag(52), rep(1,39))[,1:51]


## modeling

# ### lag time 1 and space 1
# #### lm
# star.lag.t1.s1 <- lm(y1.cases~t1.lag.cases+s1.t1.lag.cases)
# summary(star.lag.t1.s1)
# 
# #### glm with log link
# star.lag.t1.s1 <- glm(y1.cases~t1.lag.cases+s1.t1.lag.cases, family="poisson")
# summary(star.lag.t1.s1)

#### negative binomial
summary(MASS::glm.nb(y1.cases~t1.lag.cases+s1.t1.lag.cases+t1.xmat))
summary(MASS::glm.nb(y1.cases~t1.lag.cases+s1.t1.lag.acases))





# ### lag time 2 and space 1
# #### lm
# star.lag.t2.s1 <- lm(y2.cases~t2.lag.cases+s1.t2.lag.cases)
# summary(star.lag.t2.s1)
# 
# #### glm with log link
# star.lag.t2.s1 <- glm(y2.cases~t2.lag.cases+s1.t2.lag.cases, family="poisson")
# summary(star.lag.t2.s1)

#### negative binomial
summary(MASS::glm.nb(y2.cases~t2.lag.cases+s1.t2.lag.cases+t2.xmat))





# ### lag time 3 and space 1
# #### lm
# star.lag.t3.s1 <- lm(y3.cases~t3.lag.cases+s1.t3.lag.cases)
# summary(star.lag.t3.s1)
# 
# #### glm with log link
# star.lag.t3.s1 <- glm(y3.cases~t3.lag.cases+s1.t3.lag.cases, family="poisson")
# summary(star.lag.t3.s1)

#### negative binomial
summary(MASS::glm.nb(y3.cases~t3.lag.cases+s1.t3.lag.cases+t3.xmat))







# ### lag time 1 and space 2
# #### lm
# star.lag.t1.s2 <- lm(y1.cases~t1.lag.cases+s2.t1.lag.cases)
# summary(star.lag.t1.s2)
# 
# #### glm with log link
# star.lag.t1.s2 <- glm(y1.cases~t1.lag.cases+s2.t1.lag.cases, family="poisson")
# summary(star.lag.t1.s2)

#### negative binomial
summary(MASS::glm.nb(y1.cases~t1.lag.cases+s2.t1.lag.cases+t1.xmat))





# ### lag time 2 and space 2
# #### lm
# star.lag.t2.s2 <- lm(y2.cases~t2.lag.cases+s2.t2.lag.cases)
# summary(star.lag.t2.s2)
# 
# #### glm with log link
# star.lag.t2.s2 <- glm(y2.cases~t2.lag.cases+s2.t2.lag.cases, family="poisson")
# summary(star.lag.t2.s2)

#### negative binomial
summary(MASS::glm.nb(y2.cases~t2.lag.cases+s2.t2.lag.cases+t2.xmat))





# ### lag time 3 and space 2
# #### lm
# star.lag.t3.s2 <- lm(y3.cases~t3.lag.cases+s2.t3.lag.cases)
# summary(star.lag.t3.s2)
# 
# #### glm with log link
# star.lag.t3.s2 <- glm(y3.cases~t3.lag.cases+s2.t3.lag.cases, family="poisson")
# summary(star.lag.t3.s2)

#### negative binomial
summary(MASS::glm.nb(y3.cases~t3.lag.cases+s2.t3.lag.cases+t3.xmat))







