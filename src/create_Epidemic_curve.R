# Script to plot epidemic curve
## traditional and time-lapse map


# libraries

my.packages <- c("readxl", "dplyr", "magrittr", "usmap", "ggplot2", "sf", "tidyr", "viridis", "animation")

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


png(filename = "./output/covid_incidence/epidemic_curve.png", height = 5, width = 5, units = "in", res = 300, pointsize = 12, family = "sans")
barplot(n.cases~week, data=wa.covid, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F)
axis(1, at=at.points, tick=T, labels = F)
text(x = at.points-2, y = par("usr")[3]-5, labels = paste(format(date.labels, "%d %b"),' '), srt = 45, pos = 1, xpd = TRUE,cex=0.7, offset = 1.15)
axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7)
title(main = "Total COVID-19 Cases in Washington, USA \n January 2020 - January 2021", xlab = "Week", ylab = "Cases", cex.lab = 0.9)
dev.off()


# plot(n.cases~week, data=wa.covid, type = "h", axes=F, xlab='', ylab='')
# axis(1, at=date.labels, tick=T, labels = F)
# text(x = date.labels-10, y = par("usr")[3]-5, labels = paste(format(date.labels, "%d %b"),' '), srt = 45, pos = 1, xpd = TRUE,cex=.7, offset = 1.15)
# 






# plot weekly case counts for each county on map, make animated gif

weeks <- unique(covid.weekly$week)
weeks <- weeks[order(weeks)]


my.breaks <- c(0, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, 75, 100, 200, 500, 1000, 1500, 2000, 3000, 5000, 6000)
my.palette <- viridis(length(my.breaks)-1)





saveGIF(


  for(i in 1:length(weeks)){
  par(mar=c(5.1,4.1,4.1,2.1))
  temp <- left_join(counties, covid.weekly[which(covid.weekly$week==weeks[i]),], by = "county")
  
  plot(temp['total.cases'], 
       pal = my.palette, 
       breaks = my.breaks, 
       main = paste0("\n Weekly Total COVID-19 Cases \n ", format(weeks[i], "%d %b")), 
       family = "sans", ps = 12)
  }, 
  
  movie.name = "C:/Users/Cody Dailey/Documents/Github Projects/washingtonCountiesCovid19/output/covid_incidence/cases_by_week_gif.gif", 
  ani.interval = 0.5, 
  ani.width = 1500, 
  ani.height = 1500, 
  ani.res = 300

)





# heat map for correlations among counties

covid.weekly.wide.by.county <- covid.weekly[order(covid.weekly$county, covid.weekly$week),] %>% 
  pivot_wider(names_from = "county", values_from = "total.cases") 


names(covid.weekly.wide.by.county) <- gsub(" County", "", names(covid.weekly.wide.by.county))

cor.mat.by.county <- cor(covid.weekly.wide.by.county[,-1])

png(filename = "./output/covid_incidence/epidemic_curve_correlations.png", height = 7, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
heatmap(cor.mat.by.county, Rowv=NA, Colv=NA, col = viridis(15))
dev.off()


png(filename = "./output/covid_incidence/epidemic_curve_correlations_clustered.png", height = 7, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
heatmap(cor.mat.by.county, Rowv=NA, col = viridis(15))
dev.off()





# heat map for correlations among weeks

covid.weekly.wide <- covid.weekly[order(covid.weekly$county, covid.weekly$week),] %>% 
  group_by(county) %>% 
  mutate(week.number = seq_along(county)) %>% 
  select(-week) %>%
  pivot_wider(names_prefix = "week", names_from = "week.number", values_from = "total.cases") 

cor.mat.by.week <- cor(covid.weekly.wide[,-1])

png(filename = "./output/covid_incidence/weekly_cases_correlations.png", height = 7, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
heatmap(cor.mat.by.week, Rowv=NA, Colv=NA, col = viridis(50))
dev.off()







