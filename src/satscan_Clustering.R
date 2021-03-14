getwd()
dir()
dir("./output/covid_incidence/satscan/by_week")


list.files("./output/covid_incidence/satscan/by_week", full=T)
satscan.files <- list.files("./output/covid_incidence/satscan/by_week", full=T)



readLines(satscan.files[1])
satscan.output <- list()
satscan.output[[1]] <- readLines(satscan.files[1])
satscan.output[[1]]

indices <- which(satscan.output[[1]]%in%c("CLUSTERS DETECTED", "PARAMETER SETTINGS"))
indices

clusters.detected <- satscan.output[[1]][indices[1]:indices[2]-1]
clusters.detected



for(i in 1:length(satscan.files)){satscan.output[[i]] <- readLines(satscan.files[i])}

length(satscan.output)
clusters.detected <- list()

for(i in 1:length(satscan.output)){clusters.detected[[i]] <- satscan.output[[i]][which(satscan.output[[i]]%in%c("CLUSTERS DETECTED")):which(satscan.output[[i]]%in%c("PARAMETER SETTINGS"))-1]}
satscan.output[[2]]
clusters.detected[[2]]




grepl("Location IDs included", clusters.detected[[2]])
which(grepl("Location IDs included", clusters.detected[[2]]))

individual.clusters <- c()
for(i in 1:length(clusters.detected)){
  loc.indices <- which(grepl("Location IDs included", clusters.detected[[i]]))
  overlap.indices <- which(grepl("Overlap with clusters", clusters.detected[[i]]))
  each.cluster <- c()
  for(ii in 1:length(loc.indices)){
    each.cluster[ii] <- paste(clusters.detected[[i]][loc.indices[ii]:overlap.indices[ii]-1], collapse="")
  }
  individual.clusters <- c(individual.clusters, each.cluster)
}




individual.clusters <- list()
for(i in 1:length(clusters.detected)){
  loc.indices <- which(grepl("Location IDs included", clusters.detected[[i]]))
  pval.indices <- which(grepl("P-value", clusters.detected[[i]]))
  each.cluster <- list()
  for(ii in 1:length(loc.indices)){
    each.cluster[[ii]] <- clusters.detected[[i]][loc.indices[ii]:pval.indices[ii]]
    while(substr(each.cluster[[ii]][1], nchar(each.cluster[[ii]][1]), nchar(each.cluster[[ii]][1]))==","){
      each.cluster[[ii]] <- c(paste(each.cluster[[ii]][1], gsub("^[[:space:]]{2,}", "", each.cluster[[ii]][2])), each.cluster[[ii]][3:length(each.cluster[[ii]])])
    }
  }
  individual.clusters[[i]] <- each.cluster
}


individual.clusters

individual.clusters.dfs <- list()
for(i in 1:length(individual.clusters)){
  
  weekly.clusters.dfs <- list()
  
  for(ii in 1:length(individual.clusters[[i]])){
    temp <- unlist(lapply(strsplit(individual.clusters[[i]][[ii]], split="[.]{1,}:[[:space:]]"), function(x){purrr::pluck(x, 2)})) %>% t() %>% data.frame("Cluster Number"=gsub("([0-9]+).*$", "\\1", individual.clusters[[i]][[ii]][1]), .)
    temp2 <- c("Cluster Number", unlist(lapply(strsplit(individual.clusters[[i]][[ii]], split="[.]{1,}:[[:space:]]"), function(x){x%>%purrr::pluck(1)%>%gsub("(^[0-9]+[.])|(^\\s{2,})", "", .)})))
    temp3 <- setNames(temp, temp2)
    
    weekly.clusters.dfs[[ii]] <- temp3
  }
  
  individual.clusters.dfs[[i]] <- bind_rows(weekly.clusters.dfs) %>% mutate(week = weeks[i])
  
}


all.clusters.df <- bind_rows(individual.clusters.dfs) %>% mutate("Cluster Size" = NA) %>% select(week, "Cluster Number", "Cluster Size", everything())
all.clusters.df$`Cluster Size` <- sapply(all.clusters.df$`Location IDs included`, function(x){length(unlist(strsplit(x, split=", ")))})




      # 
      # gsub(".*Location IDs included.: ", "", individual.clusters[1])
      # gsub(".*Location IDs included.: ", "", individual.clusters)
      # all.clusters <- gsub(".*Location IDs included.: ", "", individual.clusters)
      # 
      # counties.in.clusters <- list()
      # for(i in 1:length(all.clusters)){counties.in.clusters[[i]] <- c(gsub(", |[[:space:]]", "", unlist(strsplit(all.clusters[i], split="County"))))}
      # counties.in.clusters[[1]]
      # counties.in.clusters[[2]]
      # counties.in.clusters[[3]]
      # counties.in.clusters[[4]]
      # 
      # 
      # 
      # county.names <- gsub(" ", "", gsub(" County", "", unique(counties$county)))
      # cluster.df <- data.frame(county = county.names)
      # for(i in 1:length(county.names)){
      #   cluster.df[,county.names[i]] <- NA
      #   for(ii in 1:length(county.names)){
      #     cluster.df[which(cluster.df$county==county.names[ii]),county.names[i]] <- length(which(grepl(county.names[i], counties.in.clusters) & grepl(county.names[ii], counties.in.clusters)))
      #   }
      # }
      # cluster.df[1:5,]
      # ncol(cluster.df)


# all clusters, all weeks

county.names <- gsub(" ", "", gsub(" County", "", unique(counties$county)))
cluster.df <- data.frame(county = county.names)
for(i in 1:length(county.names)){
  cluster.df[,county.names[i]] <- NA
  for(ii in 1:length(county.names)){
    cluster.df[which(cluster.df$county==county.names[ii]),county.names[i]] <- length(which(grepl(county.names[i], all.clusters.df$`Location IDs included`) & grepl(county.names[ii], all.clusters.df$`Location IDs included`)))
  }
}
cluster.df[1:5,]
ncol(cluster.df)



# high risk clusters, all weeks

cluster.df <- data.frame(county = county.names)
for(i in 1:length(county.names)){
  cluster.df[,county.names[i]] <- NA
  for(ii in 1:length(county.names)){
    cluster.df[which(cluster.df$county==county.names[ii]),county.names[i]] <- length(which(grepl(county.names[i], all.clusters.df$`Location IDs included`[which(all.clusters.df$`Relative risk`>1)]) & grepl(county.names[ii], all.clusters.df$`Location IDs included`[which(all.clusters.df$`Relative risk`>1)])))
  }
}
cluster.df[1:5,]
ncol(cluster.df)



# high risk clusters, before october

cluster.df <- data.frame(county = county.names)
for(i in 1:length(county.names)){
  cluster.df[,county.names[i]] <- NA
  for(ii in 1:length(county.names)){
    cluster.df[which(cluster.df$county==county.names[ii]),county.names[i]] <- length(which(grepl(county.names[i], all.clusters.df$`Location IDs included`[which(all.clusters.df$`Relative risk`>1 & all.clusters.df$week<as.Date("2020-10-01"))]) & grepl(county.names[ii], all.clusters.df$`Location IDs included`[which(all.clusters.df$`Relative risk`>1 & all.clusters.df$week<as.Date("2020-10-01"))])))
  }
}
cluster.df[1:5,]
ncol(cluster.df)



cluster.matrix <- as.matrix(cluster.df[,2:40])
rownames(cluster.matrix) <- cluster.df$county


hist(diag(cluster.matrix))
quantile(diag(cluster.matrix))
which(diag(cluster.matrix)>66)
diag(cluster.matrix)
diag(cluster.matrix)[which(diag(cluster.matrix)>66)]


cluster.matrix.nd <- cluster.matrix
diag(cluster.matrix.nd) <- 0
hist(cluster.matrix[lower.tri(cluster.matrix)])
quantile(cluster.matrix[lower.tri(cluster.matrix)])

which(cluster.matrix >30 & lower.tri(cluster.matrix))

cluster.matrix.nd[1:5,]
hist(cluster.matrix.nd[lower.tri(cluster.matrix.nd)])
which(cluster.matrix >30 & lower.tri(cluster.matrix), arr.ind=T)
ls()
cluster.matrix
cluster.matrix.nd



cluster.lw <- mat2listw(cluster.matrix.nd)
county.names
county.relation <- cbind(unique(counties$county), county.names)
county.relation
row.names(cluster.matrix.nd)
names(county.relation)
row.names(cluster.matrix.nd) <- county.relation[,1]

colnames(cluster.matrix.nd) <- county.relation[,1]
cluster.lw <- mat2listw(cluster.matrix.nd)
print(cluster.lw)
39*39



plot(counties$geometry, border = "grey")
plot(centroids, add=T)
plot(cluster.lw, st_coordinates(centroids), lwd = 1, points = F, add=T)
hist(cluster.matrix.nd[lower.tri(cluster.matrix.nd)])



cluster.matrix.nd.65 <- cluster.matrix.nd
cluster.matrix.nd.65[which(cluster.matrix.nd.65<65)] <- 0
cluster.lw65 <- mat2listw(cluster.matrix.nd.65)


cluster.matrix.nd.60 <- cluster.matrix.nd
cluster.matrix.nd.60[which(cluster.matrix.nd.60<60)] <- 0
cluster.lw60 <- mat2listw(cluster.matrix.nd.60)

cluster.matrix.nd.55 <- cluster.matrix.nd
cluster.matrix.nd.55[which(cluster.matrix.nd.55<55)] <- 0
cluster.lw55 <- mat2listw(cluster.matrix.nd.55)



cluster.matrix.nd.50 <- cluster.matrix.nd
cluster.matrix.nd.50[which(cluster.matrix.nd.50<50)] <- 0
cluster.lw50 <- mat2listw(cluster.matrix.nd.50)


cluster.matrix.nd.45 <- cluster.matrix.nd
cluster.matrix.nd.45[which(cluster.matrix.nd.45<45)] <- 0
cluster.lw45 <- mat2listw(cluster.matrix.nd.45)


cluster.matrix.nd.40 <- cluster.matrix.nd
cluster.matrix.nd.40[which(cluster.matrix.nd.40<40)] <- 0
cluster.lw40 <- mat2listw(cluster.matrix.nd.40)


cluster.matrix.nd.35 <- cluster.matrix.nd
cluster.matrix.nd.35[which(cluster.matrix.nd.35<35)] <- 0
cluster.lw35 <- mat2listw(cluster.matrix.nd.35)


cluster.matrix.nd.30 <- cluster.matrix.nd
cluster.matrix.nd.30[which(cluster.matrix.nd.30<30)] <- 0
cluster.lw30 <- mat2listw(cluster.matrix.nd.30)




cluster.matrix.nd.25 <- cluster.matrix.nd
cluster.matrix.nd.25[which(cluster.matrix.nd.25<25)] <- 0
cluster.lw25 <- mat2listw(cluster.matrix.nd.25)







cluster.matrix.nd.20 <- cluster.matrix.nd
cluster.matrix.nd.20[which(cluster.matrix.nd.20<20)] <- 0
cluster.lw20 <- mat2listw(cluster.matrix.nd.20)


cluster.matrix.nd.15 <- cluster.matrix.nd
cluster.matrix.nd.15[which(cluster.matrix.nd.15<15)] <- 0
cluster.lw15 <- mat2listw(cluster.matrix.nd.15)


cluster.matrix.nd.10 <- cluster.matrix.nd
cluster.matrix.nd.10[which(cluster.matrix.nd.10<10)] <- 0
cluster.lw10 <- mat2listw(cluster.matrix.nd.10)


cluster.matrix.nd.5 <- cluster.matrix.nd
cluster.matrix.nd.5[which(cluster.matrix.nd.5<5)] <- 0
cluster.lw5 <- mat2listw(cluster.matrix.nd.5)



plot(counties$geometry, border = "grey")
plot(centroids, add=T)
plot(cluster.lw65, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw60, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw50, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw45, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw40, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw35, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw25, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw20, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw15, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw10, st_coordinates(centroids), lwd = 1, points = F, add=T)
plot(cluster.lw5, st_coordinates(centroids), lwd = 1, points = F, add=T)


sequence.id <- seq(65,5, by=-5)
sequence.id

length(sequence.id)

get(paste0("cluster.lw", sequence.id[1]))


i=1


# plot(counties$geometry, border = "grey", main = paste0("Spatial Clustering\nCounties that Appeared Together more than ", sequence.id[i], " times"), family = "sans", ps = 12)
# plot(centroids, add=T)
# plot(get(paste0("cluster.lw", sequence.id[i])), st_coordinates(centroids), lwd = 1, points = F, add=T)  
# 
# saveGIF(
#   for(i in 1:length(sequence.id)){
#     plot(counties$geometry, border = "grey", main = paste0("Counties Appearing Together At Least ", sequence.id[i], " Times"), family = "sans", ps = 12)
#     plot(centroids, add=T)
#     plot(get(paste0("cluster.lw", sequence.id[i])), st_coordinates(centroids), lwd = 1, points = F, add=T)  
#   }, 
#   movie.name = "C:/Users/Cody Dailey/Documents/Github Projects/washingtonCountiesCovid19/output/covid_incidence/spatial_clustering_gif.gif", 
#   ani.interval = 0.5, 
#   ani.width = 1500, 
#   ani.height = 1500, 
#   ani.res = 300
# )

sequence <- unique(ceiling(quantile(cluster.matrix.nd[lower.tri(cluster.matrix.nd)], probs = seq(1,0, by=-0.01))))


getwd()
setwd("./output/covid_incidence/gif3")
setwd("./output/covid_incidence/gif4")
setwd("./output/covid_incidence/gif5")

png(filename = "spatial_clustering_%03d.png", height = 5.63, width = 10, units = "in", res = 300, pointsize = 12, family = "sans")

for(i in 1:length(sequence.id)){
  
  cluster.lw.temp <- cluster.matrix.nd%>%as.data.frame()%>%mutate_all(~ifelse(.<sequence[i], 0, .))%>%as.matrix() %>% mat2listw()
  
  par(mar = c(0,0,0.1,0), oma = c(0,0,0,0))
  plot(counties$geometry, border = "grey")
  title(main = paste0("Counties Appearing Together in Clusters At Least ", sequence[i], " Times"), line = -0.75, cex.main = 1, font.main = 1)
  plot(centroids, add=T)
  plot(cluster.lw.temp, st_coordinates(centroids), lwd = 1, points = F, add=T)  
}
dev.off()

shell("dir")
shell('"C:/Program Files/ImageMagick-7.0.11-Q16-HDRI/magick.exe" convert -delay 100 *.png spatial_clustering.gif')



setwd("../../..")

getwd()

library(flextable)
layout(matrix(c(1,2,3,3), nrow = 2), widths = c(4, 6), heights = c(3, 2.63))
# layout.show(3)

library(officer)


ppt <- read_pptx("./output/covid_incidence/satscan/officer_template.pptx")
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- ph_with(ppt, value = adj.mat.example, location = ph_location(left = 0, top = 0, width = 6, height = 5.63))
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = satscan.example, location = ph_location(left = 0, top = 0, width = 4, height = 2.63))

print(ppt, target = "./output/covid_incidence/satscan/tables.pptx")


satscan.example <- individual.clusters.dfs[[18]][1,2:13] %>% 
                      t() %>% 
                      data.frame("Clusters Detected" = sapply(
                                                        names(individual.clusters.dfs[[18]])[2:13], 
                                                        function(x){
                                                          string.temp <- x; 
                                                          if(string.temp=="Location IDs included"){
                                                            string.temp <- paste0("1.", string.temp, ".:")
                                                            }
                                                          else{
                                                            string.temp <- paste0("    ", string.temp, paste0(rep(".", 25-2-1-nchar(string.temp)), collapse = ""), ":")
                                                            };
                                                          return(string.temp)}
                                                        ),
                                 .) %>% 
                      setNames(., nm = c("Clusters Detected", " ")) %>% 
                      
                      flextable() %>%
                      width(j = 1, width = 3) %>%
                      width(j = 2, width = 5) %>% 
                      
                      border_remove() %>%
                      
                      align(part = "all", align = "left") %>% 
                      valign(part = "all", valign = "top") %>%
                      
                      font(fontname = "Lucida Console", part = "all") %>%
                      fontsize(size = 10, part = "all") %>%
                      
                      padding(part = "body", i = 2:12, j = 1, padding.left = 20) %>%
                      line_spacing(part = "all", space = 1) 
                      
save_as_image(satscan.example, path = "./output/covid_incidence/satscan/satscan_example.png")


example.cluster <- c("Grant County", "Douglas County", "Adams County", "Franklin County", "Lincoln County", "Kittitas County", "Benton County", "Chelan County", "Yakima County")

png("./output/covid_incidence/satscan/example_cluster.png", width = 8, height = 4.5, units = "in", res = 300)
par(mar = c(0,0,0,0))
plot(counties$geometry, border = "grey")
plot(counties$geometry[which(counties$county%in%example.cluster)], col = rgb(1,0,0,0.8), border = "black", add = T)
plot(counties$geometry[which(counties$county%in%c("Adams County", "Chelan County"))], border = "black", lwd = 4, add = T)
text(x = st_coordinates(st_centroid(st_geometry(counties[which(counties$county%in%c("Adams County", "Chelan County")),])))[,1], 
     y = st_coordinates(st_centroid(st_geometry(counties[which(counties$county%in%c("Adams County", "Chelan County")),])))[,2], 
     labels = c("Adams", "Chelan"), 
     adj = 0.5, 
     cex = 1)
dev.off()

example.adj.mat <- cluster.df[1:7,1:8]
names(example.adj.mat)[1] <- " "

adj.mat.example <- flextable(example.adj.mat) %>%
                      align(part = "all", align = "center") %>%
                      align(j = 1, align = "left") %>% 
                      border_remove() %>%
                      hline(i=1, j=2:8, part = "header", border = fp_border_default(color = "black", width = 3)) %>%
                      vline(i=1:7, j=1, part = "body", border = fp_border_default(color = "black", width = 3)) %>%
                      bg(part = "all", j = c(2,5), bg = "whitesmoke") %>%
                      bg(part = "body", i = c(1,4), bg = "whitesmoke") %>%
                      # bg(part = "header", bg = "gainsboro") %>%
                      # bg(part = "body", j=1, bg = "gainsboro")
                      bg(part = "body", i = c(1,4), j = c(2,5), bg = "gainsboro") %>%
                      fontsize(size = 10, part = "all")
                      

save_as_image(adj.mat.example, path = "./output/covid_incidence/satscan/adj_mat_example.png")

