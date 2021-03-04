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





county.names <- gsub(" ", "", gsub(" County", "", unique(counties$county)))

cluster.df <- data.frame(county = county.names)


for(i in 1:length(county.names)){

cluster.df[,county.names[i]] <- NA

for(ii in 1:length(county.names)){

cluster.df[which(cluster.df$county==county.names[ii]),county.names[i]] <- length(which(grepl(county.names[i], counties.in.clusters) & grepl(county.names[ii], counties.in.clusters)))

}
}




cluster.matrix.nd.65 <- cluster.matrix.nd
cluster.matrix.nd.65[which(cluster.matrix.nd.65<65)] <- 0
cluster.lw65 <- mat2listw(cluster.matrix.nd.65)


cluster.matrix.nd.25 <- cluster.matrix.nd
cluster.matrix.nd.25[which(cluster.matrix.nd.25<25)] <- 0
cluster.lw25 <- mat2listw(cluster.matrix.nd.25)
plot(counties$geometry, border = "grey")
plot(centroids, add=T)
plot(cluster.lw25, st_coordinates(centroids), lwd = 1, points = F, add=T)









sequence.id <- seq(60,5, by=5)

saveGIF(


  for(i in 1:length(sequence.id)){
	plot(counties$geometry, border = "grey", main = paste0("Counties Appearing Together At Least ", sequence.id[i], " Times"), family = "sans", ps = 12)
	plot(centroids, add=T)
	plot(get(paste0("cluster.lw", sequence.id[i])), st_coordinates(centroids), lwd = 1, points = F, add=T)  

  }, 
   
  movie.name = "C:/Users/Cody Dailey/Documents/Github Projects/washingtonCountiesCovid19/output/covid_incidence/spatial_clustering_gif.gif", 
  ani.interval = 0.5, 
  ani.width = 1500, 
  ani.height = 1500, 
  ani.res = 300
 
)



