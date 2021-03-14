
my.packages <- c("dplyr", "magrittr", "plotrix")
lapply(my.packages, library, character.only=T)












# read aggregate fasta file
sequences <- readLines("./data/processedData/genbankSequences.noAmbiguous.mafftout.trimmed.fasta")

## parse individual sequences
### create dataframe with partitioned label and sequence 
#### since sequences are massive, don't try to view in RStudio

##### find lines that start with the > character
ids.index <- which(substr(sequences,1,1)==">")

##### shell dataframe to fill in
seq.df <- data.frame(ID=NA, county=NA, date=NA, sequence=NA)[-1,]

##### loop to parse each sequence, partition labels, and concatenate sequences across many lines
for(i in 1:length(ids.index)){
  seq.df[i,] <- c(
                  gsub(">|County", "", unlist(strsplit(sequences[ids.index[i]], split="_"))),
                  paste(sequences[{ids.index[i]+1}:{if(i==length(ids.index)){length(sequences)}else{ids.index[i+1]-1}}], collapse="")
                  )
}

### extract metadata from larger dataframe
#### easier to work with and view without sequences
metadata <- seq.df %>% select(ID, county, date) %>% mutate(date=as.Date(date))








# import incidence data
## need to save from other file so this load actually works
# load("./data/processedData/covid_weekly.rds")







# simplify collection date variables to a weekly range as in incidence data
## used to align with incidence data

days.in.incidence <- seq(as.Date("2020-01-19"), as.Date("2021-01-31"), by="weeks")
all.days.in.range <- seq(as.Date("2020-01-19"), as.Date("2021-02-06"), by="days")

weekly.groupings <- data.frame(date = all.days.in.range, week = as.Date(rep(NA, length(all.days.in.range))))

for(i in 1:nrow(weekly.groupings)){
  if(weekly.groupings$date[i]%in%days.in.incidence){
    current_week <- days.in.incidence[which(days.in.incidence==weekly.groupings$date[i])]
  }
  weekly.groupings$week[i] <- current_week
}

## add week to metadata
metadata <- left_join(metadata, weekly.groupings, by="date")

## summarise for number of sequences per week, just like structure of incidence data
metadata.summary <- metadata %>% group_by(week) %>% summarise(n.seqs=n())

## join with incidence data
seq.case.comp <- left_join(wa.covid, metadata.summary, by="week") %>% mutate(n.seqs = ifelse(is.na(n.seqs), 0, n.seqs))












# visualize sequence availability alongside epidemic curve with barplots

# 
# par(mar=c(5.1,4.1,4.1,2.1))
# date.labels <- seq(min(wa.covid$week), max(wa.covid$week), length.out = 19)
# 
# at.points <- seq(0.5, 54.5, by=1)[which(wa.covid$week%in%date.labels)]
# 
# 
# png(filename = "./output/covid_incidence/epi_to_sequence_comparison.png", height = 9, width = 5, units = "in", res = 300, pointsize = 12, family = "sans")
# layout(matrix(c(1,2,3), nrow=3), heights = c(0.5,4,3))
# # layout.show(3)
# par(mar=c(0,0,0,0))
# 
# plot.new()
# text(0.5,0.5, "Comparison of COVID-19 Epidemic Curve to Collected Sequences in Washington, USA \n January 2020 - January 2021", font=2, cex=1)
# par(mar=c(3.1,4.1,0,1))
# barplot(n.cases~week, data=seq.case.comp, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F)
# axis(1, at=at.points, tick=T, labels = F)
# text(x = at.points-0.5, y = par("usr")[3]-5, labels = paste(format(date.labels, "%d %b"),' '), srt = 90, pos = 1, adj = 0.5, xpd = TRUE, cex=0.7, offset = 1.5)
# axis(2, labels = T, tick = T, cex=0.7, cex.axis = 0.7)
# title(ylab = "Cases", cex.lab = 0.9)
# 
# barplot(-n.seqs~week, data=seq.case.comp, las=2, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', width = 1, space = 0, xaxs = 'i', yaxs = 'i', axes=F)
# axis(3, at=at.points, tick=T, labels = F)
# axis(2, labels = seq(0,250,by=50), at=seq(0,-250,by=-50), tick = T, cex=0.7, cex.axis = 0.7)
# title(ylab = "Sequences", cex.lab = 0.9)
# dev.off()





















# whole bunch of mess trying to explore best way to take proportional sample

## look at how cases and sequences are distributed by month
proportional.sample.week <- seq.case.comp %>% 
                              filter(week < as.Date("2020-10-01")) %>%
                              mutate(month.year = as.Date(paste(format(week, "%Y-%m"),"01", sep='-'), format="%Y-%m-%d")) %>% 
                              group_by(month.year) %>% 
                              summarise(
                                n.cases = sum(n.cases), 
                                n.seqs = sum(n.seqs)
                                ) %>% 
                              ungroup() %>% 
                              mutate(
                                p.cases = n.cases / sum(n.cases)
                                )


## look at how sequences are distributed by week and county
metadata.summary.county.week <- metadata %>% 
                                  group_by(week, county) %>% 
                                  summarise(n.seqs=n()) %>% 
                                  mutate(county = ifelse(county=="GraysHarbor", "Grays Harbor County", 
                                                         ifelse(county=="Umatilla", "Benton County", 
                                                                paste0(county, " County"))))



## look at how cases and sequences are relatively distributed by county
proportional.sample.county <- covid.weekly %>%
                                left_join(metadata.summary.county.week, by=c("county", "week")) %>%
                                filter(week < as.Date("2020-10-01")) %>%
                                mutate(
                                  n.seqs = ifelse(is.na(n.seqs), 0, n.seqs)
                                ) %>%
                                group_by(county) %>%
                                summarise(
                                  n.cases = sum(total.cases),
                                  n.seqs = sum(n.seqs)
                                ) %>%
                                ungroup() %>%
                                mutate(
                                  p.cases = n.cases / sum(n.cases), 
                                  p.seqs = n.seqs / sum(n.seqs)
                                )




# visualize distributions of cases and sequences by county with barplot
p.cases.seqs <- t(proportional.sample.county[,4:5])
colnames(p.cases.seqs) <- gsub(" County", "", proportional.sample.county$county)


# png(filename = "./output/covid_incidence/epi_to_sequence_proportions.png", height = 5, width = 9, units = "in", res = 300, pointsize = 12, family = "sans")
par(mar=c(6.1, 4.1, 4.1, 2.1))
barplot(p.cases.seqs, beside = T, las = 2, cex.axis = 0.7, cex = 0.7, col=c("black", "lightgrey"))
legend("topleft", legend = c("Pr Cases", "Pr Sequences"), fill=c("black", "lightgrey"), cex = 0.7)
# dev.off()











# trying to figure out best way to go about subsampling sequences to better match with incidence data

## first thought, use independent joint distributions for time and county to get samples consistent with epidemic curve
### pros: simple, would be relatively uniform?
### cons: ignores the true joint distribution as it is product of marginals, assumes independence of time and location of incidence, does not incorporate number of sequences in sampling proportions

time.dist <- matrix(proportional.sample.week$p.cases, nrow=length(proportional.sample.week$p.cases))
county.dist <- matrix(proportional.sample.county$p.cases, ncol=length(proportional.sample.county$p.cases))


test <- time.dist%*%county.dist

colnames(test) <- proportional.sample.county$county

rownames(test) <- format(proportional.sample.week$month.year, "%b %Y")












## noticed yakima was main oversampled, see what happens to county dist if I just downsample there
proportional.sample.county %<>% 
  mutate(n.seqs2 = ifelse(county=="Yakima County", n.seqs*p.cases, n.seqs)) %>% 
  mutate(p.seqs2 = n.seqs2 / sum(n.seqs2))












## explore more detailed case proportions wrt time weekly, too many zeros for many counties, too sparse
proportional.sample <- covid.weekly %>%
                        left_join(metadata.summary.county.week, by=c("county", "week")) %>%
                        filter(week < as.Date("2020-10-01")) %>%
                        mutate(
                          month.year = as.Date(paste(format(week, "%Y-%m"),"01", sep='-'), format="%Y-%m-%d"),
                          n.seqs = ifelse(is.na(n.seqs), 0, n.seqs)
                               ) %>%
                        group_by(month.year, county) %>%
                        summarise(
                          n.cases = sum(total.cases),
                          n.seqs = sum(n.seqs)
                        ) %>%
                        ungroup() %>%
                        mutate(
                          p.cases = n.cases / sum(n.cases)
                        ) 




## final decision
## calculate joint distribution proportions of cases by month and county
## do similar for sequence counts by month and county
### both are done in a matrix format for easier visualization

## create a similar matrix by element-wise multiplication of case proportions to sequence counts
### result would be sequence counts scaled to case counts by month and county
#### problem: cells with small numbers of sequences are effectively eliminated; cells with large numbers of sequences dominate sample
#### solution: partial, use 2x case proportions to downsample
#### solution: partial, use downsampling only for those cells that have 50 or more sequences
### caveat = original goal was to have approx 500 sequences sampled, so number choices reflect this, result with ~600

## create matrix for case proportions by month and county
prop.cases.wide <- proportional.sample %>% select(month.year, county, p.cases) %>% pivot_wider(names_from = "county", values_from="p.cases")
prop.cases.matrix <- as.matrix(prop.cases.wide[,-1])
rownames(prop.cases.matrix) <- format(prop.cases.wide$month.year, "%b %Y")

## create matrix for sequences counts by month and county
prop.seq.sample.wide <- proportional.sample %>% select(month.year, county, n.seqs) %>% pivot_wider(names_from = "county", values_from="n.seqs")
prop.seq.matrix <- as.matrix(prop.seq.sample.wide[,-1])
rownames(prop.seq.matrix) <- format(prop.seq.sample.wide$month.year, "%b %Y")


### explore element wise product and total number of sequences resulting
sum(round(prop.seq.matrix*prop.cases.matrix*10))

### final choice of scale for case proportions
prop.sample.matrix <- prop.seq.matrix*prop.cases.matrix*2
### final choice for including small sample sizes without subsampling
prop.sample.matrix[which(prop.seq.matrix<50)] <- prop.seq.matrix[which(prop.seq.matrix<50)]
### total number of samples to be included
sum(ceiling(prop.sample.matrix))

#### verifying there were any instances where determined sample exceeded available samples
table(prop.sample.matrix>prop.seq.matrix)





# setting up for actual sampling procedure now that sampling proportions / sizes have been determined

## transform matrix format back to dataframe format
sample.sizes <- as.data.frame(prop.sample.matrix) %>% 
                  mutate(month.year = rownames(prop.sample.matrix)) %>% 
                  pivot_longer(cols = -month.year, names_to = "county", values_to = "seq.samples") %>% 
                  filter(seq.samples>0)

## address merging issues identified earlier to merge back to original metadata
metadata %<>% mutate(
                    county2 = ifelse(county=="GraysHarbor", "Grays Harbor County", ifelse(county=="Umatilla", "Benton County", paste(county, "County"))),
                    month.year = format(week, "%b %Y")
                     )


## create empty list to fill in with sample sizes
samples <- list()

## random seed for reproducibility
set.seed(20212602)
## loop going through month and county combinations to subset sequences and then sample from that subset to get determined sample size
for(i in 1:nrow(sample.sizes)){
  samples[[i]] <- sample(metadata[which(metadata$county2==sample.sizes$county[i] & metadata$month.year==sample.sizes$month.year[i]),"ID"], size = sample.sizes$seq.samples[i])
}
## indicator variable for whether or not the sequence was chosen in the sampling procedure
metadata$chosen <- ifelse(metadata$ID%in%unlist(samples), 1, 0)


## create summary of total number of sequences sampled by week and county
metadata.summary <- metadata %>% 
                      filter(chosen==1) %>% 
                      group_by(week, county2) %>% 
                      summarise(n.seqs=n()) %>% 
                      ungroup() %>% select(week, county=county2, n.seqs)




## compare the sampled sequences distribution to the case distributions

### by month and county
examine.sampling <- covid.weekly %>%
  left_join(metadata.summary, by=c("county", "week")) %>%
  filter(week < as.Date("2020-10-01")) %>%
  mutate(
    month.year = as.Date(paste(format(week, "%Y-%m"),"01", sep='-'), format="%Y-%m-%d"),
    n.seqs = ifelse(is.na(n.seqs), 0, n.seqs)
  ) %>%
  group_by(month.year, county) %>%
  summarise(
    n.cases = sum(total.cases),
    n.seqs = sum(n.seqs)
  ) %>%
  ungroup() %>%
  filter(n.seqs > 0) %>%
  mutate(
    p.cases = n.cases / sum(n.cases), 
    p.seqs = n.seqs / sum(n.seqs)
  ) %>%
  mutate(p.ratio = p.cases / p.seqs)




### by county
examine.sampling <- covid.weekly %>%
  left_join(metadata.summary, by=c("county", "week")) %>%
  filter(week < as.Date("2020-10-01") & n.seqs > 0) %>%
  group_by(county) %>%
  summarise(
    n.cases = sum(total.cases),
    n.seqs = sum(n.seqs)
  ) %>%
  ungroup() %>%
  mutate(
    p.cases = n.cases / sum(n.cases), 
    p.seqs = n.seqs / sum(n.seqs)
  ) %>%
  mutate(p.ratio = p.cases / p.seqs)




examine.sampling.county.ba <- left_join(examine.sampling%>%
                                          select(county, p.ratio.after=p.ratio, n.seqs.after=n.seqs), 
                                        proportional.sample.county%>%
                                          mutate(p.ratio.before=p.cases/p.seqs, p.ratio.adsyak=p.cases/p.seqs2, n.seqs.adsyak = as.integer(ceiling(n.seqs2)))%>%
                                          select(county, p.ratio.before, p.ratio.adsyak, n.seqs.adsyak, n.seqs.before = n.seqs, n.cases), by="county") %>%
                                select(county, p.ratio.before, p.ratio.adsyak, p.ratio.after, n.seqs.before, n.seqs.adsyak, n.seqs.after, n.cases)

sum(abs(examine.sampling.county.ba$p.ratio.before))
sum(abs(examine.sampling.county.ba$p.ratio.adsyak))
sum(abs(examine.sampling.county.ba$p.ratio.after))

mean(examine.sampling.county.ba$p.ratio.before)
mean(examine.sampling.county.ba$p.ratio.adsyak)
mean(examine.sampling.county.ba$p.ratio.after)

sum(abs(log(examine.sampling.county.ba$p.ratio.before)))
sum(abs(log(examine.sampling.county.ba$p.ratio.adsyak)))
sum(abs(log(examine.sampling.county.ba$p.ratio.after)))

mean(log(examine.sampling.county.ba$p.ratio.before))
mean(log(examine.sampling.county.ba$p.ratio.adsyak))
mean(log(examine.sampling.county.ba$p.ratio.after))



# visualize sampling bias 
## ratios
ratios.cases.to.sequences <- t(examine.sampling.county.ba[,2:4])
colnames(ratios.cases.to.sequences) <- gsub(" County", "", examine.sampling.county.ba$county)


counts.to.add <- t(examine.sampling.county.ba[,c(8,5:7)])
at.points <- seq(1.5,76.5, by=1)[-seq(4,77, by=4)]
at.points.matrix <- matrix(at.points, nrow=3)
at.points.centers <- at.points[seq(2,length(at.points), by=3)]
# axis(1, at = at.points, line=-20)
# axis(1, at = at.points.centers, line=-15)



# # png(filename = "./output/sequence_sampling/before_after_sampling_bias.png", height = 5, width = 20, units = "in", res = 300, pointsize = 12, family = "sans")
# 
# par(mar=c(7.1, 5.1, 4.1, 2.1))
# barplot(log(ratios.cases.to.sequences), beside = T, las = 1, cex.axis = 0.6, cex = 0.7, col=c("black", "lightgrey", "white"), width=1, space=c(0,1))
# axis(2, at=c(-2,2), labels=c("Higher Proportion of Sequences", "Higher Proportion of Cases"), tick = F, line = 0.75, cex.axis = 0.6)
# title(ylab = "log(Pr(cases) / Pr(sequences))", cex.lab = 0.9)
# legend("topright", legend = c("All Sequences", "Downsampled Yakima", "After Subsample"), fill=c("black", "lightgrey", "white"), cex = 0.6)
# 
# addtable2plot(x = 0.25, y = par('usr')[3]-2.5, matrix(c("      Cumulative Cases"), nrow = 1), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 1)
# addtable2plot(x = 0.25, y = par('usr')[3]-3, matrix(c("Number of Sequences"), nrow = 1), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 1)
# 
# for (i in 1:length(at.points.centers)){
#   addtable2plot(x = at.points.centers[i], y = par('usr')[3]-2.5, as.matrix(counts.to.add[1,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
# }
# 
# for (i in 1:ncol(at.points.matrix)){
#   addtable2plot(x = at.points.matrix[1,i], y = par('usr')[3]-3, as.matrix(counts.to.add[2,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
#   addtable2plot(x = at.points.matrix[2,i], y = par('usr')[3]-3, as.matrix(counts.to.add[3,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
#   addtable2plot(x = at.points.matrix[3,i], y = par('usr')[3]-3, as.matrix(counts.to.add[4,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
# }
# # dev.off()













examine.sampling.county.ba <- left_join(examine.sampling%>%
                                          mutate(p.diff.after = p.cases - p.seqs) %>%
                                          select(county, p.diff.after), 
                                        proportional.sample.county%>%
                                          mutate(p.diff.before=p.cases-p.seqs, p.diff.adsyak=p.cases-p.seqs2)%>%
                                          select(county, p.diff.before, p.diff.adsyak), by="county") %>%
  select(county, p.diff.before, p.diff.adsyak, p.diff.after)

sum(abs(examine.sampling.county.ba$p.diff.before))
sum(abs(examine.sampling.county.ba$p.diff.adsyak))
sum(abs(examine.sampling.county.ba$p.diff.after))

mean(examine.sampling.county.ba$p.diff.before)
mean(examine.sampling.county.ba$p.diff.adsyak)
mean(examine.sampling.county.ba$p.diff.after)


## differences 
diffs.cases.to.sequences <- t(examine.sampling.county.ba[,2:4])
colnames(diffs.cases.to.sequences) <- gsub(" County", "", examine.sampling.county.ba$county)




# 
# png(filename = "./output/sequence_sampling/before_after_sampling_bias_v2.png", height = 8, width = 20, units = "in", res = 300, pointsize = 12, family = "sans")
# 
# par(mar=c(7.1, 5.1, 4.1, 2.1))
# barplot(diffs.cases.to.sequences, beside = T, las = 1, cex.axis = 0.6, cex = 0.7, col=c("black", "lightgrey", "white"), width=1, space=c(0,1))
# axis(2, at=c(-0.2,0.2), labels=c("Higher Proportion of Sequences", "Higher Proportion of Cases"), tick = F, line = 0.75, cex.axis = 0.6)
# title(ylab = "Pr(cases) - Pr(sequences)", cex.lab = 0.9)
# legend("topright", legend = c("All Sequences", "Downsampled Yakima", "After Subsample"), fill=c("black", "lightgrey", "white"), cex = 0.6)
# 
# addtable2plot(x = 0.25, y = par('usr')[3]-0.1, matrix(c("      Cumulative Cases"), nrow = 1), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 1)
# addtable2plot(x = 0.25, y = par('usr')[3]-0.125, matrix(c("Number of Sequences"), nrow = 1), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 1)
# 
# for (i in 1:length(at.points.centers)){
#   addtable2plot(x = at.points.centers[i], y = par('usr')[3]-0.1, as.matrix(counts.to.add[1,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
# }
# 
# for (i in 1:ncol(at.points.matrix)){
#   addtable2plot(x = at.points.matrix[1,i], y = par('usr')[3]-0.125, as.matrix(counts.to.add[2,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
#   addtable2plot(x = at.points.matrix[2,i], y = par('usr')[3]-0.125, as.matrix(counts.to.add[3,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
#   addtable2plot(x = at.points.matrix[3,i], y = par('usr')[3]-0.125, as.matrix(counts.to.add[4,i]), display.colnames = FALSE, display.rownames = FALSE, cex = 0.6, xjust = 0.5, hlines = FALSE)
# }
# dev.off()
# 












# entropy

-sum(proportional.sample.county$p.cases*log(proportional.sample.county$p.cases))
-sum(examine.sampling$p.cases*log(examine.sampling$p.cases))


-sum(proportional.sample.county$p.seqs[which(proportional.sample.county$p.seqs!=0)]*log(proportional.sample.county$p.seqs[which(proportional.sample.county$p.seqs!=0)]))

-sum(proportional.sample.county$p.seqs2[which(proportional.sample.county$p.seqs2!=0)]*log(proportional.sample.county$p.seqs2[which(proportional.sample.county$p.seqs2!=0)]))

-sum(examine.sampling$p.seqs*log(examine.sampling$p.seqs))



# total seqs
sum(proportional.sample.county$n.seqs)
sum(proportional.sample.county$n.seqs2)
sum(examine.sampling$n.seqs)









# rewrite fasta file with only sampled sequences
## set up dataframe with labels and sequences
chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date")) %>%
                  filter(chosen==1) %>% 
                  mutate(seqID = paste0(">",paste(ID, county, date, sep="_"))) %>%
                  select(seqID, sequence)

## write the file
# write.table(chosen.seq.df, file="./data/processedData/subsample.fasta", sep="\n", col.names = FALSE, row.names = FALSE, quote=FALSE)

















# metapops / regionalization

metapop1 <- gsub(' County', '', counties$county[c(3,11,39)])

metapop2 <- gsub(' County', '', counties$county[c(17,27,31)])


metadata$metapop <- ifelse(metadata$county%in%metapop1, "mp1", ifelse(metadata$county%in%metapop2, "mp2", "mp3"))




# chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date"))%>%mutate(seqID = paste0(">", paste(ID, county, date, metapop, sep="_")))%>%select(seqID, sequence)
# write.table(chosen.seq.df, file="./data/processedData/subsample_metapop_roc_cor.fasta", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)


west <- c("SanJuan", "Whatcom", "Skagit", "Island", "Clallam", "Snohomish", "Jefferson", "Kitsap", "King", "GraysHarbor", "Mason", "Pierce", "Thurston", "Pacific", "Lewis", "Wahkiakum", "Cowlitz", "Skamania", "Clark")

metadata$metapop <- ifelse(metadata$county%in%west, "west", "east")


# chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date"))%>%mutate(seqID = paste0(">", paste(ID, county, date, metapop, sep="_")))%>%select(seqID, sequence)
# write.table(chosen.seq.df, file="./data/processedData/subsample_metapop_east_west.fasta", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)



metapop.1 <- c(metapop1, metapop2, "Kitsap", "Skagit", "Whatcom")


metadata$metapop <- ifelse(metadata$county%in%metapop.1, "mp1", "mp2")

# chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date"))%>%mutate(seqID = paste0(">", paste(ID, county, date, metapop, sep="_")))%>%select(seqID, sequence)
# write.table(chosen.seq.df, file="./data/processedData/subsample_metapop_roc_cor2.fasta", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)




west <- c("SanJuan", "Whatcom", "Skagit", "Island", "Clallam", "Snohomish", "Jefferson", "Kitsap", "King", "GraysHarbor", "Mason", "Pierce", "Thurston", "Pacific", "Lewis", "Wahkiakum", "Cowlitz", "Skamania", "Clark")
east1 <- c("Okanagon", "Chelan", "Douglas", "Kittitas")

metadata$metapop <- ifelse(metadata$county%in%west, "west", ifelse(metadata$county%in%east1, "east1", "east2"))

chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date")) %>%
  filter(chosen==1) %>% 
  mutate(seqID = paste0(">",paste(ID, county, date, sep="_"))) %>%
  select(seqID, sequence)

# chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date"))%>%mutate(seqID = paste0(">", paste(ID, county, date, metapop, sep="_")))%>%select(seqID, sequence)
# write.table(chosen.seq.df, file="./data/processedData/subsample_metapop_east_west.fasta", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)




metadata$metapop <- ifelse(metadata$county%in%west & !metadata$county%in%c("King", "Pierce"), "West", ifelse(metadata$county%in%c("King", "Pierce"), "mp1", "east"))


chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date")) %>%
  filter(chosen==1) %>% 
  mutate(seqID = paste0(">",paste(ID, county, date, metapop, sep="_"))) %>%
  select(seqID, sequence)


# chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date"))%>%mutate(seqID = paste0(">", paste(ID, county, date, metapop, sep="_")))%>%select(seqID, sequence)
# write.table(chosen.seq.df, file="./data/processedData/subsample_metapop_clust.fasta", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)



west1 <- c("King", "Pierce")
west2 <- c("Whatcom", "Skagit", "Snohomish", "Island", "SanJuan")
west3 <- west[which(!west%in%c(west1, west2))]

east1 <- c("Yakima", "Kittitas")
east3 <- c("Asotin", "Spokane", "WallaWalla", "Columbia", "Garfield", "Whitman", "PendOreille")




metadata$metapop <- ifelse(metadata$county%in%west1, "west1", 
                           ifelse(metadata$county%in%west2, "west2", 
                                  ifelse(metadata$county%in%west3, "west3", 
                                         ifelse(metadata$county%in%east1, "east1", 
                                                ifelse(metadata$county%in%east3, "east3", "east2")))))


chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date")) %>%
  filter(chosen==1) %>% 
  mutate(seqID = paste0(">",paste(ID, county, date, metapop, sep="_"))) %>%
  select(seqID, sequence)


# chosen.seq.df <- full_join(metadata, seq.df%>%mutate(date=as.Date(date)), by=c("ID", "county", "date"))%>%mutate(seqID = paste0(">", paste(ID, county, date, metapop, sep="_")))%>%select(seqID, sequence)
# write.table(chosen.seq.df, file="./data/processedData/subsample_metapop_clust2.fasta", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
















metapop.categories <- data.frame(county = unique(counties$county)) %>%
                        mutate(county2 = gsub(" ", "", gsub(" County", "", county))) %>%
                        mutate(
                          mp.ew = ifelse(county2%in%west, "West", "East"), 
                          mp.roc1 = ifelse(county2%in%metapop1, "MP1", 
                                           ifelse(county2%in%metapop2, "MP2", "MP3")), 
                          mp.roc2 = ifelse(county2%in%metapop.1, "MP1", "MP2"),
                          mp.roc.ew = ifelse(county2%in%metapop1, "MP1", ifelse(county2%in%metapop2, "MP2", ifelse(county2%in%west, "West", "East"))), 
                          mp.clust = ifelse(county2%in%west & !county2%in%c("King", "Pierce"), "West", ifelse(county2%in%c("King", "Pierce"), "MP1", "East")),
                          mp.clust2 = ifelse(county2%in%west1, "West1", 
                                             ifelse(county2%in%west2, "West2", 
                                                    ifelse(county2%in%west3, "West3", 
                                                           ifelse(county2%in%east1, "East1", 
                                                                  ifelse(county2%in%east3, "East3", "East2")))))
                          
                          ) %>%
                        full_join(counties, by="county") %>%
                        st_sf()




# png(filename = "./output/sequence_sampling/metapop_east_west.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
par(mar=c(0,0,0,0))
plot(metapop.categories$geometry, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.ew=="West")], col = viridis(2)[1], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.ew=="East")], col = viridis(2)[2], add = TRUE, lwd = 2.5, border = "grey")
par(mar=c(0,0,0,0))
# .image_scale_factor(c("West", "East"), col = viridis(2), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(c("", ""), col = viridis(2), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
text(x=c(2.5,2.5), y=c(1, 2), labels = c("West", "East"), xpd=TRUE, cex=10/12)
# dev.off()



# png(filename = "./output/sequence_sampling/metapop_roc_cor.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
par(mar=c(0,0,0,0))
plot(metapop.categories$geometry, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc1=="MP1")], col = viridis(3)[1], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc1=="MP2")], col = viridis(3)[2], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc1=="MP3")], col = viridis(3)[3], add = TRUE, lwd = 2.5, border = "grey")
par(mar=c(0,0,0,0))
# .image_scale_factor(c("MP3", "MP2", "MP1"), col = viridis(3)[c(3,2,1)], key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(c("", "", ""), col = viridis(3)[c(3,2,1)], key.length = lcm(2), key.width = lcm(3), key.pos = 4)
text(x=c(2.5,2.5,2.5), y=c(1, 2, 3), labels = c("MP3", "MP2", "MP1"), xpd=TRUE, cex=10/12)
# dev.off()




# png(filename = "./output/sequence_sampling/metapop_roc_cor2.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")
layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
par(mar=c(0,0,0,0))
plot(metapop.categories$geometry, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc2=="MP1")], col = viridis(2)[1], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc2=="MP2")], col = viridis(2)[2], add = TRUE, lwd = 2.5, border = "grey")
par(mar=c(0,0,0,0))
# .image_scale_factor(c("MP2", "MP1"), col = viridis(2)[2:1], key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(c("", ""), col = viridis(2)[2:1], key.length = lcm(2), key.width = lcm(3), key.pos = 4)
text(x=c(2.5,2.5), y=c(1, 2), labels = c("MP2", "MP1"), xpd=TRUE, cex=10/12)
# dev.off()






# png(filename = "./output/sequence_sampling/metapop_roc_cor_ew.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")

layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
par(mar=c(0,0,0,0))
plot(metapop.categories$geometry, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc.ew=="West")], col = viridis(4)[1], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc.ew=="East")], col = viridis(4)[2], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc.ew=="MP2")], col = viridis(4)[3], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.roc.ew=="MP1")], col = viridis(4)[4], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(!metapop.categories$county2%in%metadata$county)], col = rgb(220/255, 220/255, 220/255, alpha = 0.5), add = TRUE, lwd = 2.5, border = "grey")
par(mar=c(0,0,0,0))
# .image_scale_factor(c("West", "East"), col = viridis(2), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(c("", "", "", ""), col = viridis(4), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
text(x=c(2.5,2.5,2.5,2.5), y=c(1, 2, 3, 4), labels = c("West", "East", "MP2", "MP1"), xpd=TRUE, cex=10/12)
# dev.off()






# png(filename = "./output/sequence_sampling/metapop_clust.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")

layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
par(mar=c(0,0,0,0))
plot(metapop.categories$geometry, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust=="West")], col = viridis(3)[1], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust=="East")], col = viridis(3)[2], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust=="MP1")], col = viridis(3)[3], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(!metapop.categories$county2%in%metadata$county)], col = rgb(220/255, 220/255, 220/255, alpha = 0.5), add = TRUE, lwd = 2.5, border = "grey")
# plot(metapop.categories$geometry[which(metapop.categories$county2%in%metadata$county)], border = "grey", lwd = 2.5, add = T)
par(mar=c(0,0,0,0))
# .image_scale_factor(c("West", "East"), col = viridis(2), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(c("", "", ""), col = viridis(3), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
text(x=c(2.5,2.5,2.5), y=c(1, 2, 3), labels = c("West", "East", "MP1"), xpd=TRUE, cex=10/12)
# dev.off()






# png(filename = "./output/sequence_sampling/metapop_clust2.png", height = 5, width = 7, units = "in", res = 300, pointsize = 12, family = "sans")

layout(matrix(1:2, ncol = 2), widths = c(1, lcm(3)))
par(mar=c(0,0,0,0))
plot(metapop.categories$geometry, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust2=="West1")], col = viridis(6)[1], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust2=="West2")], col = viridis(6)[2], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust2=="West3")], col = viridis(6)[3], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust2=="East1")], col = viridis(6)[4], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust2=="East2")], col = viridis(6)[5], add = TRUE, lwd = 2.5, border = "grey")
plot(metapop.categories$geometry[which(metapop.categories$mp.clust2=="East3")], col = viridis(6)[6], add = TRUE, lwd = 2.5, border = "grey")

plot(metapop.categories$geometry[which(!metapop.categories$county2%in%metadata$county)], col = rgb(220/255, 220/255, 220/255, alpha = 0.5), add = TRUE, lwd = 2.5, border = "grey")

par(mar=c(0,0,0,0))
# .image_scale_factor(c("West", "East"), col = viridis(2), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
.image_scale_factor(c("", "", "", "", "", ""), col = viridis(6), key.length = lcm(2), key.width = lcm(3), key.pos = 4)
text(x=c(2.5,2.5,2.5,2.5,2.5,2.5), y=c(1, 2, 3, 4, 5, 6), labels = c("West1", "West2", "West3", "East1", "East2", "East3"), xpd=TRUE, cex=10/12)
# dev.off()



















wa.ew <- metapop.categories %>% group_by(mp.ew) %>% summarise(geometry=st_combine(geometry))
wa.ew2 <- metapop.categories %>% group_by(mp.ew) %>% summarise(geometry=st_centroid(st_union(geometry)))
wa.roc.cor <- metapop.categories %>% group_by(mp.roc1) %>% summarise(geometry = st_combine(geometry))
wa.roc.cor2 <- metapop.categories %>% group_by(mp.roc2) %>% summarise(geometry = st_combine(geometry))


wa.ew.centroids <- st_centroid(st_geometry(wa.ew))

wa.ew.centroids.coords <- st_coordinates(wa.ew.centroids)



plot(wa.ew$geometry, border="grey")
plot(wa.ew["mp.ew"], col=viridis(2), add=T)
plot(wa.ew.centroids, pch=16, col="white", add=T, cex=2)
curvedarrow(wa.ew.centroids.coords[1,],wa.ew.centroids.coords[2,], lwd=2, lty=1, lcol="white", curve=0.1, arr.pos = 0.9)
# selfarrow(wa.ew.centroids.coords[1,], lwd=2, lty=1, lcol="white", path = "R", arr.length = 2, curve = c(1,1))
curvedarrow(ew.centroids[2,],ew.centroids[1,], lwd=4, lty=1, lcol="white", arr.col="white", curve=0.1, arr.pos =0.9, arr.type = "triangle")

curvedarrow(wa.ew.centroids.coords[1,], wa.ew.centroids.coords[1,]+0.1*wa.ew.centroids.coords[1,], lwd = 2, lty = 2, lcol="white", curve = 0.5, arr.pos = 2/3, arr.type = "triangle")
curvedarrow(wa.ew.centroids.coords[1,]+0.1*wa.ew.centroids.coords[1,], wa.ew.centroids.coords[1,], lwd = 2, lty = 2, lcol="white", curve = 0.5, arr.pos = 1/3, arr.type = "triangle")


curvedarrow(wa.ew.centroids.coords[2,], wa.ew.centroids.coords[2,]-0.1*wa.ew.centroids.coords[2,], lwd = 2, lty = 2, lcol="white", curve = 0.5, arr.pos = 2/3, arr.type = "triangle")
curvedarrow(wa.ew.centroids.coords[2,]-0.1*wa.ew.centroids.coords[2,], wa.ew.centroids.coords[2,], lwd = 2, lty = 2, lcol="white", curve = 0.5, arr.pos = 1/3, arr.type = "triangle")
