
data.photo <- read.csv("data/All_photo.csv", header=TRUE)
sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
str(data.photo)
data.photo <- dplyr::filter(data.photo, data.photo$species != "bat" & data.photo$species != "commongreenmagpie" & 
                              data.photo$species != "greateryellownape"& data.photo$species !="greenmagpie"& 
                              data.photo$species!="treeshrew"& data.photo$species != "junglefowl"& data.photo$species!="silverpheasant"& 
                              data.photo$species!="squirrel"& data.photo$species!="bird" & 
                              data.photo$species!="rat" & data.photo$species != "unknown" & data.photo$species != "human" &
                             data.photo$species!="human2" & data.photo$species != "hunter" & data.photo$species != "watermonitor"
                            & data.photo$species != "dog" & data.photo$species != "cattle" )
table(data.photo$species)

list <- data.frame(aggregate(data.photo$n ~ data.photo$species + data.photo$camera, FUN =sum))
names(list) <- c("spp", "camera", "n")
spp.li <- as.character(unique(list$spp))

occ_data <- matrix(999, nrow = 22*115, ncol = 3 )
occ_data[,1] <-  tolower(sitecovs$NO)
j = 1
k = 115
for (i in 1:22) {      # fit spp into data, each spp repeat for 115 times
    occ_data[j:k,2] = spp.li[i]  
  j = j + 115
  k = k + 115
} 

for (i in 1:nrow(list)) {
  n <- which(occ_data[,2] == list[i,1] & occ_data[,1] == list[i,2])
  occ_data[n,3] <- list[i,3]
}

occ_data[which(occ_data[,3] == 999),3] <- 0
occ_data <- data.frame(occ_data, stringsAsFactors = FALSE)
names(occ_data) <- c("Station","Species","Count")
occ_data$Count <- as.numeric(occ_data$Count)
sum(list[,3])
sum(occ_data[,3])

write.csv(occ_data,file="/Users/chencheng/Desktop/data/occupancy/data/occ_data_by_station.csv", row.names = FALSE)
