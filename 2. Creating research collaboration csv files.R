list_file <- list.files(data_folder)
Full_df <- NULL

for (file in list_file)
{
  df <- read.csv(paste(data_folder,file,sep=''), header=FALSE, sep=',')
  Full_df <- rbind(Full_df,df)
}

colnames(Full_df) <- c("Year","Country1","Country2","Whole_count","Fractional_count")
write.csv(Full_df,"RC_All.csv", row.names = FALSE)

CANZUK<- Full_df[(Full_df$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))|(Full_df$Country2 %in% c('CAN','AUS', 'NZL', 'GBR')),]

CANZUK2 <- CANZUK[order(CANZUK$Year),]  
for (i in 1:nrow(CANZUK2))
{
  if (CANZUK2[i,2]>CANZUK2[i,3])
  {
    temp <- CANZUK2[i,2]
    CANZUK2[i,2] <- CANZUK2[i,3]
    CANZUK2[i,3] <- temp
  }
}
write.csv(CANZUK2,"RC_CANZUK.csv", row.names = FALSE)

