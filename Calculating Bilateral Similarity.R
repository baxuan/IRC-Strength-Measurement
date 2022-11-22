library(igraph)
library(moments)

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}



###################################
#Calculated by Association strenght
###################################

#1951-1980 Whole Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Whole_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Association_Strength_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Association_Strength_value <-  RC$Whole_count/(Countries_5180[Countries_5180$Country==RC$Country1,2]*Countries_5180[Countries_5180$Country==RC$Country2,2])
  Association_Strength_list <- rbind(Association_Strength_list, Association_Strength_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Association_Strength_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count","Association_Strength_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Association_Strength_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Association_Strength_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Association_Strength_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Association_Strength_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Association_Strength_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Association_Strength_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Association_Strength_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Association_Strength_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.6,
     vertex.label.degree=lab.locs)


#1951-1980 Fractional Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Fractional_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Association_Strength_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Association_Strength_value <-  RC$Fractional_count/(Countries_5180[Countries_5180$Country==RC$Country1,2]*Countries_5180[Countries_5180$Country==RC$Country2,2])
  Association_Strength_list <- rbind(Association_Strength_list, Association_Strength_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Association_Strength_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count","Association_Strength_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Association_Strength_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Association_Strength_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Association_Strength_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Association_Strength_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Association_Strength_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Association_Strength_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Association_Strength_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Association_Strength_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.6,
     vertex.label.degree=lab.locs)


#1981-2000 Whole Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Whole_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Association_Strength_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Association_Strength_value <-  RC$Whole_count/(as.double(Countries_8100[Countries_8100$Country==RC$Country1,2])*as.double(Countries_8100[Countries_8100$Country==RC$Country2,2]))
  Association_Strength_list <- rbind(Association_Strength_list, Association_Strength_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Association_Strength_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count","Association_Strength_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Association_Strength_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Association_Strength_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Association_Strength_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Association_Strength_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Association_Strength_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Association_Strength_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Association_Strength_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Association_Strength_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.6,
     vertex.label.degree=lab.locs)


#1981-2000 Fractional Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Fractional_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Association_Strength_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Association_Strength_value <-  RC$Fractional_count/(as.double(Countries_8100[Countries_8100$Country==RC$Country1,2])*as.double(Countries_8100[Countries_8100$Country==RC$Country2,2]))
  Association_Strength_list <- rbind(Association_Strength_list, Association_Strength_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Association_Strength_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count","Association_Strength_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Association_Strength_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Association_Strength_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Association_Strength_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Association_Strength_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Association_Strength_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Association_Strength_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Association_Strength_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Association_Strength_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.6,
     vertex.label.degree=lab.locs)


#2001-2020 Whole Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Whole_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Association_Strength_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Association_Strength_value <-  RC$Whole_count/(as.double(Countries_0120[Countries_0120$Country==RC$Country1,2])*as.double(Countries_0120[Countries_0120$Country==RC$Country2,2]))
  Association_Strength_list <- rbind(Association_Strength_list, Association_Strength_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Association_Strength_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count","Association_Strength_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Association_Strength_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Association_Strength_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Association_Strength_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Association_Strength_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Association_Strength_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Association_Strength_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Association_Strength_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Association_Strength_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.6,
     vertex.label.degree=lab.locs)


#2001-2020 Fractional Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Fractional_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Association_Strength_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Association_Strength_value <-  RC$Fractional_count/(as.double(Countries_0120[Countries_0120$Country==RC$Country1,2])*as.double(Countries_0120[Countries_0120$Country==RC$Country2,2]))
  Association_Strength_list <- rbind(Association_Strength_list, Association_Strength_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Association_Strength_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count","Association_Strength_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Association_Strength_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Association_Strength_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Association_Strength_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Association_Strength_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Association_Strength_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Association_Strength_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Association_Strength_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Association_Strength_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.6,
     vertex.label.degree=lab.locs)




##############################
#Calculated by Inclusion index
##############################

#1951-1980 Whole Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Whole_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Inclusion_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Inclusion_value <-  RC$Whole_count/(min(Countries_5180[Countries_5180$Country==RC$Country1,2],Countries_5180[Countries_5180$Country==RC$Country2,2]))
  Inclusion_list <- rbind(Inclusion_list, Inclusion_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Inclusion_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count","Inclusion_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Inclusion_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Inclusion_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Inclusion_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Inclusion_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Inclusion_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Inclusion_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Inclusion_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Inclusion_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5, vertex.label.cex = 0.5,
     vertex.label.degree=lab.locs)


#1951-1980 Fractional Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Fractional_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Inclusion_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Inclusion_value <-  RC$Fractional_count/min(Countries_5180[Countries_5180$Country==RC$Country1,2],Countries_5180[Countries_5180$Country==RC$Country2,2])
  Inclusion_list <- rbind(Inclusion_list, Inclusion_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Inclusion_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count","Inclusion_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Inclusion_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Inclusion_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Inclusion_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Inclusion_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Inclusion_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Inclusion_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Inclusion_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Inclusion_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5, vertex.label.cex = 0.5,
     vertex.label.degree=lab.locs)


#1981-2000 Whole Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Whole_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Inclusion_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Inclusion_value <-  RC$Whole_count/min(Countries_8100[Countries_8100$Country==RC$Country1,2],Countries_8100[Countries_8100$Country==RC$Country2,2])
  Inclusion_list <- rbind(Inclusion_list, Inclusion_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Inclusion_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count","Inclusion_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Inclusion_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Inclusion_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Inclusion_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Inclusion_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Inclusion_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Inclusion_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Inclusion_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Inclusion_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5, vertex.label.cex = 0.5,
     vertex.label.degree=lab.locs)


#1981-2000 Fractional Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Fractional_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Inclusion_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Inclusion_value <-  RC$Fractional_count/min(Countries_8100[Countries_8100$Country==RC$Country1,2],Countries_8100[Countries_8100$Country==RC$Country2,2])
  Inclusion_list <- rbind(Inclusion_list, Inclusion_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Inclusion_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count","Inclusion_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Inclusion_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Inclusion_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Inclusion_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Inclusion_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Inclusion_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Inclusion_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Inclusion_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Inclusion_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5, vertex.label.cex = 0.5,
     vertex.label.degree=lab.locs)


#2001-2020 Whole Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Whole_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Inclusion_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Inclusion_value <-  RC$Whole_count/min(Countries_0120[Countries_0120$Country==RC$Country1,2],Countries_0120[Countries_0120$Country==RC$Country2,2])
  Inclusion_list <- rbind(Inclusion_list, Inclusion_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Inclusion_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count","Inclusion_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Inclusion_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Inclusion_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Inclusion_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Inclusion_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Inclusion_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Inclusion_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Inclusion_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Inclusion_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.5,
     vertex.label.degree=lab.locs)


#2001-2020 Fractional Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Fractional_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Inclusion_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Inclusion_value <-  RC$Fractional_count/min(Countries_0120[Countries_0120$Country==RC$Country1,2],Countries_0120[Countries_0120$Country==RC$Country2,2])
  Inclusion_list <- rbind(Inclusion_list, Inclusion_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Inclusion_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count","Inclusion_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Inclusion_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Inclusion_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Inclusion_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Inclusion_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Inclusion_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Inclusion_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Inclusion_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Inclusion_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.5,
     vertex.label.degree=lab.locs)




###############################
#Calculated by Salton's measure
###############################

#1951-1980 Whole Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Whole_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Salton_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Salton_value <-  RC$Whole_count/(sqrt(Countries_5180[Countries_5180$Country==RC$Country1,2]*Countries_5180[Countries_5180$Country==RC$Country2,2]))
  Salton_list <- rbind(Salton_list, Salton_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Salton_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count","Salton_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Salton_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Salton_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Salton_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Salton_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Salton_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Salton_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Salton_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Salton_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#1951-1980 Fractional Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Fractional_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Salton_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Salton_value <-  RC$Fractional_count/(sqrt(Countries_5180[Countries_5180$Country==RC$Country1,2]*Countries_5180[Countries_5180$Country==RC$Country2,2]))
  Salton_list <- rbind(Salton_list, Salton_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Salton_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count","Salton_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Salton_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Salton_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Salton_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Salton_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Salton_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Salton_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Salton_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Salton_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#1981-2000 Whole Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Whole_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Salton_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Salton_value <-  RC$Whole_count/(sqrt(as.double(Countries_8100[Countries_8100$Country==RC$Country1,2])*as.double(Countries_8100[Countries_8100$Country==RC$Country2,2])))
  Salton_list <- rbind(Salton_list, Salton_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Salton_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count","Salton_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Salton_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Salton_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Salton_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Salton_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Salton_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Salton_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Salton_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Salton_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#1981-2000 Fractional Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Fractional_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Salton_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Salton_value <-  RC$Fractional_count/(sqrt(Countries_8100[Countries_8100$Country==RC$Country1,2]*Countries_8100[Countries_8100$Country==RC$Country2,2]))
  Salton_list <- rbind(Salton_list, Salton_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Salton_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count","Salton_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Salton_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Salton_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Salton_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Salton_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Salton_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Salton_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Salton_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Salton_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#2001-2020 Whole Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Whole_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Salton_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Salton_value <-  RC$Whole_count/(sqrt(as.double(Countries_0120[Countries_0120$Country==RC$Country1,2])*as.double(Countries_0120[Countries_0120$Country==RC$Country2,2])))
  Salton_list <- rbind(Salton_list, Salton_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Salton_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count","Salton_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Salton_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Salton_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Salton_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Salton_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Salton_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Salton_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Salton_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Salton_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#2001-2020 Fractional Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Fractional_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Salton_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Salton_value <-  RC$Fractional_count/(sqrt(as.double(Countries_0120[Countries_0120$Country==RC$Country1,2])*as.double(Countries_0120[Countries_0120$Country==RC$Country2,2])))
  Salton_list <- rbind(Salton_list, Salton_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Salton_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count","Salton_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Salton_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Salton_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Salton_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Salton_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Salton_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Salton_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Salton_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Salton_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


################################
#Calculated by Jaccard's measure
################################

#1951-1980 Whole Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Whole_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Jaccard_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Jaccard_value <-  RC$Whole_count/(Countries_5180[Countries_5180$Country==RC$Country1,2]+Countries_5180[Countries_5180$Country==RC$Country2,2]-RC$Whole_count)
  Jaccard_list <- rbind(Jaccard_list, Jaccard_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Jaccard_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Whole_count","Jaccard_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Jaccard_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Jaccard_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Jaccard_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Jaccard_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Jaccard_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Jaccard_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Jaccard_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Jaccard_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#1951-1980 Fractional Count
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Fractional_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_5180_list <- unique(c(CANZUK_5180$Country1,CANZUK_5180$Country2))
Countries_5180_RC_count <- data.frame()
for (country in Countries_5180_list)
{
  RC_count <- sum(CANZUK_5180[CANZUK_5180$Country1==country,3])+sum(CANZUK_5180[CANZUK_5180$Country2==country,3])
  Countries_5180_RC_count <- rbind(Countries_5180_RC_count,RC_count)
}

Countries_5180 <- cbind(Countries_5180_list,Countries_5180_RC_count)
colnames(Countries_5180) <- c("Country","Size")

Jaccard_list <- data.frame()
for (i in 1:nrow(CANZUK_5180))
{
  RC <- CANZUK_5180[i,]
  Jaccard_value <-  RC$Fractional_count/(Countries_5180[Countries_5180$Country==RC$Country1,2]+Countries_5180[Countries_5180$Country==RC$Country2,2]-RC$Fractional_count)
  Jaccard_list <- rbind(Jaccard_list, Jaccard_value)
}
CANZUK_5180 <- cbind(CANZUK_5180,Jaccard_list)
colnames(CANZUK_5180) <- c("Country1","Country2","Fractional_count","Jaccard_value")

CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

print(CANZUK_5180[CANZUK_5180$In,])

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Jaccard_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Jaccard_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Jaccard_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Jaccard_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Jaccard_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Jaccard_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Jaccard_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Jaccard_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#1981-2000 Whole Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Whole_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Jaccard_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Jaccard_value <-  RC$Whole_count/(Countries_8100[Countries_8100$Country==RC$Country1,2]+Countries_8100[Countries_8100$Country==RC$Country2,2]-RC$Whole_count)
  Jaccard_list <- rbind(Jaccard_list, Jaccard_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Jaccard_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Whole_count","Jaccard_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Jaccard_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Jaccard_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Jaccard_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Jaccard_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Jaccard_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Jaccard_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Jaccard_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Jaccard_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#1981-2000 Fractional Count
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Fractional_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_8100_list <- unique(c(CANZUK_8100$Country1,CANZUK_8100$Country2))
Countries_8100_RC_count <- data.frame()
for (country in Countries_8100_list)
{
  RC_count <- sum(CANZUK_8100[CANZUK_8100$Country1==country,3])+sum(CANZUK_8100[CANZUK_8100$Country2==country,3])
  Countries_8100_RC_count <- rbind(Countries_8100_RC_count,RC_count)
}

Countries_8100 <- cbind(Countries_8100_list,Countries_8100_RC_count)
colnames(Countries_8100) <- c("Country","Size")

Jaccard_list <- data.frame()
for (i in 1:nrow(CANZUK_8100))
{
  RC <- CANZUK_8100[i,]
  Jaccard_value <-  RC$Fractional_count/(Countries_8100[Countries_8100$Country==RC$Country1,2]+Countries_8100[Countries_8100$Country==RC$Country2,2]-RC$Fractional_count)
  Jaccard_list <- rbind(Jaccard_list, Jaccard_value)
}
CANZUK_8100 <- cbind(CANZUK_8100,Jaccard_list)
colnames(CANZUK_8100) <- c("Country1","Country2","Fractional_count","Jaccard_value")

CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

print(CANZUK_8100[CANZUK_8100$In,])

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Jaccard_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Jaccard_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Jaccard_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Jaccard_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Jaccard_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Jaccard_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Jaccard_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Jaccard_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#2001-2020 Whole Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Whole_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Jaccard_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Jaccard_value <-  RC$Whole_count/(Countries_0120[Countries_0120$Country==RC$Country1,2]+Countries_0120[Countries_0120$Country==RC$Country2,2]-RC$Whole_count)
  Jaccard_list <- rbind(Jaccard_list, Jaccard_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Jaccard_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Whole_count","Jaccard_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Jaccard_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Jaccard_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Jaccard_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Jaccard_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Jaccard_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Jaccard_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Jaccard_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Jaccard_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)


#2001-2020 Fractional Count
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Fractional_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count")

#Calculating the total number of IRCs for each country
Countries_0120_list <- unique(c(CANZUK_0120$Country1,CANZUK_0120$Country2))
Countries_0120_RC_count <- data.frame()
for (country in Countries_0120_list)
{
  RC_count <- sum(CANZUK_0120[CANZUK_0120$Country1==country,3])+sum(CANZUK_0120[CANZUK_0120$Country2==country,3])
  Countries_0120_RC_count <- rbind(Countries_0120_RC_count,RC_count)
}

Countries_0120 <- cbind(Countries_0120_list,Countries_0120_RC_count)
colnames(Countries_0120) <- c("Country","Size")

Jaccard_list <- data.frame()
for (i in 1:nrow(CANZUK_0120))
{
  RC <- CANZUK_0120[i,]
  Jaccard_value <-  RC$Fractional_count/(Countries_0120[Countries_0120$Country==RC$Country1,2]+Countries_0120[Countries_0120$Country==RC$Country2,2]-RC$Fractional_count)
  Jaccard_list <- rbind(Jaccard_list, Jaccard_value)
}
CANZUK_0120 <- cbind(CANZUK_0120,Jaccard_list)
colnames(CANZUK_0120) <- c("Country1","Country2","Fractional_count","Jaccard_value")

CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

print(CANZUK_0120[CANZUK_0120$In,])

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Jaccard_value, decreasing = TRUE), ],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Jaccard_value, decreasing = TRUE), ],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Jaccard_value, decreasing = TRUE), ],3)

#Collaboration map
g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Jaccard_value) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Jaccard_value)
g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Jaccard_value[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Jaccard_value[1], "dashed", ifelse(E(g1)$Weight>=top2$Jaccard_value[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=4, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)
