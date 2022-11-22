library(igraph)
library(moments)

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}


#########################
#1951-1980 Whole Counting

#Collaboration map
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Whole_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Weight")
CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Weight,decreasing = TRUE),],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Weight,decreasing = TRUE),],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Weight,decreasing = TRUE),],3)

g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Weight) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Weight)

g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Weight[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Weight[1], "dashed", ifelse(E(g1)$Weight>=top2$Weight[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)

V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5,vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)

#Distribution statistics
summary(CANZUK_5180$Weight)

CANZUK_5180_In_Whole_weight_skewness <- skewness(CANZUK_5180[CANZUK_5180$In==TRUE,]$Weight)
print(CANZUK_5180_In_Whole_weight_skewness)
CANZUK_5180_In_Whole_weight_kurtosis <- kurtosis(CANZUK_5180[CANZUK_5180$In==TRUE,]$Weight)
print(CANZUK_5180_In_Whole_weight_kurtosis)
hist(CANZUK_5180[CANZUK_5180$In==TRUE,]$Weight, prob=FALSE, xlim=c(0,1000), ylim=c(0,3))

CANZUK_5180_Out_Whole_weight_skewness <- skewness(CANZUK_5180[CANZUK_5180$In==FALSE,]$Weight)
print(CANZUK_5180_Out_Whole_weight_skewness)
CANZUK_5180_Out_Whole_weight_kurtosis <- kurtosis(CANZUK_5180[CANZUK_5180$In==FALSE,]$Weight)
print(CANZUK_5180_Out_Whole_weight_kurtosis)
hist(CANZUK_5180[CANZUK_5180$In==FALSE,]$Weight, prob=FALSE, xlim=c(0,1000), ylim=c(0,400))

print(edge_density(g))
g0 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE), delete.vertices = TRUE)
print(edge_density(g0))

vertice_weight_med <- median(CANZUK_5180$Whole_count)
g <- set.edge.attribute(g,"weight",value = CANZUK_5180$Whole_count/vertice_weight_med)
print(strength(g))

#Print the ratios of fractional counts to the corresponding medians during period 1951-1980
CANZUK_5180_ordered<-CANZUK_5180[order(CANZUK_5180$Country1),]
CANZUZ_only_ordered <- CANZUK_5180_ordered[CANZUK_5180_ordered$In,]
print(CANZUZ_only_ordered$Weight/edge_weight_med)
print(top3$Weight/edge_weight_med)



#########################
#1951-1980 Fractional Count

#Collaboration map
RC_5180 <- read.csv("RC_CANZUK_51-80.csv",header = TRUE)
Bilateral_RC_5180 <- RC_5180[RC_5180$Country1!=RC_5180$Country2,]
CANZUK_5180 <- aggregate(Bilateral_RC_5180$Fractional_count, by=list(Country1=Bilateral_RC_5180$Country1,Country2=Bilateral_RC_5180$Country2), FUN=sum)
colnames(CANZUK_5180) <- c("Country1","Country2","Weight")
CANZUK_5180$In <- (CANZUK_5180$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_5180$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_5180_Co <- CANZUK_5180[CANZUK_5180$In==FALSE,]

top1 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Weight,decreasing = TRUE),],1)
top2 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Weight,decreasing = TRUE),],2)
top3 <- head(CANZUK_5180_Co[order(CANZUK_5180_Co$Weight,decreasing = TRUE),],3)

g <- graph.edgelist(as.matrix(CANZUK_5180[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_5180$Weight) 
g <- set.edge.attribute(g,"In",value = CANZUK_5180$In) 

edge_weight_med <- median(CANZUK_5180$Weight)

g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Weight[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Weight[1], "dashed", ifelse(E(g1)$Weight>=top2$Weight[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5,vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)

#Distribution statistics
summary(CANZUK_5180$Weight)


CANZUK_5180_In_Fractional_weight_skewness <- skewness(CANZUK_5180[CANZUK_5180$In==TRUE,]$Weight)
print(CANZUK_5180_In_Fractional_weight_skewness)
CANZUK_5180_In_Fractional_weight_kurtosis <- kurtosis(CANZUK_5180[CANZUK_5180$In==TRUE,]$Weight)
print(CANZUK_5180_In_Fractional_weight_kurtosis)
hist(CANZUK_5180[CANZUK_5180$In==TRUE,]$Weight, prob=FALSE, xlim=c(0,1000), ylim=c(0,3))


CANZUK_5180_Out_Fractional_weight_skewness <- skewness(CANZUK_5180[CANZUK_5180$In==FALSE,]$Weight)
print(CANZUK_5180_Out_Fractional_weight_skewness)
CANZUK_5180_Out_Fractional_weight_kurtosis <- kurtosis(CANZUK_5180[CANZUK_5180$In==FALSE,]$Weight)
print(CANZUK_5180_Out_Fractional_weight_kurtosis)
hist(CANZUK_5180[CANZUK_5180$In==FALSE,]$Weight, prob=FALSE, xlim=c(0,1000), ylim=c(0,400))


print(edge_density(g))

vertice_weight_med <- median(CANZUK_5180$Fractional_count)
g <- set.edge.attribute(g,"weight",value = CANZUK_5180$Fractional_count/vertice_weight_med)
print(strength(g))

#Print the ratios of fractional counts to the corresponding medians during period 1951-1980
CANZUK_5180_ordered<-CANZUK_5180[order(CANZUK_5180$Country1),]
CANZUZ_only_ordered <- CANZUK_5180_ordered[CANZUK_5180_ordered$In,]
print(CANZUZ_only_ordered$Weight/edge_weight_med)
print(top3$Weight/edge_weight_med)


#########################
#1981-2000 Whole Counting

#Collaboration map
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Whole_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Weight")
CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Weight,decreasing = TRUE),],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Weight,decreasing = TRUE),],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Weight,decreasing = TRUE),],3)

g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Weight) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Weight)

g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Weight[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Weight[1], "dashed", ifelse(E(g1)$Weight>=top2$Weight[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5,vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)

#Distribution statistics
summary(CANZUK_8100$Weight)


CANZUK_8100_In_Whole_weight_skewness <- skewness(CANZUK_8100[CANZUK_8100$In==TRUE,]$Weight)
print(CANZUK_8100_In_Whole_weight_skewness)
CANZUK_8100_In_Whole_weight_kurtosis <- kurtosis(CANZUK_8100[CANZUK_8100$In==TRUE,]$Weight)
print(CANZUK_8100_In_Whole_weight_kurtosis)
hist(CANZUK_8100[CANZUK_8100$In==TRUE,]$Weight, prob=FALSE, ylim=c(0,3), breaks=5)


CANZUK_8100_Out_Whole_weight_skewness <- skewness(CANZUK_8100[CANZUK_8100$In==FALSE,]$Weight)
print(CANZUK_8100_Out_Whole_weight_skewness)
CANZUK_8100_Out_Whole_weight_kurtosis <- kurtosis(CANZUK_8100[CANZUK_8100$In==FALSE,]$Weight)
print(CANZUK_8100_Out_Whole_weight_kurtosis)
hist(CANZUK_8100[CANZUK_8100$In==FALSE,]$Weight, prob=FALSE, xlim=c(0,7000), ylim=c(0,800))


print(edge_density(g))
g0 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE), delete.vertices = TRUE)
print(edge_density(g0))

vertice_weight_med <- median(CANZUK_8100$Whole_count)
g <- set.edge.attribute(g,"weight",value = CANZUK_8100$Whole_count/vertice_weight_med)
print(strength(g))

#Print the ratios of whole counts to the corresponding medians during period 1981-2000
CANZUK_8100_ordered<-CANZUK_8100[order(CANZUK_8100$Country1),]
CANZUZ_only_ordered <- CANZUK_8100_ordered[CANZUK_8100_ordered$In,]
print(CANZUZ_only_ordered$Weight/edge_weight_med)
print(top3$Weight/edge_weight_med)


#########################
#1981-2000 Fractional Counting

#Collaboration map
RC_8100 <- read.csv("RC_CANZUK_81-00.csv",header = TRUE)
Bilateral_RC_8100 <- RC_8100[RC_8100$Country1!=RC_8100$Country2,]
CANZUK_8100 <- aggregate(Bilateral_RC_8100$Fractional_count, by=list(Country1=Bilateral_RC_8100$Country1,Country2=Bilateral_RC_8100$Country2), FUN=sum)
colnames(CANZUK_8100) <- c("Country1","Country2","Weight")
CANZUK_8100$In <- (CANZUK_8100$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_8100$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_8100_Co <- CANZUK_8100[CANZUK_8100$In==FALSE,]

top1 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Weight,decreasing = TRUE),],1)
top2 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Weight,decreasing = TRUE),],2)
top3 <- head(CANZUK_8100_Co[order(CANZUK_8100_Co$Weight,decreasing = TRUE),],3)

g <- graph.edgelist(as.matrix(CANZUK_8100[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_8100$Weight) 
g <- set.edge.attribute(g,"In",value = CANZUK_8100$In) 

edge_weight_med <- median(CANZUK_8100$Weight)

g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Weight[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Weight[1], "dashed", ifelse(E(g1)$Weight>=top2$Weight[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)

#Distribution statistics
summary(CANZUK_8100$Weight)


CANZUK_8100_In_Fractional_weight_skewness <- skewness(CANZUK_8100[CANZUK_8100$In==TRUE,]$Weight)
print(CANZUK_8100_In_Fractional_weight_skewness)
CANZUK_8100_In_Fractional_weight_kurtosis <- kurtosis(CANZUK_8100[CANZUK_8100$In==TRUE,]$Weight)
print(CANZUK_8100_In_Fractional_weight_kurtosis)
hist(CANZUK_8100[CANZUK_8100$In==TRUE,]$Weight, prob=FALSE, xlim=c(0,1000), ylim=c(0,3))


CANZUK_8100_Out_Fractional_weight_skewness <- skewness(CANZUK_8100[CANZUK_8100$In==FALSE,]$Weight)
print(CANZUK_8100_Out_Fractional_weight_skewness)
CANZUK_8100_Out_Fractional_weight_kurtosis <- kurtosis(CANZUK_8100[CANZUK_8100$In==FALSE,]$Weight)
print(CANZUK_8100_Out_Fractional_weight_kurtosis)
hist(CANZUK_8100[CANZUK_8100$In==FALSE,]$Weight, prob=FALSE, xlim=c(0,1000), ylim=c(0,800))

print(edge_density(g))

vertice_weight_med <- median(CANZUK_8100$Fractional_count)
g <- set.edge.attribute(g,"weight",value = CANZUK_8100$Fractional_count/vertice_weight_med)
print(strength(g))

#Print the ratios of fractional counts to the corresponding medians during period 1981-2000
CANZUK_8100_ordered<-CANZUK_8100[order(CANZUK_8100$Country1),]
CANZUZ_only_ordered <- CANZUK_8100_ordered[CANZUK_8100_ordered$In,]
print(CANZUZ_only_ordered$Weight/edge_weight_med)
print(top3$Weight/edge_weight_med)




#########################
#2001-2020 Whole Counting

#Collaboration map
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Whole_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Weight")
CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Weight,decreasing = TRUE),],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Weight,decreasing = TRUE),],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Weight,decreasing = TRUE),],3)

g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Weight) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Weight)

g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Weight[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Weight[1], "dashed", ifelse(E(g1)$Weight>=top2$Weight[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)

#Distribution statistics
summary(CANZUK_0120$Weight)


CANZUK_0120_In_Whole_weight_skewness <- skewness(CANZUK_0120[CANZUK_0120$In==TRUE,]$Weight)
print(CANZUK_0120_In_Whole_weight_skewness)
CANZUK_0120_In_Whole_weight_kurtosis <- kurtosis(CANZUK_0120[CANZUK_0120$In==TRUE,]$Weight)
print(CANZUK_0120_In_Whole_weight_kurtosis)
hist(CANZUK_0120[CANZUK_0120$In==TRUE,]$Weight, prob=FALSE, xlim=c(0,60000), breaks=5, ylim=c(0,3))

CANZUK_0120_Out_Whole_weight_skewness <- skewness(CANZUK_0120[CANZUK_0120$In==FALSE,]$Weight)
print(CANZUK_0120_Out_Whole_weight_skewness)
CANZUK_0120_Out_Whole_weight_kurtosis <- kurtosis(CANZUK_0120[CANZUK_0120$In==FALSE,]$Weight)
print(CANZUK_0120_Out_Whole_weight_kurtosis)
hist(CANZUK_0120[CANZUK_0120$In==FALSE,]$Weight, prob=FALSE, xlim=c(0,60000), ylim=c(0,1000), breaks = 15)


print(edge_density(g))
g0 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE), delete.vertices = TRUE)
print(edge_density(g0))

vertice_weight_med <- median(CANZUK_0120$Whole_count)
g <- set.edge.attribute(g,"weight",value = CANZUK_0120$Whole_count/vertice_weight_med)
print(strength(g))

#Print the ratios of whole counts to the corresponding medians during period 1981-2000
CANZUK_0120_ordered<-CANZUK_0120[order(CANZUK_0120$Country1),]
CANZUZ_only_ordered <- CANZUK_0120_ordered[CANZUK_0120_ordered$In,]
print(CANZUZ_only_ordered$Weight/edge_weight_med)
print(top3$Weight/edge_weight_med)


#########################
#2001-2020 Fractional Counting

#Collaboration map
RC_0120 <- read.csv("RC_CANZUK_01-20.csv",header = TRUE)
Bilateral_RC_0120 <- RC_0120[RC_0120$Country1!=RC_0120$Country2,]
CANZUK_0120 <- aggregate(Bilateral_RC_0120$Fractional_count, by=list(Country1=Bilateral_RC_0120$Country1,Country2=Bilateral_RC_0120$Country2), FUN=sum)
colnames(CANZUK_0120) <- c("Country1","Country2","Weight")
CANZUK_0120$In <- (CANZUK_0120$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))&(CANZUK_0120$Country2 %in% c('CAN','AUS', 'NZL', 'GBR'))
CANZUK_0120_Co <- CANZUK_0120[CANZUK_0120$In==FALSE,]

top1 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Weight,decreasing = TRUE),],1)
top2 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Weight,decreasing = TRUE),],2)
top3 <- head(CANZUK_0120_Co[order(CANZUK_0120_Co$Weight,decreasing = TRUE),],3)

g <- graph.edgelist(as.matrix(CANZUK_0120[c(1,2)]), directed = FALSE)
g <- set.edge.attribute(g,"Weight",value = CANZUK_0120$Weight) 
g <- set.edge.attribute(g,"In",value = CANZUK_0120$In) 

edge_weight_med <- median(CANZUK_0120$Weight)

g1 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE | E(g)$Weight>=top3$Weight[3]), delete.vertices = TRUE)
V(g1)$color <- ifelse(V(g1)$name %in% c('CAN','GBR','AUS', 'NZL') , "blue", "green")
E(g1)$lty <- ifelse(E(g1)$In==TRUE, "solid", ifelse(E(g1)$Weight>=top1$Weight[1], "dashed", ifelse(E(g1)$Weight>=top2$Weight[2], "dotdash","dotted")))
E(g1)$color <- ifelse(E(g1)$In==TRUE, "blue", "green")
E(g1)$width <- ifelse(E(g1)$Weight>=edge_weight_med,3,1)
V(g1)$size <-15

lab.locs <- radian.rescale(x=1:length(V(g1)), direction=-1, start=0)

plot(g1, layout = layout_in_circle(g1), vertex.label.dist=5, vertex.label.cex = 0.7,
     vertex.label.degree=lab.locs)

#Distribution statistics
summary(CANZUK_0120$Weight)


CANZUK_0120_In_Fractional_weight_skewness <- skewness(CANZUK_0120[CANZUK_0120$In==TRUE,]$Weight)
print(CANZUK_0120_In_Fractional_weight_skewness)
CANZUK_0120_In_Fractional_weight_kurtosis <- kurtosis(CANZUK_0120[CANZUK_0120$In==TRUE,]$Weight)
print(CANZUK_0120_In_Fractional_weight_kurtosis)
hist(CANZUK_0120[CANZUK_0120$In==TRUE,]$Weight, prob=FALSE, xlim=c(0,30000), ylim=c(0,3))

CANZUK_0120_Out_Fractional_weight_skewness <- skewness(CANZUK_0120[CANZUK_0120$In==FALSE,]$Weight)
print(CANZUK_0120_Out_Fractional_weight_skewness)
CANZUK_0120_Out_Fractional_weight_kurtosis <- kurtosis(CANZUK_0120[CANZUK_0120$In==FALSE,]$Weight)
print(CANZUK_0120_Out_Fractional_weight_kurtosis)
hist(CANZUK_0120[CANZUK_0120$In==FALSE,]$Weight, prob=FALSE, xlim=c(0,30000), ylim=c(0,1000))

print(edge_density(g))
g0 <- subgraph.edges(graph=g, eids=which(E(g)$In==TRUE), delete.vertices = TRUE)
print(edge_density(g0))

vertice_weight_med <- median(CANZUK_0120$Fractional_count)
g <- set.edge.attribute(g,"weight",value = CANZUK_0120$Fractional_count/vertice_weight_med)
print(strength(g))

#Print the ratios of fractional counts to the corresponding medians during period 1981-2000
CANZUK_0120_ordered<-CANZUK_0120[order(CANZUK_0120$Country1),]
CANZUZ_only_ordered <- CANZUK_0120_ordered[CANZUK_0120_ordered$In,]
print(CANZUZ_only_ordered$Weight/edge_weight_med)
print(top3$Weight/edge_weight_med)


##############################
#Drawing distribution boxplots

library(tidyverse)
require(scales)

whole_distribution <- data.frame()
whole_distribution <- rbind(whole_distribution, data.frame(period = "1951-1980", frequency = CANZUK_5180$Weight))
whole_distribution <- rbind(whole_distribution, data.frame(period = "1981-2000", frequency = CANZUK_8100$Weight))
whole_distribution <- rbind(whole_distribution, data.frame(period = "2001-2017", frequency = CANZUK_0120$Weight))
ggplot(whole_distribution, aes(x=period, y=frequency, fill=period)) + geom_boxplot() + coord_trans(y="log2")


#need to reload CANZUK_5180, CANZUK_8100, and CANZUK_0120 of factional counting accordingly beforehand
fractional_distribution <- data.frame()
fractional_distribution <- rbind(fractional_distribution, data.frame(period = "1951-1980", frequency = CANZUK_5180$Weight))
fractional_distribution <- rbind(fractional_distribution, data.frame(period = "1981-2000", frequency = CANZUK_8100$Weight))
fractional_distribution <- rbind(fractional_distribution, data.frame(period = "2001-2017", frequency = CANZUK_0120$Weight))
ggplot(fractional_distribution, aes(x=period, y=frequency, fill=period)) + geom_boxplot() + coord_trans(y="log2") 
