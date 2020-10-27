install.packages("stats")
install.packages("heuristica")
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("sqldf")
install.packages("plotly")
install.packages("fmsb")
install.packages("devtools")
install.packages("ClusterR") 
install.packages("cluster") 
install.packages("corrplot")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("factoextra")
#Loading All The Required Library packages.
library(stats)
library(readr)
library(dplyr) 
library(sqldf)
library(ggplot2)
library(fmsb)
library(plotly)
library(ggthemes)
library(devtools)
library(factoextra)
library(corrplot)
library(ClusterR) 
library(cluster) 
library(rpart)
library(rpart.plot)

#Set As Working Directory
setwd("~/GBUS 721")
#Loading The Dataset
VideoGamesDS <- read.csv("FINAL PROJECT GBUS 721/archive/Video_Games_Sales_as_at_22_Dec_2016.csv")
View(VideoGamesDS)
VideoGamesDS_backup<-VideoGamesDS
str(VideoGamesDS)

# Viewing the first 10 DataFrame records
head(VideoGamesDS, 10)

#Removing Na Values and Data Manipulation
df<-read.csv("FINAL PROJECT GBUS 721/archive/Video_Games_Sales_as_at_22_Dec_2016.csv")
df
df[df=="N/A"]<-NA
df<-df[complete.cases(df),]
sum(is.na(df))#O Na Values
View(df)

#correlation plot for quantitative data
cp<-(cor(df[sapply(df, function(x) !is.factor(x))]))
corrplot::corrplot(cp,method="circle",order="hclust")
#Selecting the data records after 1995.
df$Year_of_Release<-as.numeric(as.character(df$Year_of_Release))
df<-df[df$Year_of_Release>=1995,]
#Summary of dataset 
summary(df)
#Pca using prcomp
VDG.pca <- prcomp(df[,c(3,6:14)], center = TRUE,scale. = TRUE)
summary(VDG.pca)

#fitting PCA model
#Selecting the sales and observations
vd<-(df[,6:14])
vd.year<-df[,3]
vd.pca<-prcomp(vd,center = TRUE, scale. = TRUE)
print(vd.pca)
#Distribution of pc metrics
plot(vd.pca,main="Principal Component Splits")
predict(vd.pca,newdata=tail(vd, 2))
#Visualizing PCA
PCV<- function(PC, x="PC1", y="PC2") {
  data <- data.frame( PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y))
  datapc <- data.frame(varnames=row.names(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size =2, vjust=2, color="blue")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="black")
  plot+ggtitle("Top Principal Components Factors")+theme_grey()
}
#Principle Component Analysis Fit
PCV(vd.pca)

#Using Plotly to diplay year wise sales
df$Year<-as.numeric(as.character(df$Year_of_Release))
SS<- sqldf("SELECT Year, sum(NA_Sales) as NAM, sum(EU_Sales) as EU,sum(JP_Sales) as JP, sum(Other_Sales) as Other, sum(Global_Sales) as Global from df group by Year order by Year")
plot_ly(y=SS,mode='lines')%>%
  add_trace(y=SS$EU,x=SS$Year,name='Europe Sales',mode = 'lines')%>%
  add_trace(y=SS$NAM,x=SS$Year,name='NorthAmerican Sales',mode = 'lines')%>%
  add_trace(y=SS$Global,x=SS$Year,name='Global Sales',mode = 'lines')%>%
  add_trace(y=SS$JP,x=SS$Year,name='Japan Sales',mode = 'lines')%>%
  add_trace(y=SS$Other,x=SS$Year,name='Other Regions',mode = 'lines')%>%
  layout(title = "Regionwise Sales Analysis Vs Yearwise trends",
         scene = list( xaxis = list(title = "Year"), 
                       yaxis = list(title = "Sales")))

#Selecting Top 10 Publishers
Publisher<- sqldf('SELECT Publisher, sum(Global_Sales) as Sales from df group by Publisher order by Sales desc')[c(1:10),]
print(Publisher[,1])
#Year Wise Analysis of Sales across all regions.
Yearwise<-sqldf("SELECT Platform, Year_of_Release, sum(NA_Sales) as NAM, sum(EU_Sales) as EU,sum(JP_Sales) as JP, sum(Other_Sales) as Other, sum(Global_Sales) as Global from df group by Platform, Year_of_Release,Platform")
View(Yearwise)

#Platform Oriented Analysis
tmp<-df[df$Publisher==Publisher[10,1],]#vary publisher from 1 to 10 to observe patterns
#Publisher[x,1] where x=1..10.
frame<-sqldf('SELECT Platform, sum(Global_Sales) as Sales from tmp group by Platform')
frame_t<- t(frame[,2:ncol(frame)])
colnames(frame_t)<-frame[,1]
frame_t<-as.data.frame(frame_t)
data=rbind(rep(max(frame$Sales),ncol(frame_t)) , rep(min(frame$Sales),ncol(frame_t)) , frame_t)
radarchart(data,title ="Sales Across Various Platforms:Konami Digital Entertainment" , axistype=1 , 
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           cglcol="blue", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8 )

#Genre Oriented Analysis
tmp<-df[df$Publisher==Publisher[10,1],]#vary publisher from 1 to 10 to observe patterns
#Publisher[x,1] where x=1..10.
frame<-sqldf('SELECT Genre, sum(Global_Sales) as Sales from tmp group by Genre')
frame_t<- t(frame[,2:ncol(frame)])
colnames(frame_t)<-frame[,1]
frame_t<-as.data.frame(frame_t)
data=rbind(rep(max(frame$Sales),ncol(frame_t)) , rep(min(frame$Sales),ncol(frame_t)) , frame_t)
radarchart(data,title ="Sales Across Various Genres:Konami Digital Entertainment" , axistype=1 , 
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           cglcol="green", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8 )

# k Means
df_g<-df[,6:14]
# Fitting K-Means clustering Model  
# to training dataset 
set.seed(100) # Setting seed 
kmeans.re <- kmeans(df_g, centers = 3, nstart = 30) 
kmeans.re 
#cluster analysis
kmeans.re$cluster 
cm <- table(df$Genre, kmeans.re$cluster) 
cm 

#Selecting user and critic factors for clustering
plot(df_g[c("Critic_Score","Critic_Count","User_Score","User_Count")]) 
plot(df_g[c("Critic_Score","Critic_Count","User_Score","User_Count")],  
     col = kmeans.re$cluster) 
plot(df_g[c("Critic_Score","Critic_Count","User_Score","User_Count")],col = kmeans.re$cluster,  
     main = "K-means with 3 clusters") 
## Ploting cluster centers 
kmeans.re$centers 
kmeans.re$centers[, c("Critic_Score","Critic_Count","User_Score","User_Count")]
points(kmeans.re$centers[, c("Critic_Score","Critic_Count","User_Score","User_Count")],  
       col = 1:3, pch = 10, cex = 5)  

## Visualizing clusters 
y_kmeans <- kmeans.re$cluster 
clusplot(df_g[,c("Critic_Score","Critic_Count","User_Score","User_Count")], 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 2, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste("Cluster Distribution"), 
         xlab = 'Critic and User_Score', 
         ylab = 'Critic and User_Count') 
