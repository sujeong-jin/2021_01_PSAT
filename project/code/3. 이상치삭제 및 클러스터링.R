############################
####### 이상치 삭제 ########
############################

cluster <- fread("clustering_dong.csv")

# 이상치 확인
boxplot(cluster$학교제외)
boxplot(cluster$factor1)

summary(cluster$학교제외)
summary(cluster$factor1)

# 3분위 값에 1.5IQR을 더한 상위 극단치보다 큰 값이 몇 행에 있는지 확인합니다. 
which(cluster$학교제외 > summary(cluster$학교제외)[5] + 1.5*IQR(cluster$학교제외))

# 이상치 삭제
no_outlier <- cluster[-which(cluster$학교제외 > summary(cluster$학교제외)[5] + 1.5*IQR(cluster$학교제외)),]
## 대체기관수가 많아서 삭제된 많은 애들이 factor1 값도 작음

no_outlier2 <- no_outlier[-which(no_outlier$factor1 < summary(no_outlier$factor1)[2] - 1.5*IQR(no_outlier$factor1)),]

delete1 <- cluster[which(cluster$학교제외 > summary(cluster$학교제외)[5] + 1.5*IQR(cluster$학교제외)),]
write.csv(delete1, "대체기관수 많아 삭제.csv", row.names = F)

delete2 <- no_outlier[which(no_outlier$factor1 < summary(no_outlier$factor1)[2] - 1.5*IQR(no_outlier$factor1)),]
write.csv(delete2, "위험도지수 낮아 삭제.csv", row.names = F)

write.csv(no_outlier2, "이상치 삭제.csv", row.names = F)

############################
#### K-means 클러스터링 ####
############################

library(cluster)
library(fpc)

no_outlier2 <- fread("이상치 삭제.csv")

set.seed(2021)
result<-NULL
for (k in 1:20){
  result[[k]]<-kmeans(no_outlier2[,c(3:4)],k)
}

## 1) Elbow point  
fviz_nbclust(no_outlier2[,c(3:4)], FUN=kmeans, method = "wss", k.max = 20) 

## 2) Average Silhouette
# 평균 실루엣이 최대가 되도록 하는 k가 군집의 수로 적절합니다. 
avgsil<-numeric(20)
for (k in 2:20){
  si<-summary(silhouette(result[[k]]$cluster,dist(no_outlier2[,c(3:4)])))
  avgsil[k]<-si$avg.width
}
avgsil

library("factoextra")

fviz_nbclust(no_outlier2[,c(3:4)], FUN=kmeans, method = "silhouette", k.max = 20) 

nc <- NbClust(no_outlier2[,c(3:4)], min.nc = 2, max.nc = 15, method = "kmeans")

## 3) 시각화
# k=2,3,4,5일 때 클러스터링을 시각화하여 살펴보겠습니다.

fviz_cluster(result[[5]],data=no_outlier2[,c(3:4)],geom="point",stand=FALSE)

no_outlier2$cluster <- result[[5]]$cluster

write.csv(no_outlier2, "k-means.csv", row.names = F)

############################
### K-medoids 클러스터링 ###
############################

library(factoextra)
library(cluster)
library(tidyverse)

dong <- read.csv(file="clustering_dong3.csv",header=T)
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

nor<-normalize(dong$factor1)  # 변수 스케일링해주기
dong$factor1<- nor
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
nor1<-normalize(dong$학교제외)
dong$학교제외 <- nor1

cluster_1 <- dong[,c(3,4)] # 클러스터링할 변수만 추출하기 (factor1과 학교제외변수수)
cluster_1

corrplot::corrplot(cor(cluster_1), method = 'number') 
cluster_1 = scale(cluster_1) %>% as_tibble()

#Funcluter = pam 
fviz_nbclust(x = cluster_1, FUNcluster = pam, method='wss') #Elbow point값확인
fviz_nbclust(x = cluster_1 , FUNcluster = pam, method = "silhouette")

pam1 <- pam(cluster_1, 3)
pam1$silinfo$avg.width # silhouette값 확인: 0.443279
fviz_cluster(pam1)
head(pam1)

result_pam <- cbind(pam1$clustering, cluster_1)
result_pam <- as.data.frame(result_pam)

boxplot(위험도지수 ~ cluster_1, data = result_pam)
boxplot(대체기관수 ~ cluster_1, data = result_pam)

ggplot(result_pam, aes(x=cluster_1, y=위험도지수, fill = cluster_1)) + geom_boxplot() + theme_classic()
ggplot(result_pam, aes(x=cluster_1, y=대체기관수, fill = cluster_1)) + geom_boxplot() + theme_classic()


############################
###### GMM 클러스터링 ######
############################

library(ClusterR)
library(cluster)
library(data.table)
library(tidyverse)
final_data = fread('이상치 삭제.csv',data.table = F)

# 실루엣 값 살펴보기
avgsil<-numeric(30)
for (k in 2:30){
  gmm = GMM(final_data[,c(3,4)],k,dist_mode = 'eucl_dist',seed_mode = 'random_spread',
            km_iter = 10,em_iter = 10,verbose = F)
  pr = predict_GMM(final_data[,c(3,4)],gmm$centroids,gmm$covariance_matrices,gmm$weights)
  si<-summary(silhouette(pr$cluster_labels,dist(final_data[,c(3,4)])))
  avgsil[k]<-si$avg.width
}
avgsil

# 클러스터링 진행
gmm = GMM(final_data[,c(3,4)],4,dist_mode = 'eucl_dist',seed_mode = 'random_spread',
          km_iter = 10,em_iter = 10,verbose = F)

pr = predict_GMM(final_data[,c(3,4)],gmm$centroids,gmm$covariance_matrices,gmm$weights)

si<-summary(silhouette(pr$cluster_labels,dist(final_data[,c(3,4)])))
si$avg.width

# 시각화
dong_final5 = final_data
dong_final5$cluster = pr$cluster_labels
dong_final5 %>% 
  mutate(cluster = as.factor(cluster)) %>%
  ggplot(aes(x = factor1,y = 학교제외)) +
  geom_point(aes(color = cluster),size = 4) +
  scale_color_brewer(palette = 'Set2') +
  theme(
    panel.background = element_rect(fill='white', color='black', linetype='solid'))

# 클러스터 개수 확인
table(pr$cluster_labels)

# 타겟 클러스터 행정동 확인
target_clust = dong_final5 %>% filter(cluster == 0)
target_clust %>% head
write.csv(target_clust,'target_clust.csv',row.names = F)


############################
#### DBSCAN 클러스터링 #####
############################

if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/factoextra")
#install.packages('factoextra')
library(devtools)
library(factoextra)

final_data <-read.csv('이상치 삭제.csv')

df <- final_data[, 3:4]
plot(df)

library(dbscan)

#DBSCAN의 원 논문(참조 [1])에서는 2차원 데이터에 대해 실험을 해보니 MinPts 가 4개와 5개 이상 간의 k-dist plot (아래 설명 예정) 의 큰 변동이 없는 반면에 MinPts 가 점점 커질 수록 연산량(computation)이 상당히 커지므로 2차원 데이터에서는 MinPts = 4 개로 하는 것을 권장하고 있습니다.

#출처: https://rfriend.tistory.com/588 [R, Python 분석과 프로그래밍의 친구 (by R Friend)]

dbscan::kNNdistplot(df, k=4)
abline(h = 0.27, lty = 2)

dbscan::kNNdistplot(df, k=5)
abline(h = 0.3, lty = 2)

dbscan::kNNdistplot(df, k=6)
abline(h = 0.3, lty = 2)

dbscan::kNNdistplot(df, k=10)
abline(h = 0.3, lty = 2)
#k 를 4 ~ 7 까지 바꾸어 가면서 sorted k-dist plot 을 그려보니 k가 커질수록 elbow 지점의 Eps 가 조금씩 커져가는 것처럼 보임 ,그러나 큰 차이는 없음.

## DBSCAN clustering
set.seed(1004)
db <- dbscan::dbscan(df, eps = 0.27, minPts =4)

## or using fpc package
# library(fpc)
# db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)


## Plot DBSCAN results
library("factoextra")
fviz_cluster(db, df, stand = FALSE, frame = FALSE, geom = "point")

#https://rstudio-pubs-static.s3.amazonaws.com/320180_c39d043794c349938dc4f76d91e70d2f.html
library(fpc)    
plotcluster(df, db$cluster) 
library(cluster)
sil <- silhouette(db$cluster, dist(df))######
# plot silhouette
library(factoextra)
fviz_silhouette(sil)
sil.dbscan <- silhouette(db$cluster, dist(df))
summary(sil.dbscan)

result<-cbind(final_data,db$cluster)
write.csv(result,'DBSCAN.csv')

############################
#### HDBSCAN 클러스터링 ####
############################

#https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html
cl <- hdbscan(df, minPts = 9)
cl
plot(df, col=cl$cluster+1, pch=20)#The ‘flat’ results are stored in the ‘cluster’ member. Noise points are given a value of 0, so increment by 1.
plot(cl$hc, main="HDBSCAN* Hierarchy")

#논문: Spatial clustering, DBSCAN, HDBSCAN, OPTICS, Groundwater aquifer,
#The suggested value of MinPts in the literature is around 10 

#fviz_cluster(cl, df, stand = FALSE, frame = FALSE, geom = "point")
plot(cl, gradient = c("yellow", "orange", "red", "blue"))


#https://rstudio-pubs-static.s3.amazonaws.com/320180_c39d043794c349938dc4f76d91e70d2f.html
library(fpc)    
plotcluster(df, cl$cluster) 
library(cluster)
sil <- silhouette(cl$cluster, dist(df))######
# plot silhouette
library(factoextra)
fviz_silhouette(sil)
sil.dbscan <- silhouette(cl$cluster, dist(df))
summary(sil.dbscan)
result2<-cbind(final_data,cl$cluster)
write.csv(result2,'HDBSCAN.csv')

############################
###### SOM 클러스터링 ######
############################

scaled <- scale(no_outlier2[,c(3:4)])
library(kohonen)

gr <- somgrid(xdim = 3, ydim = 3, topo = "hexagonal")
ss <- supersom(scaled, gr, rlen = 200, alpha = c(0.05, 0.01))
plot(ss, type="changes")
plot(ss, type="count", main="Node Counts")
plot(ss, type="dist.neighbours", main = "SOM neighbour distances")
plot(ss, type="codes", codeRendering = "segments")
plot(ss, type="quality", main = "mapping quality")  # 할당된 element 들의 유사도
print(ss$unit.classif)  #각 행의 할당된 클러스터!


#colors function for the charts
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#plotting the heatmap for each variable
par(mfrow=c(1,2))
for (j in 1:2){
  plot(ss,type="property",property=ss$codes[[1]][,j],main = colnames(no_outlier2[,3:4])[j],palette.name=coolBlueHotRed,cex=0.5)
}
par(mfrow=c(1,1))

no_outlier2$class <- ss$unit.classif
no_outlier2 %>% filter(class == 9)
no_outlier2 %>% filter(class == 7)
no_outlier2 %>% group_by(class) %>% summarise(n = n())

write.csv(no_outlier2, "SOM.csv", row.names = F)

############################
##### 계층적 클러스터링 ####
############################

library(data.table)
library(tidyverse)
library(caret)
library(wCorr)
library(corrplot)
library(GGally)
library(car)
library(fmsb)
library(gvlma)
library(reshape)
library(flexclust)
library(NbClust)
library(factoextra)
library(cluster)

total =  fread('이상치 삭제.csv', data.table=F)

hc <- scale(total[,c(3,4)])
d = dist(hc)

fit <- hclust(d, method='ward.D')

plot(fit, hang=-1, cex=0.1)
nc <- NbClust(hc, distance="euclidean", method="ward.D")
plot_total= plot(fit, hang=-10, k=4)

plot_total2 = rect.hclust(fit, k=4)
clusters <- cutree(fit, k=4)
table(clusters)
clusters = as.data.frame(clusters)

total_2 <- cbind(total, clusters)
total_2 <- as.data.frame(total_2)

fviz_nbclust(total_2, FUN= hcut, method = "silhouette", cex=1)  

clusplot(total_2, clus = total_2$clusters , color=TRUE, lines=0, cex=1)

#추가해볼게
total_2 %>% group_by(행정동) %>% summarise(mean_1 = mean(factor1), mean_2 = mean(학교제외))

k = kmeans(total_2, 3)

k$cluster
