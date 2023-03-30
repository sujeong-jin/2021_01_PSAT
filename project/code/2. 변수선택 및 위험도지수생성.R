library(data.table)
library(tidyverse)
library(lm.beta)
library(magrittr)
library(corrplot)


########################## Best Subset Selection ###########################

total_gu <- fread("total_gu_new.csv")
total_dong <- fread("total_dong_new.csv")

## 상관계수 & 상관플랏
correlation <- cor(total_gu %>% select(-자치구))
corrplot(correlation, addCoef.col="black")

## 다중선형회귀
model <- lm(아동학대신고 ~ . -자치구, data = total_gu)
summary(model)

## 잔차플랏 확인
plot(model)

## 회귀가정 검정

### 독립성 검정
car::durbinWatsonTest(model)  # 독립성 만족

### 등분산성 검정
car::ncvTest(model)  # 등분산성 만족

### 다중공선성
vif(model)

### gvlma
library(gvlma)

gvmodel <- gvlma(model)
summary(gvmodel)

## 모두 만족

## Best subset selection
library(leaps)
regfit.full=regsubsets(아동학대신고~.,data=total_gu[,-1])
reg.summary <- summary(regfit.full)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
points(4,reg.summary$cp[4],pch=20,col="red")

## Best model
best.model <- lm(아동학대신고 ~ `5천만원이상비율` + cctv_rate + 아동수 + bene_gagu_rate, data = total_gu)
summary(best.model)

## 최적의 변수 조합: 5천만원이상비율, cctv_rate, 아동수, bene_gagu_rate

plot(best.model)

summary(gvlma(best.model)) # 모두 만족

############################
##### Factor Analysis ######
############################

library(psych)
library(GPArotation)

## 정규화
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

total_dong$아동수<- normalize(total_dong$아동수)

## 유의한 변수 4개로 위험도 지수 만들기 - 행정동 단위
dong.fact <- factanal(total_dong[,c(6,7,8,10)], factors = 1, rotation="varimax")
print(dong.fact, cutoff = 0)

f1 <- dong.fact$loadings[,1]

for (i in 1:nrow(total_dong)){
  factor1 = sum(total_dong[i,c(6,7,8,10)] * f1)
  total_dong$factor1[i] = factor1
}

total_dong %<>% select(행정구, 행정동, factor1, 학교제외)

total_dong$factor1 <- normalize(total_dong$factor1)*10

write.csv(total_dong, "clustering_dong.csv", row.names = F)

############################
############ PCA ###########
############################

library(MVA)
library(tidyverse)
library(data.table)
library(gridExtra)
library(caret)
library(MLmetrics)
library(data.table)
library(gridExtra)

data_dong<-read.csv('total_dong_new.csv')
#data_dong2<-data_dong[,-c(1,2,3)]
data_dong<-data_dong%>%select('X5천만원이상비율', 'bene_gagu_rate', '아동수', 'cctv_rate')

data_dong_pca<-prcomp(data_dong,scale=T, center=T)
data_dong_pca
summary(data_dong_pca)

x<-data.frame(가중치=c(0,0,0,0))

for (i in 1:4){
  x$가중치[i]<-data_dong_pca$rotation[i,1]/(abs(data_dong_pca$rotation[1,1])+abs(data_dong_pca$rotation[2,1])+abs(data_dong_pca$rotation[3,1])+abs(data_dong_pca$rotation[4,1]))
}



for (i in 1:nrow(data_dong)){
  danger = as.numeric(data_dong[i, 1] * x$가중치[1] + data_dong[i, 2] * x$가중치[2] + 
                        data_dong[i, 3] * x$가중치[3] + data_dong[i, 4] * x$가중치[4])
  data_dong$위험도지수[i] = danger 
}

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

data_dong$위험도지수<-normalize(data_dong$위험도지수)*100

write.csv(data_dong,'data_dong_danger_PCA2.csv')


############################
######### 상관분석 #########
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

# --- 가중치 생성

total =  fread('total_gu_new.csv', data.table=F)
total = total[,c(1,2,6,7,8,9)]
total_name = total[,1]


colnames(total) = c('gu','a','b','c','d','e', 'f')


lm_fit <- lm(a~b+c+d+e+f , data = total)
summary(lm_fit)



cor(a, b+c+d+e+f)
vif(lm_fit)


cor = cor(total[,c(2,3,4,5,6)], use='all.obs', method='pearson')
as.data.frame(cor)
corrplot(cor, method='number')
cor

A = abs(colSums(cor))
A = as.data.frame(A)
B = A[1,]

b_cor = cor[1,2]/B
c_cor = cor[1,3]/B
d_cor = cor[1,4]/B
e_cor = cor[1,5]/B


wow = c(b_cor, c_cor, d_cor, e_cor)
wow = as.data.frame(wow)

rownames(wow) = c('5천만원이상비율','CCTV 비율','아동수','기초수급가구비율')
colnames(wow) = c('가중치')

wow = t(wow)
as.data.frame(wow)
write.csv(wow, '가중치.csv')

# --- 위험도지수 생성

total_dong =  fread('total_dong_new.csv', data.table=F)
total_name2 = total_dong[,2]

final = (wow[,1]*total_dong[,6] + wow[,2]*total_dong[,7] + wow[,3]*total_dong[,8] + wow[,4]*total_dong[,9])
#final = abs(final)
#final = 1- final
final = as.data.frame(final)

colnames(final) = c('위험도 지수')

risk2 <- cbind(total_name2, final)
colnames(risk2) = c('동','위험도 지수')


risk2[,2] = risk2[,2]*100

write.csv(risk2, '동별 위험도 지수.csv')

#오름차순
risk3 <- risk2[,2]
risk3 = sort(risk3)
risk3 = as.data.frame(risk3)
colnames(risk3) = c('위험도 지수')
risk_down = left_join(risk3, risk2, by='위험도 지수')
risk_down = risk_down[-3,]
head(risk_down)

#내림차순
risk4 <- risk2[,2]
risk4 = sort(risk4, decreasing=TRUE)
risk4 = as.data.frame(risk4)
colnames(risk4) = c('위험도 지수')
risk_up = left_join(risk4, risk2, by='위험도 지수')
head(risk_up)

############################
######### 경로분석 #########
############################

library(lavaan)
library(semPlot)
library(tidyverse)
library(lm.beta)
library(magrittr)

gu <- read.csv(file="total_gu_new (1).csv",header=T)

wisc.model1 = '
 아동학대신고~  + bene_gagu_rate + 아동수 + X5천만원이상비율+cctv_rate'

fit1 = lavaan::cfa(wisc.model1,data = gu,std.lv = TRUE,orthogonal = TRUE)


semPlot::semPaths(fit1,whatLabels="std",style="lisrel",
                  nCharNodes=0,layout="tree2", edge.label.cex = 1, sizeLat = 10, sizeMan=8)

semPlot::semPaths(fit1, whatLabels = "std",intercepts = FALSE, style="lisrel",
                  nCharNodes = 0,
                  nCharEdges =0,
                  curveAdjacent = TRUE, title=TRUE,layout = "tree2", edge.label.cex = 1, sizeLat = 10, sizeMan=8,curvePivot=TRUE)

#-------------------------------------------------------------------------------------------

# --- 지도 시각화

dong_info = readxl::read_excel('행정동코드_매핑정보_2018.xlsx',sheet=1,col_names = TRUE) %>% as.data.frame()
dong_info = dong_info[-1,] # 첫줄은 필요없으니까 날리기

# 마찬가지로 신사동 구분해줘야 함
dong_info[(dong_info$시군구명 == '관악구') & (dong_info$행정동명 == '신사동'), '행정동명'] = '신사동_관'
dong_info[(dong_info$시군구명 == '강남구') & (dong_info$행정동명 == '신사동'), '행정동명'] = '신사동_강'


# 행정동코드 숫자형태로 바꿔주기
dong_info = dong_info %>%
  mutate(행정동코드 = as.integer(통계청행정동코드)) %>%
  dplyr::select(행정동코드,시군구명,행정동명)

# 만든 데이터셋과 행정동 메타데이터: 항동 제외하고는 행정동명 일치!
setdiff(dong_info$행정동명,total_dong$동)
setdiff(total_dong$동,dong_info$행정동명)

total_dong %<>% left_join(dong_info, by = c("동" = "행정동명", "자치구" = "시군구명"))

library(rgdal)
map = readOGR('Z_SOP_BND_ADM_DONG_PG.shp')

new_map = fortify(map, region = 'ADM_DR_CD')
new_map$id = as.numeric(new_map$id)
seoul_map = new_map[new_map$id <= 1174099,]

map_data = left_join(total_dong, seoul_map, by=c('행정동코드'='id'))

options(scipen=999)


ggplot() + 
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = 위험도지수), color = 'lightgrey')+
  scale_fill_gradient2(low = "#4B8CDE",mid = "white", high = "#FF8080", midpoint = 50) + ggtitle('위험도지수') +
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background = element_rect(fill="white", color="darkgrey"),
        legend.title=element_text(size=10),
        strip.text = element_text(face="bold")) +
  labs(fill = " ")

## 상위행정동, 하위행정동
total_dong[order(total_dong$위험도지수),] %>% head(5)
total_dong[order(total_dong$위험도지수),] %>% tail(8)

## 항동 제거
total_dong %<>% filter(동 != "항동")

write.csv(total_dong, "위험도지수_회귀분석.csv", row.names = F)
