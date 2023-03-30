############################
##### 최종 후보 행정동 #####
############################

# gaussian mixture 타겟 클러스터
gaussian <- fread("cluster_latlon.csv")
data1 <- gaussian %>% select(행정동)

# 현재 존재하는 아보전으로 커버되는 행정동 리스트
coverage <- readxl::read_excel("커버리지내행정동.xlsx", col_names = T)
coverage %<>% select(행정동)

# 전체 행정동에서 현재 커버되는 행정동 빼줌
total_dong <- fread("total_dong_new.csv")
hubo <- total_dong %>% filter(!(행정동 %in% coverage$행정동))

# 현재 커버되지 않는 행정동과 가우시안 믹스처로 뽑은 행정동 중 겹치는 애들만 뽑음
hubo %<>% inner_join(data1, by = "행정동")
hubo %<>% left_join(gaussian, by = c("행정구", "행정동"))

# 필요한 변수만 추출
hubo %<>% select(행정동, 아동수 , center_lat, center_long)

write.csv(hubo, "행정동후보.csv", row.names = F)

## 5km 반경을 기준으로 최종 후보 행정동 리스트 추출
remain <- fread("5km행정동.csv", encoding = "UTF-8")
remain %<>% inner_join(data1, by = c("ADM_DR_NM" = "행정동"))
remain %<>% left_join(gaussian, by = c("ADM_DR_NM" = "행정동"))

write.csv(remain, "5km반경행정동.csv", row.names = F)


############################
########### LSCP ###########
############################

library(tbart) # P-Median 함수가 저장되어 있는 패키지
library(geosphere) # 위경도로 거리를 구할 수 있는 패키지
library(data.table)
library(tidyverse)

# 거리 계산 함수
make_dis_mat <- function(lon, lat, p_median) {
  n = length(lon)
  dist_mat <- matrix(NA, n, n)
  for (i in 1:n) {
    lon1 <- lon[i]
    lat1 <- lat[i]
    if (i %% 20 == 0) {
      cat(i, '')
    }
    for (j in 1:n) {
      lon2 <- lon[j]
      lat2 <- lat[j]
      dist_mat[i, j] <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    }
  } # 거리를 1대1 대응을 통해 구함
  return(dist_mat)
}

# 2.5km 이내 아동수 합산 함수
cnt_nearby_child = function(dist_mat,child_vec) {
  nearby_child = rep(NA,length(child_vec)) # Initializing
  for (i in 1:nrow(dist_mat)) {
    child_i = 0 # Initializing
    for (j in 1:ncol(dist_mat)) {
      if (dist_mat[i,j] <= 2500) {
        child_i = child_i + child_vec[j] # (자신 포함) 근방에 위치하면 더해줌
      }
    }
    nearby_child[i] = child_i 
  }
  return(nearby_child)
}

LSCP = function(dist_mat,child_vec) {
  k = 0 # 설치 개수
  target = NULL # 설치 위치
  traversal = NULL # 추가했거나, 추가한 곳 2.5km 이내에 위치한 곳
  remain = NULL
  
  cat('설치 위치: ',target,'\n')
  cat('남은 아동수: ',sum(child_vec),'\n')
  remain = c(remain,sum(child_vec))
  
  while (sum(child_vec) != 0) { # 커버되지 않은 아동 없을 때까지 반복반복
    # 입지 후보지 대상으로 2.5km 이내 합산 아동수 계산 후 정렬
    n_nearby = cnt_nearby_child(dist_mat,child_vec)
    nearby_child = data.frame(
      idx = 1:length(n_nearby),
      n = n_nearby)
    nearby_child = nearby_child %>% arrange(desc(n)) %>% filter(!(idx %in% traversal))
    
    # 선정된 후보지 추가
    point = nearby_child[1,1]
    target = c(target,point)
    k = k + 1
    
    # 선정된 후보지 근방의 아동수 0으로 할당
    for (i in 1:ncol(dist_mat)) {
      if (dist_mat[point,i] <= 2500) {
        child_vec[i] = 0
        traversal = c(traversal,i)
      }
    }
    remain = c(remain,sum(child_vec))
    cat('설치 위치: ',target,'\n')
    cat('남은 아동수: ',sum(child_vec),'\n')
    cat('---------------------------------------------------------------------\n')
  }
  return(list(k = k,target = target,remain = remain))
}

# main
setwd("D:/학회/2021-1/주제분석/final_data")
cand_dong = fread('5km반경행정동.csv',data.table = F)
dist_mat = make_dis_mat(cand_dong$center_long,cand_dong$center_lat)

needed_k = LSCP(dist_mat,cand_dong$total_dong_new_아동수)
needed_k$remain %>% length

score = needed_k$remain[c(1:21)] - needed_k$remain[c(2:22)]
plot(1:21,score,type = 'l')


############################
#### 반경내 수요 최대화 ####
############################

library(tbart) # P-Median 함수가 저장되어 있는 패키지
library(geosphere) # 위경도로 거리를 구할 수 있는 패키지
library(data.table)
library(tidyverse)

# 거리 계산 함수
make_dis_mat <- function(lon, lat, p_median) {
  n = length(lon)
  dist_mat <- matrix(NA, n, n)
  for (i in 1:n) {
    lon1 <- lon[i]
    lat1 <- lat[i]
    if (i %% 20 == 0) {
      cat(i, '')
    }
    for (j in 1:n) {
      lon2 <- lon[j]
      lat2 <- lat[j]
      dist_mat[i, j] <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    }
  } # 거리를 1대1 대응을 통해 구함
  return(dist_mat)
}

# 2.5km 이내 아동수 합산 함수
cnt_nearby_child = function(dist_mat,child_vec) {
  nearby_child = rep(NA,length(child_vec)) # Initializing
  for (i in 1:nrow(dist_mat)) {
    child_i = 0 # Initializing
    for (j in 1:ncol(dist_mat)) {
      if (dist_mat[i,j] <= 2500) {
        child_i = child_i + child_vec[j] # (자신 포함) 근방에 위치하면 더해줌
      }
    }
    nearby_child[i] = child_i 
  }
  return(nearby_child)
}

# 5km 이내 중복되는 시설 없게 선별하는 함수
select_by_max_child = function(num_k,dist_mat,nearby_child) {
  df = cbind(1:length(nearby_child),nearby_child)
  df = df[order(df[,2],decreasing = TRUE),] # 아동수에 따라 정렬
  
  current = 2
  target = c(1) # 첫번째는 무조건 추가하므로
  while (length(target) < num_k) { # 최종 입지 더 찾아야 함
    idx = 1
    traversal = target[idx]
    flag = 'Y'
    
    cat('current:',df[current,1],'traversal:',df[traversal,1],'\n')
    
    while (traversal <= target[length(target)]) { # 아직 다 안 돌았음
      if (dist_mat[df[current,1],df[traversal,1]] < 5000) {
        flag = 'N'
        break
      }
      if (traversal == target[length(target)]) break
      idx = idx + 1
      traversal = target[idx]# 다음것 탐색
    }
    if (flag == 'Y') target = c(target,current)
    
    cat('-------------------------','flag:',flag,'target:',df[target,1],'\n')
    
    current = current + 1
  }
  return(df[target,1])
}


# 데이터셋
cand_dong = fread('5km반경행정동.csv',data.table = F)
dist_mat = make_dis_mat(cand_dong$center_long,cand_dong$center_lat)
cand_dong$nearby_child = cnt_nearby_child(dist_mat,cand_dong$total_dong_new_아동수)

cand_dong %>% arrange(desc(nearby_child)) %>% head(8)

# 입지 선정
p8 = select_by_max_child(8,dist_mat,cand_dong$nearby_child)

cand_dong[p8,]

# screeplot
plot(x = 1:8, y = cand_dong[p8,'nearby_child'], 
     type = 'b', ylab = '')
dist_mat[29,27]

############################
######### P-median #########
############################

library(tbart) # P-Median 함수가 저장되어 있는 패키지
library(geosphere) # 위경도로 거리를 구할 수 있는 패키지
library(data.table)
library(tidyverse)


# 거리계산한 행렬을 먼저 생성하고, 이거를 사용해서 p-median을 사용하자.
make_dis_mat <- function(lon, lat, p_median) {
  n = length(lon)
  dist_mat <- matrix(NA, n, n)
  
  for (i in 1:n) {
    lon1 <- lon[i]
    lat1 <- lat[i]
    if (i %% 20 == 0){
      cat(i, '')
    }
    for (j in 1:n){
      lon2 <- lon[j]
      lat2 <- lat[j]
      dist_mat[i, j] <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    }
  } # 거리를 1대1 대응을 통해 구함
  return(dist_mat)
}


p_median <- function(distance_matrix, num_p, seed) {
  set.seed(seed) # 시드지정
  initial_value = sample(1:nrow(distance_matrix), num_p)  # 인덱스 num
  result_vec <- tb.raw(distance_matrix, initial_value, T) # index num
  distance_matrix[-result_vec, -result_vec] <- 0
  distance_matrix[result_vec,] <- 0
  
  for_vec <- 1:nrow(distance_matrix)  # 1~92
  for_vec <- for_vec %>% setdiff(result_vec)  # 입지점 인덱스만 제외됨
  for (i in for_vec) {
    a <- distance_matrix[i, result_vec] # 각 입지점과 수요지들간의 거리
    a[a > 2500] <- 0  # 커버리지 범위인 2.5km보다 먼 거리는 모두 0으로 없앰
    a[which(a != distance_matrix[i, result_vec] %>% min)] <- 0 
    # 입지점과 수요지까지의 조합 중 최단거리만 남김
    distance_matrix[i, result_vec] <- a 
    # 즉, 모든 수요점들이 p개의 입지점까지의 거리 중 최단거리인 곳에만 남고 나머지는 거리가 0으로 바뀌는 것!
  }
  return(distance_matrix)
}

data <- fread("5km반경행정동.csv")
data %<>% rename(아동수 = total_dong_new_아동수)
data_lon <- data$center_long
data_lat <- data$center_lat


# P-median의 수요(아동 인구)를 고려하는 의미로, 아동 인구의 quantile값을 기준으로 2,3,4배 가중치를 줌
data$아동수 %>% summary()
#data$quant <- ifelse(data$아동수 <= 1892, 1,
#                     ifelse(data$아동수 <= 2918, 2,
#                            ifelse(data$아동수 <= 4865, 3, 4)))

data$quant <- ifelse(data$아동수 <= 2070, 1,
                     ifelse(data$아동수 <= 2960, 2,
                            ifelse(data$아동수 <= 4430, 3, 4)))


data_lon <- c(data_lon, rep(data[data$quant == 2,"center_long"], 1),
              rep(data[data$quant == 3,"center_long"], 2),
              rep(data[data$quant == 4,"center_long"], 3))

data_lat <- c(data_lat, rep(data[data$quant == 2,"center_lat"], 1),
              rep(data[data$quant == 3,"center_lat"], 2),
              rep(data[data$quant == 4,"center_lat"], 3))

### 행정동의 중심점을 기준으로 거리행렬 계산
data_lon %<>% unlist() %>% as.vector()
data_lat %<>% unlist() %>% as.vector()

distmat = make_dis_mat(data_lon, data_lat)

### P-median알고리즘을 중심수를 변화해가며 적용
build_1 = p_median(distmat, 1, seed = 42)
# Configuration:  15  Score: 2176242 
build_2 = p_median(distmat, 2, seed = 42)
# Configuration:  47 77 Score: 1545832
build_3 = p_median(distmat, 3, seed = 42)
# Configuration:  47 22 72  Score: 1201239   
build_4 = p_median(distmat, 4, seed = 42)
# Configuration:  47 91 25 61  Score: 958522.4 
build_5 = p_median(distmat, 5, seed = 42)
# Configuration:  47 88 25 13 78  Score: 830970.4 
build_6 = p_median(distmat, 6, seed = 42)
build_7 = p_median(distmat, 7, seed = 42)
build_8 = p_median(distmat, 8, seed = 42)
build_9 = p_median(distmat, 9, seed = 42)
build_10 = p_median(distmat, 10, seed = 42)

# 입지 개수 선정
# plot(x = 1:10, y = c(2176242 , 1545832, 1201239, 958522.4 , 830970.4, 754316.7, 699167.7, 640093.9 , 581367.7, 534764.7), 
#     type = 'b', ylab = '', main = '최종입지 screeplot', xlab = "기관 개수")

plot(x = 1:10, y = c(1066013 , 769286.5, 559091, 405237 , 349316, 311575.5, 274237.6, 247356.3 , 218683.9, 198507.6), 
     type = 'b', ylab = '', main = '최종입지 screeplot', xlab = "기관 개수")

# 최종 입지 행정동 확인
## 1개 설치
num1 <- matrix(c(data_lat[c(11)], data_lon[c(11)]), nrow = 1, ncol = 2)
num3 <- matrix(c(data_lat[c(49, 22, 72)], data_lon[c(49, 22, 72)]), nrow = 3, ncol = 2)

final <- data[c(47, 22, 72),]
final <- data[c(10,44,29)]

write.csv(final, "최종입지행정동5km.csv", row.names = F)