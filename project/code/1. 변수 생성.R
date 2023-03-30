setwd('D:/학회/2021-1/주제분석/final_data')

need_packages = c("data.table", "tidyverse", "gridExtra",'readxl')
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

#################################
### 아동청소년보호기관 데이터 ###
#################################

protect_kids = fread('여성가족부_아동청소년보호기관정보_20200421.csv',data.table = FALSE)
dim(protect_kids)
head(protect_kids)

# --- 잘못된 정보 수정
protect_kids %>%
  filter(EDU시군구 != 주소지시군구,시도 == '서울특별시',EDU시군구 != '') %>% 
  dplyr::select(c(아동청소년보호기관명,시도,EDU시도,주소지시군구,EDU시군구)) # 서울특별시만 골라서 봄

protect_kids[2786,'주소지시군구'] = '은평구'
protect_kids[2786,'행정동명'] = '역촌동'

# --- 서울특별시 행 추출
protect_kids = protect_kids %>%
  filter(시도 == '서울특별시')

# --- 사용하지 않는 변수 제거
protect_kids = protect_kids %>%
  dplyr::select(-c(우편고지발송기관고유번호,시도,읍면동정보,우편번호,상세건물명1,상세건물명2,기관대표자,기관구분값,완료여부,기관코드,EDU일련번호,EDU시도,EDU시군구,EDU읍면동,V21,V22,V23,V24,주소지지도X좌표,주소지지도Y좌표))

# --- 적절하지 않은 행 제거
protect_kids = protect_kids %>%
  filter(!grepl('학원',아동청소년보호기관명),!grepl('독서실',아동청소년보호기관명),!grepl('주민센터',아동청소년보호기관명,기관주소))

protect_kids %>%
  filter((!grepl('어린이집',아동청소년보호기관명)) &
           (!grepl('유치원',아동청소년보호기관명)) &
           (!grepl('아동센터',아동청소년보호기관명)) &
           (!grepl('방과후교실',아동청소년보호기관명)) &
           (!grepl('학교',아동청소년보호기관명)) &
           (!grepl('아동복지센터',아동청소년보호기관명)) &
           (!grepl('돌봄센터',아동청소년보호기관명)) &
           (!grepl('수련관',아동청소년보호기관명)) &
           (!grepl('문화의집',아동청소년보호기관명)) &
           (!grepl('청소년',아동청소년보호기관명)) &
           (!grepl('유스',아동청소년보호기관명))
         
  ) %>%
  dplyr::select(아동청소년보호기관명) %>% as.vector()

protect_kids = protect_kids %>% 
  filter(!아동청소년보호기관명 %in% c('도봉숲속마을',
                            '북한산생태탐방연수원',
                            '서울올림픽파크텔',
                            '드림발레스튜디오',
                            '명일점와와학습코칭센터',
                            '팔로알토아카데미',
                            '바둑의품격'))

# --- 행정동명 NA 채우기
na_row = protect_kids %>%
  filter(행정동명 == '')

protect_kids[protect_kids$아동청소년보호기관명 == '강일늘사랑유치원','행정동명'] = bowl$dong[1]
protect_kids[protect_kids$아동청소년보호기관명 == '한영중고등학교병설한영유치원','행정동명'] = bowl$dong[2]

# --- 행정동명 통일시키기
protect_kids = protect_kids %>%
  mutate(행정동명 = sub('제1','1',행정동명)) %>%
  mutate(행정동명 = sub('제2','2',행정동명)) %>%
  mutate(행정동명 = sub('제3','3',행정동명)) %>%
  mutate(행정동명 = sub('제4','4',행정동명)) %>%
  mutate(행정동명 = sub('제5','5',행정동명)) %>%
  mutate(행정동명 = sub('제6','6',행정동명)) %>%
  mutate(행정동명 = sub('제7','7',행정동명)) %>%
  mutate(행정동명 = sub('제8','8',행정동명))

# --- 중복된 행정동명 구분시키기
protect_kids[(protect_kids$주소지시군구 == '관악구') & (protect_kids$행정동명 == '신사동'),'행정동명'] = '신사동_관'
protect_kids[(protect_kids$주소지시군구 == '강남구') & (protect_kids$행정동명 == '신사동'),'행정동명'] = '신사동_강'

write_csv(protect_kids,'shelter.csv')

# ---------- 구별로 정리 ---------- #
gu_shelter = protect_kids %>% 
  mutate(gu = 주소지시군구) %>%
  dplyr::select(c(아동청소년보호기관명,gu)) %>%
  group_by(gu) %>%
  summarize(n_shelter = n()) %>% as.data.frame()

write_csv(gu_shelter,'gu_shelter.csv')

# ---------- 행정동별로 정리 ---------- #
dong_shelter = protect_kids %>% 
  mutate(dong = 행정동명) %>%
  dplyr::select(c(아동청소년보호기관명,dong)) %>%
  group_by(dong) %>%
  summarize(n_shelter = n()) %>% as.data.frame()

write_csv(dong_shelter,'dong_shelter.csv')

#############################
### 가정폭력상담소 데이터 ###
#############################

counseling = read_excel('서울특별시 가정폭력상담소 정보.xlsx',sheet=1,col_names = TRUE) %>% as.data.frame()

# --- 사용하지 않는 변수 제거
counseling = counseling %>% 
  dplyr::select(-c(번호,인허가일자,영업상태명,폐업일자,휴업시작일자,휴업종료일자,재개업일자,소재지면적,소재지우편번호,
              여성복지시설종류,여성복지시설종류명,전화번호,인허가번호,상세영업상태명,`위치정보(X)`,`위치정보(Y)`))

# --- 중복된 행 제거
counseling = counseling[-19,]

# --- NA 방지하기 위해 주소지 변경
counseling[17,'소재지전체주소'] = '서울특별시 양천구 목동 923-6'
counseling[19,'소재지전체주소'] = '서울특별시 종로구 행촌동 1-31'

# --- 행정동명 변수 추가
counseling$행정동명 = bowl$dong

# --- 시군구 변수 추가
counseling$시군구 = do.call('cbind',strsplit(counseling$소재지전체주소,split = ' '))[2,]

write.csv(counseling,'counseling.csv')

# ---------- 구별로 정리 ---------- #
gu_counseling = counseling %>% 
  mutate(gu = 시군구) %>%
  dplyr::select(c(사업장명,gu)) %>%
  group_by(gu) %>%
  summarize(n_counseling = n()) %>% as.data.frame()

write_csv(gu_counseling,'gu_counseling.csv')

# ---------- 행정동별로 정리 ---------- #
dong_counseling = counseling %>% 
  mutate(dong = 행정동명) %>%
  dplyr::select(c(사업장명,dong)) %>%
  group_by(dong) %>%
  summarize(n_counseling = n()) %>% as.data.frame()

write_csv(dong_counseling,'dong_counseling.csv')

###########################
### 아동복지시설 데이터 ###
###########################

child_welfare = fread('서울특별시 사회복지시설(아동복지시설) 목록.csv',data.table = FALSE)

# --- 사용하지 않는 변수 제거
child_welfare = child_welfare %>% 
  dplyr::select(-c(시설코드,`시설종류명(시설유형)`,`시설종류상세명(시설종류)`,`자치구(시)구분`,시설장명,시군구코드))

# --- 중복된 행 제거
child_welfare %>%
  group_by(시설명,시군구명) %>%
  mutate(n = n()) %>%
  filter(n != 1) %>%
  as.data.frame()

child_welfare[child_welfare$시설명 == '신금호아이꿈누리터(성동11호점 우리동네키움센터)',]
child_welfare[child_welfare$시설명 == '송파희망세상지역아동센터',]

child_welfare = child_welfare[-c(646,647),]

# --- 아동복지시설이 아닌 행 제거
child_welfare = child_welfare %>%
  filter(!(시설명 == '조대봉님'))

# --- 잘못된 시군구 변경
unique(child_welfare$시군구명) # 서울특별시
child_welfare[child_welfare$시군구명 == '서울특별시',]
child_welfare[40,'시군구명'] = '마포구' # 마포구로 옮겼다고 함
child_welfare[40,'시설주소'] = '서울시 마포구 만리재로 14'

# --- 행정동 변수 추가
child_welfare$행정동명 = bowl$dong

nona_row = child_welfare %>%
  filter(!is.na(행정동명))

na_row = fread('strange.csv',data.table = F)
na_row$행정동명 = bowl$dong

child_welfare = rbind(nona_row,na_row)

# --- 주소지가 서울이 아닌 행 제거
child_welfare = child_welfare %>%
  filter(행정동명 != '배방읍') %>%
  filter(행정동명 != '원곡면') %>%
  filter(행정동명 != '양서면') %>%
  filter(행정동명 != '어모면') %>%
  filter(행정동명 != '기배동') %>%
  filter(행정동명 != '동문1동') %>%
  filter(행정동명 != '동천동') %>%
  filter(행정동명 != '증포동')

# --- 행정동명 통일시키기
child_welfare[child_welfare$행정동명 == '면목제3.8동','행정동명'] = '면목3.8동'

# --- 중복된 행정동명 구분시키기
child_welfare[(child_welfare$시군구명 == '관악구') & (child_welfare$행정동명 == '신사동'),'행정동명'] = '신사동_관'
child_welfare[(child_welfare$시군구명 == '강남구') & (child_welfare$행정동명 == '신사동'),'행정동명'] = '신사동_강'

write_csv(child_welfare,'child_welfare.csv')

# ---------- 구별로 정리 ---------- #
gu_welfare_fac = child_welfare %>% 
  mutate(gu = 시군구명) %>%
  dplyr::select(c(시설명,gu)) %>%
  group_by(gu) %>%
  summarize(n_welfare = n()) %>% as.data.frame()

write_csv(gu_welfare_fac,'gu_welfare_fac.csv')

# ---------- 행정동별로 정리 ---------- #
dong_welfare_fac = child_welfare %>% 
  mutate(dong = 행정동명) %>%
  dplyr::select(c(시설명,dong)) %>%
  group_by(dong) %>%
  summarize(n_welfare = n()) %>% as.data.frame()

write_csv(dong_welfare_fac,'dong_welfare_fac.csv')

# --- Join & NA는 시설이 없다는 말이므로 0으로 채우기 
gu_accessibility = gu_shelter %>% full_join(gu_counseling,by = 'gu') %>% full_join(gu_welfare_fac,by = 'gu')
dong_accessibility = dong_shelter %>% full_join(dong_counseling,by = 'dong') %>% full_join(dong_welfare_fac,by = 'dong')

gu_accessibility = mutate_at(gu_accessibility,c('n_shelter','n_counseling','n_welfare'), ~replace(., is.na(.), 0))
gu_accessibility

dong_accessibility = mutate_at(dong_accessibility,c('n_shelter','n_counseling','n_welfare'), ~replace(., is.na(.), 0))
dong_accessibility

#############################
### 고위기시설 데이터 ###
#############################

library(tidyverse)
library(magrittr)
library(readxl)

wetak <- read_excel("가정위탁지원센터.xlsx")
lifefam <- read_excel("공동생활가정.xlsx")
lifefam %<>% drop_na() 
welfare <- read_excel("아동복지시설.xlsx")
welfare %<>% drop_na()

lifefam %<>% select(시설종별, 시설명칭, 소재지) %>% 
  rename(시설명 = 시설명칭)

welfare %<>% select(시설종별, 시설명, 소재지)
welfare %<>% rbind(lifefam)

wetak %<>% mutate(시설종별 = "가정위탁지원센터") %>% 
  select(시설종별, 시설명, 소재지)

welfare %<>% rbind(wetak)

### kakao API
library(httr)
library(jsonlite)

# 검색할 주소 목록

address_list <- welfare$소재지

# Web API key

KAKAO_MAP_API_KEY = "KAKAOKEY"

# 결과 저장용 데이터프레임

bowl1 = data.frame(address = address_list,
                   gu = rep(NA, length(address_list)),
                   dong = rep(NA, length(address_list)))

# 주소별 반복
for(i in 1:length(address_list)){
  print(i)
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = address_list[i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  gu <- coord$documents$address$region_2depth_name
  dong <- coord$documents$address$region_3depth_h_name
  
  if (length(dong) > 0) {
    bowl1$gu[i] <- gu
    bowl1$dong[i] <- dong
  } else {
    bowl1$gu[i] <- NA
    bowl1$dong[i] <- NA
  }
}

for(i in 105:length(address_list)){
  print(i)
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = address_list[i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  gu <- coord$documents$address$region_2depth_name
  dong <- coord$documents$address$region_3depth_h_name
  
  if (length(dong) > 0) {
    bowl1$gu[i] <- gu
    bowl1$dong[i] <- dong
  } else {
    bowl1$gu[i] <- NA
    bowl1$dong[i] <- NA
  }
}

welfare[30, 1:3] <- list("보호치료시설", "살레시오청소년센터", "서울특별시 영등포구 디지털로 424")

bowl1 %<>% mutate(address = as.character(address))
bowl1[30, 1:3] <- list("서울특별시 영등포구 디지털로 424", "영등포구", "대림1동")
bowl1[40, 2:3] <- list("서대문구", "신촌동")
bowl1[45, 2:3] <- list("광진구", "구의2동")
bowl1[71, 2:3] <- list("양천구", "신월3동")
bowl1[72, 2:3] <- list("양천구", "신월3동")
bowl1[87, 2:3] <- list("송파구", "마천1동")
bowl1[94, 2:3] <- list("마포구", "상암동")
bowl1[104, 2:3] <- list("관악구", "신원동")

welfare %<>% left_join(bowl1, by = c("소재지" = "address")) %>% unique()

welfare_op <-  welfare %>% group_by(dong) %>% 
  summarise(n = n())

write.csv(welfare_op, "고위기시설수.csv", row.names = F)


#### 기존 데이터와 합치기

welfare <- fread("고위기시설수.csv")
replace_op <- fread("dong_accessibility.csv", encoding = "UTF-8")

#### 기존데이터와 겹치는지 확인!

shelter <- fread("여성가족부_아동청소년보호기관정보_20200421.csv")
n_welfare <- fread("서울특별시 사회복지시설(아동복지시설) 목록.csv")

sum(lifefam$시설명칭 %in% shelter$아동청소년보호기관명)
n_welfare[n_welfare$시설명 %in% shelter$아동청소년보호기관명,] #368개 겹침
sum(welfare$시설명 %in% shelter$아동청소년보호기관명)
sum(wetak$시설명 %in% shelter$아아동청소년보호기관명)
shelter[shelter$아동청소년보호기관명 %in% n_welfare$시설명,]


#############################
### 경찰서, 파출소 데이터 ###
#############################

address<-read.csv('경찰청_경찰관서 위치, 주소_20200409.csv')

# ----- 서울청 소속의 행만 추출
address=address %>%
  filter(청 == '서울청')

# ----- 시군구 변수 생성 (주소에서 시군구 정보 추출)
address=address%>%
  mutate(구 = case_when(
    str_detect(주소, '종로구')~'종로구',
    str_detect(주소, '중구')~'중구',
    str_detect(주소, '용산구')~'용산구',
    str_detect(주소, '성동구')~'성동구',
    str_detect(주소, '광진구')~'광진구',
    str_detect(주소, '동대문구')~'동대문구',
    str_detect(주소, '중랑구')~'중랑구',
    str_detect(주소, '성북구')~'성북구',
    str_detect(주소, '강북구')~'강북구',
    str_detect(주소, '도봉구')~'도봉구',
    str_detect(주소, '노원구')~'노원구',
    str_detect(주소, '은평구')~'은평구',
    str_detect(주소, '서대문구')~'서대문구',
    str_detect(주소, '마포구')~'마포구',
    str_detect(주소, '양천구')~'양천구',
    str_detect(주소, '강서구')~'강서구',
    str_detect(주소, '구로구')~'구로구',
    str_detect(주소, '금천구')~'금천구',
    str_detect(주소, '영등포구')~'영등포구',
    str_detect(주소, '동작구')~'동작구',
    str_detect(주소, '관악구')~'관악구',
    str_detect(주소, '서초구')~'서초구',
    str_detect(주소, '강남구')~'강남구',
    str_detect(주소, '송파구')~'송파구',
    str_detect(주소, '강동구')~'강동구'
  ))

# ----- 데이터의 NA 확인
colSums(is.na(address)) %>% as.data.frame()

# ----- 주소 JOIN 시키기
names(bowl)[names(bowl)=='address']<-'주소'
address=inner_join(address,bowl,'주소')

# ----- 행정동명 NA값 손크롤링으로 채우기 & 신사동 행정동명 수정
address$dong[53]<-'구의2동'
address$dong[76]<-'독산4동'
address$dong[155]<-'독산4동'
address$dong[157]<-'독산4동'

address$dong[6]<-'신사동_강'
address$dong[51]<-'신사동_관'

# ----- 불필요한 column 제거
address = address %>% select(-c(청, X좌표,Y좌표,long,lat))

# ----- 중복된 행 제거
address= unique(address[,c('서','지구대파출소','주소','구','dong')]) 

# ----- 이상한 열, 행 제거
unique(address$서)
address=address%>%filter(서!='서울청')
unique(address$dong)
unique(address$지구대파출소)#파출소가 아닌 경찰서가 적힌 것 있음 경찰서의 주소인가봄!!
address_ps=address%>%filter(!str_detect(지구대파출소,'경찰서'))
unique(address_ps$지구대파출소)
#사실 구의 경찰청과 경찰서의 개수는 기존의 것에 정리되어있음(경찰청은 2곳임따라서 유의미하지 않을 것!)

# ---------- 구별로 정리 ---------- #

# 경찰서
gu_police_s = address %>% 
  mutate(gu = 구) %>%
  select(c(gu,서)) %>%
  group_by(gu) %>%
  summarize(서 = n_distinct(서)) %>% as.data.frame()

# 문제점이 있긴 함 왜냐하면 세종로 파출소가 중구인데 종로경찰서 관할이라 종로구에도 적히는 현상 생김(문제 생기는 현상 빝의 chunk에서 확인할 수 있음 179번째줄!) 그래서 3을 2로!! 바꿔주기!!

gu_police_s$서[24]<-2

write_csv(gu_police_s,'gu_police_s.csv')


# 지구대파출소
gu_ps = address_ps %>% 
  mutate(gu = 구) %>%
  select(c(지구대파출소,gu)) %>%
  group_by(gu) %>%
  summarize(지구대파출소 = n_distinct(지구대파출소)) %>% as.data.frame()

write_csv(gu_ps,'gu_ps.csv')

# ---------- 행정동별로 정리 ---------- #

# 경찰서
dong_police_s = address %>% 
  mutate(dong = dong) %>%
  select(c(서,dong)) %>%
  group_by(dong) %>%
  summarize(서 = n_distinct(서)) %>% as.data.frame()

write_csv(dong_police_s,'dong_police_s.csv')## 일단 동별 서를 했는데 의미가 없어보임...그래서 지구대 파출소~!

# 지구대파출소
dong_ps = address_ps %>% 
  mutate(dong = dong) %>%
  select(c(지구대파출소,dong)) %>%
  group_by(dong) %>%
  summarize(지구대파출소 = n_distinct(지구대파출소)) %>% as.data.frame()##하지만 지구대 파출소도...ㅠㅠㅠ 아근데 총 행정동이랑 join(다른 분거랑)하면 달라질수도!

write_csv(dong_ps,'dong_ps.csv')

############################
### 가정폭력 건수 데이터 ###
############################

family<-read.csv('경찰청 서울특별시지방경찰청_경찰관서별 가정폭력 검거 현황_20191231.csv')

# --- NA 확인
colSums(is.na(family)) %>% as.data.frame()

# --- 불필요한 column제거
family = family %>% select(-c(검거인원,구속,불구속))##검거 인원이 아닌 검거 건수가 더 중요하다고 생각, 구속 여부는 아동보호전문기관 입지 설정에 필요 없다고 생각

# --- 위의 데이터 이용해서 서별 구 찾기!
gu_seo = address %>% 
  mutate(seo = 서) %>%
  select(c(seo,구)) %>%
  group_by(seo)%>%
  summarize(구 = n_distinct(구))%>% as.data.frame()

address%>%filter(서=='서울종로경찰서')## 신기한 것: 중구에 있는 세종로파출소가 종로경찰서 관할임https://www.smpa.go.kr/user/nd9096.do

family[,"gu"]<-c('중구','종로구','중구','서대문구','종로구','용산구','성북구','동대문구','마포구','영등포구','성동구','동작구','광진구','은평구','강북구','금천구','중랑구','강남구','관악구','강서구','강동구','성북구','구로구','서초구','양천구','송파구','노원구','서초구','은평구','도봉구','강남구')
#서초구: 방배경찰서, 서초
#성북구:성북, 종암, 은평구: 서부, 은평중구:중부,남대문#*세종로파출소는 예외#앞에서 중구:3으로나오는데 그냥 놔둠! 그쪽 관할도 하니까!**** 괜찮을까...

# ---------- 구별로 정리 ---------- #
gu_family = family %>% 
  mutate(gu = gu) %>%
  select(c(검거건수,gu)) %>%
  group_by(gu) %>%
  summarize(검거건수 = sum(검거건수)) %>% as.data.frame()

write_csv(gu_family,'gu_family.csv')

#############################
### 서울시 5대범죄 데이터 ###
#############################

crime<-readxl::read_excel('서울시범죄1.xlsx')
crime<-crime[c(2:27),] #1번 행 삭제 변수명 뒤의 숫자가 홀수이면 발생, 짝수이면 검거임 

# --- 데이터 형태 수치형으로 변환
for (i in 3:14){
  crime[[i]]<-as.numeric(crime[[i]])
}

# --- 검거율 변수 추가 (발생건수가 검거건수보다 중요하다고 생각되며 검거율이 유의미할 것이라고 예상)

for (k in 1:26){
  crime[k,4]<-round(crime[k,4]/crime[k,3]*100,2)
}
for (k in 1:26){
  crime[k,6]<-round(crime[k,6]/crime[k,5]*100,2)
}
for (k in 1:26){
  crime[k,8]<-round(crime[k,8]/crime[k,7]*100,2)
}
for (k in 1:26){
  crime[k,10]<-round(crime[k,10]/crime[k,9]*100,2)
}
for (k in 1:26){
  crime[k,12]<-round(crime[k,12]/crime[k,11]*100,2)
}
for (k in 1:26){
  crime[k,14]<-round(crime[k,14]/crime[k,13]*100,2)
}

# ----- 불필요한 column제거
crime=crime%>%select(-기간)

# ----- 변수명 바꾸기
names(crime) <- c("gu","사건 합계","평균 검거율","살인사건 발생","살인사건 검거율","강도사건 발생","강도사건 검거율","강간강제추행사건 발생","강간강제추행사건 검거율", "절도사건 발생","절도사건 검거율","폭력사건 발생","폭력사건 검거율")

#문제 발생: 2019년 이전의 사건이 2019년에 검거되는 경우가 있어 검거율이 100이 넘어가는 경우가 발생

write_csv(crime,'gu_crime.csv')


# ----- 총 범죄 건수 계산
gu_crimes = gu_police_s %>% full_join(gu_ps,by = 'gu') %>% full_join(gu_family,by = 'gu')%>% full_join(crime,by = 'gu')

for (i in 2:4){
  gu_crimes[26,i]<-sum(gu_crimes[1:25,i])
}

dong_crimes = dong_police_s %>% full_join(dong_ps,by = 'dong')

write_csv(gu_crimes,'gu_crimes_total.csv')
write_csv(dong_crimes,'dong_crimes_total.csv')

#################################
### 서울시 장애인 현황 데이터 ###
#################################

### 행정동별 장애인 아동 데이터
data_dong <- readxl::read_excel(path = "handicapped1.xlsx",
                                sheet = "Sheet1", 
                                col_names=TRUE)

# ----- NA 확인
colSums(is.na(data_dong))
data_dong <- as.data.frame(data_dong)

# ----- 필요한 변수 선택
data_dong <- data_dong[,1:12]
data_dong <- data_dong[,-c(1,4,5,6)]

# ----- 필요없는 행 제거 (기타 혹은 소계 관련 행)
data_dong <- data_dong[-c(1,2,3),-1]
data_dong <- data_dong %>% filter(!(동 %in% c("종로구","중구","용산구","성동구","광진구","동대문구","중랑구",
                                             "성북구","강북구","도봉구","노원구","은평구","마포구","양천구",
                                             "강서구","구로구","금천구","동작구","관악구","서초구","강남구",
                                             "강동구","서대문구","영등포구","송파구")))
data_dong <- data_dong %>% filter(!(동=="소계" | 동=="기타"))

# ----- 데이터 형태 수치형으로 변환 
data_dong[,2:7] <- lapply(data_dong[,2:7],as.numeric)

# ----- 행정동별 장애인 아동 수 총합 변수 생성
data_dong <- data_dong %>% mutate(동별장애아동수=rowSums(data_dong[,2:6]))

# ----- 데이터 중복 확인
unique(data_dong[,1:7]) %>% nrow()

# ------- 행정동별 장애인 아동수 총합 ------- #
data_dong <-data_dong %>% select("동","동별장애아동수")
data_dong
write_csv(data_dong,'data_dong.csv')


### 행정구별 장애인 아동수 총합
data_gu <- readxl::read_excel(path = "handicapped1.xlsx",
                              sheet = "Sheet1", 
                              col_names=TRUE)
data_gu <- as.data.frame(data_gu)

# ----- 필요한 변수만 추출
data_gu <- data_gu[,1:12]
data_gu <- data_gu[,-c(1,4,5,6)]

# ----- 필요한 행만 추출 (자치구별 장애아동수 소계)
data_gu <- data_gu %>% filter(동 == '소계')

# ----- 데이터 수치형으로 바꿔준 후 구별 장애인 아동수 총합 계산
data_gu[,3:8] <- lapply(data_gu[,3:8],as.numeric)
data_gu <- data_gu %>% mutate(구별장애아동수=rowSums(data_gu[,3:8]))

data_gu <- data_gu %>% select("자치구","구별장애아동수")
write_csv(data_gu,'data_gu.csv')


#################################
### 서울시 아동 인구수 데이터 ###
#################################

### 행정동별
kids <- readxl::read_excel(path = "kids.xlsx",
                           sheet = "Sheet1", 
                           col_names=TRUE)

# ----- NA값 확인
anyNA(kids)
colSums(is.na(kids))

# ----- 중복된 행이 있는지 확인하기
unique(kids[,c('0세','1세','2세','3세','4세','5세','6세','7세','8세','9세','10세','11세','12세','13세',
               '14세','15세','16세','17세','18세')])%>% nrow()

# ----- 필요한 행만 추출
kids <- kids %>% filter(성별=="계") # 성별이 남,여, 계로 나눠있어서 우리가 필요한 총합인 "계"만 추출
kids<-kids[-1,] #서울특별시 행은 필요없으니 삭제
kids_dong <- kids %>% filter(!(동 %in% c("종로구","중구","용산구","성동구","광진구","동대문구","중랑구",
                                        "성북구","강북구","도봉구","노원구","은평구","마포구","양천구",
                                        "강서구","구로구","금천구","동작구","관악구","서초구","강남구",
                                        "강동구","서대문구","영등포구","송파구"))) # 구와 행정동이 섞여있으므로 행정구 행을 지워줘야함

# ----- 필요없는 열 제거하기(성별)
kids <- kids %>% select(-2)

# ----- 만 0 ~ 18세 총 아동수 변수 생성: 행정동에서 만 0세부터 18세까지 아동 수 총합 열을 생성해주기
kids_dong <- kids_dong %>% mutate(동별아동수=rowSums(kids_dong[,2:19]))

write.csv(kids_dong, "kids_dong.csv")


### 행정구별
kids_gu <- readxl::read_excel(path = "kids.xlsx",
                              sheet = "Sheet1", 
                              col_names=TRUE)

# --- 필요한 행만 추출
kids_gu <- kids %>% filter(동 %in% c("종로구","중구","용산구","성동구","광진구","동대문구","중랑구",
                                    "성북구","강북구","도봉구","노원구","은평구","마포구","양천구",
                                    "강서구","구로구","금천구","동작구","관악구","서초구","강남구",
                                    "강동구","서대문구","영등포구","송파구"))
kids_gu <- kids_gu %>% filter(성별=="계") # 각 행정구당 남, 여, 계 3개씩 있으므로 우리가 필요한 "계"에 해당하는 행만 출력

# --- 행정구별인데 열이름이 "동"으로 돼있어서 "자치구"로 변경
colnames(kids_gu)[1]<-"자치구"

# --- 변수 선택: 필요없는 열인 "성별" 삭제
kids_gu <- kids_gu %>% select(-2)

# --- 변수 생성: 만 0세부터 18세까지 아동 수의 총합 열 생성해주기
kids_gu <- kids_gu %>% mutate(구별아동수=rowSums(kids_gu[,2:19]))

write.csv(kids_gu,"kids_gu.csv")

# ----- JOIN 하기
# join하는데 있어서 0세-18세 아동수가 있으면 보기 힘들어서 join은 전체 아동 수만으로 했음!
sumkids_gu <- kids_gu %>% select("자치구","구별아동수")
sumkids_dong <- kids_dong %>% select("동","동별아동수")

dong_family = full_join(data_dong, sumkids_dong, by="동")
gu_family = full_join(data_gu, sumkids_gu, by="자치구")

write.csv(dong_family, "dong_family.csv")
write.csv(gu_family,"gu_family.csv")

########################
### 재혼 가정 데이터 ###
########################

rm = fread('재혼가정.csv', data.table=F) 

# --- 필요없는 행 제거
rm = rm[,-1]

# --- 행과 열 이름 설정 & 필요없는 열 제거
rownames(rm) = c('연도','합계','종로구','중구','용산구','성동구','광진구','동대문구','중랑구','성북구','강북구','도봉구','노원구','은평구','서대문구','마포구','양천구','강서구','구로구','금천구','영등포구','동작구','관악구','서초구','강남구','송파구','강동구')
colnames(rm) = c('2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')
rm = rm[-1,]

# --- 결측치_확인
colSums(is.na(rm))

# --- 쓰지않을_데이터_삭제
rm = rm[-1,]

# --- 년도 맞춰주기
rm = rm[,-c(1:9, 11)]
rm = as.data.frame(rm)

# --- 열과 행 이름 재설정
colnames(rm) = c('gu_remarriange')
rownames(rm) = c('종로구','중구','용산구','성동구','광진구','동대문구','중랑구','성북구','강북구','도봉구','노원구','은평구','서대문구','마포구','양천구','강서구','구로구','금천구','영등포구','동작구','관악구','서초구','강남구','송파구','강동구')


################################
### 미성년가족_출산율 데이터 ###
################################

yp = fread('출산율.csv', data.table=F) 

# --- 행과 열에서 쓰지않는 데이터 제거
yp = yp[,-1]
yp = yp[-c(1,2),]

# --- 행과 열 이름 설정
colnames(yp) = c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')
rownames(yp) = c('서울특별시', '종로구','중구','용산구','성동구','광진구','동대문구','중랑구','성북구','강북구','도봉구','노원구','은평구','서대문구','마포구','양천구','강서구','구로구','금천구','영등포구','동작구','관악구','서초구','강남구','송파구','강동구')

# --- 결측치_확인
colSums(is.na(yp))

# --- 쓰지않을 데이터 삭제
yp = yp[-1,]

# --- 년도 맞춰주기
yp = yp[,-c(1:14)]

# --- 열과 행 이름 재설정
yp = as.data.frame(yp)
colnames(yp) = c('gu_young')
rownames(yp) = c('종로구','중구','용산구','성동구','광진구','동대문구','중랑구','성북구','강북구','도봉구','노원구','은평구','서대문구','마포구','양천구','강서구','구로구','금천구','영등포구','동작구','관악구','서초구','강남구','송파구','강동구')


#########################
### 한부모가정 데이터 ###
#########################
sp = fread('한 부모 가정.csv', data.table=F)

# --- 필요없는 데이터 제거
sp = sp[,-1]
sp = sp[-1,]

# --- 행과 열의 이름 설정
colnames(sp) = c('2015','2016','2017','2018','2019')
rownames(sp) = c('서울특별시', '종로구','중구','용산구','성동구','광진구','동대문구','중랑구','성북구','강북구','도봉구','노원구','은평구','서대문구','마포구','양천구','강서구','구로구','금천구','영등포구','동작구','관악구','서초구','강남구','송파구','강동구')

# --- 결측치_확인
colSums(is.na(sp))

# --- 쓰지않을 데이터 삭제
sp = sp[-1,]
sp = sp[,-c(1:4)]
sp = as.data.frame(sp)

# --- 행과 열 이름 재설정
colnames(sp) = c('gu_single')
rownames(sp) = c('종로구','중구','용산구','성동구','광진구','동대문구','중랑구','성북구','강북구','도봉구','노원구','은평구','서대문구','마포구','양천구','강서구','구로구','금천구','영등포구','동작구','관악구','서초구','강남구','송파구','강동구')


write.csv(rm, 'gu_remarriage_complete.csv')
write.csv(yp, 'gu_young_complete.csv')
write.csv(sp, 'gu_single_complete.csv')


############################
### 구별 이혼가구 데이터 ###
############################

gu_d = fread('구별 이혼.csv', data.table = F)

# --- 필요하지 않은 데이터 삭제
gu_d = gu_d[-1,]

# --- 열 이름 설정
colnames(gu_d) = c('구','가정 수')

# --- 데이터 차원과 결측치 확인
dim(gu_d)
colSums(is.na(gu_d))

############################
### 동별 이혼가구 데이터 ###
############################

dong_d = fread('동별 이혼.csv', data.table = T)

# --- 데이터 차원과 결측값 확인
dim(dong_d)
colSums(is.na(dong_d))

write.csv(gu_d,'gu_divorce.csv')
write.csv(dong_d, 'dong_divorce.csv')

#############################
### 기초생활수급자 데이터 ###
############################# 

basic <- readxl::read_excel("서울시 국민기초생활보장 수급자 (동별) 통계.xlsx", sheet = 1, col_names = TRUE,
                            na = '-')
pop_gu <- readxl::read_excel("2020년 주민등록인구.xlsx", sheet = 1, col_names = TRUE)
pop_dong <- readxl::read_excel("2020 행정동별 주민등록인구.xlsx", sheet = 1, col_names = TRUE)

basic[is.na(basic$`총 수급자(가구)`),"총 수급자(가구)"] <- 0

basic_gu <-  basic %>% select(자치구, 동, `총 수급자(가구)`, `총 수급자(인원)`) %>% 
  filter(자치구 != '합계', 자치구 != '본청', 동 != '소계') %>% 
  group_by(자치구) %>% 
  summarise(bene_gagu = mean(`총 수급자(가구)`), bene_people = mean(`총 수급자(인원)`))

basic_dong <-  basic %>% select(자치구, 동, `총 수급자(가구)`, `총 수급자(인원)`) %>% 
  filter(자치구 != '합계', 자치구 != '본청', 동 != '소계') %>% 
  group_by(자치구,동) %>% 
  summarise(bene_gagu = mean(`총 수급자(가구)`), bene_people = mean(`총 수급자(인원)`))

pop_gu %<>% select(행정구, 세대, `내국인(계)`) %>% 
  filter(행정구 != "서울시")

basic_gu %<>% left_join(pop_gu, by = c("자치구" = "행정구")) %>% 
  rename(행정구 = 자치구) %>% 
  group_by(행정구) %>% 
  summarise(bene_gagu = bene_gagu,
            bene_people = bene_people,
            bene_gagu_rate = bene_gagu/세대 * 100, 
            bene_people_rate = bene_people/`내국인(계)` * 100)

############################
### 주민등록 인구 데이터 ###
############################

pop_dong %<>% select(행정동, 세대, `내국인(계)`)
pop_dong[pop_dong$행정동 == "신사동", '행정동'] <- "신사동_관"
pop_dong[pop_dong$행정동 == "신사동2", "행정동"] <- "신사동_강"
pop_dong %<>% mutate(행정동 = str_replace(행정동, "(\\w+)([제])(\\d)([동])", "\\1\\3\\4"))
pop_dong[pop_dong$행정동 == "면목제3.8동", "행정동"] <- "면목3.8동"

basic_dong[basic_dong$자치구 == "강남구" & basic_dong$동 == "신사동", "동"] <- "신사동_강"
basic_dong[basic_dong$자치구 == "관악구" & basic_dong$동 == "신사동", "동"] <- "신사동_관"
basic_dong[basic_dong$동 == "종로5·6가동", "동"] <- "종로5.6가동"
basic_dong %<>% filter(동 != "기타")

basic_dong %<>% left_join(pop_dong, by = c("동" = "행정동")) %>% 
  rename(행정동 = 동) %>% 
  group_by(행정동) %>% 
  summarise(bene_gagu = bene_gagu,
            bene_people = bene_people,
            bene_gagu_rate = bene_gagu/세대 * 100,
            bene_people_rate = bene_people/`내국인(계)` * 100)

basic_dong %<>% rbind(c(행정동 = "둔촌1동", bene_gagu = 0, bene_people = 0, bene_gagu_rate = 0, bene_people_rate = 0))

write.csv(basic_gu, "./basic_gu.csv", row.names = F)
write.csv(basic_dong, "./basic_dong.csv", row.names = F)

############################
#### 부동산가격 데이터 #####
############################

apt <- fread("아파트(매매)__실거래가.csv")

apt %<>% select(시군구, 단지명, `전용면적(㎡)`, `거래금액(만원)`, 도로명) %>% 
  mutate(`거래금액(만원)` = as.numeric(str_replace(`거래금액(만원)`, ",", "")),
         단위가격 = `거래금액(만원)`/`전용면적(㎡)`,
         행정구 = str_replace(시군구, "(\\w+[시]) (\\w+[구])(.*) (.+)", "\\2"),
         도로명 = paste0(행정구," ", 도로명)) %>% 
  select(-c(시군구))


library(httr) # GET()
library(jsonlite) # fromJSON()

# 검색할 주소 목록

address_list1 <- address_list %>% unique()

# Web API key

KAKAO_MAP_API_KEY = "KAKAOKEY"

# 결과 저장용 데이터프레임

bowl2 = data.frame(도로명 = address_list1,
                      행정구 = rep(NA, length(address_list1)),
                      행정동 = rep(NA, length(address_list1)))

# 주소별 반복
for(i in 1:length(address_list1)){
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = address_list1[i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  
  coord <- res %>% content(as = 'text') %>% fromJSON()
  gu <- coord$documents$address$region_2depth_name
  dong <- coord$documents$address$region_3depth_h_name
  
  if (length(dong) > 0) {
    bowl2$행정구[i] <- gu
    bowl2$행정동[i] <- dong
  } else {
    bowl2$행정구[i] <- NA
    bowl2$행정동[i] <- NA
  }
}


# 손크롤링 시작^^! 271개밖에 안된다... 나는 할 수 있다...
bowl2[bowl2$도로명 == "강남구 개포로 310", "행정동"] <- "개포1동"
bowl2[bowl2$도로명 == "강남구 도곡로1길 22", "행정동"] <- "역삼1동"
bowl2[bowl2$도로명 == "강서구 곰달래로18길 33-1", "행정동"] <- "화곡1동"
bowl2[bowl2$도로명 == "강서구 화곡로21길 71-1", "행정동"] <- "화곡3동"
bowl2[bowl2$도로명 == "강서구 월정로20길 82-8	", "행정동"] <- "화곡1동"
bowl2[bowl2$도로명 == "광진구 뚝섬로 45", "행정동"] <- "자양3동"
bowl2[bowl2$도로명 == "구로구 구로중앙로18길 11-1", "행정동"] <- "구로4동"
bowl2[bowl2$도로명 == "금천구 시흥대로 291-1", "행정동"] <- "독산1동"
bowl1[bowl2$도로명 == "동대문구 한천로24길 74-11", "행정동"] <- "장안1동"
bowl2[bowl2$도로명 == "동작구 사당로23길 4", "행정동"] <- "사당3동"
bowl2[bowl2$도로명 == "마포구  18", "행정동"] <- "공덕동"
bowl2[bowl2$도로명 == "서초구 신반포로19길 10", "행정동"] <- "반포2동"
bowl2[bowl2$도로명 == "성북구  37", "행정동"] <- "보문동"
bowl2[bowl2$도로명 == "성북구  64", "행정동"] <- "안암동"
bowl2[bowl2$도로명 == "성북구  38", "행정동"] <- "안암동"
bowl2[bowl2$도로명 == "송파구 올림픽로33길 17", "행정동"] <- "잠실4동"
bowl2[bowl2$도로명 == "송파구 올림픽로 399", "행정동"] <- "잠실4동"
bowl2[bowl2$도로명 == "송파구 동남로23가길 8-27", "행정동"] <- "오금동"
bowl2[bowl2$도로명 == "은평구 서오릉로2길 12-1", "행정동"] <- "녹번동"
bowl2[bowl2$도로명 == "은평구 연서로28길 12-1", "행정동"] <- "대조동"
bowl2[bowl2$도로명 == "은평구 통일로65길 3-1", "행정동"] <- "대조동"
bowl2[bowl2$도로명 == "은평구 연서로28길 8-1", "행정동"] <- "대조동"
bowl2[bowl2$도로명 == "은평구 서오릉로 107-1", "행정동"] <- "역촌동"
bowl2[bowl2$도로명 == "종로구 평창문화로 171", "행정동"] <- "평창동"
bowl2[bowl2$도로명 == "은평구 은평로 116-1", "행정동"] <- "응암1동"
bowl2[bowl2$도로명 == "종로구 세검정로 384-13", "행정동"] <- "평창동"


apt %<>% left_join(bowl2, by = c("도로명" = "도로명")) %>% 
  select(-c(행정구.y)) %>% 
  drop_na() %>% # 손크롤링으로도 명확히 알기 어려운 도로명 2개에 대한 데이터 20개 삭제
  rename(행정구 = 행정구.x)

apt %<>% group_by(행정구, 행정동) %>% 
  summarise(평균단위가격 = mean(단위가격), 아파트매매수 = n()) %>%
  filter(행정동 != "")

apt[apt$행정구 == "강남구" & apt$행정동 == "신사동", "행정동"] <- "신사동_강"
apt[apt$행정구 == "관악구" & apt$행정동 == "신사동", "행정동"] <- "신사동_관"

write.csv(apt, "./apt_실거래가.csv", row.names = F)





dandok <- fread("단독다가구(매매)_실거래가.csv")
dandok %<>% select(시군구, `연면적(㎡)`, `거래금액(만원)`, 도로명) %>% 
  mutate(`거래금액(만원)` = as.numeric(str_replace_all(`거래금액(만원)`, ",", "")),
         단위가격 = `거래금액(만원)`/`연면적(㎡)`,
         행정구 = str_replace(시군구, "(\\w+[시]) (\\w+[구])(.*) (.+)", "\\2"),
         도로명 = paste0(행정구," ", 도로명)) %>% 
  select(-c(시군구)) %>% 
  filter(도로명 != "null")  # 도로명없으면 찾기 불가능..!



address_list <- dandok$도로명 %>% unique()

# Web API key

KAKAO_MAP_API_KEY = "KAKAOKEY"

# 결과 저장용 데이터프레임

bowl1 = data.frame(도로명 = address_list,
                      행정동 = rep(NA, length(address_list)))


# 주소별 반복
for(i in 1:length(address_list)){
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = address_list[i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  if (length(coord$documents$address_type[1]) == 0) {
    bowl1$행정동[i] <- NA
    
  } else {
    long <- coord$documents$x[1]
    lat <- coord$documents$y[1]
    
    res1 <- GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
                query = list(x = long, y = lat),
                add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
    
    coord1 <- res1 %>% content(as = 'text') %>% fromJSON()
    
    bowl1$행정동[i] <- coord1$documents$region_3depth_name[2]
  }
}

dandok %<>% left_join(bowl1, by = "도로명") %>% 
  group_by(행정구, 행정동) %>% 
  summarise(평균단위가격 = mean(단위가격), 단독다가구매매수 = n())

dandok[dandok$행정구 == "강남구" & dandok$행정동 == "신사동", "행정동"] <- "신사동_강"
dandok[dandok$행정구 == "관악구" & dandok$행정동 == "신사동", "행정동"] <- "신사동_관"

write.csv(dandok, "./dandok_실거래가.csv", row.names = F)



yeonlib <- fread("연립다세대(매매)__실거래가.csv")
yeonlib %<>% select(시군구, 건물명, `전용면적(㎡)`, `거래금액(만원)`, 도로명) %>% 
  mutate(`거래금액(만원)` = as.numeric(str_replace_all(`거래금액(만원)`, ",", "")),
         단위가격 = `거래금액(만원)`/`전용면적(㎡)`,
         행정구 = str_replace(시군구, "(\\w+[시]) (\\w+[구])(.*) (.+)", "\\2"),
         도로명 = paste0(행정구," ", 도로명),
         도로명 = str_replace(도로명, "(\\w+[구]) (\\w+\\d*[길|로]) (\\d+-\\d+|\\d+)", "\\1 \\2")) %>% 
  select(-c(시군구)) %>% 
  filter(도로명 != "null")  # 도로명없으면 찾기 불가능..!



address_list <- yeonlib$도로명 %>% unique()

# Web API key

KAKAO_MAP_API_KEY = "KAKAOKEY"

# 결과 저장용 데이터프레임

bowl3 = data.frame(도로명 = address_list,
                      행정동 = rep(NA, length(address_list)))


# 주소별 반복
for(i in 1:length(address_list)){
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = address_list[i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  if (length(coord$documents$address_type[1]) == 0) {
    bowl3$행정동[i] <- NA
    
  } else {
    long <- coord$documents$x[1]
    lat <- coord$documents$y[1]
    
    res1 <- GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
                query = list(x = long, y = lat),
                add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
    
    coord1 <- res1 %>% content(as = 'text') %>% fromJSON()
    
    bowl3$행정동[i] <- coord1$documents$region_3depth_name[2]
  }
}

bowl3[bowl3$도로명 == "강서구 양천로47가길", "행정동"] <- "가양1동"
bowl3[bowl3$도로명 == "강서구 공항대로7가길", "행정동"] <- "공항동"
bowl3[bowl3$도로명 == "강서구 공항대로2가길", "행정동"] <- "공항동"
bowl3[bowl3$도로명 == "강서구 공항대로4가길", "행정동"] <- "공항동"
bowl3[bowl3$도로명 == "강서구 방화대로7가길", "행정동"] <- "공항동"
bowl3[bowl3$도로명 == "강서구 등촌로13가길", "행정동"] <- "등존2동"
bowl3[bowl3$도로명 == "강서구 등촌로39가길", "행정동"] <- "등촌2동"
bowl3[bowl3$도로명 == "강서구 양천로30가길", "행정동"] <- "가양1동"
bowl3[bowl3$도로명 == "강서구 방화대로47가길", "행정동"] <- "방화3동"
bowl3[bowl3$도로명 == "강서구 양천로24가길", "행정동"] <- "방화1동"
bowl3[bowl3$도로명 == "강서구 방화대로52가길", "행정동"] <- "방화3동"
bowl3[bowl3$도로명 == "강서구 화곡로42가길", "행정동"] <- "화곡본동"
bowl3[bowl3$도로명 == "강서구 월정로28가길", "행정동"] <- "화곡1동"
bowl3[bowl3$도로명 == "강서구 가로공원로78가길", "행정동"] <- "화곡1동"
bowl3[bowl3$도로명 == "강서구 가로공원로80가길", "행정동"] <- "화곡1동"
bowl3[bowl3$도로명 == "강서구 곰달래로49가길", "행정동"] <- "화곡4동"
bowl3[bowl3$도로명 == "강서구 화곡로13가길", "행정동"] <- "화곡3동"
bowl3[bowl3$도로명 == "강서구 등촌로5가길", "행정동"] <- "화곡4동"
bowl3[bowl3$도로명 == "강서구 화곡로27가길", "행정동"] <- "우장산동"
bowl3[bowl3$도로명 == "강서구 월정로30가길", "행정동"] <- "화곡1동"
bowl3[bowl3$도로명 == "강서구 화곡로44가길", "행정동"] <- "화곡6동"
bowl3[bowl3$도로명 == "강서구 강서로18가길", "행정동"] <- "화곡8동"
bowl3[bowl3$도로명 == "강서구 곰달래로31가길", "행정동"] <- "화곡2동"
bowl3[bowl3$도로명 == "강서구 화곡로31가길", "행정동"] <- "우장산동"
bowl3[bowl3$도로명 == "강서구 까치산로24가길", "행정동"] <- "화곡6동"

yeonlib %<>% left_join(bowl3, by = "도로명") %>% 
  drop_na() %>% # 손크롤링도 실패한 15개 삭제
  group_by(행정구, 행정동) %>% 
  summarise(평균단위가격 = mean(단위가격), 연립다세대매매수 = n())

yeonlib[yeonlib$행정구 == "강남구" & yeonlib$행정동 == "신사동", "행정동"] <- "신사동_강"
yeonlib[yeonlib$행정구 == "관악구" & yeonlib$행정동 == "신사동", "행정동"] <- "신사동_관"

write.csv(yeonlib, "./yeonlib_실거래가.csv", row.names = F)


officetel <- fread("오피스텔(매매)__실거래가.csv")

officetel %<>% select(시군구, 단지명, `전용면적(㎡)`, `거래금액(만원)`, 도로명) %>% 
  mutate(`거래금액(만원)` = as.numeric(str_replace_all(`거래금액(만원)`, ",", "")),
         단위가격 = `거래금액(만원)`/`전용면적(㎡)`,
         행정구 = str_replace(시군구, "(\\w+[시]) (\\w+[구])(.*) (.+)", "\\2"),
         도로명 = paste0(행정구," ", 도로명)) %>% 
  select(-c(시군구)) %>% 
  filter(도로명 != "null")  # 도로명없으면 찾기 불가능..! (1091개)



address_list <- officetel$도로명 %>% unique()

# Web API key

KAKAO_MAP_API_KEY = "KAKAOKEY"

# 결과 저장용 데이터프레임

bowl4 = data.frame(도로명 = address_list,
                      행정동 = rep(NA, length(address_list)))


# 주소별 반복
for(i in 1:length(address_list)){
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = address_list[i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  if (length(coord$documents$address_type[1]) == 0) {
    bowl4$행정동[i] <- NA
    
  } else {
    long <- coord$documents$x[1]
    lat <- coord$documents$y[1]
    
    res1 <- GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
                query = list(x = long, y = lat),
                add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
    
    coord1 <- res1 %>% content(as = 'text') %>% fromJSON()
    
    bowl4$행정동[i] <- coord1$documents$region_3depth_name[2]
  }
}

bowl4[bowl4$도로명 == "강남구 남부순환로 2763", "행정동"] <- "도곡1동"
bowl4[bowl4$도로명 == "강서구 방화동로 18-3", "행정동"] <- "공항동"
bowl4[bowl4$도로명 == "강서구 곰달래로 112", "행정동"] <- "화곡1동"
bowl4[bowl4$도로명 == "광진구 능동로37길 6-1", "행정동"] <- "중곡1동"
bowl4[bowl4$도로명 == "금천구 독산로 353-1", "행정동"] <- "독산3동"
bowl4[bowl4$도로명 == "금천구 시흥대로75길 5", "행정동"] <- "시흥1동"
bowl4[bowl4$도로명 == "마포구 만리재로7길 2-3", "행정동"] <- "공덕동"
bowl4[bowl4$도로명 == "서대문구 이화여대1길 21", "행정동"] <- "신촌동"
bowl4[bowl4$도로명 == "서대문구 동교로 296-42", "행정동"] <- "연희동"
bowl4[bowl4$도로명 == "성북구 정릉로26길 48-3", "행정동"] <- "정릉2동"
bowl4[bowl4$도로명 == "송파구 백제고분로21길 7-1", "행정동"] <- "삼전동"
bowl4[bowl4$도로명 == "은평구 진흥로9길 3-3", "행정동"] <- "역촌동"
bowl4[bowl4$도로명 == "중구 난계로11길 18-4", "행정동"] <- "황학동"


officetel %<>% left_join(bowl4, by = "도로명") %>% 
  group_by(행정구, 행정동) %>% 
  summarise(평균단위가격 = mean(단위가격), 오피스텔매매수 = n())

officetel[officetel$행정구 == "강남구" & officetel$행정동 == "신사동", "행정동"] <- "신사동_강"
officetel[officetel$행정구 == "관악구" & officetel$행정동 == "신사동", "행정동"] <- "신사동_관"

write.csv(officetel, "./officetel_실거래가.csv", row.names = F)


setwd("C:/Users/seses/Desktop/P-SAT/회귀 주제분석/data/회귀분석 변수")

total_gu <- fread("total_gu.csv")

apt_gu <- apt %>% group_by(행정구) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 아파트매매수 = sum(아파트매매수))

dandok_gu <- dandok %>% group_by(행정구) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 단독다가구매매수 = sum(단독다가구매매수))

yeonlib_gu <- yeonlib %>% group_by(행정구) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 연립다세대매매수 = sum(연립다세대매매수))

officetel_gu <- officetel %>% group_by(행정구) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 오피스텔매매수 = sum(오피스텔매매수))

total_gu %<>% left_join(apt_gu, by = c("자치구" = "행정구")) %>% 
  left_join(dandok_gu, by = c("자치구" = "행정구")) %>%
  left_join(yeonlib_gu, by = c("자치구" = "행정구")) %>%
  left_join(officetel_gu, by = c("자치구" = "행정구")) %>% 
  select(-c(평균단위가격.x, 평균단위가격.y, 평균단위가격.x.x, 평균단위가격.y.y)) %>% 
  mutate(전체매매 = (아파트매매수 + 단독다가구매매수 + 연립다세대매매수 + 오피스텔매매수),
             부동산단위가격 = 
               (평균단위가격_apt * 아파트매매수 / 전체매매) + (평균단위가격_yeonlib * 연립다세대매매수 / 전체매매) +
               (평균단위가격_dandok * 단독다가구매매수 / 전체매매) + (평균단위가격_off * 오피스텔매매수 / 전체매매)) %>% 
  select(-c(아파트매매수, 연립다세대매매수, 오피스텔매매수, 단독다가구매매수, 전체매매))



write.csv(total_gu, "./total_gu_부동산합.csv", row.names = F)

price <- total_gu %>% select(평균단위가격_apt, 평균단위가격_off, 평균단위가격_yeonlib, 평균단위가격_dandok)
price_cor <- cor(price)
corrplot(price_cor, addCoef.col="white")


apt_dong <- apt %>% group_by(행정구,행정동) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 아파트매매수 = sum(아파트매매수))

dandok_dong <- dandok %>% group_by(행정구,행정동) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 단독다가구매매수 = sum(단독다가구매매수))

yeonlib_dong <- yeonlib %>% group_by(행정구,행정동) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 연립다세대매매수 = sum(연립다세대매매수))

officetel_dong <- officetel %>% group_by(행정구,행정동) %>% 
  summarise(평균단위가격 = mean(평균단위가격), 오피스텔매매수 = sum(오피스텔매매수))

# --- 동단위 데이터 생성
# 동별 장애아동수, 아동수
dong_family <- fread("dong_family.csv")
dong_family %<>% select(-c(V1))
dong_family

# 구별 이혼가정 수
dong_divorce <- fread("dong_divorce.csv")
dong_divorce %<>% select(-c(V1)) %>% 
  rename(이혼가정수 = `가정수`)
dong_divorce

# 구별 경찰서 개수
dong_crimes_total <- fread("dong_crimes_total.csv", encoding = 'UTF-8')
dong_crimes_total %<>% select(dong, 서, 지구대파출소) %>% 
  filter(dong != '합계')

# 구별 기초생활수급자 수
basic_dong <- fread("basic_dong.csv")
basic_dong %<>% select(행정동, bene_gagu_rate, bene_people_rate)
basic_dong


total_dong <- dong_family %>% left_join(dong_divorce, by = c("동" = "행정동")) %>% 
  left_join(dong_crimes_total, by = c("동" = "dong")) %>% 
  left_join(basic_dong, by = c("동" = "행정동"))

total_dong %<>% rename(행정동 = 동, 장애아동수 = 동별장애아동수, 아동수 = 동별아동수, 경찰서 = 서, 
                          기초수급가구비율 = bene_gagu_rate, 기초수급인구비율 = bene_people_rate)

total_dong %<>% left_join(apt_dong, by = c("행정동")) %>% 
  left_join(dandok_dong, by = c("행정동", "행정구"), suffix = c("_apt", "_dandok")) %>%
  left_join(yeonlib_dong, by = c("행정동", "행정구")) %>%
  left_join(officetel_dong, by = c("행정동","행정구"), suffix = c("_yeonlib", "_off"))

colSums(is.na(total_dong))

total_dong[is.na(total_dong$이혼가정수), "이혼가정수"] <- 0
total_dong[is.na(total_dong$경찰서), "경찰서"] <- 0
total_dong[is.na(total_dong$지구대파출소), "지구대파출소"] <- 0
total_dong[is.na(total_dong$아파트매매수), "아파트매매수"] <- 0
total_dong[is.na(total_dong$단독다가구매매수), "단독다가구매매수"] <- 0
total_dong[is.na(total_dong$연립다세대매매수), "연립다세대매매수"] <- 0
total_dong[is.na(total_dong$오피스텔매매수), "오피스텔매매수"] <- 0

# 자치구 NA 직접 채워줌
total_dong[행정동 == "삼청동", "행정구"] <- "종로구"
total_dong[행정동 == "가회동", "행정구"] <- "종로구"
total_dong[행정동 == "둔촌1동", "행정구"] <- "강동구"

# 소속 자치구 평균대체

APT_gu <-  total_dong %>% group_by(행정구) %>% 
  summarise(mean.apt = mean(평균단위가격_apt, na.rm = T))

for (i in 1:nrow(total_dong)){
  if (is.na(total_dong[i, "평균단위가격_apt"])){
    total_dong[i, "평균단위가격_apt"] <- APT_gu[APT_gu$행정구 == as.character(total_dong[i, "행정구"]),"mean.apt"]
  } else{
    total_dong[i, "평균단위가격_apt"] <- total_dong[i, "평균단위가격_apt"]
  }
}

COA_gu <-  total_dong %>% group_by(행정구) %>% 
  summarise(mean.yeonlib = mean(평균단위가격_yeonlib, na.rm = T))

for (i in 1:nrow(total_dong)){
  if (is.na(total_dong[i, "평균단위가격_yeonlib"])){
    total_dong[i, "평균단위가격_yeonlib"] <- COA_gu[COA_gu$행정구 == as.character(total_dong[i, "행정구"]),"mean.yeonlib"]
  } else{
    total_dong[i, "평균단위가격_yeonlib"] <- total_dong[i, "평균단위가격_yeonlib"]
  }
}

OFF_gu <-  total_dong %>% group_by(행정구) %>% 
  summarise(mean.off = mean(평균단위가격_off, na.rm = T))

for (i in 1:nrow(total_dong)){
  if (is.na(total_dong[i, "평균단위가격_off"])){
    total_dong[i, "평균단위가격_off"] <- OFF_gu[OFF_gu$행정구 == as.character(total_dong[i, "행정구"]),"mean.off"]
  } else{
    total_dong[i, "평균단위가격_off"] <- total_dong[i, "평균단위가격_off"]
  }
}

DAN_gu <-  total_dong %>% group_by(행정구) %>% 
  summarise(mean.dan = mean(평균단위가격_dandok, na.rm = T))

for (i in 1:nrow(total_dong)){
  if (is.na(total_dong[i, "평균단위가격_dandok"])){
    total_dong[i, "평균단위가격_dandok"] <- DAN_gu[DAN_gu$행정구 == as.character(total_dong[i, "행정구"]),"mean.dan"]
  } else{
    total_dong[i, "평균단위가격_dandok"] <- total_dong[i, "평균단위가격_dandok"]
  }
}

colSums(is.na(total_dong))


total_dong %<>%  mutate(전체매매 = (아파트매매수 + 단독다가구매매수 + 연립다세대매매수 + 오피스텔매매수),
                            부동산단위가격 = 
                              (평균단위가격_apt * 아파트매매수 / 전체매매) + (평균단위가격_yeonlib * 연립다세대매매수 / 전체매매) +
                              (평균단위가격_dandok * 단독다가구매매수 / 전체매매) + (평균단위가격_off * 오피스텔매매수 / 전체매매)) %>% 
  select(-c(아파트매매수, 연립다세대매매수, 오피스텔매매수, 단독다가구매매수, 전체매매, 평균단위가격_apt, 평균단위가격_dandok, 평균단위가격_yeonlib, 평균단위가격_off))

total_dong[is.na(total_dong$부동산단위가격), "부동산단위가격"] <- 0


write.csv(total_dong, "./total_dong.csv", row.names = F)


############################
####### CCTV 데이터 ########
############################

############################################################################
######################## CCTV 데이터 불러오기 ##############################
############################################################################

cctv <- readxl::read_excel("12_04_08_E_CCTV정보.xlsx")

cctv %<>% select(소재지도로명주소, 소재지지번주소, 설치목적구분, 카메라대수, 위도, 경도) %>% 
  filter(설치목적구분 == "생활방범" | 설치목적구분 == "기타" | 설치목적구분 == "어린이보호" | 
                 설치목적구분 == "다목적")

cctv %<>% mutate(카메라대수 = as.numeric(카메라대수), 위도 = as.numeric(위도), 경도 = as.numeric(경도))


############################################################################
##################### 위도,경도 이용해 카카오 API ##########################
############################################################################

library(httr) # GET()
library(jsonlite) # fromJSON()
library(progress)

KAKAO_MAP_API_KEY <- "KAKAOKEY"

address_list1 = list(cctv$경도, cctv$위도)

bowl1 <- data.frame(경도 = address_list1[[1]], 위도 = address_list1[[2]],
                      gu = rep(NA, length(address_list1[[1]])),
                      dong = rep(NA, length(address_list1[[1]])))


pb <- progress_bar$new(total = length(address_list1[[1]]))

for (i in 1:length(address_list1[[1]])){
  pb$tick()
  Sys.sleep(0.01)
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
             query = list(x = address_list1[[1]][i], y = address_list1[[2]][i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  dong <- coord$documents$region_3depth_name[2]
  gu <- coord$documents$region_2depth_name[2]
  
  if (length(dong) > 0){
    bowl1$gu[i] <- gu
    bowl1$dong[i] <- dong
  } else {
    bowl1$gu[i] <- NA
    bowl1$dong[i] <- NA
  }
}

bowl1[32396, "gu"] <- "성북구"
bowl1[32396, "dong"] <- "성북동"

bowl1 %<>% select(-c(위도, 경도))

cctv %<>% cbind(bowl1)

cctv$gu %>% unique()
cctv$dong %>% unique()

#### 나머지 cctv도 돌려보기
cctv1 <- readxl::read_excel("12_04_08_E_CCTV정보.xlsx")


cctv1 %<>% select(소재지도로명주소, 소재지지번주소, 설치목적구분, 카메라대수, 위도, 경도) %>% 
  filter(설치목적구분 != "생활방범" & 설치목적구분 != "기타" & 설치목적구분 != "어린이보호" & 
                 설치목적구분 != "다목적")


address_list2 = list(cctv1$경도, cctv1$위도)

bowl2 <- data.frame(경도 = address_list2[[1]], 위도 = address_list2[[2]],
                      gu = rep(NA, length(address_list2[[1]])),
                      dong = rep(NA, length(address_list2[[1]])))


pb <- progress_bar$new(total = length(address_list2[[1]]))

for (i in 1:length(address_list2[[1]])){
  pb$tick()
  Sys.sleep(0.01)
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
             query = list(x = address_list2[[1]][i], y = address_list2[[2]][i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  dong <- coord$documents$region_3depth_name[2]
  gu <- coord$documents$region_2depth_name[2]
  
  if (length(dong) > 0){
    bowl2$gu[i] <- gu
    bowl2$dong[i] <- dong
  } else {
    bowl2$gu[i] <- NA
    bowl2$dong[i] <- NA
  }
}


bowl2 %<>% select(-c(위도, 경도))

cctv1 %<>% cbind(bowl2)

cctv %<>% rbind(cctv1)

############################################################################
######################### 데이터확인 후 수정 ###############################
############################################################################

cctv$gu %>% unique()

cctv %<>% filter(gu != "음성군", gu != "횡성군", gu != "용인시 수지구", gu != "광명시", gu != "과천시",
                 gu != "고양시 덕양구", gu != "양평군", gu != "화성시", gu != "구리시", gu != "의정부시")

cctv$gu %>% unique()

############################################################################
########################## 자치구 NA 채우기 ################################
############################################################################

cctv %>% group_by(gu) %>% 
  summarise(n = n()) %>% view()

cctv[cctv$gu == "",]$소재지지번주소 %>% unique()

## 1
cctv %<>% 
  mutate(gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 산5-1", '구로구'))

## 3-20
cctv %<>% 
  mutate(gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 천왕동 278-1", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 천왕동 13-66", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 195-7", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 621-8", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 오류동 248", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 331-1", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 오류동 347-1", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 79-1", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 314-61", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 66-30", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 3-46", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 52-6", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 410-1", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 산6-1", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 온수동 155-4", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 614-178", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 341-2", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 342-1", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 궁동 155-3", '구로구'))

## 21-40
cctv %<>% 
  mutate(gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 45-2", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 120-5", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 614-168", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 오류동 159-4", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 47", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 온수동 118-5", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 62", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 314-14", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 46-14", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 오류동 14-471", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 궁동 190-130", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 온수동 93-14", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 오류동 135-69", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 689-7", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 224-2", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 궁동 40-3", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 458-4", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 고척동 57-22", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 궁동 279-6", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 271-17", '구로구'))

## 41-60
cctv %<>% 
  mutate(gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 501-22", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 온수동 9-31", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 64-21", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 1217", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 120-25", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 오류동 18-98", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 263-23", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 가리봉동 13-21", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 오류동 16-4", '구로구'),
         gu = replace(gu, 소재지지번주소 == "다산동 399-33", '중구'),
         gu = replace(gu, 소재지지번주소 == "청구동 843", '중구'),
         gu = replace(gu, 소재지지번주소 == "만리동2가 6-1", '중구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 186-21", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 189-6", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 90-22", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 125-91", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 신도림동 285-34", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 810-2", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 1125-15", '구로구'),
         gu = replace(gu, 소재지지번주소 == "만리동1가 62-12", '중구'))

## 61-73
cctv %<>% 
  mutate(gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 621-10", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 천왕동 3-2", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 천왕동 3-3", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 701", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 603-46", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 218-8", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 631", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 683-11", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 구로동 124", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 신도림동 271-62", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 천왕동 257-6", '구로구'),
         gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 87-3", '구로구'))

## 2
cctv %<>% 
  mutate(gu = replace(gu, 소재지지번주소 == "서울특별시 구로구 개봉동 298-75", '구로구'),
         gu = replace(gu, 소재지도로명주소 == "청구로1길 23", '중구'))

############################################################################
########################## 헹정동 NA 채우기 ################################
############################################################################

## 1
cctv %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 산5-1", '신정3동'))

## 3-20
cctv %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 천왕동 278-1", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 천왕동 13-66", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 195-7", '개봉2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 621-8", '구로5동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 오류동 248", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 331-1", '고척2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 오류동 347-1", '오류1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 79-1", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 314-61", '개봉3동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 66-30", '고척1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 3-46", '구로5동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 52-6", '고척1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 410-1", '개봉2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 산6-1", '고척2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 온수동 155-4", '수궁동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 614-178", '구로2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 341-2", '고척1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 342-1", '고척2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 궁동 155-3", '수궁동'))

## 21-40
cctv %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 45-2", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 120-5", '구로4동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 614-168", '구로2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 오류동 159-4", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 47", '구로5동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 온수동 118-5", '수궁동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 62", '구로5동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 314-14", '개봉3동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 46-14", '고척1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 오류동 14-471", '오류1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 궁동 190-130", '수궁동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 온수동 93-14", '수궁동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 오류동 135-69", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 689-7", '구로1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 224-2", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 궁동 40-3", '수궁동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 458-4", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 고척동 57-22", '고척1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 궁동 279-6", '수궁동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 271-17", '개봉3동'))

## 41-60
cctv %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 501-22", '구로5동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 온수동 9-31", '수궁동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 64-21", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 1217", '구로3동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 120-25", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 오류동 18-98", '오류1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 263-23", '개봉3동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 가리봉동 13-21", '가리봉동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 오류동 16-4", '오류1동'),
         dong = replace(dong, 소재지지번주소 == "다산동 399-33", '다산동'),
         dong = replace(dong, 소재지지번주소 == "청구동 843", '청구동'),
         dong = replace(dong, 소재지지번주소 == "만리동2가 6-1", '중림동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 186-21", '구로3동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 189-6", '개봉2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 90-22", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 125-91", '구로3동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 신도림동 285-34", '신도림동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 810-2", '구로3동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 1125-15", '구로3동'),
         dong = replace(dong, 소재지지번주소 == "만리동1가 62-12", '중림동'))

## 61-73
cctv %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 621-10", '구로2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 천왕동 3-2", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 천왕동 3-3", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 701", '구로1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 603-46", '구로5동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 218-8", '구로2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 631", '개봉1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 683-11", '구로1동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 구로동 124", '구로4동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 신도림동 271-62", '신도림동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 천왕동 257-6", '오류2동'),
         dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 87-3", '개봉1동'))

## 2
cctv %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "서울특별시 구로구 개봉동 298-75", '개봉3동'),
         dong = replace(dong, 소재지도로명주소 == "청구로1길 23", '청구동'))


############################################################################
###################### 없는 자치구 데이터 채우기 ###########################
############################################################################

gangdong <- fread("서울특별시_강동구_CCTV_20200911_1600136294390_157488.csv")
gangdong %>% view()


## kakao API

address_list3 = list(gangdong$경도, gangdong$위도)

bowl3 <- data.frame(경도 = address_list3[[1]], 위도 = address_list3[[2]],
                      gu = rep(NA, length(address_list3[[1]])),
                      dong = rep(NA, length(address_list3[[1]])))


pb <- progress_bar$new(total = length(address_list3[[1]]))

for (i in 1:length(address_list3[[1]])){
  pb$tick()
  Sys.sleep(0.01)
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
             query = list(x = address_list3[[1]][i], y = address_list3[[2]][i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  dong <- coord$documents$region_3depth_name[2]
  gu <- coord$documents$region_2depth_name[2]
  
  if (length(dong) > 0){
    bowl3$gu[i] <- gu
    bowl3$dong[i] <- dong
  } else {
    bowl3$gu[i] <- NA
    bowl3$dong[i] <- NA
  }
}

bowl3 %<>% select(-c(위도, 경도))

gangdong %<>% cbind(bowl3)

gangdong %<>% select(소재지도로명주소, 소재지지번주소, 설치목적구분, 카메라대수, 위도, 경도, gu, dong)


cctv %<>% mutate(카메라대수 = as.integer(카메라대수), 위도 = as.numeric(위도), 경도 = as.numeric(경도))
cctv %<>% rbind(gangdong)

cctv %<>% filter(gu != "하남시")

write.csv(cctv, "플러스 강동구.csv", row.names = F)

#### 도봉구 데이터

dobong <- fread("서울특별시 도봉구_CCTV 현황(공유데이터광장)_20210428.csv")

dobong %<>% mutate(위도 = NA, 경도 = NA, gu = "도봉구") %>% 
  select(주소, 지번주소, 설치목적구분, 카메라대수, 위도, 경도, gu, 동구분)

colnames(dobong) <- colnames(cctv)

cctv %<>% rbind(dobong)


dongdaemon <- fread("동대문구_cctv설치현황.csv")
dongdaemon %<>% select(소재지도로명주소, 소재지지번주소, 설치목적구분, 카메라대수, X, Y) %>% 
  rename(위도 = X, 경도 = Y)

## kakao API

address_list4 = dongdaemon$소재지도로명주소

bowl4 <- data.frame(주소 = address_list4,
                      gu = rep(NA, length(address_list4)),
                      dong = rep(NA, length(address_list4)))


pb <- progress_bar$new(total = length(address_list4))

for(i in 1:length(address_list4)){
  pb$tick()
  Sys.sleep(0.01)
  
  res <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
             query = list(query = address_list4[i]),
             add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  
  coord <- res %>% content(as = 'text') %>% fromJSON()
  
  if (length(coord$documents$address_type) == 0) {
    bowl4$dong[i] <- NA
    bowl4$gu[i] <- NA
  } else if (coord$documents$address_type == 'ROAD'){
    bowl4$dong[i] <- NA
    bowl4$gu[i] <- NA
  } else {
    dong <- coord$documents$address$region_3depth_h_name
    gu <- coord$documents$address$region_2depth_name
    
    if (length(dong) > 0) {
      bowl4$dong[i] <- dong
      bowl4$gu[i] <- gu
    } else {
      bowl4$dong[i] <- NA
      bowl4$gu[i] <- NA
    }
  }
}

############################################################################
########################### NA 채우기 & 수정 ###############################
############################################################################

bowl4 %<>% 
  mutate(dong = replace(dong, 주소 == "전농동 35", '전농2동'),
         dong = replace(dong, 주소 == "답십리로40길 5", '답십리1동'),
         dong = replace(dong, 주소 == "서울시립대로4길 70", '답십리1동'),
         dong = replace(dong, 주소 == "답십리로 149-14", '답십리1동'),
         dong = replace(dong, 주소 == "전농로2나길 12", '답십리2동'),
         dong = replace(dong, 주소 == "고산자로28길", '용신동'),
         dong = replace(dong, 주소 == "고산자로28길 61", '용신동'),
         dong = replace(dong, 주소 == "외대역동로22길", '이문1동'),
         dong = replace(dong, 주소 == "휘경로11길 일대", '이문1동'),
         dong = replace(dong, 주소 == "천호대로87길 48", '장안1동'),
         dong = replace(dong, 주소 == "장한평26다길 115-5", '장안1동'),
         dong = replace(dong, 주소 == "한천로24길 47-2", '장안1동'),
         dong = replace(dong, 주소 == "장한로36길 23-8", '장안2동'),
         dong = replace(dong, 주소 == "장한로26다길 1", '장안2동'),
         dong = replace(dong, 주소 == "전농로20가길 5", '전농2동'),
         dong = replace(dong, 주소 == "사가정로20길 36", '전농2동'),
         dong = replace(dong, 주소 == "사가정로 187", '전농2동'),
         dong = replace(dong, 주소 == "제기로2길 17로", '제기동'),
         dong = replace(dong, 주소 == "휘경로 38-7", '휘경1동'),
         dong = replace(dong, 주소 == "외대역동로1길 70-51", '휘경1동'),
         dong = replace(dong, 주소 == "망우로1길 13", '휘경1동'),
         dong = replace(dong, 주소 == "망우로18길 94", '휘경2동'),
         dong = replace(dong, 주소 == "망우로18길 70", '휘경2동'),
         dong = replace(dong, 주소 == "이문로42길 80", '이문2동'),
         dong = replace(dong, 주소 == "장안벚꽃로3길 12", '장안2동'),
         dong = replace(dong, 주소 == "답십리로69길 111", '장안2동'),
         dong = replace(dong, 주소 == "장한로22길 18", '장안2동'),
         dong = replace(dong, 주소 == "망우로16길 41", '휘경2동'),
         dong = replace(dong, 주소 == "휘경로 58-1", '휘경1동'),
         dong = replace(dong, 주소 == "장한로26길 27", '장안2동'),
         dong = replace(dong, 주소 == "시립대로29길 3", '전농2동'),
         dong = replace(dong, 주소 == "장한로24길 30", '장안2동'),
         dong = replace(dong, 주소 == "답십리로56길 125", '답십리2동'),
         dong = replace(dong, 주소 == "답십리로29길", '전농1동'))

bowl4 %<>% select(-주소)

dongdaemon %<>% cbind(bowl4)

## 1-25
dongdaemon %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "답십리동 544-3", '답십리1동'),
         dong = replace(dong, 소재지지번주소 == "답십리동 645-7", '답십리1동'),
         dong = replace(dong, 소재지지번주소 == "답십리동 225-23", '답십리1동'),
         dong = replace(dong, 소재지지번주소 == "답십리2동 990-5", '답십리2동'),
         dong = replace(dong, 소재지지번주소 == "답십리동 14-40", '답십리2동'),
         dong = replace(dong, 소재지지번주소 == "답십리2동 43-3", '답십리2동'),
         dong = replace(dong, 소재지지번주소 == "답십리2동 58-97", '답십리2동'),
         dong = replace(dong, 소재지지번주소 == "용신동 39-1008", '용신동'),
         dong = replace(dong, 소재지지번주소 == "용신동 396-1", '용신동'),
         dong = replace(dong, 소재지지번주소 == "용두동 238-154", '용신동'),
         dong = replace(dong, 소재지지번주소 == "용신동 756-13", '용신동'),
         dong = replace(dong, 소재지지번주소 == "용신동 687-72", '용신동'),
         dong = replace(dong, 소재지지번주소 == "용두동 39-994", '용신동'),
         dong = replace(dong, 소재지지번주소 == "용두동 129-117", '용신동'),
         dong = replace(dong, 소재지지번주소 == "이문동 71-5", '이문1동'),
         dong = replace(dong, 소재지지번주소 == "이문동 48-3", '이문2동'),
         dong = replace(dong, 소재지지번주소 == "장안2동 309-7", '장안2동'),
         dong = replace(dong, 소재지지번주소 == "전농1동 644-88", '전농1동'),
         dong = replace(dong, 소재지지번주소 == "전농1동 273-7", '전농1동'),
         dong = replace(dong, 소재지지번주소 == "전농2동 103-391", '전농1동'),
         dong = replace(dong, 소재지지번주소 == "전농동 688-11", '전농2동'),
         dong = replace(dong, 소재지지번주소 == "전농동 688-7", '전농2동'),
         dong = replace(dong, 소재지지번주소 == "제기동 67-29", '제기동'),
         dong = replace(dong, 소재지지번주소 == "청량리동 199-232", '청량리동'))

## 26-55
dongdaemon %<>% 
  mutate(dong = replace(dong, 소재지지번주소 == "청량리동 46-14", '청량리동'),
         dong = replace(dong, 소재지지번주소 == "휘경2동 281-29", '휘경2동'),
         dong = replace(dong, 소재지지번주소 == "휘경2동 49-20", '휘경2동'),
         dong = replace(dong, 소재지지번주소 == "휘경동 368-3", '휘경2동'),
         dong = replace(dong, 소재지지번주소 == "휘경동 194-42", '휘경1동'),
         dong = replace(dong, 소재지지번주소 == "답십리동 998-1", '답십리2동'),
         dong = replace(dong, 소재지지번주소 == "용두동 715", '용신동'),
         dong = replace(dong, 소재지지번주소 == "제기동 132-1", '제기동'),
         dong = replace(dong, 소재지지번주소 == "전농동 199-56", '전농1동'),
         dong = replace(dong, 소재지지번주소 == "휘경동 295-39", '휘경2동'),
         dong = replace(dong, 소재지지번주소 == "장안동 93-29", '장안2동'),
         dong = replace(dong, 소재지지번주소 == "전농동 156-8", '전농2동'),
         dong = replace(dong, 소재지지번주소 == "휘경동 43-54", '휘경2동'),
         dong = replace(dong, 소재지지번주소 == "장안동 190-62", '장안1동'),
         dong = replace(dong, 소재지지번주소 == "장안동 348", '장안2동'),
         dong = replace(dong, 소재지지번주소 == "장안동 386", '장안1동'),
         dong = replace(dong, 소재지지번주소 == "장안동 325", '장안2동'),
         dong = replace(dong, 소재지지번주소 == "답십리동 294-69", '답십리1동'),
         dong = replace(dong, 소재지지번주소 == "전농동 553-1", '전농1동'),
         dong = replace(dong, 소재지지번주소 == "답십리동 294-3", '답십리1동'),
         dong = replace(dong, 소재지지번주소 == "답십리동 465-12", '답십리1동'),
         dong = replace(dong, 소재지지번주소 == "전농동 529-10", '전농1동'),
         dong = replace(dong, 소재지지번주소 == "제기동 728-3", '제기동'),
         dong = replace(dong, 소재지지번주소 == "휘경동 118-10", '휘경1동'),
         dong = replace(dong, 소재지지번주소 == "전농동 150-62", '전농2동'),
         dong = replace(dong, 소재지지번주소 == "이문동 264-509", '이문1동'),
         dong = replace(dong, 소재지지번주소 == "이문동 264-199", '이문1동'),
         dong = replace(dong, 소재지지번주소 == "휘경동 228-3", '휘경1동'),
         dong = replace(dong, 소재지지번주소 == "장안동 297-7", '장안2동'))


# 정확한 위치 알 수 없어 삭제
dongdaemon %<>% filter(소재지도로명주소 != "배봉산 근린공원 산책로") 

# 구에는 모두 동대문구 삽입
dongdaemon %<>% mutate(gu = "동대문구") %>% 
  mutate(gu = replace(gu, 소재지지번주소 == "하월곡동 산5-57", "성북구")) %>% 
  mutate(dong = replace(dong, 소재지지번주소 == "하월곡동 산5-57", "월곡2동"))

colnames(dongdaemon) <- colnames(cctv)

cctv %<>% rbind(dongdaemon)

cctv %<>% filter(dong != "하양읍")

write.csv(cctv, "cctv_total.csv", row.names = F)

#### 자치구/행정동별 개수

cctv_gu <-  cctv %>% group_by(gu) %>% 
  summarise(n = n())


cctv_dong <- cctv %>% group_by(gu, dong) %>% 
  summarise(n = n())

############################################################################
######################### 면적 대비 비율 변수 ##############################
############################################################################

gu_size <- readxl::read_excel("자치구별 면적.xlsx", col_names = T)

gu_size %<>% select(자치구, 면적) %>% 
  filter(자치구 != "서울시")

cctv_gu %<>% left_join(gu_size, by = c("gu" = "자치구"))

cctv_gu %<>% mutate(`cctv_rate` = n/면적)

write.csv(cctv_gu, "cctv_size_rate_gu.csv", row.names = F)


dong_size <- readxl::read_excel("행정동별 면적.xlsx", col_names = T)

dong_size %<>% select(자치구, 동, 면적) %>% 
  filter(동 != "합계", 동 != "소계")


# 행정동별 이름 수정
cctv_dong %<>% 
  mutate(dong = replace(dong, dong == "면목제3.8동", "면목3.8동"))

dong_size %<>% mutate(동 = replace(동, 동 == "종로5·6가동", "종로5.6가동"))

total_dong <- cctv_dong %>% left_join(dong_size, by = c("gu" = "자치구", "dong" = "동"))

## 신정3동이 구로구로 되어있는 행이 있음 -> 양천구로 변경
total_dong[total_dong$dong=="신정3동" & total_dong$gu == "양천구","n"] <- 99
total_dong %<>% filter(dong != "신정3동" | gu != "구로구")

## 항동의 크기 없음
total_dong[total_dong$dong == "항동", "면적"] <- 1.4

total_dong %<>% mutate(`cctv_rate` = n/면적)
write.csv(total_dong, "cctv_size_rate_dong.csv", row.names = F)


############################
#### 5천만원이상 데이터 ####
############################

###########소득데이터에서 5천만원비율 변수 가져오기(행정동별데이터)########
library(readxl)
library(tidyverse)
library(dplyr)
setwd("C:/Users/cheol/Desktop/주제분석")

mapping<-readxl::read_excel(path="mapping.xlsx",sheet='sheet1')
income<-read.csv(file = "per_cate_income.csv",header=T)
dong<-read.csv(file="total_dong2.csv",
               header=T)
gu<- read.csv(file="total_gu2.csv",
              header=T)

income$행정동코드 <- gsub("[[:punct:]]", "", income$행정동코드) #데이터 수치형 변수에 ""가 있어서 ""제거해주기
as.numeric(mapping$행정동코드)

income1 <- full_join(income,mapping,by="행정동코드")

income1 <- income1 %>% select(c(6,9,10))

income1<-rename(income1,"행정동"="행정동명")
income1 <- full_join(dong,income1,by="행정동")
total_dong3 <- income1 %>% select(-8)
total_dong3<- total_dong3 %>% rename("5천만원이상비율"="X5천만원이상")

write_csv(total_dong3,"total_dong3.csv")

########################구별데이터#####################
mapping<-readxl::read_excel(path="mapping.xlsx",sheet='sheet1')
income<-read.csv(file = "per_cate_income.csv",header=T)
dong<-read.csv(file="total_dong2.csv",
               header=T)
gu<- read.csv(file="total_gu2.csv",
              header=T)
income$행정동코드 <- gsub("[[:punct:]]", "", income$행정동코드)
as.numeric(mapping$행정동코드)


income1 <- full_join(income,mapping,by="행정동코드")

income1 <- income1 %>% select(c(6,9,10))


income1 <- income1 %>% group_by(시군구명) %>% mutate(평균 = X5천만원이상/n())


income1$오천만원이상비율 <- income1$평균
income1$자치구 <- income1$시군구명
income1<- income1[,-c(2,4)]
income1 <- income1 %>% select(X5천만원이상,행정구,행정동명,오천만원이상비율)
income1

total_gu3<-full_join(gu,income1,by="자치구")


df<-income1 %>% group_by(시군구명) %>% 
  summarize(M = mean(X5천만원이상))
df

gu <- gu[c(order(gu$자치구)),]

gu['5천만원이상비율'] <- df$M
rownames(gu)
rownames(gu) <- 1:25

gu<- gu %>% rename('5천만원이상비율' = '평균')
total_gu3 <- gu
total_gu3
write_csv(total_gu3,"total_gu3.csv")
getwd()
library(data.table)
data <- fread('total_dong3.csv',encoding='UTF-8')

write.csv(total_dong3,'total_dong3.csv')