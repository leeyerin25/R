library(tidyverse)
library(corrplot)
library(skimr)
library(naniar)

TIPS <- read.csv('c:/2022dm2/tips.csv', sep = ',')
dim(TIPS)

DF <- data.frame(TIPS)
str(DF)

#sex, smoker, day, time 을 factor 로 변환하라
DF <- DF %>% mutate(sex=factor(sex), smoker=factor(smoker), day=factor(day), time=factor(time))
str(DF)

#변수를 추가하라tiprate : tip/total_bill . 총지불액 대비 봉사료비율
DF <- DF %>% mutate(tiprate=tip/total_bill)
str(DF)

#결측치 확인
naniar::vis_miss(DF, sort_miss=TRUE)


#간단탐색 요약통계량제시
summary(DF)

#평균지불하는 봉사료 평균과 표편
mean(DF$tiprate)
sd(DF$tiprate)

skim(DF)


#tiprate의 분포 (히스토그램,상자그림) 그림이좀다름
par(mfrow=c(2,1))
hist(DF$tiprate, prob=TRUE) 
lines(density(DF$tiprate))
boxplot(DF$tiprate, horizontal=TRUE)

#질문: 남성의 봉사료비율이 여자보다 높은가 ?
#(tiprate ~ sex) 성별 tiprate의 평균, 표준편차를 구하고 상자그림 그리기

DF %>%
  group_by(sex) %>%
  summarize_at(c('tiprate'), list("mean(tiprate)"=mean, "sd(tiprate)"=sd), na.rm=TRUE)

boxplot(tiprate~sex, data=DF)


#질문: 흡연자가 봉사료비율이 더 높은가?

DF %>%
  group_by(smoker) %>%
  summarize_at(c('tiprate'), list("mean(tiprate)"=mean, "sd(tiprate)"=sd), na.rm=TRUE)


#질문: 성별x흡연별 봉사료비율의 순서. 남자 흡연자, .., 여자 비흡연자 등 누가 가장 높고, 누가 가장 낮은가 ?
DF %>% group_by(smoker, sex) %>% summarize(mean(tiprate), sd(tiprate))

#박스그림이상함
boxplot(tiprate~smoker+sex, data=DF)

#질문: 어느 요일이 가장 봉사료비율이 높은가 ?
#(tiprate ~ day) 요일별 tiprate의 평균, 표준편차를 구하고 상자그림 그리기

DF %>%
  group_by(day) %>%
  summarize_at(c('tiprate'), list("mean(tiprate)"=mean, "sd(tiprate)"=sd))

boxplot(tiprate~day, data=DF)


#질문: 저녁식사의 봉사료비율이 점심보다 높은가 ?
#(tiprate ~ time) 식사시간대별 tiprate의 평균, 표준편차를 구하고 상자그림 그리기

DF %>%
  group_by(time) %>%
  summarize_at(c('tiprate'), list("mean(tiprate)"=mean, "sd(tiprate)"=sd))

#질문: 일행이 많을수록 봉사료비율이 낮아지는가 ?
#size 와 tiprate 간 상관계수를 구하고 산점도 그리기

cor(DF$size, DF$tiprate)


plot(DF$tiprate~DF$size, data=DF)

#질문: 총지불금액이 많을수록 봉사료비율이 낮아지는가 ?
#total_bill 와 tiprate 간 상관계수를 구하고 산점도 그리기

cor(DF$tiprate, DF$total_bill)

plplot(DF$tiprate~DF$size, data=DF)

#연속형 변수들간 상관행렬을 시각화하시오 ( corrplot )
#이상치가 있는가 


corrplot<-(cor(DF))

data<-DF[c(1,2,8)] 
r<-cor(data, method="pearson") 
round(r, 2)
corrplot(r,method="circle",type="lower")


