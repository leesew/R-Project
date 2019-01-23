library(dplyr)
library(ggplot2)
library(ggiraphExtra)
library(dygraphs)
library(xts)
library(gridExtra)
library(extrafont)
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))
#----------------------------------------------------------
data4 <- read.csv("d:/workspace/R/project1/excel/전국 관광숙박업 등록현황_2009-1.csv")
View(data4)
data4 <- rename(data4, site="구분", first = "관광호텔.4", second="가족호텔", third="호스텔", fourth="휴양콘도미니엄.소계")
data4 <- data4 %>%  filter(site!="합계")
visit4 <- data4 %>% select(site, first, second, third, fourth ) %>% arrange(-first) %>% head(10)
x2 <- ggplot(data=visit4, aes(x=reorder(site, -first), y=first, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 관광 호텔 (시,도 별)") + xlab("지역")+ ylab("호텔수") + scale_fill_brewer(palette="Spectral") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5))) + theme_bw()
x3 <- ggplot(data=visit4, aes(x=reorder(site, -second), y=second, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 가족 호텔 (시,도 별)") + xlab("지역")+ ylab("호텔수") +scale_fill_brewer(palette="Spectral")+
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))+ theme_bw()
x4 <- ggplot(data=visit4, aes(x=reorder(site, -third), y=third, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 호스텔 (시,도 별)") + xlab("지역")+ ylab("호텔수") +scale_fill_brewer(palette="Spectral")+ 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))+ theme_bw()
x5 <- ggplot(data=visit4, aes(x=reorder(site, -fourth), y=fourth, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 휴양 콘도 (시,도 별)") + xlab("지역")+ ylab("호텔수") + scale_fill_brewer(palette="Spectral")+
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))+ theme_bw()
grid.arrange(x2,x3,x4,x5, ncol=2, nrow=2)


data1 <- read.csv("d:/workspace/R/project1/excel/hotel2013.csv")
View(data1)
data1 <- rename(data1, site="구분", first = "관광호텔.10", second="가족호텔", third="호스텔", fourth="휴양콘도미니엄.소계")
data1 <- data1 %>%  filter(site!="합계")
visit3 <- data1 %>% select(site, first, second, third, fourth ) %>% arrange(-first) %>% head(10)
z2 <- ggplot(data=visit3, aes(x=reorder(site, -first), y=first, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 관광 호텔 (시,도 별)") + xlab("지역")+ ylab("호텔수") + scale_fill_brewer(palette="Spectral") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5))) + theme_bw()
z3 <- ggplot(data=visit3, aes(x=reorder(site, -second), y=second, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 가족 호텔 (시,도 별)") + xlab("지역")+ ylab("호텔수") +scale_fill_brewer(palette="Spectral")+
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))+ theme_bw()
z4 <- ggplot(data=visit3, aes(x=reorder(site, -third), y=third, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 호스텔 (시,도 별)") + xlab("지역")+ ylab("호텔수") +scale_fill_brewer(palette="Spectral")+ 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))+ theme_bw()
z5 <- ggplot(data=visit3, aes(x=reorder(site, -fourth), y=fourth, fill=site)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+ 
  labs(title = "전국 휴양 콘도 (시,도 별)") + xlab("지역")+ ylab("호텔수") + scale_fill_brewer(palette="Spectral")+
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))+ theme_bw()
grid.arrange(z2,z3,z4,z5, ncol=2, nrow=2)



data2 <- read.csv("d:/workspace/R/project1/excel/관광숙박업 운영실적_2013(지역별).csv")
View(data2)
data2 <- rename(data2, site=X, first = "외국인", second="내국인")
options(scipen=100)
z6 <- ggplot(data=data2, aes(x=reorder(site, first), y=first, fill=site)) + geom_bar(stat="identity", position="dodge2", width=0.7, colour="black") +
  labs(title = "외국인 숙박 이용 실적(단위: 천원)") + xlab("지역")+ ylab("돈") +
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5))) +
  coord_flip() + theme_bw()
z7 <- ggplot(data=data2, aes(x=reorder(site, second), y=second, fill=site)) + geom_bar(stat="identity", position="dodge2", width=0.7, colour="black") +
  labs(title = "내국인 숙박 이용 실적(단위: 천원)") + xlab("지역")+ ylab("돈") +
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5))) +
  coord_flip() + theme_bw()
grid.arrange(z6,z7,ncol=2)


#----------------------------------------------------------
library(dplyr)
data <- read.csv("d:/workspace/R/project1/excel/주요관광지점 입장객_2013(1~12).csv", header=T, stringsAsFactors=T)
#-----2006년 대전지역 관광지 내.외국인 방문 현황 
data5 <- read.csv("d:/workspace/R/project1/excel/대전 주요 관광지 2006.csv", header=T, stringsAsFactors=T)
View(data5)
data5 <- rename(data5, region = "관광지", human="내.외국인", site="군구", total=X2006년)
visit3 <- data5 %>% select(site, region, human, total ) %>%  filter(human=="내국인") %>% arrange(desc(site)) %>% head(15)
View(visit3)
options(scipen=100)
z8 <- ggplot(data=visit3, aes(x=reorder(region, total), y=total, fill=site)) + 
  geom_bar(stat="identity", position="dodge", width=0.7, colour="black") + coord_flip()+labs(title = "대전 지역 관광지 방문자수(2006년 내국인)") + xlab("대표관광지")+ ylab("방문자수") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))  + 
  scale_fill_brewer(palette="Pastel1")
z8
visit4 <- data5 %>% select(site, region, human, total ) %>%  filter(human=="외국인") %>% arrange(desc(site)) %>% head(15)
z9 <- ggplot(data=visit4, aes(x=reorder(region, total), y=total, fill=site)) + 
  geom_bar(stat="identity", position="dodge", width=0.7, colour="black") + coord_flip()+labs(title = "대전 지역 관광지 방문자수(2006년 외국인)") + xlab("대표관광지")+ ylab("방문자수") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))  + 
  scale_fill_brewer(palette="Pastel1")
z9
grid.arrange(z8,z9,ncol=2)
#-------------2018년 대전 관광지 내.외국인 현황
data6 <- read.csv("d:/workspace/R/project1/excel/대전 주요 관광지 2018.csv", header=T, stringsAsFactors=T)
View(data6)
data6 <- rename(data6, region = "관광지", human="내.외국인", site="군구", total=X2018년)
visit5 <- data6 %>% select(site, region, human, total ) %>%  filter(human=="내국인") %>% arrange(desc(site))
View(visit5)
options(scipen=100)
z9 <- ggplot(data=visit5, aes(x=reorder(region, total), y=total, fill=site)) + 
  geom_bar(stat="identity", position="dodge", width=0.7, colour="black") + coord_flip()+labs(title = "대전 지역 관광지 방문자수(2018년 내국인)") + xlab("대표관광지")+ ylab("방문자수") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))  + 
  scale_fill_brewer(palette="Pastel1")
z9
visit6 <- data6 %>% select(site, region, human, total ) %>%  filter(human=="외국인") %>% arrange(desc(site)) %>% head(15)
z10 <- ggplot(data=visit6, aes(x=reorder(region, total), y=total, fill=site)) + 
  geom_bar(stat="identity", position="dodge", width=0.7, colour="black") + coord_flip()+labs(title = "대전 지역 관광지 방문자수(2018년 외국인)") + xlab("대표관광지")+ ylab("방문자수") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))  + 
  scale_fill_brewer(palette="Pastel1")
z10
grid.arrange(z8,z9,ncol=2)
#--- 대전 숙박업소 이용요금 변화에 대한 가설검정 ;;;
data7 <- read.csv("d:/workspace/R/project1/excel/객실 이용률 현황_2006.csv", header=T, stringsAsFactors=T)
View(data7)
data7 <- data7[-c(5),]
data7 <- rename(data7, site="구분", use=이용률...  , pay=X1객실당.평균.판매요금.원.)
z11 <- ggplot(data=data7, aes(x=reorder(use, -pay), y=pay, fill=site)) + geom_bar(stat="identity") + geom_col(position = "dodge2") + geom_text(aes(label = pay), vjust = 1.5, color = "white",  position = position_dodge(.9), size = 3) +
  labs(title = "2006년 대전 평균 숙박 요금") + xlab("이용률")+ ylab("금액(단위: 원)") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))

data8 <- read.csv("d:/workspace/R/project1/excel/객실 이용률 현황_2013.csv", header=T, stringsAsFactors=T)
View(data8)
data8 <- data8[-c(4),]
data8 <- rename(data8, site="구분", use=이용률...  , pay=X1객실당.평균.판매요금.원.)
data8 <- data8 %>% arrange(desc(use))
z12 <- ggplot(data=data8, aes(x=reorder(use, -pay), y=pay, fill=site)) + geom_bar(stat="identity") + geom_col(position = "dodge2") + geom_text(aes(label = pay), vjust = 1.5, color = "white",  position = position_dodge(.9), size = 3) +
  labs(title = "2018년 대전 평균 숙박 요금") + xlab("이용률")+ ylab("금액(단위: 원)") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))
grid.arrange(z11,z12,ncol=2)
#-----t.test
# 2006년과 2013년 요금 변화률에 대한 가설 검증
#2006년도 숙박 평균 요금
View(data7)
data7 <- data7[-c(2),] #중구 제외 시킴
View(data8)
mean_7 <- mean(data7$pay)
mean_7
# 2013년도  
head(teen_ten_flu)
# 1-sample test
mean_8 <- data8$pay
mean_8
s <- 54102
n <- 3
mean_8 <- mean(mean_8)
options(digits=2)
mean_8
sd_mean_8 <- sd(mean_8)
sd_mean_8
mmean_8 <- mean_8-s/(sd_mean_8/sqrt(n))
al <- mean_8-s
al
zl <- 14304/(sd_mean_8/sqrt(n))
zl # 통계량 2.08
qt(0.95, df=3)
# 95%의 신뢰구간을 가진 평균의 대한 t.test
t.test(mean_8, mu=54102, alternative="less")
library(graphics)
df <- 2
x <- seq(-3.3,3.3, by=0.01)
y <- dt(x, df=df)
xa <- qt(0.95, df=df)
plot(x,y,type="l", ylim=c(-0.02, 0.5),lty=1, axes=F, main="1-Sample Test")
abline(h=0)
dt <- dt(x, df=df)
qt <- qt(0.95, df=df)
qt.g <- round(qt,2)
lines(c(xa,xa), c(1,0), lty=2)
polygon(c(qt.g,x[x>qt.g],3.3), c(0, y[x>qt.g],0), col="red")
text(qt,-0.02, expression(t[0.025]==2.920))
#-------------------------------------------------------
data10 <- read.csv("d:/workspace/R/project1/excel/pay2006_2013_row.csv")
data10 <- rename(data10, year=X)
data10$year <- as.factor(data10$year)
data11 <- data10 %>% select(year,중구) 
data12 <- data10 %>% select(year,서구)
data13 <- data10 %>% select(year,유성구)
View(data10)
a1 <- ggplot(data=data11, aes(x=year,y=중구,fill=year)) + geom_bar(stat="identity")+ geom_col(position = "dodge2") + 
  geom_text(aes(label = 중구), vjust = 1, color = "black",  position = position_dodge(.5), size = 3) +
  labs(title = "대전 중구 숙박요금 변화율") + xlab("연도")+ ylab("금액(단위: 원)") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))  + coord_polar(theta="x")
a2 <- ggplot(data=data12, aes(x=year,y=서구,fill=year)) + geom_bar(stat="identity")+ geom_col(position = "dodge2") + 
  geom_text(aes(label = 서구), vjust = 1, color = "black",  position = position_dodge(.5), size = 3) +
  labs(title = "대전 서구 숙박요금 변화율") + xlab("연도")+ ylab("금액(단위: 원)") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))  + coord_polar(theta="x")
a3 <- ggplot(data=data13, aes(x=year,y=유성구,fill=year)) + geom_bar(stat="identity")+ geom_col(position = "dodge2") + 
  geom_text(aes(label = 유성구), vjust = 1, color = "black",  position = position_dodge(.5), size = 3) +
  labs(title = "대전 유성구 숙박요금 변화율") + xlab("연도")+ ylab("금액(단위: 원)") + 
  (theme(plot.title = element_text(family = "malgun", face = "bold", size=15, color="darkblue", hjust=0.5)))  + coord_polar(theta="x")
grid.arrange(a1,a2,a3, ncol=3)

             