library(ggplot2)
str(USArrests)
write.csv(USArrests, file="arrests.csv")
usa <- read.csv("d:/workspace/R/easy_R/arrests.csv")
library(tibble)
str(usa)
View(usa)
usa$X <- tolower(usa$X)

state_map <- map_data("state")
str(state_map)

library(ggiraphExtra)
ggChoropleth(data=usa, aes(fill=number, map_id=X), map=state_map, interactive=T)
#----------------------------------------------------------------------------------------------------------------------------------------
# 10대 아이의 사망원인과 사망률 (미국전역)
# 출처 https://catalog.data.gov/dataset/age-adjusted-death-rates-for-the-top-10-leading-causes-of-death-united-states-2013
teen_death <- read.csv("d:/workspace/R/project1/excel/10_death.csv", header=T, stringsAsFactors=T) 
View(teen_death)
teen_death <- data.frame(teen_death)
teen_death$State <- tolower(teen_death$State)
library(dplyr)
library(readxl)
teen_death <- rename(teen_death, deathrate = "Age.adjusted.Death.Rate" ) # 질병율 변수명 바꿈
teen_death_graph <- teen_death %>% filter(Cause.Name=="Kidney disease") # 신장병 선택
teen_death_graph1 <- teen_death %>% filter(Cause.Name=="Influenza and pneumonia") # 폐암 선택
teen_death_graph2 <- teen_death %>% filter(Cause.Name=="CLRD") 
teen_death_graph3 <- teen_death %>% filter(Cause.Name=="Cancer")
state_map <- map_data("state")
library(dplyr)
library(ggplot2)
install.packages("ggiraphExtra")
library(ggiraphExtra)
# 지역별 신장병 사망률의 대한 그래프 시각화
d1 <- ggChoropleth(data=teen_death_graph, aes(fill=deathrate, map_id=State), map=state_map,  palette="RdPu", title="신장병 사망률") 
d2 <- ggChoropleth(data=teen_death_graph1, aes(fill=deathrate, map_id=State), map=state_map,  palette="RdPu", title="폐암 사망률")
d3 <- ggChoropleth(data=teen_death_graph1, aes(fill=deathrate, map_id=State), map=state_map,  palette="RdPu", title="만성 하부 호흡기 질환 사망률")
d4 <- ggChoropleth(data=teen_death_graph1, aes(fill=deathrate, map_id=State), map=state_map,  palette="RdPu", title="암 사망률") 
grid.arrange(d1,d2,d3,d4, nrow=2,ncol=2)
# 인터랙티브 추가
h1 <- ggChoropleth(data=teen_death_graph, aes(fill=deathrate, map_id=State), map=state_map,  palette="RdPu", title="신장병 사망률", interactive=T)
h1
# 신장병 사망률이 높았던 상위 4개 지역에 다른 질병 사망률도 높을까?의 대한 비교 그래프(2000~2005사이)
library(ggthemes)
teen_death_kid <- teen_death %>% filter(Cause.Name=="Kidney disease") %>% arrange(-deathrate)
teen_kid <- teen_death_kid %>% filter(Year >=2000 & Year <= 2005)
teen_kid <- teen_kid %>% select(Year, State,deathrate) %>% filter(State %in% c("louisiana","mississippi","west virginia","indiana"))

z1 <- ggplot(data=teen_kid, aes(x=Year, y=deathrate, fill=State)) + geom_bar(stat="identity", position="dodge2", width=0.7, colour="black") +labs(title = "신장병 사망률 (2000~2005년)") + xlab("연도")+ ylab("사망률") + 
  (theme(plot.title = element_text(family = "", face = "bold", size=15, color="blue", hjust=0.5)))   + scale_fill_brewer(palette="Pastel1")
teen_death_flu <- teen_death %>% filter(Cause.Name=="Influenza and pneumonia") %>% arrange(-deathrate)  
teen_kid2 <- teen_death_flu %>% filter(Year >=2000 & Year <= 2005)
teen_kid2 <- teen_kid2 %>% select(Year, State, deathrate) %>% filter(State %in% c("louisiana","mississippi","west virginia","indiana"))

z2 <- ggplot(data=teen_kid2, aes(x=Year, y=deathrate, fill=State)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+labs(title = "폐암 사망률 (2000~2005년)") + xlab("연도")+ ylab("사망률") + 
  (theme(plot.title = element_text(family = "", face = "bold", size=15, color="blue", hjust=0.5)))  + scale_fill_brewer(palette="Pastel1")
teen_death_clrd <- teen_death %>% filter(Cause.Name=="CLRD") %>% arrange(-deathrate)  
teen_kid3 <- teen_death_clrd %>% filter(Year >=2000 & Year <= 2005)
teen_kid3 <- teen_kid3 %>% select(Year, State, deathrate) %>% filter(State %in% c("louisiana","mississippi","west virginia","indiana"))
x2 <- ggplot(data=teen_kid3, aes(x=Year, y=deathrate, fill=State)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+labs(title = "호흡기 질환 사망률 (2000~2005년)") + xlab("연도")+ ylab("사망률") + 
  (theme(plot.title = element_text(family = "", face = "bold", size=15, color="blue", hjust=0.5)))  + scale_fill_brewer(palette="Pastel1")
teen_death_can <- teen_death %>% filter(Cause.Name=="Cancer") %>% arrange(-deathrate)  
teen_kid4 <- teen_death_can %>% filter(Year >=2000 & Year <= 2005)
teen_kid4 <- teen_kid4 %>% select(Year, State, deathrate) %>% filter(State %in% c("louisiana","mississippi","west virginia","indiana"))
x3 <- ggplot(data=teen_kid4, aes(x=Year, y=deathrate, fill=State)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+labs(title = "암 사망률 (2000~2005년)") + xlab("연도")+ ylab("사망률") + 
  (theme(plot.title = element_text(family = "", face = "bold", size=15, color="blue", hjust=0.5)))  + scale_fill_brewer(palette="Pastel1")
z3 <- z1 + facet_grid(facets=. ~ State)+ coord_polar(theta="x")
z4 <- z2 + facet_grid(facets=. ~ State)+ coord_polar(theta="x")
x5 <- x3 + facet_grid(facets=. ~ State)+ coord_polar(theta="x") 
x4 <- x2 + facet_grid(facets=. ~ State)+ coord_polar(theta="x")

library(gridExtra)
grid.arrange(z1,z2,x2,x3, nrow=2)  
#폴라 그래프 표현
grid.arrange(x4,x5, nrow=2)


#-----------------------------------------------------------------------------------------------------------------------
#2000~2010년 전체의 신장병과 폐암 사망률 변화 한눈에 보기
teen_death_kid <- teen_death %>% filter(Cause.Name=="Kidney disease") %>% arrange(-deathrate)
teen_kid1 <- teen_death_kid %>% filter(Year >=2000 & Year <= 2010)
teen_kid1 <- teen_kid1 %>% select(Year, State,deathrate) %>% filter(State %in% c("louisiana","mississippi","west virginia","indiana"))
library(ggthemes)
z11 <- ggplot(data=teen_kid1, aes(x=Year, y=deathrate, fill=State)) + geom_bar(stat="identity", position="dodge2", width=0.7, colour="black") +labs(title = "신장병 사망률 (2000~2010년)") + xlab("연도")+ ylab("사망률") + 
  (theme(plot.title = element_text(family = "", face = "bold", size=15, color="blue", hjust=0.5))) + geom_line(size=0.6, color="red", alpha=0.5) + scale_fill_brewer(palette = "Accent") 
scale_fill_brewer(palette="Pastel1")
z11
z3 <- z11 +facet_grid(facets=. ~ State)+ coord_polar(theta="x")

teen_death_flu <- teen_death %>% filter(Cause.Name=="Influenza and pneumonia") %>% arrange(-deathrate)  
teen_kid3 <- teen_death_flu %>% filter(Year >=2000 & Year <= 2010)
teen_kid3 <- teen_kid3 %>% select(Year, State, deathrate) %>% filter(State %in% c("louisiana","mississippi","west virginia","indiana"))

z22 <- ggplot(data=teen_kid3, aes(x=Year, y=deathrate, fill=State)) + geom_bar(stat="identity", position="dodge", width=0.7, colour="black")+labs(title = "폐암 사망률 (2000~2016년)") + xlab("연도")+ ylab("사망률") + 
  (theme(plot.title = element_text(family = "", face = "bold", size=15, color="blue", hjust=0.5))) + geom_line(size=0.6, color="red", alpha=0.5) + scale_fill_brewer(palette = "Accent")
scale_fill_brewer(palette="Pastel1") 
z22  
z4 <- z22 + facet_grid(facets=. ~ State)+ coord_polar(theta="x")
library(gridExtra)
#폴라 그래프 표현
grid.arrange(z3,z4, nrow=2)
#----------------------------------------------------------------------------------------------------------------------------------------
# 가설검정 2000년도 4개 지역의 10대 아이 신장병 사망률
#2000년 4개 지역의 신장별 사망률의 평균이 ??였었다. 10년이 지난 2010년에 신장별 사망률을 계속 증가 했는가?에 대한 가설 검정
teen_past_flu <- teen_death %>% select (Year, State, Cause.Name, deathrate) %>% filter(Year == 2000 & State %in% c("louisiana","mississippi","west virginia","indiana") & Cause.Name=="Kidney disease") %>% arrange(-deathrate)
teen_ten_flu <- teen_death %>% select (Year, State, Cause.Name, deathrate) %>% filter(Year == 2010 & State %in% c("louisiana","mississippi","west virginia","indiana") & Cause.Name=="Kidney disease") %>% arrange(-deathrate)
# 2000년도 신장병 사망률이 높았던 4개 지역의 평균
mean_flu <- mean(teen_past_flu$deathrate)
mean_flu
# 2010년도 신장병 사망률 
head(teen_ten_flu)
# 1-sample test
ten_flu <- teen_ten_flu$deathrate
s <- 20.375
n <- 4
mean_ten <- mean(ten_flu)
mean_ten
sd_ten <- sd(ten_flu)
flu_ten <- mean_ten-s/(sd_ten/sqrt(n))
al <- mean_ten-s
al
zl <- 3.2/(sd_ten/sqrt(n))
zl # 통계량 2.08
qt(0.95, df=3)
# 95%의 신뢰구간을 가진 평균의 대한 t.test
t.test(ten_flu, mu=20.375, alternative="two.sided")
library(graphics)
df <- 3
x <- seq(-3,3, by=0.01)
y <- dt(x, df=df)
xa <- qt(0.95, df=df)
plot(x,y,type="l", ylim=c(-0.02,0.5), lty=1, axes=F, main="1-Sample Test")
abline(h=0)
dt <- dt(x, df=df)
qt <- qt(0.95, df=df)
qt
qt.g <- round(qt,2)
lines(c(xa,xa), c(1,0), lty=2)
polygon(c(qt.g,x[x>qt.g],3), c(0, y[x>qt.g],0), col="red")



# 독립된 두 표본의 평균차이에 대한 추정과 검정 ----------------------------------------------------------------------------
install.packages("dplyr")
library(dplyr)
library(ggplot2)
str(teen_death)
teen_past_flu <- teen_death %>% select (Year, State, Cause.Name, deathrate) %>% filter(Year == 2000 & Cause.Name=="Kidney disease") %>% arrange(-deathrate)
teen_ten_flu <- teen_death %>% select (Year, State, Cause.Name, deathrate) %>% filter(Year == 2010  & Cause.Name=="Kidney disease") %>% arrange(-deathrate)
data <- rbind(teen_past_flu, teen_ten_flu)
str(data)
View(data)
str(teen_past_flu)
table(data$Year)
with(data, tapply(deathrate, Year, summary))
boxplot(deathrate ~ Year,
       data=data,
       main="2000년과 2010년 신장병 사망률", xlab="연도", ylab="신장병 사망률")
library(ggplot2)
ggplot(data, aes(x=deathrate,color="", fill="variable")) + (geom_histogram(binwidth=2, alpha=0.6)) + facet_grid(Year ~ .) + ggtitle("histogram by Year")
# 평균은 큰 차이가 없으나 분산은 조금 차이가 있다. 그러면 분산의 대한 차이가 있는지 확인 해보
# 2000, 2010년의 분산 동일성 가정 테스트 F-test
var.test(deathrate ~ Year, data=data)
# p-value값이 0.3641이 나왔으므로 유의수준 5% 에서 귀무가설을 채택하고 두 집단의 분산이 서로 같다는 
# 대립 가설을 채택한다.
# 두집단의 모평균 차이
t.test(deathrate ~ Year, data=data, alternative=c("two.sided"), var.equal=F, conf.level=0.95)
# p-value값이 0.063이므로 두 년도의 모평균에는 차이가 없다라고 볼 수 있다.
t.test(deathrate ~ Year, data=data, alternative=c("two.sided"), var.equal=F, conf.level=0.95)
#----------------------------------------------------------------------------------------------------------------------------------------
# 1999~2016년 빈곤율 데이터 추출
library(readxl)
allPoverty <- read_excel("d:/workspace/R/project1/excel/allpovu.xls", col_names=F) 
# 출처 https://www.census.gov/data/datasets/time-series/demo/saipe/model-tables.html
View(allPoverty)
allPoverty$X__3 <- tolower(allPoverty$X__3)
# 년도별 변수명 수정
allPoverty <- rename(allPoverty, State = "X__3", allPov2016 = "X__11",allPov2015 = "X__15",allPov2014 = "X__19",allPov2013 = "X__23"
                     ,allPov2012 = "X__27",allPov2011 = "X__31",allPov2010 = "X__35", allPov2009 = "X__39",allPov2008 = "X__43",
                     allPov2007 = "X__47",allPov2006 = "X__51",allPov2005 = "X__55",allPov2004 = "X__59",allPov2003 = "X__63",
                     allPov2002 = "X__67",allPov2001 = "X__71",allPov2000 = "X__75",allPov1999 = "X__79") 
View(allPoverty)

#회귀분석 정리---------------------------------------------------------------------------------------------------------------------------
# 1999~2016년 플로리다 10대 (0~17세) 빈곤한 아이 수
mean1 <- allPoverty %>% select(State,allPov2016, allPov2015,allPov2014,allPov2013,allPov2012,allPov2011,
                               allPov2010,allPov2009,allPov2008,allPov2007,allPov2006,allPov2005,allPov2004,allPov2003,allPov2002,
                               allPov2001,allPov2000,allPov1999) %>% filter(State %in% c("florida")) 
# 1999~2016년 미국 전역에 10대 빈곤한 아이 수
mean2 <- allPoverty %>% select(State, allPov2016, allPov2015,allPov2014,allPov2013,allPov2012,allPov2011,
                               allPov2010,allPov2009,allPov2008,allPov2007,allPov2006,allPov2005,allPov2004,allPov2003,allPov2002,
                               allPov2001,allPov2000,allPov1999) %>% filter(State %in% c("united states")) 
View(mean1)
mean1 <- as.numeric(mean1)
mean2 <- as.numeric(mean2)
options(digits=3)
# 미국전역에 대한 플로리다의 10대 아이 빈곤률 
floridamean <- (mean1/mean2)*100 
floridamean <- data.frame(floridamean)
floridamean <- floridamean[-1,]
#write.csv(floridamean, file="floridamean1.csv")
#floridamean <- read.csv("d:/workspace/R/excel/floridamean1.csv") # 행 삭제를 위해 읽고 쓰기를 반복
# 1999~2016년 플로리다에 신장병으로 으로 죽은 10대 아이의 퍼센트율
teen_death <- read.csv("d:/workspace/R/project1/excel/10_death.csv", header=T, stringsAsFactors=T) 
teen_death <- data.frame(teen_death)
teen_death$State <- tolower(teen_death$State)
teen_death <- rename(teen_death, deathrate = "Age.adjusted.Death.Rate" )
teen_death <- teen_death %>% select (Year, State, Cause.Name, deathrate) %>% filter(Year >=1999 & State %in% c("louisiana") &
                                                                                      Cause.Name=="Kidney disease") 
teen_past_flu <- teen_death %>% select (Year, State, Cause.Name, deathrate) %>% filter(Year == 2000 & Cause.Name=="Kidney disease") %>% arrange(-deathrate)
teen_ten_flu <- teen_death %>% select (Year, State, Cause.Name, deathrate) %>% filter(Year == 2010  & Cause.Name=="Kidney disease") %>% arrange(-deathrate)
View(teen_past_flu)
# 회귀 계수
View(teen_death)
# 플로리다의 10대 빈곤률과 폐렴 사망률과의 관계
povdata <- data.frame(teen_death$deathrate, floridamean$floridamean) 
options(digits=1)
mean.x <- mean(povdata$teen_death.deathrate)
mean.y <- mean(povdata$floridamean.floridamean)
sxy <- sum((povdata$teen_death.deathrate- mean.x)*(povdata$floridamean.floridamean-mean.y))
sxx <- sum((povdata$teen_death.deathrate-mean.x)^2)
(b1 <- sxy/sxx)
(b0 <- mean.y - b1*mean.x)
lm(floridamean.floridamean ~ teen_death$deathrate, data=povdata)
# 표본상관계수
cor.test(teen_death$deathrate, floridamean$floridamean)
# 회귀식 그래프 표현
out=lm(floridamean$floridamean ~ teen_death$deathrate, data=povdata)
summary(out)
plot(floridamean$floridamean ~ teen_death$deathrate, main="1999~2016년 루이지아나의 빈곤률과 신장병 사망률과의 관계",xlab="10대 아이의 사망율(2010년~2016년 까지)", ylab="루이지아나의 빈곤율" )
abline(out, col="red")
require(ggplot2)
ggplot(data=povdata, aes(x=teen_death$deathrate, y=floridamean$floridamean)) + geom_count()+geom_smooth(method="lm") + labs(x = "신장병 사망률", y="빈곤률", title="빈곤률과 신장병 사망률과의 관계") + theme(plot.title = element_text(family = "malgun", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
#-------------
library(extrafont)
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))


install.packages("car")
require(car)
# 다항 회귀식
ggplot(povdata,aes(x=teen_death$deathrate,y=floridamean$floridamean))+geom_point()+geom_smooth(colour="red")+geom_smooth(method="lm",colour="green",se=FALSE)

#인터랙티브 그래프-----------------------------------------------------------------------------------------------------------------------
# 몬타나의 10대 아이 사망률과 빈곤률과의 관계
mean1 <- allPoverty %>% select(State, allPov2016, allPov2015,allPov2014,allPov2013,allPov2012,allPov2011,
                               allPov2010,allPov2009,allPov2008,allPov2007,allPov2006,allPov2005,allPov2004,allPov2003,allPov2002,
                               allPov2001,allPov2000,allPov1999) %>% filter(State %in% c("montana")) 
# 1999~2016년 플로리다 10대 (0~17세) 빈곤한 아이 수
mean2 <- allPoverty %>% select(State, allPov2016, allPov2015,allPov2014,allPov2013,allPov2012,allPov2011,
                               allPov2010,allPov2009,allPov2008,allPov2007,allPov2006,allPov2005,allPov2004,allPov2003,allPov2002,
                               allPov2001,allPov2000,allPov1999) %>% filter(State %in% c("united states")) 
# 1999~2016년 미국 전역에 10대 빈곤한 아이 수
View(mean2)
View(mean1)
mean1 <- as.numeric(mean1)
mean2 <- as.numeric(mean2)
options(digits=3)
monmean <- (mean1/mean2)*100 
# 미국전역에 대한 몬타나의 10대 아이 빈곤률 
monmean <- data.frame(monmean)
monmean <- monmean[-1,]
View(monmean)
monmean <- t(monmean)
#년도 추가
y <- c(1999:2016)
monmean <- cbind(monmean, Year=y)
View(monmean)
monmean <- data.frame(monmean)

View(teen_death)
library(xts)
teen_death <- rename(teen_death, date = "Year" )
monmean <- rename(monmean, date= "Year")
p <- ggplot(data=povdata, aes(x=teen_death$deathrate,y=monmean$monmean)) + geom_bar(position="dodge")
library(plotly)
ggplotly(p)

# 2007~2017년까지 미국 전역에 실업률 표 불러오기 (실업률에 따른 사망원인 관련)-----------------------------------------------------------
unemploy <- read_excel("d:/workspace/R/project1/excel/Unemployment.xls")
View(unemploy)
unemploy <- data.frame(unemploy)
unemploy$X__2 <- tolower(unemploy$X__2)
unemploy <- rename(unemploy, State = "X__2", unemp2007 = "X__9",unemp2008 = "X__13",unemp2009 = "X__17",unemp2010 = "X__21"
                     ,unemp2011 = "X__25",unemp2012 = "X__29",unemp2013 = "X__33" , unemp2014 = "X__37",unemp2015 = "X__41",
                   unemp2016 = "X__45",unemp2017 = "X__49")
View(unemploy)


# 2010~2017년까지 미국 전역 인구의 출생률과 사망률 

#----------------------------------------------------------------------------------------------------------------------------------------
cancer <- teen_death %>% filter(State=="montana" & Cause.Name=="Cancer" & Year>=1999)
cancer1 <- teen_death %>% filter(State=="wyoming" & Cause.Name=="Cancer" & Year>=1999)
cancer2 <- teen_death %>% filter(State=="new mexico" & Cause.Name=="Cancer" & Year>=1999)
cancer3 <- teen_death %>% filter(State=="nevada" & Cause.Name=="Cancer" & Year>=1999)
View(cancer)
options(digits=3)

p <- ggplot(data=cancer, aes(x=Year, y=Deaths, color="rating")) + geom_line(color="orangered",size=3 ,alpha=0.5) + geom_point(size=4, aes(colour=factor(State)),shape = 21, colour = "red", fill = "white", size = 2, stroke = 2) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "몬타나의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
p1 <- ggplot(data=cancer1, aes(x=Year, y=Deaths))  + geom_line(color="orangered",size=3 ,alpha=0.5)+ geom_point(size=4, aes(colour=factor(State)),shape = 21, colour = "red", fill = "white", size = 2, stroke = 2) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "와이오밍의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
p2 <- ggplot(data=cancer2, aes(x=Year, y=Deaths))+ geom_line(color="orangered",size=3 ,alpha=0.5) + geom_point(size=4, aes(colour=factor(State)),shape = 21, colour = "red", fill = "white", size = 2, stroke = 2) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "뉴 멕시코의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
p3 <- ggplot(data=cancer3, aes(x=Year, y=Deaths)) + geom_line(color="orangered",size=3 ,alpha=0.5)+ geom_point(size=4, aes(colour=factor(State)),shape = 21, colour = "red", fill = "white", size = 2, stroke = 2) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "네바다의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
p3

library(gridExtra)
grid.arrange(p,p1,p2,p3)

#------
# 자살률이 낮았던 지역 4개의 암 사망자 수 그래프-----------------------------------------------------------------------------------------
View(teen_death)
lcancer <- teen_death %>% filter(State=="new york" & Cause.Name=="Cancer" & Year>=1999)
lcancer1 <- teen_death %>% filter(State=="illinois" & Cause.Name=="Cancer" & Year>=1999)
lcancer2 <- teen_death %>% filter(State=="new jersey" & Cause.Name=="Cancer" & Year>=1999)
lcancer3 <- teen_death %>% filter(State=="massachusetts" & Cause.Name=="Cancer" & Year>=1999)
View(cancer)
options(digits=3)

par(mfrow=c(2,2))
lp <- ggplot(data=lcancer, aes(x=Year, y=Deaths)) + geom_line(color="gold",size=3 ,alpha=0.5) + geom_point(size=4, aes(colour=factor(State)), shape = 21, colour = "orange", fill = "white", size = 2, stroke = 2) + scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "뉴욕의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
lp1 <- ggplot(data=lcancer1, aes(x=Year, y=Deaths)) + geom_line(color="gold",size=3 ,alpha=0.5)+ geom_point(size=4, aes(colour=factor(State)),shape = 21, colour = "orange", fill = "white", size = 2, stroke = 2) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "일리노이주의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
lp2 <- ggplot(data=lcancer2, aes(x=Year, y=Deaths))+ geom_line(color="gold",size=3 ,alpha=0.5) + geom_point(size=4, aes(colour=factor(State)),shape = 21, colour = "orange", fill = "white", size = 2, stroke = 2) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "뉴 저지의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
lp3 <- ggplot(data=lcancer3, aes(x=Year, y=Deaths))+ geom_line(color="gold",size=3 ,alpha=0.5) + geom_point(size=4, aes(colour=factor(State)),shape = 21, colour = "orange", fill = "white", size = 2, stroke = 2) + 
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +  labs(title = "마사추세츠의 10대 아이 암 사망자수 ") + xlab("연도")+ ylab("암 사망자 수 ") + (theme(plot.title = element_text(family = "", face = "bold", size=15, color="DarkBlue", hjust=0.5)))
library(gridExtra)
grid.arrange(lp,lp1,lp2,lp3)
?grid.arrange
library(viridis)

#---------------------------------------------------------------------------------------------------------------------------
