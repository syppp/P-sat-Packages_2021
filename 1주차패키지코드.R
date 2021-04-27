#########
###ch1###
#########
library(plyr)
library(tidyverse)
library(data.table)

#0. 
setwd('C:/Users/samsung/Desktop/대학교/4학년 2학기/피셋/패키지/1주차')
getwd()


data <- fread('data.csv',data.table = FALSE, header = TRUE)


#1.데이터 확인하기
data %>%str()
colSums(is.na(data))
sum(is.na(data))

data$patient_id %>% unique() %>% length()  
data$sex %>% unique() 
data$age %>% unique()  

data$country %>% unique() 
data$province %>% unique()
data$city %>% unique() 
data$confirmed_date %>% unique() 
data$state %>% unique()


#2-1 
data <- na.omit(data)

#2-2
data <- data[-which(data$sex == "" | data$age == ""  | data$city == ""),]

data$sex %>% unique() 
data$age %>% unique()  
data$city %>% unique() 
data$confirmed_date %>% unique() %>% length() 

#3. 
data <- data %>% filter(data$country == 'Korea') %>% dplyr::select(-4)

#4.

data$province <- data$province %>%
  plyr::revalue(c('서울' = '서울특별시','부산'= '부산광역시','대구'='대구광역시','인천'='인천광역시',
                  '대전'='대전광역시','세종'='세종특별자치시','울산'='울산광역시','제주도'='제주특별자치도'))

#5. 
data$confirmed_date <- as.Date(data$confirmed_date)

#6. 
data <- data %>% group_by(confirmed_date) %>% dplyr::mutate(confirmed_number = n()) %>% ungroup()

#7. 
data <- data %>% mutate(wday =ifelse(wday(data$confirmed_date)==1|wday(data$confirmed_date)==6 , "주말","주중") )

#8. 
with(data %>% group_by(age,confirmed_date) %>% dplyr::summarise(n = n()),
     tapply(n, age, summary))

#########
###ch2###
#########
#1. line plot
data %>% ggplot() + geom_line(aes(confirmed_date,confirmed_number),color= 'skyblue') +
  annotate("text", x=data$confirmed_date[which.max(data$confirmed_number)], 
           y=143,fontface=2,label="2020-##-##(#명)",hjust = 1.1, size = 4.6 , color = "navy") +
  geom_point(mapping =aes(x =confirmed_date[which.max(confirmed_number)] ,y = 143 ), color="navy", size =2)+
  theme_classic() + labs(title = "코로나 확진자수 추이\n -국내인 기준") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 17, color = "black"))  

  
#1-2. line plot
data %>% ggplot() + geom_line(aes(confirmed_date,confirmed_number, group = province,color = province))+
  facet_wrap(~province)


#2. bar plot
data %>%  group_by(province,state) %>% dplyr::summarise(n = n()) %>%
  ggplot(aes(x = reorder(province,n), y = n, fill = state,color = state)) +
  geom_bar(stat = 'identity',alpha = 0.3) + coord_flip()+
  ylab("확진자 수") + xlab("지역")


data %>%  group_by(province) %>% dplyr::summarise(n = n()) %>%
  ggplot(aes(x = reorder(province,n), y = n)) +
  geom_bar(stat = 'identity',alpha = 0.3) + coord_flip()+
  ylab("확진자 수") + xlab("지역")


#3.box plot
data %>% group_by(age,confirmed_date) %>% dplyr::summarise(n = n())%>% 
  ggplot(aes(x = as.factor(age),
             y = n,
             fill = age,
             color = age))+
  stat_boxplot(geom = "errorbar",width=0.7)+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  theme_classic() + xlab("age") +ylab("일단위 확진자수")


#3-2
summary(aov(n ~ age, data = data %>%  group_by(age,province) %>% dplyr::summarise(n = n())))

#4. 
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

h <- readOGR('CTPRVN_202101/TL_SCCO_CTPRVN.shp')
h <- fortify(h, region = 'CTP_KOR_NM')

map <-left_join(h, data%>% group_by(province) %>% summarise( n = n()), by = c('id'= 'province'))


ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group,fill = n)) +
  ggtitle('지역별 누적 확진자 수') +
  scale_fill_gradient(low = 'white', high = 'red') 

#########
###ch3###
#########

library(MASS)

reg <- Boston

#1
corrplot::corrplot(cor(reg),method= "number",type = 'upper')

#2
reg %>%
  gather(key, val, -medv) %>%
  ggplot(aes(x = val, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "skyblue") +
  facet_wrap(~key, scales = "free") +
  theme_light() +
  ggtitle("Scatter plot of dependent variables vs Median Value (medv)") 

#3
set.seed(1234)

index <- caret::createDataPartition(
  reg$medv,    
  times=1,    
  p=0.3,      
  list=FALSE,
)

train <-reg[-index,]
val <- reg[index,]

#3-2

model1<- lm(medv~., train)
summary(model1)

#3-3
pred1 <- predict(model1, val)
MLmetrics::RMSE(pred1,val$medv)

#4 coefficients plot 


df <- data.frame(name = names(coefficients(model1)),
                 value = coefficients(model1)) 

df %>% ggplot(aes(x = reorder(name, value), y = value)) + 
  geom_col(aes(fill = value, color = value), alpha = 0.2) + 
  coord_flip() +
  geom_text(aes(label = round(value,2)), size = 4.5, position = position_stack(0.5)) + 
  scale_fill_gradient2(low = "blue",mid= "yellow",
                       high = "red",
                       midpoint = 0, 
                       limit = c(min(df$value),max(df$value))) +
  scale_color_gradient2(low = "blue",mid= "yellow",
                        high = "red",
                        midpoint = 0, 
                        limit = c(min(df$value),max(df$value))) +
  theme_classic() + theme(legend.position = 'none') +xlab('intercept and variables')
