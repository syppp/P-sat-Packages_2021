setwd('C:/Users/samsung/Desktop/대학교/4학년 2학기/피셋/패키지/3주차')

library(tidyverse)
library(data.table)

###############
##데이터 제공##
###############

#hospital <- fread('healthcare-dataset-stroke-data.csv', data.table = FALSE)
#set.seed(1234)

#index <- caret::createDataPartition(
#  as.factor(hospital$stroke),    
#  times=1,    
#  p=0.2,      
#  list=FALSE,
#)

#data <- hospital[-index,] #210
#test <- hospital[index,] #91

#write.csv(data,'data.csv',row.names = FALSE)
#write.csv(test,'test.csv',row.names = FALSE)

#as.factor(data$stroke) %>% summary() #0: 3888, 1: 199
#as.factor(test$stroke) %>% summary() #0 : 973, 1: 50

#############################
###chapter1. 전처리 및 EDA###
#############################
###문제 0

data <-  fread('data.csv', data.table = FALSE)
test <-  fread('test.csv', data.table = FALSE)

lapply(data, unique)
##########
###data###
##########

###문제 1. bmi numeric으로 바꾸고 mean imputation
data$bmi <- as.numeric(data$bmi)

sum(is.na(data))
colSums(is.na(data))

data$bmi <- replace(data$bmi, is.na(data$bmi), mean(data$bmi,na.rm = TRUE))

### 문제 2. factor 처리
data[,unlist(lapply(data, is.character))]  <- lapply(select_if(data,is.character), as.factor) 


### 문제 3. id 제거
data <- data %>% select(-1)


###EDA 1. stroke 별 분포(범주형)

a <- data%>% filter(stroke==1) %>% select(-c(11,2,8,9)) %>%
  gather(key,value) %>% ggplot(aes(key, fill=value))+ 
  theme(legend.position = "bottom",panel.background = element_rect(fill='white'),
        plot.title = element_text(hjust =0.5),legend.title = element_blank(),
        axis.line = element_line(colour='black')) +
  geom_bar(position='fill',alpha = 0.5) +ggtitle("Stroke : 1") +xlab("variable") + ylab("") +
  coord_flip()

b <- data%>% filter(stroke==0) %>% select(-c(11,2,8,9)) %>%
  gather(key,value) %>% ggplot(aes(key, fill=value)) + 
  theme(legend.position = "bottom",panel.background = element_rect(fill='white'),
        plot.title = element_text(hjust =0.5),legend.title = element_blank(),
        axis.line = element_line(colour='black')) +
  geom_bar(position='fill',alpha = 0.5)+ ggtitle("Stroke : 0") +xlab("variable") + ylab("") +
  coord_flip()

gridExtra::grid.arrange(a,b,ncol = 2)


###EDA 2. stroke 별 분포(age,avg_glucose_level,bmi)
a <- data %>% filter(stroke == 1) %>% select(c(2,8,9)) %>% gather(key,value) %>%
  ggplot() + geom_density(mapping=aes(x=value, colour = key)) +
  theme(panel.background = element_rect(fill='white'),
        plot.title = element_text(hjust =0.5),legend.title = element_blank(),
        axis.line = element_line(colour='black')) +
  ggtitle("Stroke : 1") +xlab("variable")

b <- data %>% filter(stroke == 0) %>% select(c(2,8,9)) %>% gather(key,value) %>%
  ggplot() + geom_density(mapping=aes(x=value, colour = key)) +
  theme(panel.background = element_rect(fill='white'),
        plot.title = element_text(hjust =0.5),legend.title = element_blank(),
        axis.line = element_line(colour='black')) +
  ggtitle("Stroke : 0") +xlab("variable")

gridExtra::grid.arrange(a,b)

### 문제 6. 카이스퀘어 독립성검정

df <- data.frame(cate_Var =  data[,-c(2,8,9,11)] %>% colnames,
                 chi = rep(NA,7))
for (i in 1:7){
  x <- xtabs(~stroke + get(df$cate_Var[i]) , data = data) %>% chisq.test()
  df$chi[i] <- ifelse(x$p.value>0.05, "accept","denied")
}


### 문제 7.변수 정제

data <- data %>% select(-df[which(df$chi == "accept"),1])


##########
###test###
##########

###bmi numeric으로 바꾸기
test$bmi <- as.numeric(test$bmi)

sum(is.na(test))
colSums(is.na(test))

###bmi mean imputation
test$bmi <- replace(test$bmi, is.na(test$bmi), mean(test$bmi,na.rm = TRUE))

###factor 처리
test[,unlist(lapply(test, is.character))]  <- lapply(select_if(test,is.character), as.factor) 


###id 및 변수 제거
test <- test %>% select(-c(1,2,8))
#######################
###chapter2.catboost###
#######################
library(catboost)
library(caret)
library(MLmetrics)

###문제 1. gridsearch df


cv_param <- expand.grid(depth = c(4,6,8), iterations = c(100,200))
logloss <- rep(NA,6)
logloss_cb <- cbind(cv_param,logloss)

### 문제2,3 gridsearch1

set.seed(1234)
n_split <-5
cv <- caret::createFolds(as.factor(data$stroke),k=n_split)

start <- Sys.time()
for (j in 1:6) {
  logloss_result <- c()
  for( i in 1:n_split){
    idx <- cv[[i]]
    train_x<- data[-idx,]
    val_x <- data[idx,]
    
    train_pool <- catboost.load_pool(
      data = train_x[,-9],
      label = train_x[,9]
    )
    
    val_pool <- catboost.load_pool(
      data = val_x[,-9],
      label = val_x[,9]
    )
    set.seed(1234) 
    catboost <- catboost.train(learn_pool=train_pool, 
                                 params = list(iterations = logloss_cb$iterations[j],
                                               loss_function = 'Logloss',
                                               depth = logloss_cb$depth[j],
                                               od_type = 'Iter'))
    
    logloss_temp <- LogLoss(catboost.predict(catboost,val_pool, prediction_type = 'Class'), val_x$stroke)
    logloss_result <- c(logloss_temp, logloss_result)
  }
  logloss_cb$logloss[j] <- mean(logloss_result)
}
CatBoost_Runtime <- Sys.time() - start  #2.048295 mins

logloss_cb
logloss_cb[which.min(logloss_cb$logloss),] #1.74091


### 문제 4. test

train_pool <- catboost.load_pool(
  data = data[,-9],
  label = data[,9]
)

test_pool <- catboost.load_pool(
  data = test[,-9],
  label = test[,9]
)

set.seed(1234) 
catboost <- catboost.train(learn_pool=train_pool,params = list(iterations = 100,
                                                               loss_function = 'Logloss',
                                                               depth = logloss_cb$depth[1]))

LogLoss(catboost.predict(catboost,test_pool, prediction_type = 'Class'), test$stroke) #1.721875



#########################
###chapter3.clustering###
#########################
library(factoextra)
library(cluster)

set.seed(1234)
###kmeans(age, avg_glucose_level, bmi)
cluster_1 <- scale(data[,c(1,6,7)]) %>% as_tibble()  
fviz_nbclust(x = cluster_1, FUNcluster = kmeans, method='wss') 
fviz_nbclust(x = cluster_1, FUNcluster = kmeans, method = "silhouette")

kmeans1 <- kmeans(cluster_1, nstart = 1, iter.max = 30, centers = 3)
fviz_cluster(kmeans1, cluster_1, ggtheme = theme_classic())

###boxplot

data$cluster <- kmeans1$cluster

p1 <- data %>% 
  ggplot(aes(x = as.factor(cluster), y = age, color = as.factor(cluster), fill = as.factor(cluster))) +
  geom_boxplot(outlier.shape = NA,alpha = 0.6) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  stat_boxplot(geom = "errorbar",width=0.7) + 
  xlab('cluster')+
  scale_fill_manual(values = c("#845ec2","#ffc75f", "#ff5e78")) +
  scale_color_manual(values = c("#845ec2","#ffc75f", "#ff5e78"))


p2 <- data %>% 
  ggplot(aes(x = as.factor(cluster), y = avg_glucose_level, color = as.factor(cluster), fill = as.factor(cluster))) +
  geom_boxplot(outlier.shape = NA,alpha = 0.6) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  stat_boxplot(geom = "errorbar",width=0.7) + 
  xlab('cluster')+
  scale_fill_manual(values = c("#845ec2","#ffc75f", "#ff5e78")) +
  scale_color_manual(values = c("#845ec2","#ffc75f", "#ff5e78"))

p3 <- data %>% 
  ggplot(aes(x = as.factor(cluster), y = bmi, color = as.factor(cluster), fill = as.factor(cluster))) +
  geom_boxplot(outlier.shape = NA,alpha = 0.6) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  stat_boxplot(geom = "errorbar",width=0.7) + 
  xlab('cluster')+
  scale_fill_manual(values = c("#845ec2","#ffc75f", "#ff5e78")) +
  scale_color_manual(values = c("#845ec2","#ffc75f", "#ff5e78"))

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

