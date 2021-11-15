library(dplyr)
library(funModeling)
library(ggplot2)

setwd('C:/Users/Windows/Desktop/AAP/TP2')
getwd()
df=read.csv("datos.csv",header=TRUE,sep=",")
df_test = read.csv("df_test.csv", header=TRUE, sep=",")

head(df)
dim(df)
str(df)
library(tidyr)
library(dplyr)

df$seasonNumber[df$seasonNumber == ""] <- "\\N"

df$seasonNumber[df$seasonNumber == ""] <- "N/A"

df$seasonNumber=as.numeric(df$seasonNumber)

df$seasonNumber[is.na(df$seasonNumber)]=median(df$seasonNumber,na.rm=TRUE)



  x

table(df$endYear)

table(df$episodeNumber)

df$episodeNumber[df$episodeNumber == ""] <- "\\N"

df$episodeNumber[df$episodeNumber == ""] <- "N/A"

df$episodeNumber<-as.numeric(df$episodeNumber)

df$episodeNumber[is.na(df$episodeNumber)]=median(df$episodeNumber,na.rm=TRUE)

table(df$runtimeMinutes)

df$runtimeMinutes[df$runtimeMinutes == ""] <- "\\N"

df$runtimeMinutes[df$runtimeMinutes == ""] <- "N/A"


df$runtimeMinutes=as.numeric(df$runtimeMinutes)

df$runtimeMinutes[is.na(df$runtimeMinutes)]=median(df$runtimeMinutes,na.rm=TRUE)

mean(is.na(df$runtime))

df$endYear=NULL
df$language=NULL
df$original_language=NULL
df$runtime=NULL
df$budget=NULL
df$popularity=NULL
df$revenue=NULL
df$isOriginalTitle=NULL
df$endYear=NULL
df$overview=NULL
df$production_companies=NULL
df$production_countries=NULL
df$attributes=NULL
df$runtime=NULL 
df$release_date=NULL
df$genres_y=NULL
df$status=NULL
df$video=NULL
df$tagline=NULL
df$adult=NULL


freq(df$titleType)
table(df$titleType)
df$titleType[df$titleType == ""] <- "tvEpisode"

df$Comedy<- ifelse(grepl('Comedy',df$genres_x, fixed = TRUE),1,0)

df$Drama<- ifelse(grepl('Drama',df$genres_x, fixed = TRUE),1,0)

df$Adventure<- ifelse(grepl('Adventure',df$genres_x, fixed = TRUE),1,0)

df$Reality<- ifelse(grepl('Reality',df$genres_x, fixed = TRUE),1,0)

df$Short<- ifelse(grepl('Short',df$genres_x, fixed = TRUE),1,0)

df$Documentary<- ifelse(grepl('Documentary',df$genres_x, fixed = TRUE),1,0)

df$Crime<- ifelse(grepl('Crime',df$genres_x, fixed = TRUE),1,0)

df$Animation<- ifelse(grepl('Animation',df$genres_x, fixed = TRUE),1,0)

df$Music<- ifelse(grepl('Music',df$genres_x, fixed = TRUE),1,0)

df$genres_x=NULL
df$types=NULL

#install.packages("fastDummies")
library(fastDummies)
df<-dummy_cols(df, select_columns = "titleType")
df$titleType=NULL

###df_test 
table(df_test$isAdult)

df_test$'numVotes'[df_test$'numVotes' == "\\N"] <- NA
df_test$'titleType'[df_test$'titleType' == " "] <- NA
df_test$'genres_x'[df_test$'genres_x' == "\\N"] <- NA
df_test$'seasonNumber'[df_test$'seasonNumber' == ""] <- NA
df_test$'startYear'[df_test$'startYear' == "\\N"] <- NA
df_test$'runtimeMinutes'[df_test$'runtimeMinutes' == "\\N"] <- NA
df_test$'episodeNumber'[df_test$'episodeNumber' == " "] <- NA


# 
# df <- df %>% left_join(budget,by='startYear')
# 
# 
# df$budget <- ifelse(is.na(df$budget),df$median_budget,df$budget)
# 
# mean(df$budget[df$startYear==2019],na.rm=T)/mean(df$budget[df$startYear==2018],na.rm=T)

df_test$endYear = NULL
df_test$language = NULL
df_test$original_language = NULL
df_test$overview = NULL
df_test$production_companies = NULL
df_test$production_countries = NULL
df_test$release_date = NULL
df_test$status = NULL
df_test$tagline = NULL
df_test$video = NULL
df_test$isOriginalTitle = NULL
df_test$budget = NULL
df_test$popularity = NULL
df_test$revenue = NULL
df_test$runtime = NULL
df_test$types = NULL
df_test$adult = NULL
df_test$attributes = NULL
df_test$genres_y = NULL
df_test$genres_x = NULL
# 
# df %>%
#   group_by(directors) %>%
#   summarise(movies=n(),avg_score=mean(averageRating)) %>%
#   filter(movies>10) %>% 
#   arrange(desc(avg_score)

df_test$Comedy<- ifelse(grepl('Comedy',df_test$genres_x, fixed = TRUE),1,0)

df_test$Drama<- ifelse(grepl('Drama',df_test$genres_x, fixed = TRUE),1,0)

df_test$Adventure<- ifelse(grepl('Adventure',df_test$genres_x, fixed = TRUE),1,0)

df_test$Reality<- ifelse(grepl('Reality',df_test$genres_x, fixed = TRUE),1,0)

df_test$Short<- ifelse(grepl('Short',df_test$genres_x, fixed = TRUE),1,0)

df_test$Documentary<- ifelse(grepl('Documentary',df_test$genres_x, fixed = TRUE),1,0)

df_test$Crime<- ifelse(grepl('Crime',df_test$genres_x, fixed = TRUE),1,0)

df_test$Animation<- ifelse(grepl('Animation',df_test$genres_x, fixed = TRUE),1,0)

df_test$Music<- ifelse(grepl('Music',df_test$genres_x, fixed = TRUE),1,0)


df_test$Comedy[is.na(df_test$Comedy)]<-0
df_test$Drama[is.na(df_test$Drama)]<-0
df_test$Documentary[is.na(df_test$Documentary)]<-0
df_test$Crime[is.na(df_test$Crime)]<-0
df_test$Animation[is.na(df_test$Animation)]<-0
df_test$Adventure[is.na(df_test$Adventure)]<-0
df_test$Reality[is.na(df_test$Reality)]<-0
df_test$Music[is.na(df_test$Music)]<-0
df_test$Short[is.na(df_test$Short)]<-0

# df$titleType <- as.factor(df$titleType)
# 
# df %>% group_by(titleType) %>%
#   summarise(rating=mean(averageRating),count=n()) %>% 
#   arrange(desc(rating))
# 
# # df <- df %>% filter(titleType %in% c('tvEpisode','video','movie','tvMovie','short','tvSeries'))

df_test$genres_x = NULL

df_test<-dummy_cols(df_test, select_columns = "titleType")
df_test$titleType=NULL

df_test$runtimeMinutes <- as.numeric(df_test$runtimeMinutes)
df_test$runtimeMinutes[is.na(df_test$runtimeMinutes)]=median(df_test$runtimeMinutes,na.rm=TRUE)
df_test$seasonNumber <- as.numeric(df_test$seasonNumber)
df_test$seasonNumber[is.na(df_test$seasonNumber)] = median(df_test$seasonNumber,na.rm=TRUE)
df_test$episodeNumber <- as.numeric(df_test$episodeNumber)
df_test$episodeNumber[is.na(df_test$episodeNumber)] = median(df_test$episodeNumber,na.rm=TRUE)
df_test$numVotes[is.na(df_test$numVotes)] = median(df_test$numVotes,na.rm=TRUE)

##
#df_test$ordering[is.na(df_test$ordering)] = mean(df_test$ordering,na.rm=TRUE)
#df_test$isAdult <- as.factor(df_test$isAdult)#
#
##########################################

library(caret)
set.seed(770);particion=createDataPartition(y=df$averageRating,p=0.3,list=FALSE)
entrenar=df[particion,]
test=df[-particion,]
write.csv(entrenar,file = "entrenar1.csv",row.names = F)
write.csv(test,file = "test1.csv",row.names = F)
write.csv(df_test,file = "resultados1.csv",row.names = F)

###########################################

