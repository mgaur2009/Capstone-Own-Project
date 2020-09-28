######Install all needed packages if not present

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(caret)) install.packages("caret")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")
if(!require(corrplot)) install.packages("corrplot")
if(!require(data.table)) install.packages("data.table")
invisible(gc())
invisible(memory.limit(size = 56000))
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
###Loading all the needed libraries
library(tidyverse)
library(caret)
library(data.table)
library(corrplot)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")
head(movielens)



# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Spliting of edx into train and test data for the purpose of building model

# Training set will be 90 % of edx data and test set will be 10% of edx data

set.seed(1, sample.kind = "Rounding")
test_index<-createDataPartition(edx$rating,times=1,p=0.1, list=FALSE)
train_set<-edx[-test_index,]
temp_1<-edx[test_index,]

#Ensure userId and MovieId in the test set are also in train set

test_set<-temp_1%>%semi_join(train_set,by="movieId")%>%semi_join(train_set,by="userId")

# Add rows removed from test set back into train set

removed<-anti_join(temp_1,test_set)
train_set<-rbind(train_set,removed)

rm(removed,temp_1,test_index)


# Data Cleaning

 # No NA value present in our data

str(edx)
any(is.na(edx))

head(edx)


n_distinct(edx$movieId)
n_distinct(edx$userId)

mean(edx$rating)
median(edx$rating)


### Add attributes year of release and year in which movie was rated

edx<-edx%>%mutate(year=as.numeric(str_sub(title,-5,-2)))%>%
  mutate(rated_year=year(as_datetime(timestamp)))%>%
  mutate(age=rated_year-year)
head(edx)

train_set<-train_set%>%mutate(year=as.numeric(str_sub(title,-5,-2)))
train_set<-train_set%>%mutate(rated_year=year(as_datetime(timestamp)))
train_set<-train_set%>%mutate(age=rated_year-year)
head(train_set)
test_set<-test_set%>%
  mutate(year=as.numeric(str_sub(title,-5,-2)),rated_year=year(as_datetime(timestamp)),age=rated_year-year)



######Data Visualization
# Visualize correlation between numeric variables of the edx data set

cor.data<-cor(edx[,c("userId","movieId","rating","timestamp","year","rated_year","age")])
cor.data
corrplot(cor.data,method='color')

# Visualize distribution of the ratings for each of the attributes

##Distribution of ratings


edx%>%group_by(rating)%>%summarize(n=n())%>%
  ggplot(aes(rating,n))+geom_bar(stat="identity",alpha=.5, color="black",fill="red")+theme_bw()+
  scale_x_continuous(breaks=seq(min(0),max(5),by=0.5))+
  ylab("No of Ratings")+geom_vline(xintercept = 3.512)+
  geom_text(aes(x=3.512,label="mean rating", y=2000000),angle=90)

prop.table(table(as.factor(train_set$rating)))                                 

####Visualization of Ratings by Movies

edx%>%group_by(movieId)%>%
  summarize(n=n())%>%ggplot(aes(n))+
  geom_histogram(bins = 30,color="red",fill="blue")+
  scale_x_log10()+xlab("no of ratings")+ylab('no of movies')

edx%>%group_by(movieId)%>%
  ggplot(aes(movieId))+geom_histogram(bins = 20,alpha=.5)

edx%>%group_by(movieId)%>%
  summarize(mean_ratings=mean(rating))%>%
  ggplot(aes(mean_ratings))+
  geom_histogram(bins =  20, color="black")


####Visualization of ratings by users


edx%>%group_by(userId)%>%
  summarize(n=n())%>%ggplot(aes(n))+
  geom_histogram(bins = 30,color="red",fill="blue")+
  scale_x_log10()+xlab("no of ratings")+ylab('no of users')


edx%>%group_by(userId)%>%
  summarize(mean_ratings=mean(rating))%>%
  ggplot(aes(mean_ratings))+
  geom_histogram(bins=20,color="red")



#####Visualization and analysis of Release year, rating year and Age of Movie
##1. Year
edx%>%group_by(year)%>%
  summarize(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(bins=20,color="black")+
  xlab("no of ratings")+ylab("no of years")

edx%>%group_by(year)%>%
  summarize(mean_rating=mean(rating))%>%
  ggplot(aes(year,mean_rating))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept = 3.51)

edx%>%group_by(year)%>%
  count()%>%
  ggplot(aes(year,n,color="red"))+
  geom_line()


###2. rating year
edx%>%group_by(rated_year)%>%
  summarize(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(bins=20,color="black")+
  xlab("no of ratings")+ylab("no of rated years")

edx%>%group_by(rated_year)%>%
  summarize(mean_rating=mean(rating))%>%
  ggplot(aes(rated_year,mean_rating))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept = 3.51)

edx%>%group_by(rated_year)%>%
  count()%>%
  ggplot(aes(rated_year,n))+
  geom_bar(stat = "identity")


##3.
edx%>%group_by(age)%>%
  summarize(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(bins=20,color="black")+
  xlab("no of ratings")+ylab("Movie Ages")

edx%>%group_by(age)%>%
  summarize(mean_rating=mean(rating))%>%
  ggplot(aes(age,mean_rating))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept = 3.51)

edx%>%group_by(age)%>%
  count()%>%
  ggplot(aes(age,n))+
  geom_bar(stat="identity")+ylab("No of ratings")

### Visualization of Ratings distribution by Genres

n_distinct(train_set$genres)
  
edx%>%group_by(genres)%>%
  summarize(n=n())%>%arrange(desc(n))%>%
  head(10)

edx%>%group_by(genres)%>%
  summarize(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(bins=20,color="black",fill="red")+
  xlab("no of ratings")+ylab("No of Genres")

edx%>%group_by(genres)%>%
  summarize(n=n(),mean_rating=mean(rating))%>%
  top_n(50,n)%>%
  ggplot(aes(mean_rating,genres))+
  geom_bar(stat="identity",color="blue",fill="red",show.legend = FALSE)+
  geom_col()+xlab("No of ratings")+
  geom_vline(xintercept = 3.51)
  
##########Building Prediction Model#########
######Define RMSE

RMSE<-function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

# Basic model using average rating
mu<-mean(train_set$rating)

##### naive RMSE

basic_RMSE<-RMSE(mu,test_set$rating)
basic_RMSE
rmse_calculations<-data.frame(method="Using just the mean rating", RMSE=basic_RMSE)
rmse_calculations%>%knitr::kable("pipe")

## AGE EFFECT ONLY

age_avgs<-train_set%>%group_by(age)%>%summarize(b_a=mean(rating-mu))
age_avgs%>%ggplot(aes(b_a))+geom_histogram(bins=30,alpha=.5,color="red")
predicted_ratings<-mu+test_set%>%left_join(age_avgs,by="age")%>%pull(b_a)
age_effect_rmse<-RMSE(predicted_ratings,test_set$rating)
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Age effect",RMSE=age_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")

#### Movie effect only

mu<-mean(train_set$rating)

movie_averages<-train_set%>%
  group_by(movieId)%>%summarize(b_i=mean(rating-mu))

movie_averages%>%ggplot(aes(b_i))+
  geom_histogram(bins = 20,alpha=0.5,color="black",fill="red")+
  theme_bw()

predicted_ratings<-mu+test_set%>%
  left_join(movie_averages,by="movieId")%>%
  pull(b_i)

movie_effect_rmse<-RMSE(predicted_ratings,test_set$rating)
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie effect",RMSE=movie_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")


#### Adding User Effect
mu<-mean(train_set$rating)
mu
train_set%>%group_by(userId)%>%
  summarize(b_u=mean(rating))%>%
  ggplot(aes(b_u))+geom_histogram(bins=20,color='black')

user_averages<-train_set%>%
  left_join(movie_averages,by="movieId")%>%
  group_by(userId)%>%summarize(b_u=mean(rating-mu-b_i))

predicted_ratings<-test_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  mutate(pred=mu+b_i+b_u)%>%pull(pred)

movie_user_effect_rmse<-RMSE(predicted_ratings,test_set$rating)
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie+User effect",RMSE=movie_user_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")


#### Adding Year effect

train_set%>%group_by(year)%>%
  summarize(b_y=mean(rating))%>%
  ggplot(aes(b_y))+geom_histogram(bins=20,color="black")

year_averages<-train_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  group_by(year)%>%summarize(b_y=mean(rating-mu-b_i-b_u))

predicted_ratings<-test_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  left_join(year_averages,by="year")%>%
  mutate(pred_year=mu+b_i+b_u+b_y)%>%
  pull(pred_year)

movie_user_year_effect_rmse<-RMSE(predicted_ratings,test_set$rating)
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie+User+Year effect",RMSE=movie_user_year_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")

###Adding genre effect
train_set%>%group_by(genres)%>%
  filter(n()>1000)%>%summarise(b_g=mean(rating))%>%
  ggplot(aes(b_g))+geom_histogram(bins=20,alpha=0.5,color="black",fill="red")+theme_bw()

genres_averages<-train_set%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  left_join(year_averages,by="year")%>%
  group_by(genres)%>%
  summarize(b_g=mean(rating-mu-b_i-b_u-b_y))

predicted_ratings<-test_set%>%left_join(movie_averages,by="movieId")%>%
  left_join(user_averages,by="userId")%>%
  left_join(year_averages,by="year")%>%
  left_join(genres_averages,by="genres")%>%
  mutate(pred_genre=mu+b_i+b_u+b_y+b_g)%>%
  pull(pred_genre)
  
movie_user_year_genre_effect_rmse<-RMSE(predicted_ratings,test_set$rating) 
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Movie+User+Year+Genre effect",RMSE=movie_user_year_genre_effect_rmse))
rmse_calculations%>%knitr::kable("pipe")
#rmse_calculations<-rmse_calculations%>%filter(method!="movie user year and genre effect")

###Regularization Model

## why do we go for regularization model
test_set%>%
  left_join(movie_averages,by="movieId")%>%
  mutate(residual=rating-(mu+b_i))%>%
  arrange(desc(abs(residual)))%>%
  slice(1:10)%>%
  pull(title)

movie_titles<-train_set%>%
  select(movieId,title)%>%
  distinct()

train_set%>%count(movieId)%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(movie_titles,by="movieId")%>%
  arrange(desc(b_i))%>%
  slice(1:10)%>%
  pull(n)


train_set%>%count(movieId)%>%
  left_join(movie_averages,by="movieId")%>%
  left_join(movie_titles,by="movieId")%>%
  arrange(b_i)%>%
  slice(1:10)%>%
  pull(n)
  
### the best movies according to our estimate

### 
#####1k- Fold Cross Validation using k=5 to find optimum lambda
### Split the data into 10 parts

set.seed(1,sample.kind = "Rounding")
lambdas<-seq(0,10,0.25)
folds<-createFolds(train_set$rating,k=10,returnTrain = T)

rmses<- matrix(nrow=10, ncol=length(lambdas))

lambdas

ks<- seq(1,10,1)
ks

for(k in ks){
  train_set_cv<-train_set[folds[[k]],]
  temp<-train_set[-folds[[k]],]
  
  
#make sure userId and movieId in the test set are also in train set
  test_set_cv<-temp%>%
    semi_join(train_set_cv,by="movieId")%>%
    semi_join(train_set_cv,by="userId")

#Add rows removed from test set to train set
  
  removed<-anti_join(temp,test_set_cv)
  train_set_cv<-rbind(train_set_cv,removed)
  
  mu<-mean(train_set_cv$rating)
  
  rmses[k,]<-sapply(lambdas,function(l){
    
    b_i<-train_set_cv%>%
      group_by(movieId)%>%
      summarize(b_i=sum(rating-mu)/(n()+l))
    
    b_u<-train_set_cv%>%
     left_join(b_i,by="movieId")%>%
      group_by(userId)%>%
      summarize(b_u=sum(rating-mu-b_i)/(n()+l))
    
    b_g<-train_set_cv%>%
      left_join(b_i,by="movieId")%>%
      left_join(b_u,by="userId")%>%
      group_by(genres)%>%
      summarize(b_g=sum(rating-mu-b_i-b_u)/(n()+l))
    
    
    predicted_ratings<-test_set_cv%>%
      left_join(b_i,by="movieId")%>%
      left_join(b_u,by="userId")%>%
      left_join(b_g,by="genres")%>%
      mutate(y_hat=mu+b_i+b_u+b_g)%>%
      pull(y_hat)
    
    return(RMSE(predicted_ratings,test_set_cv$rating))
      
  })
    
}
  
rmses
rmses_cv<-colMeans(rmses)


qplot(lambdas,rmses_cv)
lambda_opt<-lambdas[which.min(rmses_cv)]
lambda_opt

#####Evaluation with test set using optimal lambda#####
mu<-mean(train_set$rating)
mu
movie_reg_averages<-train_set%>%
  group_by(movieId)%>%
  summarize(b_i=sum(rating-mu)/(n()+lambda_opt))

user_reg_averages<-train_set%>%
  left_join(movie_reg_averages,by="movieId")%>%
  group_by(userId)%>%
  summarize(b_u=sum(rating-mu-b_i)/(n()+lambda_opt))

genre_reg_averages<-train_set%>%
  left_join(movie_reg_averages,by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu-b_i-b_u)/(n()+lambda_opt))


predicted_ratings<-test_set%>%
  left_join(movie_reg_averages, by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  left_join(genre_reg_averages,by="genres")%>%
  mutate(pred=mu+b_i+b_u+b_g)%>%
  pull(pred)


Reg_cv_rmse<-RMSE(predicted_ratings,test_set$rating)
rmse_calculations<-rbind(rmse_calculations,data.frame(method="Regularisation with 5-fold CV on Test set",RMSE=Reg_cv_rmse))
rmse_calculations%>%knitr::kable("pipe")


### Regularization on Validation using lambda_min
###  Adding year, Rating year and Age columns to Validation set
validation<-validation%>%mutate(year=as.numeric(str_sub(title,-5,-2)))
validation<-validation%>%mutate(rated_year=year(as_datetime(timestamp)))
validation<-validation%>%mutate(age=rated_year-year)


mu_edx<-mean(edx$rating)

movie_reg_averages<-edx%>%
  group_by(movieId)%>%
  summarize(b_i=sum(rating-mu_edx)/(n()+lambda_opt))

user_reg_averages<-edx%>%
  left_join(movie_reg_averages,by="movieId")%>%
  group_by(userId)%>%
  summarize(b_u=sum(rating-mu_edx-b_i)/(n()+lambda_opt))

genre_reg_averages<-edx%>%
  left_join(movie_reg_averages,by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu_edx-b_i-b_u)/(n()+lambda_opt))

 

predicted_ratings_val<-validation%>%
  left_join(movie_reg_averages, by="movieId")%>%
  left_join(user_reg_averages,by="userId")%>%
  left_join(genre_reg_averages,by="genres")%>%
  mutate(pred_validation=mu+b_i+b_u+b_g)%>%
  pull(pred_validation)


RMSE_valid<-RMSE(predicted_ratings_val,validation$rating)

rmse_calculations<-rbind(rmse_calculations,data.frame(method="Final Regularization on Validation with optimal Lambda",RMSE=RMSE_valid))
rmse_calculations%>%knitr::kable("pipe",caption = "RMSE Results Table")
