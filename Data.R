## Data Preparation

## Before going through the details of those two prediction problems, firstly prepare and clean the data as follows.

## Download the tmdb movie dataset from Kaggle.
setwd("~/Desktop/Dataset") 
raw = read.csv("tmdb_5000_movies.csv",stringsAsFactors = F)

## Data Clean

## 1. Remove instances which have at least one NA variable
movies = raw
movies = movies[complete.cases(movies), ]

## 2. Remove instances which are duplicated (duplicated based on title)
movies = movies[!duplicated(movies$title),]

## 3. Extract genresID, keywordsID, companyID and countryABB
library(stringr)
genresID = str_extract_all(movies$genres, "[0-9]+[0-9]") 

for( i in 1:dim(movies)[1]){
  # extract first three genres of each movie if possible; otherwise, return 0
  if(is.na(genresID[[i]][1])){movies$genresID1[i] = 0}else{movies$genresID1[i] = genresID[[i]][1]}
  if(is.na(genresID[[i]][2])){movies$genresID2[i] = 0}else{movies$genresID2[i] = genresID[[i]][2]}
  if(is.na(genresID[[i]][3])){movies$genresID3[i] = 0}else{movies$genresID3[i] = genresID[[i]][3]}
}

movies$keywordsID = str_extract(movies$keywords, "[0-9]+[0-9]") #extract first keyword 
movies$companyID = str_extract(movies$production_companies, "[0-9]+[0-9]") #extract produce company 
movies$countryABB = substr(movies$production_countries,18,19) #extarct produce country

## 4. Change data types
movies$release_date = as.Date(movies$release_date) 
movies$original_language = as.factor(movies$original_language) 
movies$genresID1 = as.factor(movies$genresID1) 
movies$genresID2 = as.factor(movies$genresID2) 
movies$genresID3 = as.factor(movies$genresID3) 
movies$keywordsID = as.factor(movies$keywordsID) 
movies$companyID = as.factor(movies$companyID) 
movies$countryABB = as.factor(movies$countryABB)

## 5. Extract year and month
movies$year = format(movies$release_date,"%Y") #extract year 
movies$year = as.integer(movies$year)
movies$month = format(movies$release_date,"%m") #extract month 
movies$month = as.integer(movies$month)

## As required, I ignore the information after the release data, for example, popularity and vote_count. 
## In addition, I select the covariates which I hypothesize might influence those to target variables.

## Select variables that I am interested in (including dependet variables)
data = na.omit(movies)
data = data[,c(1,4,6,14,21:23,25:28,13,19)]
colnames(data)

## I use all odd id as the training data and even id as testing data.

## Use all odd id as training data and even id as testing data.
is.odd = function(x) {x %% 2 != 0}
is.even = function(x) {x %% 2 == 0}
train = data[which(is.odd(data$id)),] 
test = data[which(is.even(data$id)),]

