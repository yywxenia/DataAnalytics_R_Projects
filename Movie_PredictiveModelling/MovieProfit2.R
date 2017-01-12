#########################################################################
### Project Part 2:
#########################################################################
# Part 1: Clean data
# Load the dataset:
setwd('/Users/yywxenia/Desktop/Fall2016/6242_DataVisual/HWs/Project1')
load('movies_merged')
dim(movies_merged)         # Its dimention is (40789, 39)

# Omit all rows in which gross and budget are not available (NAs):
sum(is.na(movies_merged$Gross)) 
sum(is.na(movies_merged$Budget)) 
nona_movies = movies_merged[complete.cases(movies_merged$Gross),]

# Removed NAs, dataset name is "nona_movies":
dim(nona_movies)

# Remove all movies released prior to 2000:
# Only keep the matched data, again move NAs from Released:
sum(is.na(nona_movies$Year))        # Year has no NAs
sum(is.na(nona_movies$Released))    # Release has 50 NAs, we donot need to remove NAs
nona_movies$Released2 = as.double(format(nona_movies$Released, "%Y")) 
year_movies = nona_movies[complete.cases(nona_movies$Released2),]
year_movies = year_movies[year_movies$Year > 2000, ]


# Compute MSE
mse = function(fit_results){
  return (mean(fit_results$residuals^2))      
} 

# Compute test MSE
test_mse = function(model, test_data){
  testX = test[, !(colnames(test) %in% c("PROFIT"))] 
  testY = test$PROFIT
  pred_test = predict(model, testX)
  #return (mean( (testY - predict(model, testX))^2 ))
  endpoints = data.frame(testY, pred_test)
  endpoints = endpoints[complete.cases(endpoints),]   
  return (mean((endpoints[,1] - endpoints[,2])^2))
}

  
#########################################################################
# (1) Use linear regression to predict profit based on ALL available numeric variables.

### Do not include Domestric_Gross" and "Gross" as explanatory variables, because profit = Gross-Budget
year_movies$PROFIT = (year_movies$Gross-year_movies$Budget)
year_movies = year_movies[, !(colnames(year_movies) %in% c("Gross", "Domestic_Gross"))] 

### Column "Metascore" is string("number"). So firstly we transfer it to be NUM:
year_movies  = year_movies[complete.cases(year_movies$Metascore),]   
year_movies$Metascore = as.numeric(as.character(year_movies$Metascore))

### Runtime could be see as numerical as well:
year_movies$Runtime = (gsub("\\D", "", year_movies$Runtime))
year_movies$Runtime = as.numeric(year_movies$Runtime)

### For Awards, we treat the sum of awards as a numeric column:
year_movies$Awards[year_movies$Awards == "N/A"] = NA   
year_movies$Awards[is.na(year_movies$Awards)] = 0
for (r in c(1:length(year_movies$Awards))){
  year_movies$TotalAwards[r] = 
    sum(as.numeric(unique(unlist(regmatches(year_movies$Awards[r], gregexpr("[0-9]+", year_movies$Awards[r]))))))
}

nums = sapply(year_movies, is.numeric)                     # available numeric variables
num_movies = year_movies[ , nums]
num_movies = num_movies[complete.cases(num_movies),]       # remove NA rows

#Make sure everything is numeric:
for(i in seq(ncol(num_movies))) {
  num_movies[,i] = as.numeric(as.character(num_movies[,i]))
}

### Since column "Year", "Date" and "Released2" are all time variable years, ignore for Q1 analysis:
num_movies = num_movies[, !(colnames(num_movies) %in% c("Year", "Date", "Released2"))] 

colnames(num_movies)
dim(num_movies)


### Specifically, we define the dependent variable "profit" as "Gross revenue - budget".
counts = 1
splits = c(1:19)
percents = seq(0.05,0.95,0.05)
lr1_split = matrix(1:38, 19)

for (s in seq(0.05,0.95,0.05)){
  temp = matrix(1:20, 10)
  mse1 = 0
  mse2 = 0
  # Iterate 10 times inner:
  for(iter in 1:50){
    shufdata = num_movies[sample(nrow(num_movies)),]    #Shuffle results will change everytime
    index_test = seq(1, nrow(shufdata)*s)               #Shuffle index results
    train = shufdata[index_test, ]
    test = shufdata[-index_test, ]
    
    LR1 = lm(PROFIT ~ Runtime + Metascore + imdbRating + imdbVotes + tomatoMeter + tomatoRating + 
               tomatoReviews + tomatoFresh + tomatoRotten + tomatoUserMeter + tomatoUserRating + 
               tomatoUserReviews + TotalAwards + Budget, data = train)
    trainMse = mse(LR1)
    mse1 = mse1 + mse(LR1)
    mse2 = mse2 + test_mse(LR1, test)
  }
  
  lr1_split[counts, 1] = mse1/50
  lr1_split[counts, 2] = mse2/50
  counts = counts + 1
}
lr1_split  = cbind(lr1_split , percents)
colnames(lr1_split) = c("train_mse","test_mse","test %")
print (lr1_split)


# Graph the train and test MSE as a function of the train set size.
library(ggplot2)
library(GGally)
library(corrplot)
library(tidyr)


graph1 = data.frame(train = lr1_split[,1], test = lr1_split[,2],sampling = lr1_split[,3])
graph1 %>% gather(key, MSE, train, test) %>%
  ggplot(aes(x=sampling, y=MSE, colour=key)) +
  geom_line()+theme_bw()+
  ggtitle("Figure 1: (BASE) MSE with diff partitions") + 
  labs(x="Train Sampling",y="train & test MSE")


# (2) Try to improve the prediction quality in (1) as much as possible by adding feature transformations of the numeric variables. 
#     Explore both numeric transformations such as power transforms and non-numeric transformations of the numeric variables like binning 
#     Explain which transformations you used and why you chose them. 
#     Graph the train and test MSE as a function of the train set size?

### Multicollinearity: Variables highly related to the other variables
# If 60% or more of the selected reviews are FRESH **tomatoFresh**, then the movie receives as a "FRESH" rating. 
# tomatoMeter = tomatoFresh/tomatoReviews;

### Remove tomatoRotten; 
### Remove highly correlated variables (with multicollinearity) 
#graph2 = data.frame(num_movies$tomatoRating, num_movies$imdbRating, 
#                    num_movies$tomatoUserRating, num_movies$tomatoUserMeter)
#ggpairs(graph2, columnLabels = c("toRat", "iRat", "toURat", "toUMet"),
#       title="Figure 2: Variables correlations 1")
#graph3 = data.frame(num_movies$tomatoMeter,num_movies$tomatoFresh)
#ggpairs(graph3, columnLabels = c("tMet","tFre"),
#        title="Figure 3: Variables correlations 2")
#Q2num = num_movies[, !(colnames(num_movies) %in% c("tomatoRotten", "tomatoMeter", "tomatoRating", 
#                                                   "tomatoUserRating","imdbRating"))] 
#Q2num = Q2num[,c(1,2,3,4,5,6,7,8,9,10)]
#LR2 = lm(PROFIT ~ Runtime + Metascore + imdbVotes + tomatoFresh + tomatoReviews + tomatoUserMeter +  tomatoUserReviews + 
#            TotalAwards + Budget, data = Q2num)

### Get an idea for transformation:
#LR3 = lm(PROFIT ~ Runtime + Metascore + log(imdbVotes) + tomatoReviews +
#           tomatoFresh + tomatoUserMeter + log(tomatoUserReviews) + 
#           Budget + TotalAwards, 
#         data = Q2num)
#summary(LR3)

temp=num_movies[, (colnames(num_movies) %in% c("tomatoRating", "tomatoMeter", 
                                                  "tomatoFresh","tomatoReviews",
                                                  "tomatoUserRating","tomatoUserMeter", 
                                                  "tomatoUserReviews", "imdbRating", "imdbVotes"))] 
library(formattable)
correlationship = matrix(0, nrow = 9, ncol = 9)
rownames(correlationship) = paste(colnames(temp), 1:9)
colnames(correlationship) = paste(colnames(temp), 1:9)
for (t in 1:(ncol(temp))) {
  for (tt in 1:(ncol(temp))){
    correlationship[tt, t] = cor(temp[,t], temp[,tt])
    }
}
print (formattable(correlationship, digits = 2, format = "f"))




Q2num = num_movies[, !(colnames(num_movies) %in% c("tomatoRotten"))] 
Q2num = Q2num[,c(1,2,3,4,5,6,7,8,9,10,11,12,14,13)] 
LR_base = lm(PROFIT ~ Runtime + Metascore + imdbRating + imdbVotes + tomatoMeter + tomatoRating + 
           tomatoReviews + tomatoFresh + tomatoUserMeter +  tomatoUserRating + tomatoUserReviews + 
           Budget + TotalAwards, 
         data = Q2num)
summary(LR_base)

##############################
### Play around transformation (Checking correlations with PROFIT):
selected_trans = c(0,0,0,0,0,0,0,0,0,0,0,0,0)
variables_corr = c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for (var in 1:(ncol(Q2num)-1)) {
  power_corr = c(0,0,0,0) 
  selected_trans[var] = 0
  for (pow in 1:3) {
    power_corr[pow] = cor(Q2num$PROFIT, Q2num[, var]^(4-pow))
  }
  for (c in power_corr){
    selected_trans[var] = 4-which.max(abs(power_corr))
    variables_corr[var] = cor(Q2num$PROFIT, Q2num[, var]^selected_trans[var])
  }
}

##############################
### Log
offset = abs(min(Q2num$PROFIT))+1   # Inoder to treat the negative values of PROFIT
log_corr = c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for (var in 1:(ncol(Q2num)-1)) {
  log_corr[var] = cor(log(Q2num$PROFIT+offset), log(Q2num[,var]))
}

log_corr2 = c(0,0,0,0,0,0,0,0,0,0,0,0,0)
for (var in 1:(ncol(Q2num)-1)) {
  log_corr2[var] = cor(Q2num$PROFIT, log(Q2num[,var]))
}

power_table = matrix(1:39, 13)
power_table[, 1] = colnames(Q2num[,1:13])
power_table[, 2] = variables_corr
power_table[, 3] = selected_trans
power_table = cbind(power_table, log_corr)
power_table = cbind(power_table, log_corr2)
colnames(power_table) = c("Variables","Power-Corr-Profit","Best-Power" ,"Log-Log-Corr", "Log-Profit-Corr")
print (as.data.frame(power_table))


##########################
## binning Awards:
Q2num$TotalAwards[Q2num$TotalAwards == "N/A"] = NA   
Q2num$TotalAwards[is.na(Q2num$TotalAwards)] = 0
Q2num$NoneAwards = 0
Q2num$SomeAwards = 0
Q2num$ManyAwards = 0

for (r in c(1:length(Q2num$TotalAwards))){
  h = sum(as.numeric(unique(unlist(
    regmatches(Q2num$TotalAwards[r], gregexpr("[0-9]+", Q2num$TotalAwards[r]))))))
  if (h==0){
    Q2num$NoneAwards[r] = 1
  } else if (1<= h && h <=5){
    Q2num$SomeAwards[r] = 1
  } else if(h >5){
    Q2num$ManyAwards[r] = 1
  }
}

#########################################
library(glmnet)
print (as.data.frame(power_table))

Q2new = Q2num
logs = Q2new[, !(colnames(Q2new) %in% c("PROFIT", "NoneAwards", "SomeAwards", "ManyAwards",
                                       "tomatoMeter", "TotalAwards", "TomatoFresh"))] 
Q2new = Q2num
poly = Q2new[, !(colnames(Q2new) %in% c("PROFIT", "NoneAwards", "SomeAwards", "ManyAwards", "TotalAwards"))] 

### Add logs coloumns: 
colnames(logs)
for (col in 1:ncol(logs)){
  Q2new[,17+col] = log(logs[, col])
}

### Add polynominals: 
for (col2 in 1:(ncol(poly))){
  Q2new[,28+col2] = (Q2new[, col2])^2
}
for (col3 in 1:(ncol(poly))){
  Q2new[,40+col3] = (Q2new[, col3])^2
}

head(Q2new)
Q2new = Q2new[,c(1:13, 15:52, 14)]    # put PROfIT at last column

### Try Lasso:
Q2new = do.call(data.frame,lapply(Q2new, function(x) replace(x, is.infinite(x), 0)))
Q2new = Q2new[apply(Q2new, 1, function(x) !all(x!=0)),]

target = Q2new$PROFIT
inputs = as.matrix(Q2new[!(names(Q2new) %in% c("PROFIT", "NoneAwards", "SomeAwards","ManyAwards", "TotalAwards"))])
lassofit = cv.glmnet(inputs, y=target, alpha = 1, nfold = 5, family = "gaussian", type.measure = 'mse')
plot(lassofit)


#lassofit = cv.glmnet(inputs, y=target, alpha = 1, nfold = 5, family = "gaussian", type.measure = 'mse')
#plot(lassofit)

lassofit = glmnet(inputs, y=target, alpha = 4, lambda = c(100000))
#lassofit$beta 
ypred <- predict(lassofit, newx = inputs)
RMSE2 <- mean((target - ypred)^2)
print(RMSE2-mse(LR1))
print(abs(RMSE2-mse(LR1))/mse(LR1))

plot(lassofit)



#################################################
print (as.data.frame(power_table))
LR2 = lm(PROFIT ~ log(Runtime) + Metascore + imdbRating*imdbVotes + 
           tomatoRating*tomamtoReview +
           tomatoUserRating*tomatoUserReviews +
           Budget + ManyAwards, data = Q2num)

mse(LR2)-mse(LR1)
summary(LR2)



###### New model performance:
counts = 1
splits = c(1:19)
percents = seq(0.05,0.95,0.05)
lr2_split = matrix(1:38, 19)

for (s in seq(0.05,0.95,0.05)){
  temp2 = matrix(1:20, 10)
  mse1 = 0
  mse2 = 0
  # Iterate 10 times inner:
  for(iter in 1:5){
    shufdata = Q2num[sample(nrow(Q2num)),]    #Shuffle results will change everytime
    index_test = seq(1, nrow(shufdata)*s)               #Shuffle index results
    train = shufdata[index_test, ]
    test = shufdata[-index_test, ]
    
    LR1 = lm(PROFIT ~ Runtime + log(Runtime) + Metascore + imdbRating*imdbVotes + 
               I(tomatoRating^3) +
               tomatoUserRating*tomatoUserReviews +
               I(Budget^2) + ManyAwards, data = train)
    trainMse = mse(LR1)
    mse1 = mse1 + mse(LR1)
    mse2 = mse2 + test_mse(LR1, test)
  }
  
  lr2_split[counts, 1] = mse1/5
  lr2_split[counts, 2] = mse2/5
  counts = counts + 1
}
lr2_split  = cbind(lr2_split , percents)
colnames(lr2_split) = c("train_mse","test_mse","test %")
print (lr2_split)

graph2 = data.frame(baseTrain=lr1_split[,1], baseTest=lr1_split[,2], 
                    train = lr2_split[,1], test = lr2_split[,2], sampling = lr2_split[,3])
graph2 %>% gather(key, MSE, train, test, baseTrain, baseTest) %>%
  ggplot(aes(x=sampling, y=MSE, colour=key)) +
  geom_line()+theme_bw()+
  ggtitle("Figure 2: (Improved) MSE with diff partitions") + 
  labs(x="Train Sampling",y="train & test MSE")



















### =================================================================================
### (3) Write code that featurizes genre (can use code from Part-I), actors, directors, and other categorical variables. 
###     Explain how you encoded the variables into features.
cati = sapply(year_movies, is.character)                     # available categorical variables
sum(cati)
cati_movies = year_movies[ , cati]
cati_movies[cati_movies == "N/A"] = NA

# a) Drop column "BoxOffice":
###  Since "BoxOffice" has around 99% NAs and it would affect our analysis, we devide to ignore it from cati_movies dataset.
sum(is.na(cati_movies$BoxOffice))
cati_movies = cati_movies[, !(colnames(cati_movies) %in% c("BoxOffice"))]   # drop column "BoxOffice"
cati_movies = cati_movies[complete.cases(cati_movies),]                     # remove NA rows
dim(cati_movies)






