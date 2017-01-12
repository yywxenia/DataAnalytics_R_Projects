### Prject 1
setwd('/Users/yywxenia/Desktop/Fall2016/6242_DataVisual/HWs/Project1')
load('movies_merged')

is.data.frame(movies_merged)

library(ggplot2)
library(tm)
library(tidyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(GGally)


NoNA_budget$Genre
#######################################
names(movies_merged)  # Type is the 19th column.

### Q1: The variable Type captures whether the row is a movie, a TV series, or a game. 
### Remove all rows that do not correspond to movies. How many rows did you remove?
is_movie = movies_merged[movies_merged$Type == "movie", ] #This is what we are looking for.

not_movie = movies_merged[movies_merged$Type != "movie", ] #This is the opposite "not_movie".

removed_not_movie = NROW(not_movie)
left_movies = NROW(is_movie)

dim(is_movie)
dim(not_movie)
# WE removed 789 obs of 39 variables.

### Q2: The variable Runtime represents the length of the title as a string. 
### Write R code to convert it to a numeric value (in minutes) and replace Runtime with the new numeric column.
is_movie$Numeric_Runtime = (gsub("\\D", "", is_movie$Runtime))
is_movie$Numeric_Runtime = as.numeric(is_movie$Numeric_Runtime)
# added new column named "Numeric_Runtime for Runtime.


### Investigate and describe the distribution of that value and comment on how it changes over years 
### (variable Year) and how it changes in relation to the budget (variable Budget).
head(is_movie$Runtime)
head(is_movie$Numeric_Runtime)

names(is_movie)
# (1) With Year:
NoNA_Runtime = is_movie[complete.cases(is_movie[,40]),]
dim(NoNA_Runtime)

qplot(Numeric_Runtime, Year, data = NoNA_Runtime, size=I(1.5)) + 
  stat_smooth(color = 'red',size=I(2), se=F) +
  theme_bw()+
  labs(title = "Figure 1: Runtime v.s. Year")

# (2) With Budget: 
NoNA_budget = NoNA_Runtime[complete.cases(NoNA_Runtime[,36]),]

qplot(Numeric_Runtime, Budget, data = NoNA_budget, size=I(1.5)) + 
  stat_smooth(color = 'red',size=I(2), se=F) +
  theme_bw()+
  labs(title = "Figure 2: Runtime v.s. Budget")


### Q3: Parse each text string into a binary vector with 1s representing the presence of a genre and
### 0s the absence and add it to the dataframe as additional columns. (here we used is_movie dataframe???)
is_movie_genre = NoNA_budget
is_movie_genre$Genre[is_movie_genre$Genre == "N/A"] = NA
is_movie_genre = is_movie_genre[complete.cases(is_movie_genre[,6]),]

split_genreVal = strsplit(is_movie_genre$Genre, ",")                     # NoNA_Runtime
dictionary = unique(gsub(" ", "", unlist(split_genreVal)))
dictionary[dictionary == "N/A"] = NA
Dict = dictionary[ !is.na(dictionary) ]
Dict  # print out all kinds.
length( dictionary[!is.na(dictionary)] )  #total number of objects in the list of Genre.

###---------------
sample = is_movie_genre
#unnest(sample, strsplit(sample$Genre,","))

cols = c(Dict)
vals = 0
sapply(1:length(cols), function(i)
  sample[length(cols), cols[i]] <<- vals[i])    # add multiple columns for Genres.
sample[, 41:65][is.na(sample[, 41:65])]= 0  # used to be 68
sample[, 41:65][is.na(sample$Genre)]= 0
## tail(sample)


OriginalCol = 40
for (i in c(1:length(sample$Genre))){           # i represent for row
  value = match(gsub(" ", "", unlist(strsplit(sample$Genre[i],","))), Dict) # found which values matched
  l = length(OriginalCol+value)                 # find the right column to set value 1
  for (t in c(1:l))
  {sample[i, (OriginalCol+value)[t]] = 1        # change avaliable genres to be '1', others to be '0'.
    }
}

# Simple testing our results by comparison:
sample[100:102,41:65]                     # used to be 68
sample$Genre[100:102]

# List the top 10 ranking genres.
Ranks = colSums(sample[41:65]==1)
Ranks
tops = rev(order(colSums(sample[41:65]==1)))
Top10 = Dict[tops][1:10]
tops_genre = colSums(sample[Top10]==1)
tops_genre                  # This is the top ten genres


### Plotting Gross and Genres-(using No-NA of gross data)-----------------
noNAGross = sample
noNAGross$Gross[noNAGross$Gross == "N/A"] = NA
noNAGross = noNAGross[complete.cases(is_movie_genre[,38]),]

gross_data = noNAGross$Gross 
up10 = rev(order(colSums(noNAGross[41:65]==1)))
up = Dict[up10][1:10]
tops10 = colSums(sample[up]==1)   # now the top 10 count rank is tops_up



### We need to plot seperately for Gross with each Top10 movie
Gross_df = noNAGross[,38:65]
qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-1: Drama v.s. Gross") + facet_wrap(~Drama)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-2: Comedy v.s. Gross") + facet_wrap(~Comedy)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-3: Action v.s. Gross") + facet_wrap(~Action)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-4: Adventure v.s. Gross") + facet_wrap(~Adventure)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-5: Crime v.s. Gross") + facet_wrap(~Crime)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-6: Romance v.s. Gross") + facet_wrap(~Romance)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-7: Thriller v.s. Gross") + facet_wrap(~Thriller)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-8: Horror v.s. Gross") + facet_wrap(~Horror)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-9: Sci-Fi v.s. Gross") + facet_wrap(~Sci-Fi)+theme_bw()

qplot(x =Gross, data = Gross_df, color = I('black'), fill = I('grey'),
      main = "Figure 3-10: Mystery v.s. Gross") + facet_wrap(~Mystery)+theme_bw()
#------------------

### Plot the top 10 Genres and their total counts:
MovieGenres = c("Drama","Comedy","Action","Adventure","Crime","Romance",
                "Thriller","Horror","Sci-Fi","Mystery")
qplot(x=MovieGenres, y=tops10, xlab = "Top10 Movie Genres", 
      ylab = "Top10 Genre Frequency in Dataset",
      main = 'Figure 3: Top10 Movie Genres v.s. Gross')+
  geom_point(color = "Blue", size = 5)+
  theme_bw()

#### qplot(x=MovieGenres, y=tops10, geom='line')


### Q4: Find and remove all rows where you suspect a merge error occurred 
###     based on a mismatch between these two variables.
Q4 = noNAGross
Q4n = Q4[complete.cases(Q4[,4]),]
Q4n$Released2 = as.double(format(Q4n$Released, "%Y"))

all(Q4n$Released2 == Q4n$Year) # False, test if all values are equal
tail(Q4n$Released2)
tail(Q4n$Year)

Q4m = Q4n[Q4n$Released2 ==Q4n$Year,] # remove not-matched years
all(Q4m$Released2 == Q4m$Year) # True
tail(Q4m$Released2)
tail(Q4m$Year)

nrow(Q4)-nrow(Q4m)   # compute how many rows deleted with percentage
(nrow(Q4)-nrow(Q4m))/nrow(Q4)



### Q5: Find the best release 'Date' 
Q5m = Q4m
#trend in Year:
qplot(Q5m$Year, Q5m$Gross, geom='point', 
      xlab = "Released Year", 
      ylab = "Movies' Gross Revenue",
      main = 'Figure 4: Revenue Changes of Year Released Movies')+
  geom_smooth(color='red',se=F, size=0.3)+
  theme_bw()

r = which(Q5m$Gross == max(Q5m$Gross))
Q5m$Year[r]   # max value year

# trend in Month:
Q5m$RelDate = as.integer(month(Q5m$Released))
Q5Group = aggregate(Q5m$Gross, list(Q5m$RelDate), mean)
Q5Group$Group.1 = factor(
  Q5Group$Group.1, levels = 1:12,
  labels = c("Jan","Feb","Mar","April","May","June",
             "July","Aug","Sept","Oct", "Nov","Dec"))

qplot(Q5Group$Group.1, Q5Group$x, 
      xlab = "Released Month", 
      ylab = "Mean Gross Revenue",
      main = 'Figure 4: Revenue Changes of Different Released Month',
      geom='point')+theme_bw()


# -----
# Year Trend
ggplot(data = Q5m) + 
  geom_point(mapping = aes(x = Year, y = Gross)) +
  facet_wrap(~ Drama)+
  theme_bw()+ 
  labs(title = "Figure 4-1: Years' Revenue v.s. Drama")

dra = Q5m[Q5m$Drama == 1,]
com = Q5m[Q5m$Comedy == 1,]
act = Q5m[Q5m$Action == 1,]
adv = Q5m[Q5m$Adventure == 1,]
cri = Q5m[Q5m$Crime == 1,]
rom = Q5m[Q5m$Romance == 1,]
th = Q5m[Q5m$Thriller == 1,]
hor = Q5m[Q5m$Horror == 1,]
sci = Q5m[Q5m$`Sci-Fi` == 1,]
mys = Q5m[Q5m$Mystery == 1,]

# max value year of different top genres 
a=dra$Year[which(dra$Gross == max(dra$Gross))] 
b=com$Year[which(com$Gross == max(com$Gross))] 
c=act$Year[which(act$Gross == max(act$Gross))]
d=adv$Year[which(adv$Gross == max(adv$Gross))] 
e=cri$Year[which(cri$Gross == max(cri$Gross))] 
f=rom$Year[which(rom$Gross == max(rom$Gross))] 
g=th$Year[which(th$Gross == max(th$Gross))] 
h=hor$Year[which(hor$Gross == max(hor$Gross))] 
i=sci$Year[which(sci$Gross == max(sci$Gross))] 
j=mys$Year[which(mys$Gross == max(mys$Gross))] 

Q5 = as.data.frame(tops10)
Q5$BestY = c(a,b,c,d,e,f,g,h,i,j)
Q5$MaxGross = c(max(dra$Gross),max(com$Gross),max(act$Gross),max(adv$Gross),max(cri$Gross),
                max(rom$Gross),max(th$Gross),max(hor$Gross),max(sci$Gross),max(mys$Gross))
Q5[order(Q5$BestY),]   # cannot find any trend


# Monthly Trend:
draG = aggregate(dra$Gross, list(dra$RelDate), mean)
comG = aggregate(com$Gross, list(com$RelDate), mean)
actG = aggregate(act$Gross, list(act$RelDate), mean)
advG = aggregate(adv$Gross, list(adv$RelDate), mean)
criG = aggregate(cri$Gross, list(cri$RelDate), mean)
romG = aggregate(rom$Gross, list(rom$RelDate), mean)
thG = aggregate(th$Gross, list(th$RelDate), mean)
horG = aggregate(hor$Gross, list(hor$RelDate), mean)
sciG = aggregate(sci$Gross, list(sci$RelDate), mean)
mysG = aggregate(mys$Gross, list(mys$RelDate), mean)

test = cbind(draG, comG$x, actG$x, advG$x, criG$x, romG$x, thG$x, horG$x, sciG$x,mysG$x)
test

plot(test$Group.1,test$x, lwd=2,
     type='l',
     lty=1,
     xlab = "Months",
     ylab = "Mean Gross Revenue",
     main = "Figure 5: Revenue Monthly Change Through Genres",
     ylim=c(0,430000000))
  lines(test$Group.1,test$`comG$x`,lty = 2,col='green',lwd=1.2)
  lines(test$Group.1,test$`actG$x`,lty = 3,col='red',lwd=1.2)
  lines(test$Group.1,test$`advG$x`,lty = 4,col='blue',lwd=1.2)
  lines(test$Group.1,test$`criG$x`,lty = 5,col='pink',lwd=1.2)
  lines(test$Group.1,test$`romG$x`,lty = 6,col='darksalmon',lwd=1.2)
  lines(test$Group.1,test$`thG$x`,lty = 7,col='grey',lwd=1.2)
  lines(test$Group.1,test$`horG$x`,lty = 8,col='yellow',lwd=1.2)
  lines(test$Group.1,test$`sciG$x`,lty = 9,col='orange',lwd=1.2)
  lines(test$Group.1,test$`mysG$x`,lty = 10,col='purple',lwd=1.2)
  #legend("bottom", c("dra","com","act","adv",'rom','th','hor','sci','mys'), 
         #col=c('black','green','red','blue','pink','salmon',
               #'grey','yellow','orange','purple'), lty = c(1,2,3,4,5,6,7,8,9,10))

  
  
### Q6:  (use which dataset????)
library(corrplot)
Q6 = Q5m
# Q6 = Q6[complete.cases(Q6[,22]),]

df1 = data.frame(Q6$tomatoRating, Q6$tomatoReviews)
df1=na.omit(df1)
M1 = cor(df1)
M1   # 和投票的critic专家人数关系很小，参考性强
df2 = data.frame(Q6$imdbRating,Q6$imdbVotes)
df2 = na.omit(df2)
M2 = cor(df2)
M2   # 和投票人数相关性突出。参考性不会太强

# tomatoMeter = tomatoFresh/(tomatoFresh+tomatoRotten) or tomatoFresh/tomatoReviews
df3 = data.frame(Q6$tomatoMeter,Q6$tomatoFresh, Q6$tomatoReviews,Q6$tomatoRating, Q6$tomatoRotten)
df3 = na.omit(df3)
M3 = cor(df3)
M3 

df4 = data.frame(Q6$tomatoUserMeter, Q6$tomatoUserRating)
df4 = na.omit(df4)
M4 = cor(df4)
M4 

df5 = data.frame(Q6$tomatoRating, Q6$tomatoUserRating)
df5 = na.omit(df5)
M5 = cor(df5)
M5 
### -------
corrplot(M5, method="number")
ggpairs(df3)

df6 = data.frame(Q6$tomatoFresh, Q6$Gross)
df6 = na.omit(df6)
M6 = cor(df6)   #回答哪一种和gross最高相关？
M6
ggpairs(df6)
corrplot(M6, method="number")



### Q7 How did you construct your conversion mechanism? 
### How does the gross revenue distribution changes across these three categories.
Q7=Q6
Q7$Awards[Q7$Awards == "N/A"] = NA  # or treated as = 0?
Q7m = Q7[complete.cases(Q7[,13]),]

Q7m$None = 0
Q7m$Some = 0
Q7m$Many = 0
#----
for (r in c(1:length(Q7m$Awards))){
    h = sum(as.numeric(unique(unlist(regmatches(Q7m$Awards[r], 
                                                         gregexpr("[0-9]+", Q7m$Awards[r]))))))
    if (h==0){
      Q7m$None[r] = 1
    } else if (1<= h && h <=5){
      Q7m$Some[r] = 1
    } else if(h >5){
      Q7m$Many[r] = 1
    }
}


## Plot Relation with Gross:
plot(Q7m$Gross,Q7m$Some)
r7 = data.frame(Q7m$Gross,Q7m$Some, Q7m$Many)
ggpairs(r7)
corrplot(M6, method="number")


#### Q8 : one expected and one unexpected new insights:
# 语言和评分
# 语言和得奖
# 发行国家，语言和得奖的关系， production出版商， budget和revenue的关系




#########################
qplot(x=Top10, y=tops_genre, xlab = "Top10 Movie Genres", 
      ylab = "Top10 Genre Frequency",
      main = 'Figure 2-1: Top10 Movie Genres v.s. Gross')+
  geom_bar(color = "Blue", size = 4.5)+
  theme_bw()

#----
```{r}
qplot(x =Gross, data = sample, color = I('black'), fill = I('white'),
      main = "Figure 3-1: Drama and Gross") + facet_wrap(~Drama)+theme_bw()
qplot(x =Gross, data = sample, color = I('black'), fill = I('grey'),
      main = "Figure 3-2: Comedy and Gross") + facet_wrap(~Comedy)+theme_bw()
qplot(x =Gross, data = sample, color = I('black'), fill = I('green'),
      main = "Figure 3-3: Action and Gross") + facet_wrap(~Action)+theme_bw()
qplot(x =Gross, data =sample, color = I('black'), fill = I('yellow'),
      main = "Figure 3-4: Adventure and Gross") + facet_wrap(~Adventure)+theme_bw()
qplot(x =Gross, data =sample, color = I('black'), fill = I('purple'),
      main = "Figure 3-5: Crime and Gross") + facet_wrap(~Crime)+theme_bw()
qplot(x =Gross, data =sample, color = I('black'), fill = I('blue'),
      main = "Figure 3-6: Romance and Gross") + facet_wrap(~Romance)+theme_bw()
qplot(x =Gross, data =sample, color = I('black'), fill = I('orange'),
      main = "Figure 3-7: Thriller and Gross") + facet_wrap(~Thriller)+theme_bw()
qplot(x =Gross, data =sample, color = I('black'), fill = I('salmon'),
      main = "Figure 3-8: Horror and Gross") + facet_wrap(~Horror)+theme_bw()
qplot(x =Gross, data =sample, color = I('black'), fill = I('black'),
      main = "Figure 3-9: Sci-Fi and Gross") + facet_wrap(~Sci-Fi)+theme_bw()
qplot(x =Gross, data =sample, color = I('black'), fill = I('pink'),
      main = "Figure 3-10: Mystery and Gross") + facet_wrap(~Mystery)+theme_bw()
```


