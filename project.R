#Final Project
#Packages
library(tidyverse)
library(readxl)
library(caret)
library(gains)
library(pROC)
library(ROCR)
options(scipen=999)

#EDA (Exploratory Data Analysis)
df <- read_excel("ERIMData.xlsx") %>%
  as_tibble()
head(df)
df$DinExp %>%
  summary()
# we can see that this distribution is heavily skewed towards the right, and yet a majority of
# households spend $0 on frozen dinners. We separate the data from those who spend $0 to see what
# the distribution looks like.
df[df$DinExp > 0,] %>%
  count() / count(df)
# it appears that almost 2/3 of the data did not purchase frozen meals over the course of a year.
# let's look only at those that did.
purchases <- df[df$DinExp > 0,]
bp <- purchases$DinExp %>%
  boxplot(
    main="'DinExp' Boxplot, where 'DinExp'>$0",
    ylab="Yearly Frozen Dinner Expenditures in $ ('DinExp')"
  )

bp$out %>%
  as_tibble() %>%
  arrange(desc(-value))
# there are 115 outliers >$382


# To take this abnormal distribution into account in household segmentation, we manually define
# bins based on average meals purchased per week (assuming that one meal is $5):

# Purchasing behavior bins (dinPurBins):
# Bin 1: [0] Spend $0/week (0 mpy) 
# Bin 2: (0,60] (<=1 meal per month)
# Bin 3: (60,240] (<=1 meal per week)
# Bin 4: (240,1680] (<=1 meal per day)
# Bin 5: (1680,] (>1 mpd)

dinPurBins <- c(0,0.01,60,240,1680,55000)
df$mpwBins <- cut(
  df$DinExp,
  breaks = dinPurBins,
  labels = c("[0]","(0,0.25]","(0.25,1]","(1,7]","(7,+inf)"),
  include.lowest = TRUE,
  right = FALSE
)
table(df$mpwBins) %>%
  barplot(
    main="Customer Segment Distribution",
    ylim=c(0,2500),
    xlab="Customer Segments (Avg Frozen Dinners per Week)",
    ylab="Frequency"
  )
# The distribution after segmentation is still skewed, but much less so. Let's look and see which
# segment has the greatest TPV to see which segment we should focus on marketing to.

grouped_tpv <- df %>%
  group_by(mpwBins) %>%
  summarise(tpv = sum(DinExp))
grouped_tpv$tpv %>%
  barplot(
    names.arg = grouped_tpv$mpwBins,
    ylim = c(0,100000),
    ylab = "Yearly Total Payment Volume ($)",
    xlab = "Customer Segments (Avg Frozen Dinners per Week)"
  )
# As we can see from this graphic, the consumer segment with the greatest yearly TPV is households that
# purchase 1-7 frozen dinners per week

#exclude all NA values:
df <- na.exclude(df)

# dummy variables:
df$AptResType <- ifelse(df$ResType==1,1,0)
df$ConResType <- ifelse(df$ResType==2,1,0)
df$SFResType <- ifelse(df$ResType==3,1,0)
df$MFResType <- ifelse(df$ResType==4,1,0)
df$MobResType <- ifelse(df$ResType==5,1,0)
df$OthResType <- ifelse(df$ResType==6,1,0)

df$OwnResStatus <- ifelse(df$ResStatus==1,1,0)
df$RenResStatus <- ifelse(df$ResStatus==2,1,0)
df$OthResStatus <- ifelse(df$ResStatus==3,1,0)

df$HHInc1 <- ifelse(df$HHInc==1,1,0)
df$HHInc2 <- ifelse(df$HHInc==2,1,0)
df$HHInc3 <- ifelse(df$HHInc==3,1,0)
df$HHInc4 <- ifelse(df$HHInc==4,1,0)
df$HHInc5 <- ifelse(df$HHInc==5,1,0)
df$HHInc6 <- ifelse(df$HHInc==6,1,0)
df$HHInc7 <- ifelse(df$HHInc==7,1,0)
df$HHInc8 <- ifelse(df$HHInc==8,1,0)
df$HHInc9 <- ifelse(df$HHInc==9,1,0)
df$HHInc10 <- ifelse(df$HHInc==10,1,0)
df$HHInc11 <- ifelse(df$HHInc==11,1,0)
df$HHInc12 <- ifelse(df$HHInc==12,1,0)
df$HHInc13 <- ifelse(df$HHInc==13,1,0)
df$HHInc14 <- ifelse(df$HHInc==14,1,0)

df$MEdu0 <- ifelse(df$MEdu==0,1,0)
df$MEdu1 <- ifelse(df$MEdu==1,1,0)
df$MEdu2 <- ifelse(df$MEdu==2,1,0)
df$MEdu3 <- ifelse(df$MEdu==3,1,0)
df$MEdu4 <- ifelse(df$MEdu==4,1,0)
df$MEdu5 <- ifelse(df$MEdu==5,1,0)
df$MEdu6 <- ifelse(df$MEdu==6,1,0)
df$MEdu7 <- ifelse(df$MEdu==7,1,0)
df$MEdu8 <- ifelse(df$MEdu==8,1,0)
df$MEdu9 <- ifelse(df$MEdu==9,1,0)
df$MEdu10 <- ifelse(df$MEdu==10,1,0)
df$MEdu11 <- ifelse(df$MEdu==11,1,0)

df$FEdu0 <- ifelse(df$MEdu==0,1,0)
df$FEdu1 <- ifelse(df$MEdu==1,1,0)
df$FEdu2 <- ifelse(df$MEdu==2,1,0)
df$FEdu3 <- ifelse(df$MEdu==3,1,0)
df$FEdu4 <- ifelse(df$MEdu==4,1,0)
df$FEdu5 <- ifelse(df$MEdu==5,1,0)
df$FEdu6 <- ifelse(df$MEdu==6,1,0)
df$FEdu7 <- ifelse(df$MEdu==7,1,0)
df$FEdu8 <- ifelse(df$MEdu==8,1,0)
df$FEdu9 <- ifelse(df$MEdu==9,1,0)
df$FEdu10 <- ifelse(df$MEdu==10,1,0)
df$FEdu11 <- ifelse(df$MEdu==11,1,0)

mdf <- df[,c(
  "mpwBins",
  "HHNbr",
  "MWrkHrs",
  "FWrkHrs",
  "FBirth",
  "MBirth",
  "Cable",
  "Cats",
  "Dogs",
  "YogExp",
  "AptResType", "ConResType", "SFResType", "MFResType", "MobResType", "OthResType",
  "OwnResStatus", "RenResStatus", "OthResStatus",
  "HHInc1","HHInc2","HHInc3","HHInc4","HHInc5","HHInc6","HHInc7","HHInc8","HHInc9","HHInc10",
    "HHInc11","HHInc12","HHInc13","HHInc14",
  "MEdu0","MEdu1","MEdu2","MEdu3","MEdu4","MEdu5","MEdu6","MEdu7","MEdu8","MEdu9","MEdu10","MEdu11",
  "FEdu0","FEdu1","FEdu2","FEdu3","FEdu4","FEdu5","FEdu6","FEdu7","FEdu8","FEdu9","FEdu10","FEdu11"
)]

#modeling
mdf1 <- scale(mdf[2:57])
mdf1 <- data.frame(
  mdf1,
  mdf$mpwBins
)
colnames(mdf1)[57] <- 'mpwBins'

set.seed(1)
mdf_idx <- createDataPartition(mdf1$mpwBins, p=0.6, list=FALSE)
train_mdf <- mdf1[mdf_idx,]
valid_mdf <- mdf1[-mdf_idx,]

mdf_ctrl <- trainControl(method="cv", number=10)
mdf_grid <- expand.grid(.k=c(1:10))
set.seed(1)
mdf_KNN_fit <- train(
  mpwBins~.,
  data=train_mdf,
  method="knn",
  trControl=mdf_ctrl,
  tuneGrid=mdf_grid
)
mdf_KNN_fit

mdf_KNN_pred <- predict(mdf_KNN_fit, newdata=valid_mdf)
confusionMatrix(mdf_KNN_pred, valid_mdf$mpwBins, positive='1')

# Accuracy is low and sensitivity/specificity are terrible across the board, so we try more values
# of K. quantdev.ssri.psu says to try the square root of the sample size as K, so we test up until
# 40 (the approximate square root of our training data).

mdf_grid1 <- expand.grid(.k=c(1:40))
set.seed(1)
mdf_KNN_fit1 <- train(
  mpwBins~.,
  data=train_mdf,
  method="knn",
  trControl=mdf_ctrl,
  tuneGrid=mdf_grid1
)
mdf_KNN_fit1

mdf_KNN_pred1 <- predict(mdf_KNN_fit1, newdata=valid_mdf)
confusionMatrix(mdf_KNN_pred1, valid_mdf$mpwBins, positive='1')

# Accuracy is a little bit higher, but barely better than a 50/50 guess. Based on specificity and
# sensitivity measures, placement for each variable essentially looks like a 50/50 guess with almost
# 100% failure in one or the other measure. Let's try testing specifically for our target segment,
# making the predicted variable a binary factor to see if that helps.
bin_mdf <- mdf
bin_mdf$mpwBins <- ifelse(mdf$mpwBins=='(1,7]',1,0)
colnames(bin_mdf)[1] <- 'target'

mdf2 <- scale(bin_mdf[2:57])
mdf2 <- data.frame(
  mdf2,
  bin_mdf$target
)
colnames(mdf2)[57] <- 'target'
mdf2$target <- as.factor(mdf2$target)

set.seed(1)
mdf_idx2 <- createDataPartition(mdf2$target, p=0.6, list=FALSE)
train_mdf2 <- mdf2[mdf_idx2,]
valid_mdf2 <- mdf2[-mdf_idx2,]

#We use the same control and grid as before.
set.seed(1)
mdf_KNN_fit2 <- train(
  target~.,
  data=train_mdf2,
  method="knn",
  trControl=mdf_ctrl,
  tuneGrid=mdf_grid1
)
mdf_KNN_fit2

mdf_KNN_pred2 <- predict(mdf_KNN_fit2, newdata=valid_mdf2)
confusionMatrix(mdf_KNN_pred2, valid_mdf2$target, positive='1')

# The results are still inadequate, as sensitivity is zero. This is essentially telling us that our
# model is guessing that every observation is nontarget. In order to adjust for this, we can change
# the default cutoff to one equal to the proportion of positive observations in the target class.

cutoff <- ifelse(mdf2$target=='1',1,0) %>%
  sum() / count(mdf2) %>%
  as.numeric()

mdf_KNN_pred_prob <- predict(mdf_KNN_fit2, newdata=valid_mdf2, type='prob')

confusionMatrix(
  as.factor(ifelse(mdf_KNN_pred_prob[,2]>cutoff, '1', '0')),
  valid_mdf2$target,
  positive = '1'
)

gains_valid_mdf2 <- valid_mdf2
gains_valid_mdf2$target <- as.numeric(as.character(gains_valid_mdf2$target))

gains_table <- gains(gains_valid_mdf2$target, mdf_KNN_pred_prob[,2])
gains_table

plot(
  c(0, gains_table$cume.pct.of.total*sum(gains_valid_mdf2$target)) ~ c(0, gains_table$cume.obs),
  xlab="# of cases",
  ylab="cumulative",
  main="Cumulative lift chart",
  type="l"
)
lines(
  c(0, sum(gains_valid_mdf2$target)) ~ c(0, dim(gains_valid_mdf2)[1]),
  col="red",
  lty=2
)

# While the prediction numbers aren't great, the lift curve from the KNN model lies well above
# the diagonal line, indicating that the KNN model performs slightly better than the baseline model
# in terms of predicting whether or not a household is a target customer.

# Let's attempt to compare these results with that of a logistic regression, once again predicting
# whether or not a household is in our target segment.
set.seed(1)
bin_idx <- createDataPartition(bin_mdf$target,p=0.6,list=FALSE)
train_bin_mdf <- bin_mdf[bin_idx,]
valid_bin_mdf <- bin_mdf[-bin_idx,]

m1 <- glm(
  target~.,
  family=binomial(link="logit"),
  data=train_bin_mdf
)
summary(m1)

# A significant number of our variables are perfectly correlated, so we remove all variables with
# correlation higher than 0.75.

nums <- cor(train_bin_mdf)%>%
  findCorrelation(cutoff=0.75) %>%
  sort()
train_bin_mdf[-nums]

m2 <- glm(
  target~.,
  family=binomial(link="logit"),
  data=train_bin_mdf[-nums]
)
summary(m2)

phat2 <- predict(m2, valid_bin_mdf, type = "response")
yhat2 <- ifelse(phat2 >= cutoff, 1, 0)

ytp2 <- ifelse(yhat2 == 1 & valid_bin_mdf$target == 1, 1, 0)
ytn2 <- ifelse(yhat2 == 0 & valid_bin_mdf$target == 0, 1, 0)
# Accuracy score
mean(valid_bin_mdf$target == yhat2)
# Sensitivity:
sum(ytp2) / sum(valid_bin_mdf$target == 1)
# Specificity:
sum(ytn2) / sum(valid_bin_mdf$target == 0)

# With higher accuracy, sensitivity, and specificity scores than our best KNN model, we continue
# testing variations of this logistic regression model.
colnames(train_bin_mdf[-nums])

m3 <- glm(
  target~HHNbr+MWrkHrs+FWrkHrs+Cable+RenResStatus+OthResStatus+
    HHInc1+HHInc2+HHInc3+HHInc4+HHInc5+HHInc6+HHInc7+HHInc8+
    HHInc9+HHInc10+HHInc11+HHInc12+HHInc13+HHInc14,
  family=binomial(link="logit"),
  data=train_bin_mdf
)
summary(m3)

phat3 <- predict(m3, valid_bin_mdf, type = "response")
yhat3 <- ifelse(phat3 >= cutoff, 1, 0)

ytp3 <- ifelse(yhat3 == 1 & valid_bin_mdf$target == 1, 1, 0)
ytn3 <- ifelse(yhat3 == 0 & valid_bin_mdf$target == 0, 1, 0)
# Accuracy score
mean(valid_bin_mdf$target == yhat3)
# Sensitivity:
sum(ytp3) / sum(valid_bin_mdf$target == 1)
# Specificity:
sum(ytn3) / sum(valid_bin_mdf$target == 0)

# The standard error on the housing income dummy variables and the other residence status are very
# high, so we test removing those variables.

m4 <- glm(
  target~HHNbr+MWrkHrs+FWrkHrs+Cable+RenResStatus,
  family=binomial(link="logit"),
  data=train_bin_mdf
)
summary(m4)

phat4 <- predict(m4, valid_bin_mdf, type = "response")
yhat4 <- ifelse(phat4 >= cutoff, 1, 0)

ytp4 <- ifelse(yhat4 == 1 & valid_bin_mdf$target == 1, 1, 0)
ytn4 <- ifelse(yhat4 == 0 & valid_bin_mdf$target == 0, 1, 0)
# Accuracy score
mean(valid_bin_mdf$target == yhat4)
# Sensitivity:
sum(ytp4) / sum(valid_bin_mdf$target == 1)
# Specificity:
sum(ytn4) / sum(valid_bin_mdf$target == 0)

# All performance measures remain relatively the same. We return to model "m2" as our best performing
# logistic regression model and compare it to the best performing KNN model, "mdf_KNN_fit2".

confusionMatrix(
  as.factor(ifelse(phat2 >= cutoff, '1', '0')),
  as.factor(valid_bin_mdf$target),
  positive='1'
)

gains_valid_mdf2 <- valid_mdf2
gains_valid_mdf2$target <- as.numeric(as.character(gains_valid_mdf2$target))
gains_table <- gains(gains_valid_mdf2$target, mdf_KNN_pred_prob[,2])
gains_table

gains_valid_bin <- valid_bin_mdf
gains_valid_bin$target <- as.numeric(as.character(gains_valid_bin$target))
gains_table_bin <- gains(gains_valid_bin$target, phat2)
gains_table

plot(
  c(0, gains_table$cume.pct.of.total*sum(gains_valid_mdf2$target)) ~ c(0, gains_table$cume.obs),
  xlab="# of cases",
  ylab="cumulative",
  main="Cumulative lift chart",
  type="l",
  col="red"
)
lines(
  c(0, sum(gains_valid_mdf2$target)) ~ c(0, dim(gains_valid_mdf2)[1]),
  lty=2
)
lines(
  c(0, gains_table_bin$cume.pct.of.total*sum(gains_valid_bin$target)) ~ c(0, gains_table_bin$cume.obs),
  col="blue"
)
legend(
  600,
  20,
  legend=c("'mdf_KNN_fit2'", "'m2'", "Baseline Model"),
  col=c("red", "blue", "black"),
  lty=c(1,1,2)
)

# As seen in the cumulative lift chart, 'm2' performs much better than the baseline and 'mdf_KNN_fit2'
# when we select under 1000 observations. As the number of observations approaches 1200, the
# performance of 'm2' drops below that of random selection while that of 'mdf_KNN_fit2' approaches
# the same performance as random selection.
