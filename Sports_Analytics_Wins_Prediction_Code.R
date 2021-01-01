#------------------------------------------------------------------
# Sports Analytics - Predict Wins for the team
# Singh, Gurjeet
#------------------------------------------------------------------

library(readr)
library(car)
library(fBasics)
library(ggplot2)
library(corrplot)


MoneyBall <- read.csv("MoneyBall.csv")

#-------------------------------------------------------------------------------
## 1 - DATA EXPLORATION
#-------------------------------------------------------------------------------

colnames(MoneyBall)[1] <- "INDEX"

#Understand the stats and summary
str(MoneyBall)
summary(MoneyBall)
View(t(basicStats(MoneyBall)))

##Check the distribution of the quantitative variables
par(mfrow = c(4,4))
with(MoneyBall, hist(TARGET_WINS, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BATTING_H, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BATTING_2B, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BATTING_3B, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BATTING_HR, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BATTING_BB, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BATTING_SO, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BASERUN_SB, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BASERUN_CS, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_BATTING_HBP, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_PITCHING_H, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_PITCHING_HR, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_PITCHING_BB, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_PITCHING_SO, breaks = "FD", col = "light blue")); box();
with(MoneyBall, hist(TEAM_FIELDING_E, breaks = "FD", col = "light blue")); box(); 
with(MoneyBall, hist(TEAM_FIELDING_DP, breaks = "FD", col = "light blue")); box();
par(mfrow = c(1,1))


#check the normal distribution using QQplot
par(mfrow = c(3,3))
with(MoneyBall, qqPlot(TEAM_BATTING_H,  
                            main="QQ-Plot Batting H", col = "blue"));
with(MoneyBall, qqPlot(TEAM_BATTING_BB, 
                            main="QQ-Plot Batting BB", col = "blue"));
with(MoneyBall, qqPlot(TEAM_BATTING_SO, 
                            main="QQ-Plot Batting SO", col = "blue"));
with(MoneyBall, qqPlot(TEAM_BASERUN_SB, 
                            main="QQ-Plot Baserun SB", col = "blue"));
with(MoneyBall, qqPlot(TEAM_BASERUN_CS, 
                            main="QQ-Plot Baserun CS", col = "blue"));
with(MoneyBall, qqPlot(TEAM_PITCHING_H, 
                            main="QQ-Plot Pitching H",col = "blue"));
with(MoneyBall, qqPlot(TEAM_PITCHING_BB, 
                            main="QQ-Plot Pitching BB",col = "blue"));
with(MoneyBall, qqPlot(TEAM_PITCHING_SO, 
                            main="QQ-Plot Pitching SO",col = "blue"));
with(MoneyBall, qqPlot(TEAM_FIELDING_E, 
                            main="QQ-Plot Fielding E",col = "blue"));
par(mfrow = c(1,1))

##Examine Relationships
par(mfrow = c(4,3))
with(MoneyBall, plot(TEAM_BATTING_H ,TARGET_WINS, 
                            main="TARGET_WINS vs TEAM_BATTING_H",
                            col = "dark orange"))
with(MoneyBall, plot(TEAM_BATTING_BB ,TARGET_WINS, 
                           main="TARGET_WINS vs TEAM_BATTING_BB",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_BATTING_SO,TARGET_WINS , 
                           main="TARGET_WINS vs TEAM_BATTING_SO",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_BASERUN_SB,TARGET_WINS , 
                           main="TARGET_WINS vs TEAM_BASERUN_SB",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_BASERUN_CS,TARGET_WINS , 
                           main="TARGET_WINS vs TEAM_BASERUN_CS",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_PITCHING_H ,TARGET_WINS, 
                           main="TARGET_WINS vs TEAM_PITCHING_H",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_PITCHING_BB,TARGET_WINS , 
                           main="TARGET_WINS vs TEAM_PITCHING_BB",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_PITCHING_SO,TARGET_WINS , 
                           main="TARGET_WINS vs TEAM_PITCHING_SO",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_FIELDING_E,TARGET_WINS, 
                           main="TARGET_WINS vs TEAM_FIELDING_E",
                           col = "dark orange"))
with(MoneyBall, plot(TEAM_FIELDING_DP ,TARGET_WINS, 
                           main="TARGET_WINS vs TEAM_FIELDING_DP",
                           col = "dark orange"))
par(mfrow = c(1,1))


#------------------------------------------------------------------------------
## 2 - DATA PREPARATION 
#------------------------------------------------------------------------------

#----------------------------------------
##clean missing values with median values
#----------------------------------------

summary(MoneyBall)
##clean missing values with median values

MoneyBall$IMP_TEAM_BATTING_SO <-ifelse(is.na(MoneyBall$TEAM_BATTING_SO), 
                                           752,
                                           MoneyBall$TEAM_BATTING_SO)
MoneyBall$M_TEAM_BATTING_SO <- ifelse(is.na(MoneyBall$TEAM_BATTING_SO), 
                                           1, 0)

MoneyBall$IMP_TEAM_BASERUN_SB <- ifelse(is.na(MoneyBall$TEAM_BASERUN_SB), 
                                           101, 
                                           MoneyBall$TEAM_BASERUN_SB)
MoneyBall$M_TEAM_BASERUN_SB <- ifelse(is.na(MoneyBall$TEAM_BASERUN_SB), 
                                           1, 0)

MoneyBall$IMP_TEAM_BASERUN_CS <- ifelse(is.na(MoneyBall$TEAM_BASERUN_CS), 
                                           49, 
                                           MoneyBall$TEAM_BASERUN_CS)
MoneyBall$M_TEAM_BASERUN_CS <- ifelse(is.na(MoneyBall$TEAM_BASERUN_CS), 
                                           1, 0)

MoneyBall$IMP_TEAM_PITCHING_SO <- ifelse(is.na(MoneyBall$TEAM_PITCHING_SO), 
                                           814, 
                                           MoneyBall$TEAM_PITCHING_SO)
MoneyBall$M_TEAM_PITCHING_SO <- ifelse(is.na(MoneyBall$TEAM_PITCHING_SO), 
                                           1, 0)

MoneyBall$IMP_TEAM_FIELDING_DP <- ifelse(is.na(MoneyBall$TEAM_FIELDING_DP), 
                                           149, 
                                           MoneyBall$TEAM_FIELDING_DP)
MoneyBall$M_TEAM_FIELDING_DP <- ifelse(is.na(MoneyBall$TEAM_FIELDING_DP), 
                                            1, 0)


#----------------------------------
##Checking Correlation
#----------------------------------

#selectiong variables for correlation list
corr.list <- c('TARGET_WINS','TEAM_BATTING_H','TEAM_BATTING_BB',
               'IMP_TEAM_BATTING_SO','IMP_TEAM_BASERUN_SB',
               'IMP_TEAM_BASERUN_CS','TEAM_PITCHING_H',
               'TEAM_PITCHING_BB','IMP_TEAM_PITCHING_SO',
               'TEAM_FIELDING_E','IMP_TEAM_FIELDING_DP')

#Checking correlation only for the variables above.
MoneyBall.Corr <- MoneyBall[,(names(MoneyBall) %in% corr.list )]

corrplot(cor(MoneyBall.Corr), method="number")

#----------------------------------
##creating a 1st and 99th percentile
#----------------------------------

percentile.val <- matrix(with(MoneyBall, 
                                c("TEAM_BATTING_H", 
                                      min(TEAM_BATTING_H), 
                                      max(TEAM_BATTING_H), 
                                      quantile(TEAM_BATTING_H, (0.01))[[1]],
                                      quantile(TEAM_BATTING_H, (0.99))[[1]] )), 
                                ncol = 5, nrow = 1)

colnames(percentile.val) <- c("FieldName","Min","Max","1-Percentile", 
                              "99-Percentile")

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall, 
                                c("TEAM_BATTING_2B",
                                      min(TEAM_BATTING_2B), 
                                      max(TEAM_BATTING_2B), 
                                      quantile(TEAM_BATTING_2B, (0.01))[[1]],
                                      quantile(TEAM_BATTING_2B, (0.99))[[1]] )), 
                                  ncol = 5, nrow = 1))

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                c("TEAM_BATTING_3B",
                                      min(TEAM_BATTING_3B), 
                                      max(TEAM_BATTING_3B), 
                                      quantile(TEAM_BATTING_3B, (0.01))[[1]],
                                      quantile(TEAM_BATTING_3B, (0.99))[[1]] )), 
                                  ncol = 5, nrow = 1))

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                c("TEAM_BATTING_HR",
                                      min(TEAM_BATTING_HR), 
                                      max(TEAM_BATTING_HR), 
                                      quantile(TEAM_BATTING_HR, (0.01))[[1]],
                                      quantile(TEAM_BATTING_HR, (0.99))[[1]] )), 
                                  ncol = 5, nrow = 1))

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                c("TEAM_BATTING_BB",
                                      min(TEAM_BATTING_BB), 
                                      max(TEAM_BATTING_BB), 
                                      quantile(TEAM_BATTING_BB, (0.01))[[1]],
                                      quantile(TEAM_BATTING_BB, (0.99))[[1]] )), 
                                  ncol = 5, nrow = 1))

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                c("IMP_TEAM_BATTING_SO",
                                      min(IMP_TEAM_BATTING_SO), 
                                      max(IMP_TEAM_BATTING_SO), 
                                      quantile(IMP_TEAM_BATTING_SO, (0.01))[[1]],
                                      quantile(IMP_TEAM_BATTING_SO, (0.99))[[1]] )), 
                                  ncol = 5, nrow = 1))


percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall, 
                                c("IMP_TEAM_BASERUN_SB",
                                      min(IMP_TEAM_BASERUN_SB), 
                                      max(IMP_TEAM_BASERUN_SB), 
                                      quantile(IMP_TEAM_BASERUN_SB, (0.01))[[1]],
                                      quantile(IMP_TEAM_BASERUN_SB, (0.99))[[1]] )), 
                                  ncol = 5, nrow = 1))


percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                c("IMP_TEAM_BASERUN_CS",
                                      min(IMP_TEAM_BASERUN_CS), 
                                      max(IMP_TEAM_BASERUN_CS), 
                                      quantile(IMP_TEAM_BASERUN_CS, (0.01))[[1]],
                                      quantile(IMP_TEAM_BASERUN_CS, (0.99))[[1]] )), 
                                  ncol = 5, nrow = 1))


percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                    c("TEAM_PITCHING_H",
                                      min(TEAM_PITCHING_H), 
                                      max(TEAM_PITCHING_H), 
                                      quantile(TEAM_PITCHING_H, (0.01))[[1]],
                                      quantile(TEAM_PITCHING_H, (0.99))[[1]] )), 
                               ncol = 5, nrow = 1))


percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                    c("TEAM_PITCHING_HR",
                                      min(TEAM_PITCHING_HR), 
                                      max(TEAM_PITCHING_HR), 
                                      quantile(TEAM_PITCHING_HR, (0.01))[[1]],
                                      quantile(TEAM_PITCHING_HR, (0.99))[[1]] )), 
                               ncol = 5, nrow = 1))
  

percentile.val <- rbind(percentile.val,
                         matrix(with(MoneyBall,
                                     c("TEAM_PITCHING_BB",
                                       min(TEAM_PITCHING_BB), 
                                       max(TEAM_PITCHING_BB), 
                                       quantile(TEAM_PITCHING_BB, (0.01))[[1]],
                                       quantile(TEAM_PITCHING_BB, (0.99))[[1]] )), 
                                ncol = 5, nrow = 1))

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                    c("IMP_TEAM_PITCHING_SO",
                                      min(IMP_TEAM_PITCHING_SO), 
                                      max(IMP_TEAM_PITCHING_SO), 
                                      quantile(IMP_TEAM_PITCHING_SO, (0.01))[[1]],
                                      quantile(IMP_TEAM_PITCHING_SO, (0.99))[[1]] )), 
                               ncol = 5, nrow = 1))

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                    c("TEAM_FIELDING_E",
                                      min(TEAM_FIELDING_E), 
                                      max(TEAM_FIELDING_E), 
                                      quantile(TEAM_FIELDING_E, (0.01))[[1]],
                                      quantile(TEAM_FIELDING_E, (0.99))[[1]] )), 
                               ncol = 5, nrow = 1))

percentile.val <- rbind(percentile.val,
                        matrix(with(MoneyBall,
                                    c("IMP_TEAM_FIELDING_DP",
                                      min(IMP_TEAM_FIELDING_DP), 
                                      max(IMP_TEAM_FIELDING_DP), 
                                      quantile(IMP_TEAM_FIELDING_DP, (0.01))[[1]],
                                      quantile(IMP_TEAM_FIELDING_DP, (0.99))[[1]] )), 
                               ncol = 5, nrow = 1))

View(percentile.val)

#------------------------------------------------------------------------------
##Cleaning outliers
#-------------------------------------------------------------------------------

#--1
MoneyBall$IMP_TEAM_BATTING_H <- with(MoneyBall, 
                                          ifelse(TEAM_BATTING_H < 1100, 1100,
                                               ifelse(TEAM_BATTING_H > 2000, 2000,
                                                      TEAM_BATTING_H)));

#--2
MoneyBall$IMP_TEAM_BATTING_BB <- with(MoneyBall, ifelse(TEAM_BATTING_BB < 150, 150,
                                          ifelse(TEAM_BATTING_BB > 800, 800,
                                                            TEAM_BATTING_BB)));

#--3
MoneyBall$IMP_TEAM_BATTING_SO <- with(MoneyBall, ifelse(IMP_TEAM_BATTING_SO < 72, 72,
                                          ifelse(IMP_TEAM_BATTING_SO > 1350, 1350, 
                                                           IMP_TEAM_BATTING_SO)));

#--4
MoneyBall$IMP_TEAM_BASERUN_SB <- with(MoneyBall, ifelse(IMP_TEAM_BASERUN_SB < 14, 14,
                                          ifelse(IMP_TEAM_BASERUN_SB > 500, 500,
                                                           IMP_TEAM_BASERUN_SB)));

#--5
MoneyBall$IMP_TEAM_BASERUN_CS <- with(MoneyBall, ifelse(IMP_TEAM_BASERUN_CS < 10, 10,
                                          ifelse(IMP_TEAM_BASERUN_CS > 123, 123,
                                                           IMP_TEAM_BASERUN_CS)));

#--6
MoneyBall$IMP_TEAM_PITCHING_H <- with(MoneyBall, ifelse(TEAM_PITCHING_H < 1200, 1200,
                                          ifelse(TEAM_PITCHING_H > 2200, 2200,
                                                           TEAM_PITCHING_H)));
#--7
MoneyBall$IMP_TEAM_PITCHING_HR <- with(MoneyBall, ifelse(TEAM_PITCHING_HR < 8, 8,
                                          ifelse(TEAM_PITCHING_HR > 260, 260, 
                                                           TEAM_PITCHING_HR)));
#--8
MoneyBall$IMP_TEAM_PITCHING_BB <- with(MoneyBall, ifelse(TEAM_PITCHING_BB < 119, 119,
                                           ifelse(TEAM_PITCHING_BB > 1000, 1000, 
                                                           TEAM_PITCHING_BB)));
#--9
MoneyBall$IMP_TEAM_PITCHING_SO <- with(MoneyBall, ifelse(IMP_TEAM_PITCHING_SO < 241, 241,
                                           ifelse(IMP_TEAM_PITCHING_SO > 1700, 1700, 
                                                           IMP_TEAM_PITCHING_SO)));
#--10
MoneyBall$IMP_TEAM_FIELDING_E <- with(MoneyBall, ifelse(TEAM_FIELDING_E < 65, 65,
                                           ifelse(TEAM_FIELDING_E > 500, 500,
                                                           TEAM_FIELDING_E)));
#--11
MoneyBall$IMP_TEAM_FIELDING_DP <- with(MoneyBall, ifelse(IMP_TEAM_FIELDING_DP < 71, 71,
                                            ifelse(IMP_TEAM_FIELDING_DP > 220, 220, 
                                                       IMP_TEAM_FIELDING_DP)));

#options(scipen = 111)
View(t(basicStats(MoneyBall)))


###-------------------------------------------------------------------------------------
### Re-checking distributions and relationship after fixing outliers and missing values
###-------------------------------------------------------------------------------------


##Check the distribution of the quantitative variables
par(mfrow = c(4,3))
with(MoneyBall, hist(TARGET_WINS, breaks = "FD", 
                     col = "light blue")); box();
with(MoneyBall, hist(IMP_TEAM_BATTING_H, breaks = "FD", 
                     col = "light blue")); box();
with(MoneyBall, hist(IMP_TEAM_BATTING_BB, breaks = "FD", 
                     col = "light blue")); box();
with(MoneyBall, hist(IMP_TEAM_BATTING_SO, breaks = "FD", 
                     col = "light blue")); box(); 
with(MoneyBall, hist(IMP_TEAM_BASERUN_SB, breaks = "FD", 
                     col = "light blue")); box(); 
with(MoneyBall, hist(IMP_TEAM_BASERUN_CS, breaks = "FD", 
                     col = "light blue")); box();
with(MoneyBall, hist(IMP_TEAM_PITCHING_H, breaks = "FD", 
                     col = "light blue")); box();
with(MoneyBall, hist(IMP_TEAM_PITCHING_HR, breaks = "FD", 
                     col = "light blue")); box(); 
with(MoneyBall, hist(IMP_TEAM_PITCHING_BB, breaks = "FD", 
                     col = "light blue")); box(); 
with(MoneyBall, hist(IMP_TEAM_PITCHING_SO, breaks = "FD", 
                     col = "light blue")); box(); 
with(MoneyBall, hist(IMP_TEAM_FIELDING_E, breaks = "FD", 
                     col = "light blue")); box(); 
with(MoneyBall, hist(IMP_TEAM_FIELDING_DP, breaks = "FD", 
                     col = "light blue")); box();
par(mfrow = c(1,1))

##Examine Relationships
par(mfrow = c(4,3))
with(MoneyBall, plot(IMP_TEAM_BATTING_H ,TARGET_WINS, 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_BATTING_BB ,TARGET_WINS, 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_BATTING_SO,TARGET_WINS , 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_BASERUN_SB,TARGET_WINS , 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_BASERUN_CS,TARGET_WINS , 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_PITCHING_H ,TARGET_WINS, 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_PITCHING_BB,TARGET_WINS ,
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_PITCHING_SO,TARGET_WINS , 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_FIELDING_E,TARGET_WINS, 
                     col = "dark orange"))
with(MoneyBall, plot(IMP_TEAM_FIELDING_DP ,TARGET_WINS, 
                     col = "dark orange"))
par(mfrow = c(1,1))

#--------------------------
##creating a drop list
#-------------------------------------------------------------------------------

#creating a drop list to remove not required variables.
drop.list <- c('INDEX','TEAM_BATTING_HBP','TEAM_BATTING_SO',
                'TEAM_BATTING_BB','TEAM_BASERUN_SB','TEAM_BASERUN_CS',
                'TEAM_PITCHING_SO','TEAM_FIELDING_DP','TEAM_BATTING_H',
                'TEAM_BATTING_2B','TEAM_BATTING_3B', 'TEAM_BATTING_HR',
                'TEAM_PITCHING_H','TEAM_PITCHING_HR','TEAM_PITCHING_BB'
               ,'TEAM_FIELDING_E')

#droping the variables
MoneyBall <- MoneyBall[,!(names(MoneyBall) %in% drop.list )]

View(t(basicStats(MoneyBall)))


#------------------------------------------------------------------------------
## 3- Build Models
#-------------------------------------------------------------------------------

#------------------------------
##Model_1_lm 
#------------------------------
names(MoneyBall)

##Adjusted R-squared:     0.3151  
Model_1_lm <- lm(TARGET_WINS ~ IMP_TEAM_BATTING_H  + 
                            IMP_TEAM_BATTING_BB +
                            IMP_TEAM_BASERUN_SB + M_TEAM_BASERUN_SB + 
                            IMP_TEAM_FIELDING_E +
                            IMP_TEAM_PITCHING_BB +
                            IMP_TEAM_PITCHING_H
                          ,data=MoneyBall)

summary(Model_1_lm)

#------------------------------
##Model_1_lm - Assessing the Goodness-Of-Fit in OLS Regression
#------------------------------ 

# Validating the normality assumption:
par(mfrow = c(1,2))
#Creating 2 Q-Q plots to evaluate the distribution of 
# SalePrice and L_SalePrice
qqnorm(Model_1_lm$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Standardized residuals",datax=FALSE)

qqline(Model_1_lm$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")

hist(Model_1_lm$residuals, breaks = "FD", col = "violet"); box();
par(mfrow = c(1,1))

#Validating the homoscedasticity assumption (equal variance):
residualPlots(Model_1_lm)
    # par(mfrow = c(1,1))
    # residualPlot(Model_1_lm)

#------------------------------
##Model_2_lm 
#------------------------------
names(MoneyBall)

##Adjusted R-squared:     0.3148  
Model_2_lm <- lm(TARGET_WINS ~ IMP_TEAM_BATTING_H  + 
                             IMP_TEAM_BATTING_BB +
                             IMP_TEAM_BASERUN_SB + M_TEAM_BASERUN_SB + 
                             IMP_TEAM_FIELDING_E +
                             IMP_TEAM_PITCHING_BB +
                             IMP_TEAM_PITCHING_H +
                             IMP_TEAM_PITCHING_HR
                           ,data=MoneyBall)

summary(Model_2_lm)

#------------------------------
##Model_2_lm - Assessing the Goodness-Of-Fit in OLS Regression
#------------------------------ 

# Validating the normality assumption:
par(mfrow = c(1,2))
#Creating 2 Q-Q plots to evaluate the distribution of 
# SalePrice and L_SalePrice
qqnorm(Model_2_lm$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Standardized residuals",datax=FALSE)

qqline(Model_2_lm$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")

hist(Model_2_lm$residuals, breaks = "FD", col = "violet"); box();
par(mfrow = c(1,1))

#Validating the homoscedasticity assumption (equal variance):
residualPlots(Model_2_lm)
par(mfrow = c(1,1))
residualPlot(Model_2_lm)



#------------------------------
##Model_3_lm
#------------------------------
names(MoneyBall)

##Adjusted R-squared:     0.3344  
Model_3_lm <- lm(TARGET_WINS ~ IMP_TEAM_BATTING_H  + 
                             IMP_TEAM_BATTING_BB +
                             IMP_TEAM_BASERUN_SB + M_TEAM_BASERUN_SB + 
                             IMP_TEAM_BASERUN_CS + M_TEAM_BASERUN_CS +
                             IMP_TEAM_PITCHING_H +
                             IMP_TEAM_PITCHING_BB +
                             IMP_TEAM_FIELDING_E
                           ,data=MoneyBall)

summary(Model_3_lm)

#------------------------------
##Model_3_lm - Assessing the Goodness-Of-Fit in OLS Regression
#------------------------------ 

# Validating the normality assumption:
par(mfrow = c(1,2))
#Creating 2 Q-Q plots to evaluate the distribution of 
# SalePrice and L_SalePrice
qqnorm(Model_3_lm$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Standardized residuals",datax=FALSE)

qqline(Model_3_lm$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")

hist(Model_3_lm$residuals, breaks = "FD", col = "violet"); box();
par(mfrow = c(1,1))

#Validating the homoscedasticity assumption (equal variance):
residualPlots(Model_3_lm)
    # par(mfrow = c(1,1))
    # residualPlot(Model_3_lm)

#-----------------------------------------------------------------------------
## 4- SELECT MODELS - Predictive Accuracy 
#-----------------------------------------------------------------------------

#extract the model information from summary output
Model_1_call <- Model_1_lm$call
Model_2_call <- Model_2_lm$call
Model_3_call <- Model_3_lm$call

#Printing the each model.
Model_1_call

Model_2_call

Model_3_call


# Compute the VIF values
library(car)
Model_1_lm.VIF <- as.matrix(sort(vif(Model_1_lm),decreasing=TRUE))
Model_2_lm.VIF <- as.matrix(sort(vif(Model_2_lm),decreasing=TRUE))
Model_3_lm.VIF <- as.matrix(sort(vif(Model_3_lm),decreasing=TRUE))


colnames(Model_1_lm.VIF) <- "VIF_Values"
colnames(Model_2_lm.VIF) <- "VIF_Values"
colnames(Model_3_lm.VIF) <- "VIF_Values"


View(Model_1_lm.VIF)
View(Model_2_lm.VIF)
View(Model_3_lm.VIF)

##MSE
mse.Model_1_lm <- mean(Model_1_lm$residuals^2)
mse.Model_2_lm <- mean(Model_2_lm$residuals^2)
mse.Model_3_lm <- mean(Model_3_lm$residuals^2)


##MAE
mae.Model_1_lm <- mean(abs(Model_1_lm$residuals))
mae.Model_2_lm <- mean(abs(Model_2_lm$residuals))
mae.Model_3_lm <- mean(abs(Model_3_lm$residuals))



##Creating a Table to include all the metrics
rsqrd.Mat <- matrix(c(summary(Model_1_lm)$adj.r.squared, 
                      summary(Model_2_lm)$adj.r.squared, 
                      summary(Model_3_lm)$adj.r.squared), 
                    ncol = 1)

rownames(rsqrd.Mat) <- c("Model_1_lm", "Model_2_lm", "Model_3_lm")
colnames(rsqrd.Mat) <- "Adjusted_R_Squared"


AIC.Mat <- matrix(c(AIC(Model_1_lm), 
                    AIC(Model_2_lm), 
                    AIC(Model_3_lm)), 
                  ncol = 1)
rownames(AIC.Mat) <- c("Model_1_lm", "Model_2_lm", "Model_3_lm")
colnames(AIC.Mat) <- "AIC_Values"


BIC.Mat <- matrix(c(BIC(Model_1_lm), 
                    BIC(Model_2_lm), 
                    BIC(Model_3_lm)), 
                  ncol = 1)
rownames(BIC.Mat) <-  c("Model_1_lm", "Model_2_lm", "Model_3_lm")
colnames(BIC.Mat) <- "BIC_Values"


MSE.Mat <- matrix(c(mse.Model_1_lm, 
                    mse.Model_2_lm, 
                    mse.Model_3_lm), 
                  ncol = 1)
rownames(MSE.Mat) <-  c("Model_1_lm", "Model_2_lm", "Model_3_lm")
colnames(MSE.Mat) <- "MSE_Values"

MAE.Mat <- matrix(c(mae.Model_1_lm, 
                    mae.Model_2_lm, 
                    mae.Model_3_lm), 
                  ncol = 1)
rownames(MAE.Mat) <-  c("Model_1_lm", "Model_2_lm", "Model_3_lm")
colnames(MAE.Mat) <- "MAE_Values"


final.table  <- cbind( rsqrd.Mat, 
                       AIC.Mat, 
                       BIC.Mat, 
                       MSE.Mat, 
                       MAE.Mat)

View(final.table)







