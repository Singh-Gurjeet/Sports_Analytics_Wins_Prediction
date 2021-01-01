#------------------------------------------------------------------
# Sports Analytics - Predict Wins for the team
# Singh, Gurjeet
# Stand-alone program 
#------------------------------------------------------------------

library(readr)
library(car)
library(fBasics)
library(ggplot2)
library(corrplot)


#----------------------------------------------------------------------------
## 1 - Importing a Test File and check import
#----------------------------------------------------------------------------

summary(MoneyBall_Test)

colnames(MoneyBall_Test)[1] <- "INDEX"

#----------------------------------------------------------------------------
## 2 - DATA PREPARATION 
#----------------------------------------------------------------------------

#----------------------------------
##clean missing values with median values
#----------------------------------

MoneyBall_Test$IMP_TEAM_BATTING_SO <-ifelse(is.na(MoneyBall_Test$TEAM_BATTING_SO), 
                                            752,
                                            MoneyBall_Test$TEAM_BATTING_SO)
MoneyBall_Test$M_TEAM_BATTING_SO <- ifelse(is.na(MoneyBall_Test$TEAM_BATTING_SO), 
                                           1, 0)

MoneyBall_Test$IMP_TEAM_BASERUN_SB <- ifelse(is.na(MoneyBall_Test$TEAM_BASERUN_SB), 
                                             101, 
                                             MoneyBall_Test$TEAM_BASERUN_SB)
MoneyBall_Test$M_TEAM_BASERUN_SB <- ifelse(is.na(MoneyBall_Test$TEAM_BASERUN_SB), 
                                           1, 0)

MoneyBall_Test$IMP_TEAM_BASERUN_CS <- ifelse(is.na(MoneyBall_Test$TEAM_BASERUN_CS), 
                                             49, 
                                             MoneyBall_Test$TEAM_BASERUN_CS)
MoneyBall_Test$M_TEAM_BASERUN_CS <- ifelse(is.na(MoneyBall_Test$TEAM_BASERUN_CS), 
                                           1, 0)

MoneyBall_Test$IMP_TEAM_PITCHING_SO <- ifelse(is.na(MoneyBall_Test$TEAM_PITCHING_SO), 
                                              814, 
                                              MoneyBall_Test$TEAM_PITCHING_SO)
MoneyBall_Test$M_TEAM_PITCHING_SO <- ifelse(is.na(MoneyBall_Test$TEAM_PITCHING_SO), 
                                            1, 0)

MoneyBall_Test$IMP_TEAM_FIELDING_DP <- ifelse(is.na(MoneyBall_Test$TEAM_FIELDING_DP), 
                                              149, 
                                              MoneyBall_Test$TEAM_FIELDING_DP)
MoneyBall_Test$M_TEAM_FIELDING_DP <- ifelse(is.na(MoneyBall_Test$TEAM_FIELDING_DP), 
                                            1, 0)



#----------------------------------
##Cleaning outliers
#----------------------------------

#--1
MoneyBall_Test$IMP_TEAM_BATTING_H <- with(MoneyBall_Test, 
                                          ifelse(TEAM_BATTING_H < 1100, 1100,
                                                 ifelse(TEAM_BATTING_H > 2000, 2000,
                                                        TEAM_BATTING_H)));

#--2
MoneyBall_Test$IMP_TEAM_BATTING_BB <- with(MoneyBall_Test, 
                                           ifelse(TEAM_BATTING_BB < 150, 150,
                                                   ifelse(TEAM_BATTING_BB > 800, 800,
                                                               TEAM_BATTING_BB)));

#--3
MoneyBall_Test$IMP_TEAM_BATTING_SO <- with(MoneyBall_Test, 
                                           ifelse(IMP_TEAM_BATTING_SO < 72, 72,
                                                  ifelse(IMP_TEAM_BATTING_SO > 1350, 1350, 
                                                           IMP_TEAM_BATTING_SO)));

#--4
MoneyBall_Test$IMP_TEAM_BASERUN_SB <- with(MoneyBall_Test, 
                                           ifelse(IMP_TEAM_BASERUN_SB < 14, 14,
                                                  ifelse(IMP_TEAM_BASERUN_SB > 500, 500,
                                                         IMP_TEAM_BASERUN_SB)));

#--5
MoneyBall_Test$IMP_TEAM_BASERUN_CS <- with(MoneyBall_Test, 
                                           ifelse(IMP_TEAM_BASERUN_CS < 10, 10,
                                                      ifelse(IMP_TEAM_BASERUN_CS > 123, 123,
                                                             IMP_TEAM_BASERUN_CS)));

#--6
MoneyBall_Test$IMP_TEAM_PITCHING_H <- with(MoneyBall_Test, 
                                           ifelse(TEAM_PITCHING_H < 1200, 1200,
                                                       ifelse(TEAM_PITCHING_H > 2200, 2200,
                                                               TEAM_PITCHING_H)));
#--7
MoneyBall_Test$IMP_TEAM_PITCHING_HR <- with(MoneyBall_Test, 
                                            ifelse(TEAM_PITCHING_HR < 8, 8,
                                                           ifelse(TEAM_PITCHING_HR > 260, 260, 
                                                                 TEAM_PITCHING_HR)));
#--8
MoneyBall_Test$IMP_TEAM_PITCHING_BB <- with(MoneyBall_Test, 
                                            ifelse(TEAM_PITCHING_BB < 119, 119,
                                                         ifelse(TEAM_PITCHING_BB > 1000, 1000, 
                                                             TEAM_PITCHING_BB)));
#--9
MoneyBall_Test$IMP_TEAM_PITCHING_SO <- with(MoneyBall_Test, 
                                            ifelse(IMP_TEAM_PITCHING_SO < 241, 241,
                                                      ifelse(IMP_TEAM_PITCHING_SO > 1700, 1700, 
                                                           IMP_TEAM_PITCHING_SO)));
#--10
MoneyBall_Test$IMP_TEAM_FIELDING_E <- with(MoneyBall_Test, 
                                           ifelse(TEAM_FIELDING_E < 65, 65,
                                                      ifelse(TEAM_FIELDING_E > 500, 500,
                                                        TEAM_FIELDING_E)));
#--11
MoneyBall_Test$IMP_TEAM_FIELDING_DP <- with(MoneyBall_Test, 
                                            ifelse(IMP_TEAM_FIELDING_DP < 71, 71,
                                                    ifelse(IMP_TEAM_FIELDING_DP > 220, 220, 
                                                           IMP_TEAM_FIELDING_DP)));

#------------------------------------------------------------------------------
## 3- MODEL Deployment
#-------------------------------------------------------------------------------

View(t(basicStats(MoneyBall_Test)))

P_TARGET_WINS <- with(MoneyBall_Test,  - 3.960326 
                      + 0.060744 * IMP_TEAM_BATTING_H 
                      + 0.031502 * IMP_TEAM_BATTING_BB 
                      + 0.075213 * IMP_TEAM_BASERUN_SB 
                      + 30.282484 * M_TEAM_BASERUN_SB 
                      - 0.034563 * IMP_TEAM_BASERUN_CS 
                      + 6.428316 * M_TEAM_BASERUN_CS 
                      - 0.009303 * IMP_TEAM_PITCHING_H 
                      - 0.001774 * IMP_TEAM_PITCHING_BB 
                      - 0.073967 * IMP_TEAM_FIELDING_E)

FINAL_Submission <- with(MoneyBall_Test, cbind.data.frame(INDEX, round(P_TARGET_WINS)))

colnames(FINAL_Submission) <- c("INDEX", "P_TARGET_WINS")
View(FINAL_Submission)
View(t(basicStats(FINAL_Submission[-1])))

write.csv(FINAL_Submission, "Singh_Gurjeet_MoneyBall_Test_Score.csv")




















