rm(list = ls())
soccer <-read.csv(file = "C:/Users/16109/OneDrive/Documents/ICA1/fully_linked_table.csv")

#create a new column
soccer$home_team_outcome <- ifelse(soccer$home_team_goal>soccer$away_team_goal, "win", "loss")
#split data into test and train
library(caTools)
split = sample.split(soccer, SplitRatio = 0.5)
training <- subset(soccer, split = TRUE)
test <- subset(soccer, split= FALSE)

#create factor
training$home_team_outcome <- factor(training$home_team_outcome)

#create a tree
library("rpart")
library(rpart.plot)
cart01 <- rpart(formula = home_team_outcome ~ HT_P1_Overall_Rating + HT_P2_Overall_Rating
                + HT_P3_Overall_Rating + HT_P4_Overall_Rating +  HT_P5_Overall_Rating + 
                  HT_P6_Overall_Rating + HT_P7_Overall_Rating + HT_P8_Overall_Rating + 
                  HT_P9_Overall_Rating + HT_P10_Overall_Rating + HT_P11_Overall_Rating, data = training, na.action = na.pass)
rpart.plot(cart01, type = 5, extra = 100)

x = data.frame(home_team_outcome = training$home_team_outcome, HT_P1_Overall_Rating = training$HT_P1_Overall_Rating,
               HT_P2_Overall_Rating = training$HT_P2_Overall_Rating, HT_P3_Overall_Rating = training$HT_P3_Overall_Rating,
               HT_P4_Overall_Rating = training$HT_P4_Overall_Rating, HT_P5_Overall_Rating = training$HT_P5_Overall_Rating,
               HT_P6_Overall_Rating = training$HT_P6_Overall_Rating, HT_P7_Overall_Rating = training$HT_P7_Overall_Rating,
               HT_P8_Overall_Rating = training$HT_P8_Overall_Rating, HT_P9_Overall_Rating = training$HT_P9_Overall_Rating,
               HT_P10_Overall_Rating = training$HT_P10_Overall_Rating, HT_P11_Overall_Rating = training$HT_P11_Overall_Rating)

predictHome_team_outcomeCART <- predict(cart01, x, "prob")
summary(predictHome_team_outcomeCART)

cart02 <- rpart(formula = home_team_outcome ~ AT_P1_Overall_Rating + AT_P2_Overall_Rating
                + AT_P3_Overall_Rating + AT_P4_Overall_Rating +  AT_P5_Overall_Rating + 
                  AT_P6_Overall_Rating + AT_P7_Overall_Rating + AT_P8_Overall_Rating + 
                  AT_P9_Overall_Rating + AT_P10_Overall_Rating + AT_P11_Overall_Rating, data = training)

y = data.frame(home_team_outcome = training$home_team_outcome, AT_P1_Overall_Rating = training$AT_P1_Overall_Rating,
               AT_P2_Overall_Rating = training$AT_P2_Overall_Rating, AT_P3_Overall_Rating = training$AT_P3_Overall_Rating,
               AT_P4_Overall_Rating = training$AT_P4_Overall_Rating, AT_P5_Overall_Rating = training$AT_P5_Overall_Rating,
               AT_P6_Overall_Rating = training$AT_P6_Overall_Rating, AT_P7_Overall_Rating = training$AT_P7_Overall_Rating,
               AT_P8_Overall_Rating = training$AT_P8_Overall_Rating, AT_P9_Overall_Rating = training$AT_P9_Overall_Rating,
               AT_P10_Overall_Rating = training$AT_P10_Overall_Rating, AT_P11_Overall_Rating = training$AT_P11_Overall_Rating)
rpart.plot(cart02, type = 5, extra = 100)
summary(cart02)
predictHome_team_outcomeCART1 <- predict(cart02, y, "prob")
summary(predictHome_team_outcomeCART1)


#build a c5 decision tree
library(C50)
c6 <- C5.0(formula = home_team_outcome ~ HT_P1_Overall_Rating + HT_P2_Overall_Rating
           + HT_P3_Overall_Rating + HT_P4_Overall_Rating +  HT_P5_Overall_Rating + 
             HT_P6_Overall_Rating + HT_P7_Overall_Rating + HT_P8_Overall_Rating + 
             HT_P9_Overall_Rating + HT_P10_Overall_Rating + HT_P11_Overall_Rating,
             data = training,control = C5.0Control(minCases = 100))
plot(c6)

x = data.frame(home_team_outcome = training$home_team_outcome, HT_P1_Overall_Rating = training$HT_P1_Overall_Rating,
               HT_P2_Overall_Rating = training$HT_P2_Overall_Rating, HT_P3_Overall_Rating = training$HT_P3_Overall_Rating,
               HT_P4_Overall_Rating = training$HT_P4_Overall_Rating, HT_P5_Overall_Rating = training$HT_P5_Overall_Rating,
               HT_P6_Overall_Rating = training$HT_P6_Overall_Rating, HT_P7_Overall_Rating = training$HT_P7_Overall_Rating,
               HT_P8_Overall_Rating = training$HT_P8_Overall_Rating, HT_P9_Overall_Rating = training$HT_P9_Overall_Rating,
               HT_P10_Overall_Rating = training$HT_P10_Overall_Rating, HT_P11_Overall_Rating = training$HT_P11_Overall_Rating)
pred1<- predict(object = c6, newdata = x)
summary(pred1)

c7 <- C5.0(formula = home_team_outcome ~ AT_P1_Overall_Rating + AT_P2_Overall_Rating
           + AT_P3_Overall_Rating + AT_P4_Overall_Rating +  AT_P5_Overall_Rating + 
             AT_P6_Overall_Rating + AT_P7_Overall_Rating + AT_P8_Overall_Rating + 
             AT_P9_Overall_Rating + AT_P10_Overall_Rating + AT_P11_Overall_Rating,
           data = training,control = C5.0Control(minCases = 100))
plot(c7)

y = data.frame(home_team_outcome = training$home_team_outcome, AT_P1_Overall_Rating = training$AT_P1_Overall_Rating,
               AT_P2_Overall_Rating = training$AT_P2_Overall_Rating, AT_P3_Overall_Rating = training$AT_P3_Overall_Rating,
               AT_P4_Overall_Rating = training$AT_P4_Overall_Rating, AT_P5_Overall_Rating = training$AT_P5_Overall_Rating,
               AT_P6_Overall_Rating = training$AT_P6_Overall_Rating, AT_P7_Overall_Rating = training$AT_P7_Overall_Rating,
               AT_P8_Overall_Rating = training$AT_P8_Overall_Rating, AT_P9_Overall_Rating = training$AT_P9_Overall_Rating,
               AT_P10_Overall_Rating = training$AT_P10_Overall_Rating, AT_P11_Overall_Rating = training$AT_P11_Overall_Rating)
pred2 <- predict(object = c7, newdata = y)
summary(pred2)

c8<- C5.0(formula = home_team_outcome ~ HT_P1_Overall_Rating + HT_P2_Overall_Rating
         + HT_P3_Overall_Rating + HT_P4_Overall_Rating +  HT_P5_Overall_Rating + 
           HT_P6_Overall_Rating + HT_P7_Overall_Rating + HT_P8_Overall_Rating + 
           HT_P9_Overall_Rating + HT_P10_Overall_Rating + HT_P11_Overall_Rating +
           AT_P1_Overall_Rating + AT_P2_Overall_Rating
         + AT_P3_Overall_Rating + AT_P4_Overall_Rating +  AT_P5_Overall_Rating + 
           AT_P6_Overall_Rating + AT_P7_Overall_Rating + AT_P8_Overall_Rating + 
           AT_P9_Overall_Rating + AT_P10_Overall_Rating + AT_P11_Overall_Rating,
         data = training,control = C5.0Control(minCases = 100))
plot(c8, gp = gpar(fontsize = 7.5))           
install.packages("partykit")
library("partykit")
party <- as.party(c8)
plot(party, gp = gpar(fontsize = 7.5))

z <- data.frame(home_team_outcome = training$home_team_outcome, HT_P1_Overall_Rating = training$HT_P1_Overall_Rating,
                HT_P2_Overall_Rating = training$HT_P2_Overall_Rating, HT_P3_Overall_Rating = training$HT_P3_Overall_Rating,
                HT_P4_Overall_Rating = training$HT_P4_Overall_Rating, HT_P5_Overall_Rating = training$HT_P5_Overall_Rating,
                HT_P6_Overall_Rating = training$HT_P6_Overall_Rating, HT_P7_Overall_Rating = training$HT_P7_Overall_Rating,
                HT_P8_Overall_Rating = training$HT_P8_Overall_Rating, HT_P9_Overall_Rating = training$HT_P9_Overall_Rating,
                HT_P10_Overall_Rating = training$HT_P10_Overall_Rating, HT_P11_Overall_Rating = training$HT_P11_Overall_Rating,
                AT_P1_Overall_Rating = training$AT_P1_Overall_Rating,
                AT_P2_Overall_Rating = training$AT_P2_Overall_Rating, AT_P3_Overall_Rating = training$AT_P3_Overall_Rating,
                AT_P4_Overall_Rating = training$AT_P4_Overall_Rating, AT_P5_Overall_Rating = training$AT_P5_Overall_Rating,
                AT_P6_Overall_Rating = training$AT_P6_Overall_Rating, AT_P7_Overall_Rating = training$AT_P7_Overall_Rating,
                AT_P8_Overall_Rating = training$AT_P8_Overall_Rating, AT_P9_Overall_Rating = training$AT_P9_Overall_Rating,
                AT_P10_Overall_Rating = training$AT_P10_Overall_Rating, AT_P11_Overall_Rating = training$AT_P11_Overall_Rating)
pred3<- predict.C5.0(object = c8, newdata = z)
summary(pred3)
