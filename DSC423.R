library(ggplot2)
library(dplyr)
library(GGally)
# 2018 and 2019 NFL Data
NFL2019 <- read.csv("2019_NFL.csv", header = TRUE , sep = ",")
NFL2018 <- read.csv("2018_NFL.csv", header = TRUE , sep = ",")

# Combining Datasets
NFLDATA <- rbind(NFL2018,NFL2019)
# Looking at right skewdness 
hist(NFL2019$Sk?.., breaks = 20, freq = FALSE)
hist(NFLDATA$Sk?.., breaks = 20, freq = FALSE)
boxplot(NFLDATA$Sk?..)

###### Figuring out which rows to exclude, SKIP TO ROW 70 for data NOT excluding 0 sacks
NFL2019$Sk?.. == 0
# Percent of Observations that are 0 sacks 
((869-404)/869) * 100
NFL2018$Sk?.. == 0

# 2018 and 2019 NFL Data Excluding 0 sacks 
New2019 <- read.csv("2019_NFL.csv", header = TRUE , sep = ",", nrows = 404)
New2018 <- read.csv("2018_NFL.csv", header = TRUE , sep = ",", nrows = 407)

NEWDATA <- rbind(New2018,New2019)
hist(New2019$Sk?.. , breaks = 20 , freq = FALSE)
hist(NEWDATA$Sk?.. , breaks = 20, freq = FALSE)
boxplot(NEWDATA$Sk?..)


# Get rid of irrelevant variables 
summary(NEWDATA$Pos)
summary(NFLDATA$Pos)
NEWDATA
abc <- NEWDATA[,-c(1:3,5,8:19)]
abc

# Convert MTKL. b/c percent to numeric
abc$MTkl. <- as.numeric(sub("%","",abc$MTkl.))/100

# Summary stats
summary(abc)
boxplot(abc)
hist(abc$Age, breaks = 20)
hist(abc$Bltz, breaks = 20)
hist(abc$Hrry, breaks = 20)
hist(abc$QBKD, breaks = 20)
hist(abc$Prss, breaks = 20)
hist(abc$Comb, breaks = 20)
hist(abc$MTkl, breaks = 20)
hist(abc$MTkl., breaks = 20)

cor(abc)
ggpairs(data = abc, 1:11)

# Rename Variables for Correlation Matrix
abc <- abc %>%
  rename(
    Games_Played = G,
    Games_Started = GS,
    Sacks = Skâ.., 
    QB_Pressures = Prss,
    Tackles = Comb,
    Missed_Tackles = MTkl,
    Missed_Tackle_Percent = MTkl.
  )

# Doing the correlation matrix with renamed variables 
cor(abc)
ggpairs(data = abc, 1:11)

###### Doing All the Same with Original 2018-2019 DATA

# Get rid of irrelevant variables 
NFLDATA2.0 <- NFLDATA[,-c(1:3,5,8:19)]
NFLDATA2.0

# Convert MTKL. b/c percent to numeric
NFLDATA2.0$MTkl. <- as.numeric(sub("%","",NFLDATA2.0$MTkl.))/100

# Summary stats
summary(NFLDATA2.0)
boxplot(NFLDATA2.0)
hist(NFLDATA2.0$Age, breaks = 20)
hist(NFLDATA2.0$Bltz, breaks = 20)
hist(NFLDATA2.0$Hrry, breaks = 20)
hist(NFLDATA2.0$QBKD, breaks = 20)
hist(NFLDATA2.0$Prss, breaks = 20)
hist(NFLDATA2.0$Comb, breaks = 20)
hist(NFLDATA2.0$MTkl, breaks = 20)
hist(NFLDATA2.0$MTkl., breaks = 20)

# NA's in MTkl.
cor(NFLDATA2.0, use = "complete.obs")
ggpairs(data = NFLDATA2.0, 1:11)

# Rename Variables for Correlation Matrix
NFLDATA2.0 <- NFLDATA2.0 %>%
  rename(
    Games_Played = G,
    Games_Started = GS,
    Sacks = Skâ.., 
    QB_Pressures = Prss,
    Tackles = Comb,
    Missed_Tackles = MTkl,
    Missed_Tackle_Percent = MTkl.
  )

# Doing the correlation matrix with renamed variables 
cor(NFLDATA2.0, use = "complete.obs")
ggpairs(data = NFLDATA2.0, 1:11)
