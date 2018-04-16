# MACS 30200 PS1 Part1 
library(ggplot2)
library(plyr)

# Import data
df <- read.csv("~/Spring_2018/MACS_30200/PS1/natl2016.csv", header = TRUE)
# Select variables
# Parents' demographics
m_age <- df$mager
m_educ <- df$meduc[df$meduc != 9]
m_race6 <- df$mrace6
f_age <- df$fagecomb[df$fagecomb != 99]
f_educ <- df$feduc[df$feduc != 9]
f_race6 <- df$frace6[df$frace6 != 9] 
# Newborn's health condition
cg <- df$combgest[df$combgest != 99] # combined gestation (period of gestation)
bw <- df$dbwt[df$dbwt != 9999] # birth weight 
apgar5 <- df$apgar5[df$apgar5 != 99] # 5-min apgar score
apgar10 <- df$apgar10[df$apgar10 != 99 & df$apgar10 != 88] # 10-min apgar score

# Tables of descriptive statistics

summary1 <- function(x){
  summary(x)
}
std <- function(x){
  sd(x)
}
# Descriptive statistics for parents' demographics
t1 <- sapply(list(m_age, m_educ, f_age, f_educ), summary1)
std1 <- sapply(list(m_age, m_educ, f_age, f_educ), std)
# Descriptive statistics for Newborn's health condition
t2 <- sapply (list(cg, bw, apgar5, apgar10), summary1)
std2 <- sapply(list(cg, bw, apgar5, apgar10), std)


# Distribution of mather's age 
ggplot(data=df, aes(df$mager)) + geom_histogram(breaks=seq(10, 50, by = 1.5)) + 
  labs(title="Figure 1: Histogram for Mother's Age") + labs(x="Mother's Age", y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Mean of Newborn's birth weight by father's race 
t2 <- ddply(df,~frace6,summarise,mean=mean(dbwt[dbwt != 9999]))
t2 <- t2[1:6, ]
t2$race <- c("White only", "Black only", "AIAN only", "Asian only", 
             "NHOPI only", "More than one race")
ggplot(data=t2, aes(x=race, y=mean, group=1)) + geom_line() + geom_point() + 
  labs(title="Figure 2: Mean of Newborn's \n Birth Weight by Father's Race") + 
  labs(x="Father's Race", y="Mean of Newborn's Birth Weight (g)") +
  theme(plot.title = element_text(hjust = 0.5))  + 
  theme(axis.text.x = element_text(face="bold", angle=20))

