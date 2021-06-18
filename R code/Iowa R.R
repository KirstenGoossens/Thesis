recidivism <- read.csv("~/Downloads/RecidivismIowa.csv")
View(recidivism)

levels(recidivism$Age.At.Release)[levels(recidivism$Age.At.Release)=="Under 25"] <- 1
levels(recidivism$Age.At.Release)[levels(recidivism$Age.At.Release)=="25-34"] <- 1
levels(recidivism$Age.At.Release)[levels(recidivism$Age.At.Release)=="35-44"] <- 2
levels(recidivism$Age.At.Release)[levels(recidivism$Age.At.Release)=="45-54"] <- 2
levels(recidivism$Age.At.Release)[levels(recidivism$Age.At.Release)=="55 and Older"] <- 2

#t.test(recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==1], recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==2])

#normal
SampleIowa <- recidivism[sample(nrow(recidivism), 5000), ]
head(SampleIowa)

SampleIowa$Age.At.Release <- as.numeric(SampleIowa$Age.At.Release)

shapiro.test(SampleIowa$Age.At.Release)
qqPlot(SampleIowa$Age.At.Release)

age <- as.numeric(recidivism$Age.At.Release)
hist(age)


#variance
y2 <- c(recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==1], recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==2])
group2 <- as.factor(c(rep(1, length(recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==1])), rep(2, length(recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==2]))))

leveneTest(y2, group2, center=mean)

#only normality is violated

wilcox.test(recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==1], recidivism$Recidivism...Return.to.Prison.numeric[recidivism$Age.At.Release==2], alternative = "two.sided")



