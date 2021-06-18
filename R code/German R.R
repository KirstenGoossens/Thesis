german <- read.csv("~/Downloads/German_altered.csv")
View(german)

german$Age <- as.factor(german$Age)

levels(german$Age)[levels(german$Age)=='1'] <- 1
levels(german$Age)[levels(german$Age)=='2'] <- 1
levels(german$Age)[levels(german$Age)=='3'] <- 2
levels(german$Age)[levels(german$Age)=='4'] <- 2
levels(german$Age)[levels(german$Age)=='5'] <- 2


#t.test(german$CreditApproved[german$Age==1], german$CreditApproved[german$Age==2])

german$Age <- as.numeric(german$Age)

#normal
shapiro.test(german$Age)
qqPlot(german$Age)
hist(german$Age)

#variance
y <- c(german$CreditApproved[german$Age==1], german$CreditApproved[german$Age==2])
group <- as.factor(c(rep(1, length(german$CreditApproved[german$Age==1])), rep(2, length(german$CreditApproved[german$Age==2]))))

leveneTest(y, group, center=mean)

#only normality is violated

wilcox.test(german$CreditApproved[german$Age==1], german$CreditApproved[german$Age==2], alternative = "two.sided")





