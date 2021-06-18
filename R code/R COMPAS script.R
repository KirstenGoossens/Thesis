raw_scores_altered <- read.csv("~/Downloads/raw_scores_altered.csv")
View(raw_scores_altered)


#t.test(raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==1], raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==2])

#normal
SampleCompas <- raw_scores_altered[sample(nrow(raw_scores_altered), 5000), ]
head(SampleCompas)

shapiro.test(SampleCompas$Ethnic_Code_Text)
shapiro.test(SampleCompas$ScoreText)
qqPlot(SampleCompas$ScoreText)

#variance
y1 <- c(raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==1], raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==2])
group1 <- as.factor(c(rep(1, length(raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==1])), rep(2, length(raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==2]))))

leveneTest(y1, group1, center=mean)

#only normality is violated

wilcox.test(raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==1], raw_scores_altered$ScoreText[raw_scores_altered$Ethnic_Code_Text==2], alternative = "two.sided")




