# Packages
install.packages("DataExplorer")
install.packages("mice")
install.packages("caret")
install.packages("GGally")
install.packages("scatterplot3d")
install.packages("fpc")
install.packages("skmeans")
install.packages("animation")


# data manipulation

# used for EDA 
library(DataExplorer)
# Visualization
library(ggplot2)
library(dplyr)
library(gridExtra)

library(plyr)
# QQ Plot
library(ggpubr)

# Handle missing data
library(mice)

# Scaling
library(caret)

# Extension for ggplot2
library(GGally)

library(igraph)
library(tidyverse)
library(tidygraph)
library(ggraph)

# Data Understanding
df <- read.csv("D:/Portfolio/Diabetes/pima-indians-diabetes.csv")
dim(df)
str(df)
head(df)

introduce(df)
plot_intro(df)

# Detect outliers
df
View(df)
summary(df)
colSums(df == 0)

# plot distribution
plot_histogram(df)

# Plot correlation
plot_correlation(df, type = 'continuous','X6')

# QQ Plots
p1 <- ggqqplot(df$X148, ylab = "Glucose")
p2 <- ggqqplot(df$X33.6, ylab = "BMI")
p3 <- ggqqplot(df$X6, ylab = "Pregnant")
p4 <- ggqqplot(df$X72, ylab = "DBP")
grid.arrange(p3, p4, nrow = 1)


# Glucose x BMI
bmi_glu <- ggplot(df, aes(x = X148, y = X33.6))
bmi_glu + 
  geom_point(aes(color = X1), size = 3) +
  geom_smooth(method = lm, color="Black") +
  scale_color_gradientn(colors = c("#00AFBB", "#FC4E07")) +
  theme(legend.position = "right")

# Glucose x Insulin
bmi_glu <- ggplot(df, aes(x = X148, y = X0))
bmi_glu + 
  geom_point(aes(color = X1), size = 3) +
  geom_smooth(method = lm, color="Black") +
  scale_color_gradientn(colors = c("#E69F00", "#56B4E9")) +
  theme(legend.position = "right")

# Glucose x Age
out <- df$X1
out <- as.factor(out)
class(out)
age <- ggplot(df, aes(x=X50, color=out, fill=out)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 
age
age_out <- cor.test(df$X50, df$X1,
                 method ="pearson")
age_out     
age_out$p.value
age_out$estimate
# p value = 3.332555e-11, cor = 0.2364175
# significance level alpha is 0.05
# p value is less than alpha
# cor is positive but weak relation 

# DBP x Pregnant
preg_dbp <- ggplot(data=df, aes(x=X72, y=X6, fill=out)) +
  geom_bar(stat="identity", position=position_dodge())
preg_dbp + 
  scale_fill_manual(values=c('#999999','#E69F00')) +
  theme_minimal()

# Pregnant
preg <- ggplot(df, aes(x=X6, fill=out)) + 
  geom_histogram(aes(y=..density..), alpha=1, 
                 position="identity")+
  geom_density(adjust=1.5, alpha=.4) 
preg + scale_fill_brewer(palette="Reds") + theme_minimal()

# Glucose x DBP
dbp_glu <- ggplot(df, aes(x = X148, y = X35)) +
  geom_point(aes(color = X1), size = 3) +
  geom_smooth(method = lm, color="Black") +
  scale_color_gradientn(colors = c("#69b3a2", "#404080")) +
  theme(legend.position = "right")
dbp_glu

# Glucose x DP
glu_dp <- ggplot(df, aes(x = X148, y = X0.627)) +
  geom_point(aes(color = X1), size = 3) +
  geom_smooth(method = lm, color="Black") +
  scale_color_gradientn(colors = c("#C77CFF", "#FFC425")) +
  theme(legend.position = "right")
glu_dp

create_report(df)
plot_prcomp(df)
str(df)

# Data Preparation
# Data Selection
str(df)
# select 2nd, 4th - 6th, 9th
data2 <- df[c(2,4:6, 9)]
str(data3)

# Data Preprocessing
colSums(data2==0)

# Empty Values
rem_data <- df[c(2,4:6)]
X1 <- df$X1
rem_data[rem_data == 0] <- NA
rem_data <- cbind(rem_data, X1)
head(rem_data)

data3 <- rem_data
md.pattern(data3)

# method rf is random forests imputation 
  # which is for classifciation and regression
data4 <- mice(data3, m=5, maxit=50, meth="rf", seed=500)
data4$imp$X148
data4$imp$X35
data4$imp$X0
data4$imp$X33.6
completedData <- complete(data4, )
str(completedData)
plot_missing(completedData)

# outliers
summary(completedData$X148)
summary(completedData$X35)
summary(completedData$X0)
summary(completedData$X33.6)

box_glu <- ggplot(completedData) +
  aes(x = "", y = X148) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
box_thic <- ggplot(completedData) +
  aes(x = "", y = X35) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
box_ins <- ggplot(completedData) +
  aes(x = "", y = X0) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
box_bmi <- ggplot(completedData) +
  aes(x = "", y = X33.6) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
grid.arrange(box_glu, box_thic, box_ins, box_bmi, nrow=2)

boxplot.stats(completedData$X148)$out
boxplot.stats(completedData$X35)$out
boxplot.stats(completedData$X0)$out
boxplot.stats(completedData$X33.6)$out

outliers1 <- boxplot(completedData$X35, plot=FALSE)$out
outliers2 <- boxplot(completedData$X0, plot=FALSE)$out
outliers3 <- boxplot(completedData$X33.6, plot=FALSE)$out

completedData2<-completedData
str(completedData2)

completedData2<- completedData2[-which(completedData2$X35 %in% outliers1),]
completedData2<- completedData2[-which(completedData2$X0 %in% outliers2),]
completedData2<- completedData2[-which(completedData2$X33.6 %in% outliers3),]

boxplot.stats(completedData2$X35)$out
boxplot.stats(completedData2$X0)$out
boxplot.stats(completedData2$X33.6)$out

# Justification distribution
thic <- ggplot(completedData2, aes(x = X35, y=X148)) + 
  geom_point(aes(color = X1), size = 3) +
  geom_smooth(method = lm, color="Black") +
  scale_fill_brewer(palette="Blues") + theme_minimal() +
  theme(legend.position = "right")
ins <- ggplot(completedData2, aes(x = X0, y=X148)) + 
  geom_point(aes(color = X1), size = 3) +
  geom_smooth(method = lm, color="Black") +
  scale_fill_brewer(palette="Blues") + theme_minimal() +
  theme(legend.position = "right")
bmi <- ggplot(completedData2, aes(x = X33.6, y=X148)) + 
  geom_point(aes(color = X1), size = 3) +
  geom_smooth(method = lm, color="Black") +
  scale_fill_brewer(palette="Blues") + theme_minimal() +
  theme(legend.position = "right")
grid.arrange(thic, ins, bmi, nrow=2)

# Data Integration
is.null(completedData2)
str(completedData2)
summary(completedData2)

# Data Transformation
completedData2$X148 <- as.numeric(completedData2$X148)
completedData2$X35 <- as.numeric(completedData2$X35)
completedData2$X0 <- as.numeric(completedData2$X0)
completedData2$X1 <- as.factor(completedData2$X1)
str(completedData2)
completedData2
# Renaming Columns
completedData3 <- completedData2 %>% 
  rename(
    glucose = X148,
    skin = X35,
    insulin = X0, 
    bmi = X33.6,
    outcome = X1
  )
str(completedData3)
plot_str(completedData3)

# Scaling
# first need convert to numeric for scaling, can change back after
completedData3$outcome <- as.numeric(completedData3$outcome)
sca <- as.data.frame(scale(completedData3[,c(1:5)]))
sca <- preProcess(completedData3[,c(1:5)], method=c("range"))
mm <- predict(sca, completedData3[,c(1:5)])
summary(mm)
completedData3$outcome <- as.factor(completedData3$outcome)
str(completedData3)

p1 <- ggplot(mm, aes(x=glucose, fill=outcome)) + 
  geom_histogram(aes(y=..density..), alpha=1, 
    position="identity") +
  geom_density(adjust=1.5, alpha=.4)
p2 <- ggplot(mm, aes(x=insulin, fill=outcome)) + 
  geom_histogram(aes(y=..density..), alpha=1, 
                 position="identity") +
  geom_density(adjust=1.5, alpha=.4)
p3 <- ggplot(mm, aes(x=skin, fill=outcome)) + 
  geom_histogram(aes(y=..density..), alpha=1, 
                 position="identity") +
  geom_density(adjust=1.5, alpha=.4)
p4 <- ggplot(mm, aes(x=bmi, fill=outcome)) + 
  geom_histogram(aes(y=..density..), alpha=1, 
                 position="identity") +
  geom_density(adjust=1.5, alpha=.4)
grid.arrange(p1, p2, p3, p4, nrow=2)

str(completedData3)


# NULL HYPOTHESIS 
# OPPOSITE OF HYPOTHESIS is preferred to be used instead of hypothesis itself
# since its impossible to prove a hypothesis
# NULL HYPOTHESIS - All variables do not have significant relationship 
# Aim to reject null hypothesis
ggpairs(data=mm, columns=1:5, title="Diabetes")

lm1 <- lm(bmi ~ skin, data = mm)
lm2 <- lm(insulin~glucose, data=mm)
summary(lm1)        # p value is less than 0.05 
# since less than 0.05, we can reject null hypothesis
summary(lm2)        # p value is less than 0.05
# since less than 0.05, we can reject null hypothesis

ggplot(data=completedData3, aes(lm2$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "cyan") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

# Note it is symmetric to 0 
ggplot(data = mm, aes(x = bmi, y = skin)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")
# gray shading represents confidence interval of 0.95

# prediction using only skin/glucose as predictor
predict(lm1, data.frame(skin = 44))
predict(lm2, data.frame(glucose = 57))
View(completedData3)

lm1_1 <- lm(bmi ~ skin + glucose, data=mm)
summary(lm1_1)

# adjusted r-squared isnt close to 1 so not very good
SKIN_1 <- seq(0, 1, by=0.5)
GLU_1 <- seq(0, 1, by=0.5)
pred_grid <- expand.grid(skin = SKIN_1, glucose = GLU_1)

pred_grid$Volume2 <-predict(lm1_1, new = pred_grid)
lm1_1_1 <- scatterplot3d(pred_grid$glucose, pred_grid$skin, 
                          pred_grid$Volume2, angle = 40, color = "dodgerblue", 
                          pch = 1, ylab = "Skin", xlab = "Glucose", 
                          zlab = "BMI" )
lm1_1_1$points3d(mm$glucose, mm$skin, mm$bmi, pch=16)


# prediction using glucose and skin as predictors
predict(lm1_1, data.frame(glucose = 44, skin = 15))
predict(lm1_1, data.frame(glucose = 150, skin = 30))
# predictions are pretty good

# checking interaction between skin and glucose
lm1_2 <- lm(bmi ~ glucose * skin, data = mm)
summary(lm1_2)
# no significant relationship


# test Outcome when it is numeric
completedData3$outcome <- as.numeric(completedData3$outcome)
# note that mm is the scaled data
ggpairs(data=mm, columns=1:5, title="Diabetes")
# Top highest corr with Outcome 
  # Glucose (0.50), Insulin (0.357), BMI (0.318)

lm3 <- lm(outcome ~ glucose, data=mm)
summary(lm3)
head(lm3)

predict(lm3, data.frame(glucose = 0.5))
lm3_1 <- lm(outcome ~ glucose + insulin, data=mm)
summary(lm3_1)
View(mm)
predict(lm3_1, data.frame(glucose = 0.76, insulin = 0.52))

ggplot(data = mm, mapping = aes(x = glucose)) +
  geom_freqpoly(bins = 30) +
  ggtitle("Histogram") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = mm, mapping = aes(x = glucose, y = insulin)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Glucose", y = "Insulin") +
  ggtitle(expression(atop("Scatterplot of Glucose vs. Insulin", atop(italic("With Fitted Regression Line", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

(outcome.lm <- lm(outcome ~ glucose, data = mm))
# outcome as response variable, glucose as predictor variable

summary(outcome.lm)
# lm isnt too accurate according to r squared

# clustering
install.packages("mclust")


# Calculate distance
library(factoextra)

# interactive heatmap
library(heatmaply)

# Clustering packages
library(cluster)
library(magrittr)
library(mclust)
library(fpc)
library(skmeans)

plot1<-ggplot(completedData3, aes(x = glucose, y = insulin)) + geom_point(aes(color = factor(outcome)))
plot1
# more cases when insulin is above 100 and 
  # glucose is higher than 100 
completedData3$outcome <- as.factor(completedData3$outcome)

# calculate distance
test.d <- mm[1:30,]
test.sample <- mm[1:130,]
train <- mm[31:711,]

eud <- get_dist(test.d, method="euclidean")
fviz_dist(eud, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Hierarchical Clustering
m1 <- hclust(eud, method="average")

# Dendrogram
plot(m1, hang=-1, cex=0.7)
rect.hclust(m1, k=2, border=2:5)
View(m1)

# plots which are positive and false cases

# Model based Clustering
fit <- Mclust(mm)
plot(fit)
# Bayesian Information Criterion (BIC)
# Classification
# Uncertainty
# Density

summary(fit)

# Plotting Cluster Solutions
# K Means Clustering
fit <- kmeans(test, 2)
fit

# shows the clusters based on outcome
# 1 = Positive Case
# 2 = False case
str(test)
clusplot(test, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
table(test$glucose, fit$cluster)
plot(test[c("glucose", "insulin")], col=fit$cluster)
test$outcome <- as.factor(test$outcome)
plot(test[c("glucose", "insulin")], col=test$outcome)
# By comparing the plots, can see that the clusters were 
 # clustered proerply since theyre same

plot(test[c("skin", "bmi")], col=fit$cluster)
plot(test[c("skin", "bmi")], col=test$outcome)

