# 1. Basics of probability theory; Discrete and continuous probability distributions;
# 2.Mean and variance estimators; Estimation bias and error variance;
# Confidence intervals; Role of sample size (consistency);
# 3. Introduction to hypothesis testing; Type I-II errors, power and ROC; 
# Parametric and nonparametric tests (Z test, T test ; Wilcoxon rank-sum test)
# 4. Testing distributions: Kolmogorov-Smirnov and generalizations; ANOVA and Nonparametric ANOVA
# 5. Correlation analysis; Parametric and nonparametric correlation tests (Pearson, Spearman); 
# Linear regression
# 6. Bayesian statistics: likelihood, conditional probabilities, examples of Bayesian inference


#packages and libraries

install.packages("readxl")
install.packages("moments")
install.packages("lmridge")

library("readxl")
library("ggpubr")
library(ggpubr)
library(moments)
library(ggplot2)
library(reshape)
library(e1071)
library(hrbrthemes)
library(lmridge)


#read in excel files
hate_crime_data_2016 <- read_excel("Table_13_Hate_Crime_Incidents_per_Bias_Motivation_and_Quarter_by_State_and_Agency_2016.xlsx")
hate_crime_data_2019 <- read_excel("Table_13_Hate_Crime_Incidents_per_Bias_Motivation_and_Quarter_by_State_Federal_and_Agency_2019.xlsx")

#########################################################################################################################################################
## hate crimes by state

Number_of_Hate_Crimes_2016 = c();
States_2016 = c();

i = 1;
while(i <= nrow(hate_crime_data_2016[1])){
  
  j = 0
  state = hate_crime_data_2016[i, 1]
 
  while(hate_crime_data_2016[i, 1] == state & i <= nrow(hate_crime_data_2016[1])){
    j = j + hate_crime_data_2016[i, 4] + hate_crime_data_2016[i, 5] + hate_crime_data_2016[i, 6]
    j = j + hate_crime_data_2016[i, 7] + hate_crime_data_2016[i, 8] + hate_crime_data_2016[i, 9];
    
    i = i + 1
  }
  
  Number_of_Hate_Crimes_2016 = append(Number_of_Hate_Crimes_2016, as.numeric(j))
  States_2016 = append(States_2016, as.character(state))
}


Number_of_Hate_Crimes_2019 = c();
States_2019 = c();

i = 1;
while(i <= nrow(hate_crime_data_2019[1])){
  
  j = 0
  state = as.character(hate_crime_data_2019[i, 1])
  
  while(as.character(hate_crime_data_2019[i, 1]) == state & i <= nrow(hate_crime_data_2019[1])){
    j = j + hate_crime_data_2019[i, 4] + hate_crime_data_2019[i, 5] + hate_crime_data_2019[i, 6]
    j = j + hate_crime_data_2019[i, 7] + hate_crime_data_2019[i, 8] + hate_crime_data_2019[i, 9];
    
    i = i + 1
  }
  if(state != 'Federal'){
    Number_of_Hate_Crimes_2019 = append(Number_of_Hate_Crimes_2019, as.numeric(j))
    States_2019 = append(States_2019, as.character(state))
  }else{
    next
  }
}

Hate_Crimes_by_State_2016 = data.frame(States_2016, Number_of_Hate_Crimes_2016)
Hate_Crimes_by_State_2019 = data.frame(States_2019, Number_of_Hate_Crimes_2019)

#1########################################################################################################################################################
## Basic Probability Theory: Discrete Probability


#bar plot

my_data = data.frame(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

unique_levels_2016 <- sort(unique(my_data$Hate_Crimes_by_State_2016.Number_of_Hate_Crimes_2016))
count_2016 <- table(my_data$Hate_Crimes_by_State_2016.Number_of_Hate_Crimes_2016)
count_df_2016 <- data.frame(unique_levels_2016, count_2016)

plot <- ggplot(count_df_2016, aes(unique_levels_2016, Freq, fill=unique_levels_2016))

plot + geom_bar(stat="identity") + 
  labs(title="Hate Crimes by State in 2016",
       subtitle="Reported Total Hate Crimes by Each State",
       y="Occurence of Reported Total Hate Crimes", x="Number of Hate Crimes") + 
  theme(legend.position="none")

unique_levels_2019 <- sort(unique(my_data$Hate_Crimes_by_State_2019.Number_of_Hate_Crimes_2019))
count_2019 <- table(my_data$Hate_Crimes_by_State_2019.Number_of_Hate_Crimes_2019)
count_df_2019 <- data.frame(unique_levels_2019, count_2019)

plot <- ggplot(count_df_2019, aes(unique_levels_2019, Freq, fill=unique_levels_2019))

plot + geom_bar(stat="identity") + 
  labs(title="Hate Crimes by State in 2019",
       subtitle="Reported Total Hate Crimes by Each State",
       y="Occurence of Reported Total Hate Crimes", x="Number of Hate Crimes") + 
  theme(legend.position="none")

#Frequency historgrams

#binning

bins <- 15

counts_2016 <- vector(mode = "numeric", length = bins)
counts_2019 <- vector(mode = "numeric", length = bins)
diff = 100;

for(p in 1:bins) {
  
  y_2016 = 0;
  y_2019 = 0;
  
  for(j in 1:length(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)){
    if(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016[j] < (p*diff) & Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016[j] >= (diff*(p-1))){
      y_2016 = y_2016 + 1;
    }else{
      next
    }
  }
  
  for(k in 1:length(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)){
    if(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019[k] < (p*diff) & Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019[k] >= (diff*(p-1))){
      y_2019 = y_2019 + 1;
    }else{
      next
    }
  }
  
  counts_2016[p] = y_2016
  counts_2019[p] = y_2019
  
}

mean_2016 = mean(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
mean_2019 = mean(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

hist_2016 = data.frame(counts_2016)

hist_2016$left = seq(0, 1500-diff, by=diff)
hist_2016$right = seq(diff, 1500, by = diff)
hist_2016$center = seq(diff/2, 1500 - diff/2, by = diff)

h_2016 <- ggplot(hist_2016, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = counts_2016))

h_2016 <- ggplot(data= hist_2016, aes(x=center, y=counts_2016,fill=center))+
  geom_bar(stat="identity")+
  xlab("Number of Hate Crimes") + 
  ylab("Frequency") + ggtitle("Histogram of Hate Crimes by State in 2016") +
  geom_vline(xintercept = mean_2016, color = "red", size=1.0)

h_2016

hist_2019 = data.frame(counts_2019)

hist_2019$left = seq(0, 1500-diff, by=diff)
hist_2019$right = seq(diff, 1500, by = diff)
hist_2019$center = seq(diff/2, 1500-diff/2, by = diff)

h_2019 <- ggplot(hist_2019, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = counts_2019))

h_2019 <- ggplot(data= hist_2019, aes(x=center, y=counts_2019,fill=center))+
  geom_bar(stat="identity")+
  xlab("Number of Hate Crimes") + 
  ylab("Frequency") + ggtitle("Histogram of Hate Crimes by State in 2019")+
  geom_vline(xintercept = mean_2019, color = "red", size=1.0)

h_2019

#Frequency normalized histograms (probability)

counts_2016_norm = counts_2016/sum(counts_2016)
counts_2019_norm = counts_2019/sum(counts_2019)

hist_2016_norm = data.frame(counts_2016_norm)

hist_2016_norm$left = seq(0, 1500-diff, by=diff)
hist_2016_norm$right = seq(diff, 1500, by = diff)
hist_2016_norm$center = seq(diff/2, 1500 - diff/2, by = diff)

h_2016_norm <- ggplot(hist_2016_norm, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = counts_2016_norm))

h_2016_norm <- ggplot(data= hist_2016_norm, aes(x=center, y=counts_2016_norm, fill=center))+
  geom_bar(stat="identity")+
  xlab("Number of Hate Crimes") + 
  ylab("Probability") + ggtitle("Histogram of Hate Crimes by State in 2016") +
  geom_vline(xintercept = mean_2016, color = "red", size=1.0)

h_2016_norm

hist_2019_norm = data.frame(counts_2019_norm)

hist_2019_norm$left = seq(0, 1500-diff, by=diff)
hist_2019_norm$right = seq(diff, 1500, by = diff)
hist_2019_norm$center = seq(diff/2, 1500-diff/2, by = diff)

h_2019_norm <- ggplot(hist_2019_norm, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, ymax = counts_2019_norm))

h_2019_norm <- ggplot(data= hist_2019_norm, aes(x=center, y=counts_2019_norm, fill=center))+
  geom_bar(stat="identity")+
  xlab("Number of Hate Crimes") + 
  ylab("Probability") + ggtitle("Histogram of Hate Crimes by State in 2019")+
  geom_vline(xintercept = mean_2019, color = "red", size=1.0)

h_2019_norm

#check skewness

skewness(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
skewness(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

#visualize skewness before transformation

my_data = data.frame(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)
colnames(my_data) = c("Number of Hate Crimes by State in 2016", "Number of Hate Crimes by State in 2019")

ggdensity(my_data, x = "Number of Hate Crimes by State in 2016", fill = "lightgray", title = "Hate Crimes by State in 2016") +
  scale_x_continuous(limits = c(0, 1500)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(my_data, x = "Number of Hate Crimes by State in 2019", fill = "lightgray", title = "Hate Crimes by State in 2019") +
  scale_x_continuous(limits = c(0, 1500)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#transform data

skew_2016 = log10(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
hist(skew_2016, xlab = "log10(Number of Hate Crimes)", ylab = "Frequency", main = paste("Histogram of Hate Crimes by State in 2016"))
skew_2019 = log10((Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019))
hist(skew_2019, xlab = "log10(Number of Hate Crimes)", ylab = "Frequency", main = paste("Histogram of Hate Crimes by State in 2019"))

#visualize skewness after transformation

my_data = data.frame(skew_2016, skew_2019)
colnames(my_data) = c("log10(Number of Hate Crimes by State in 2016)", "log10(Number of Hate Crimes by State in 2019)")

ggdensity(my_data, x = "log10(Number of Hate Crimes by State in 2016)", fill = "lightgray", title = "Hate Crimes by State in 2016") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(my_data, x = "log10(Number of Hate Crimes by State in 2019)", fill = "lightgray", title = "Hate Crimes by State in 2019") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#check for normality again

shapiro.test(skew_2016)
shapiro.test(skew_2019)

#2##########################################################################################################################
##Mean and Variance Estimators

my_data = data.frame(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

n_2016 = length(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
n_2019 = length(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

mean_2016 = mean(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
mean_2019 = mean(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

sd_2016 = sd(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
sd_2019 = sd(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

variance_2016 = var(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
variance_2019 = var(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)

##Estimation Bias and Error Variance


#data: X1, X2, ..., Xn
#Xi ~ N(mean, sigma)

##Confidence Intervals 

margin_2016_95 = qt(0.95, df = n_2016 - 1) *sd_2016/sqrt(n_2016)
margin_2019_95 = qt(0.95, df = n_2019 - 1) *sd_2019/sqrt(n_2019)

lowerinterval_95_2016 = mean_2016 - margin_2016_95

upperinterval_95_2016 = mean_2016 + margin_2016_95

lowerinterval_95_2019 = mean_2019 - margin_2019_95

upperinterval_95_2019 = mean_2019 + margin_2019_95

var_low_95_2016 = 21122.3785
var_up_95_2016 = 46621.9319

var_low_95_2019 = 57795.5396
var_up_95_2019 = 127568.0061


#estimators
mean_est_2016 = seq(lowerinterval_95_2016,  upperinterval_95_2016, length.out = 20)
mean_est_2019 = seq(lowerinterval_95_2019, upperinterval_95_2019, length.out = 20) 
var_est_2016 = seq(var_low_95_2016, var_up_95_2016, length.out = 20)
var_est_2019 = seq(var_low_95_2019, var_up_95_2019, length.out = 20)

#make a simulation model 

sims = 1000 #simulations

vec_bias2_mean_2016 = rep(0, 20)
vec_bias2_mean_2019 = rep(0, 20)

vec_bias2_var_2016 = rep(0, 20)
vec_bias2_var_2019 = rep(0, 20)

vec_var_var_2016 = rep(0, 20)
vec_var_var_2019 = rep(0, 20)

vec_var_mean_2016 = rep(0, 20)
vec_var_mean_2019 = rep(0, 20)

vec_mse_var_2016 = rep(0, 20)
vec_mse_var_2019 = rep(0, 20)

vec_mse_mean_2016 = rep(0, 20)
vec_mse_mean_2019 = rep(0, 20)

#assume true model is N(mean_est, and sig_est^2)
for(j in 1:20){
  
  list_of_bias_var_2016 = rep(0, sims)
  list_of_bias_var_2019 = rep(0, sims)
  
  list_of_mse_var_2016 = rep(0, sims)
  list_of_mse_var_2019 = rep(0, sims)
  
  list_of_mse_mean_2016 = rep(0, sims)
  list_of_mse_mean_2019 = rep(0, sims)
  
  var_2016 = rep(0, sims)
  var_2019 = rep(0, sims)
  
  for(i in 1:sims){
    data_2016 = rnorm(n_2016, mean_est_2016[j], sqrt(var_est_2016[j]))
    data_2019 = rnorm(n_2019, mean_est_2019[j], sqrt(var_est_2019[j]))
  
    var_2016[i] = var(data_2016)*((n_2016-1)/n_2016)
    var_2019[i]= var(data_2019)*((n_2019-1)/n_2019)
    
    b_2016 = var_calc_2016 - var_est_2016[j]
    b_2019 = var_calc_2019 - var_est_2019[j]
  
    list_of_bias_var_2016[i] = b_2016
    list_of_bias_var_2019[i] = b_2019
  
    mse_var_2016 = var_2016[i]^2*(2*n_2016-2)/((n_2016-1)^2) + b_2016^2
    mse_var_2019 = var_2019[i]^2*(2*n_2019 -2)/((n_2019-1)^2) + b_2019^2
  
    list_of_mse_var_2016[i] = mse_var_2016
    list_of_mse_var_2019[i] = mse_var_2019
    
  }
  
  vec_bias2_var_2016[j] = (mean(list_of_bias_var_2016))^2
  vec_bias2_var_2019[j] = (mean(list_of_bias_var_2019))^2
  
  vec_var_var_2016[j] = mean(var_2016)^2*(2*n_2016-2)/((n_2016-1)^2)
  vec_var_var_2019[j] = mean(var_2019)^2*(2*n_2019-2)/((n_2019-1)^2)
  
  vec_var_mean_2016[j] = mean(var_2016)/n_2016
  vec_var_mean_2019[j] = mean(var_2019)/n_2019
  
  vec_mse_mean_2016[j] = mean(var_2016)/n_2016
  vec_mse_mean_2019[j] = mean(var_2019)/n_2019
  
  vec_mse_var_2016[j] = mean(list_of_mse_var_2016)
  vec_mse_var_2019[j] = mean(list_of_mse_var_2019)
  
  
}

plot(var_est_2016, vec_mse_var_2016, xlab='Variance Estimator', ylab='MSE', ylim = c(0, 580000000), type='l', col='orange', lwd=3, lty=1, main="Estimation Bias and Error Variance for 2016 Variance Estimator")
lines(var_est_2016, vec_bias2_var_2016, col='red', lwd=3, lty=2)
lines(var_est_2016, vec_var_var_2016, col='blue', lwd=3, lty=2)
legend(22000, 550000000, c('Bias^2', 'Variance', 'MSE'), col=c('red', 'blue', 'orange'), lwd=rep(3,3), lty=c(2,2,1))

plot(var_est_2019, vec_mse_var_2019, xlab='Variance Estimator', ylab='MSE', ylim = c(0, 6500000000), type='l', col='orange', lwd=3, lty=1, main="Estimation Bias and Error Variance for 2019 Variance Estimator")
lines(var_est_2019, vec_bias2_var_2019, col='red', lwd=3, lty=2)
lines(var_est_2019, vec_var_var_2019, col='blue', lwd=3, lty=2)
legend(61000, 6500000000, c('Bias^2', 'Variance', 'MSE'), col=c('red', 'blue', 'orange'), lwd=rep(3,3), lty=c(2,2,1))

plot(mean_est_2016, vec_mse_mean_2016, xlab='Mean Estimator', ylab='MSE', ylim = c(0, 1000), type='l', col='orange', lwd=3, lty=1, main="Estimation Bias and Error Variance for 2016 Mean Estimator")
lines(mean_est_2016, vec_bias2_mean_2016, col='red', lwd=3, lty=2)
lines(mean_est_2016, vec_var_mean_2016, col='blue', lwd=3, lty=2)
legend(90, 1000, c('Bias^2', 'Variance', 'MSE'), col=c('red', 'blue', 'orange'), lwd=rep(3,3), lty=c(2,2,1))

plot(mean_est_2019, vec_mse_mean_2019, xlab='Mean Estimator', ylab='MSE', ylim = c(0, 3000), type='l', col='orange', lwd=3, lty=1, main="Estimation Bias and Error Variance for 2019 Mean Estimator")
lines(mean_est_2019, vec_bias2_mean_2019, col='red', lwd=3, lty=2)
lines(mean_est_2019, vec_var_mean_2019, col='blue', lwd=3, lty=2)
legend(130,2600, c('Bias^2', 'Variance', 'MSE'), col=c('red', 'blue', 'orange'), lwd=rep(3,3), lty=c(2,2,1))


##Confidence Intervals 

hist(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, prob = TRUE, main = "Density of Hate Crimes by State in 2016", xlab = "Number of Hate Crimes")
x <- seq(min(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016), max(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016), length = 1000)
f <- dnorm(x, mean = mean_2016, sd = sd_2016)
lines(x, f, col = "red", lwd = 2)
lines(rep(mean_2016, 3), seq(0, .0023, length.out = 3), col = "green", lwd = 2)
lines(rep(mean_2016 + 2*sd_2016, 3), y = seq(0, .00033, length.out = 3), col = "blue", lwd = 2)
lines(rep(mean_2016 + sd_2016, 3), y = seq(0, .0014, length.out = 3), col = "blue", lwd = 2)

hist(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, prob = TRUE, main = "Density of Hate Crimes by State in 2019", xlab = "Number of Hate Crimes")
x <- seq(min(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019), max(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019), length = 1000)
f <- dnorm(x, mean = mean_2019, sd = sd_2019)
lines(x, f, col = "red", lwd = 2)
lines(rep(mean_2019, 3), seq(0, .0014, length.out = 3), col = "green", lwd = 2)
lines(rep(mean_2019 + 2*sd_2019, 3), y = seq(0, .0002, length.out = 3), col = "blue", lwd = 2)
lines(rep(mean_2019 + sd_2019, 3), y = seq(0, .00085, length.out = 3), col = "blue", lwd = 2)


avg = c(mean_2016, mean_2019)
plot(c(2016.1, 2016.9), avg, ylim=range(0, 250),
     pch=19, xlab="Hate Crime Report Years", ylab="Confidence Intervals of the Mean",
     main="95% Confidence Intervals for the Years 2016 & 2019")

marg = c(margin_2016_95, margin_2019_95)
# hack: we draw arrows but with very special "arrowheads"
arrows(c(2016.1, 2016.9), avg-marg, c(2016.1, 2016.9), avg+marg, length=0.05, angle=90, code=3)


v = data.frame(c(1, 2), c(variance_2016, variance_2019))
colnames(v) = c("num", "year")

low = c(var_low_95_2016, var_low_95_2019)
up = c(var_up_95_2016, var_up_95_2019)

lowerinterval_95_2016
lowerinterval_95_2019
upperinterval_95_2016
upperinterval_95_2019
low
up

# Most basic error bar
ggplot(v) +
  geom_bar( aes(x=num, y=year), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=num, ymin=low, ymax=up), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  ggtitle("95% Confidence Intervals of the Variance 2016 & 2019")

#role of sample size

#3#################################################################################################################################################
##Introduction to hypothesis testing; Type I-II errors, power and ROC; Parametric and nonparametric tests (Z test, T test ; Wilcoxon rank-sum test)

#Type 1 error analysis


#2016
alpha = .05           # significance level of 95%
mu0_2016 = mean_2019              # hypothetical upper bound               # hypothetical upper bound

t_2016 = (mean_2016 - mu0_2016)/(sd_2016/sqrt(n_2016)) #test statistic
t_alpha_2016 = qt(1-alpha/2, df = n_2016-1) #critical value
c(-t_alpha_2016, t_alpha_2016)

p_val_2016 = 2* pt(t_2016, df = n_2016 -1)

print(paste("The test statistic ", t_2016, " is between the critical value of ", -t_alpha_2016, " and ", t_alpha_2016, ". Hence, at .05 significance level, we do not reject the null hypothesis mean = ", mu0_2016))
print(paste("The two tail p-value of the test statistic is ", p_val_2016))


#2019
alpha = .05           # significance level of 95%
mu0_2019 = mean_2016              # hypothetical upper bound               # hypothetical upper bound

t_2019 = (mean_2019 - mu0_2019)/(sd_2019/sqrt(n_2019)) #test statistic
t_alpha_2019 = qt(1-alpha/2, df = n_2019-1) #critical value
c(-t_alpha_2019, t_alpha_2019)

p_val_2019 = 2* pt(t_2019, df = n_2019 -1)

print(paste("The test statistic ", t_2019, " is between the critical value of ", -t_alpha_2019, " and ", t_alpha_2019, ". Hence, at .05 significance level, we do not reject the null hypothesis mean = ", mu0_2019))
print(paste("The two tail p-value of the test statistic is ", p_val_2019))

##Type II error analysis

#2016
SE_2016 = sd_2016/sqrt(n_2016)  # standard error estimate

I = c(alpha/2, 1-alpha/2)

q_2016 = mu0_2016 + qt(I, df=n_2016-1) * SE_2016;

q_2016

#assumed actual population mean
p_2016 = pt((q_2016-mean_2016)/SE_2016, df = n_2016 -1)
d = diff(p_2016)

print(paste("The probability of type II error for testing the null hypothesis for the mean = ", mu0_2016, " at 0.05 significance level is ", 100*d, "% and the power of the hypothesis test is ", 100*(1-d), "%"))

#2019
SE_2019 = sd_2019/sqrt(n_2019)  # standard error estimate

I = c(alpha/2, 1-alpha/2)

q_2019 = mu0_2019 + qt(I, df=n_2016-1) * SE_2019;

q_2019

#assumed actual population mean
p_2019 = pt((q_2019-mean_2019)/SE_2019, df = n_2019 -1)
d = diff(p_2019)

print(paste("The probability of type II error for testing the null hypothesis for the mean = ", mu0_2019, " at 0.05 significance level is ", 100*d, "% and the power of the hypothesis test is ", 100*(1-d), "%"))


##Parametric and nonparametric tests


#wilcoxon rank-sum test (discrete data, independent)
#sig difference
wil1 = wilcox.test(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)
wil1
#2016 mean and 2019 mean is not significantly different

#less than
wil2 = wilcox.test(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, alternative = "less")
wil2
#2019 mean is not less than 2016 mean

#less than
wil3 = wilcox.test(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, alternative = "greater")
wil3
#2019 mean is not greater than 2016 mean


#welchs t test

t.test(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)



#4###################################################################################################################################################
##Testing distributions: Kolmogorov-Smirnov and generalizations

ks.test(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)


#5###################################################################################################################################################
####transformed####

##Correlation analysis; 
#Parametric and nonparametric correlation tests (Pearson, Spearman)

#visualize data first using pearson
my_data = data.frame(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)
ggscatter(my_data, x = "Hate_Crimes_by_State_2016.Number_of_Hate_Crimes_2016", y = "Hate_Crimes_by_State_2019.Number_of_Hate_Crimes_2019", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hate Crimes by State in 2016", ylab = "Hate Crimes by State in 2019")

ggqqplot(my_data$Hate_Crimes_by_State_2016.Number_of_Hate_Crimes_2016, ylab = "Hate Crimes by State in 2016")

ggqqplot(my_data$Hate_Crimes_by_State_2019.Number_of_Hate_Crimes_2019, ylab = "Hate Crimes by State in 2019")

shapiro.test(my_data$Hate_Crimes_by_State_2016.Number_of_Hate_Crimes_2016)
shapiro.test(my_data$Hate_Crimes_by_State_2019.Number_of_Hate_Crimes_2019)

cor.test(my_data$`log10(Number of Hate Crimes by State in 2016)`, my_data$`log10(Number of Hate Crimes by State in 2019)`, method = "pearson")
cor.test(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, method = "kendall")
cor.test(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, method = "spearman")

#Linear Regression

scatter.smooth(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, xlab = "Hate Crimes by State in 2016", ylab = "Hate Crimes by State in 2019")

boxplot(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, main="2016", sub=paste("Outlier rows: ", boxplot.stats(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)$out))  # box plot for 'speed'
boxplot(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, main="2019", sub=paste("Outlier rows: ", boxplot.stats(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019)$out))

plot(density(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016), main="2016", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016), 2))) 
polygon(density(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016), col="red")
plot(density(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019), main="2019", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019), 2))) 
polygon(density(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019), col="red")

model <- lm(Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019 ~ Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016)
model
summary(model)

plot(Hate_Crimes_by_State_2016$Number_of_Hate_Crimes_2016, Hate_Crimes_by_State_2019$Number_of_Hate_Crimes_2019, pch = 16, col = "blue", xlab = "Number of Hate Crimes by State in 2016", ylab= "Number of Hate Crimes by State in 2019")
abline(model, col = "red", lwd = 2)
title("Number of Hate Crimes by State")

plot(model$residuals)
plot(cooks.distance(model), pch = 16, col = "blue")

####transformed####

#Correlation analysis; 
#Parametric and nonparametric correlation tests (Pearson, Spearman)

#visualize data first using pearson
ggscatter(my_data, x = "log10(Number of Hate Crimes by State in 2016)", y = "log10(Number of Hate Crimes by State in 2019)", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "log10(Number of Hate Crimes by State in 2016)", ylab = "log10(Number of Crimes by State in 2019)")

ggqqplot(my_data$`log10(Number of Hate Crimes by State in 2016)`, ylab = "log10(Number of Hate Crimes by State in 2016")
ggqqplot(my_data$`log10(Number of Hate Crimes by State in 2019)`, ylab = "log10(Number of Hate Crimes by State in 2019")

shapiro.test(my_data$`log10(Number of Hate Crimes by State in 2016)`)
shapiro.test(my_data$`log10(Number of Hate Crimes by State in 2019)`[2:51])

my_data$`log10(Number of Hate Crimes by State in 2019)`[1] = NA

cor.test(my_data$`log10(Number of Hate Crimes by State in 2016)`, my_data$`log10(Number of Hate Crimes by State in 2019)`, method = "pearson")

my_data$`log10(Number of Hate Crimes by State in 2019)`[1] = -Inf

#Linear Regression

scatter.smooth(my_data$`log10(Number of Hate Crimes by State in 2016)`, my_data$`log10(Number of Hate Crimes by State in 2019)`, xlab = "log10(Number of Hate Crimes by State in 2016)", ylab = "log10(Number of Hate Crimes by State in 2019)")

boxplot(my_data$`log10(Number of Hate Crimes by State in 2016)`, main="2016", sub=paste("Outlier rows: ", boxplot.stats(my_data$`log10(Number of Hate Crimes by State in 2016)`)$out)) 
boxplot(my_data$`log10(Number of Hate Crimes by State in 2019)`, main="2019", sub=paste("Outlier rows: ", boxplot.stats(my_data$`log10(Number of Hate Crimes by State in 2019)`)$out))

plot(density(my_data$`log10(Number of Hate Crimes by State in 2016)`), main="2016", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(my_data$`log10(Number of Hate Crimes by State in 2016)`), 2))) 
polygon(density(my_data$`log10(Number of Hate Crimes by State in 2016)`), col="red")
plot(density(my_data$`log10(Number of Hate Crimes by State in 2019)`[2:51]), main="2019", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(my_data$`log10(Number of Hate Crimes by State in 2019)`), 2))) 
polygon(density(my_data$`log10(Number of Hate Crimes by State in 2019)`[2:51]), col="red")

model <- lm(my_data$`log10(Number of Hate Crimes by State in 2019)`[2:51] ~ my_data$`log10(Number of Hate Crimes by State in 2016)`)
model
summary(model)

plot(my_data$`log10(Number of Hate Crimes by State in 2016)`, my_data$`log10(Number of Hate Crimes by State in 2019)`, pch = 16, col = "blue", xlab = "log10(Number of Hate Crimes by State in 2016)", ylab= "log10(Number of Hate Crimes by State in 2019)")
abline(model, col = "red", lwd = 2)
title("Hate Crimes by State in 2019 vs. Hate Crimes by State in 2016")

plot(model$residuals)
plot(cooks.distance(model), pch = 16, col = "blue")

#6#################################################################################################
##Bayesian Statistics: likelihood, conditional probabilities, examples of Bayesian inference
#using transformed data
mean_skew = c(mean(skew_2016), mean(skew_2019[2:51]))
sd_skew = c(sd(skew_2016), sd(skew_2019[2:51]))
mean_skew
sd_skew

prior_prob_A = 0.5 #either 2016 or 2019

prior_A = c(prior_prob_A, 1- prior_prob_A)

#B -> a states hate crimes in 2019 is greater than or equal to the mean
#calculate likelihood P(B obs|A unknown)
example_crime = 3

step = .001
crime = seq(0, 5, by = step)
likelihood_B = t(sapply(crime, function(h) dnorm(h, mean_skew, sd_skew)))
df_likelihood_B = data.frame(crime, likelihood_B)
colnames(df_likelihood_B) = c("crime", "2016", "2019")

melt_df_likelihood_B = melt(df_likelihood_B, id.vars = "crime", variable.name = "2016", value.name = "2019")
B.distr = ggplot(melt_df_likelihood_B, aes( x = crime, y = value, group = variable, colour = variable)) + 
  geom_line(aes(group = variable, colour = variable)) + 
  geom_vline(xintercept = example_crime) + xlab("log10(Number of Hate Crimes)") +
  ylab("Probability Density") + ggtitle("Hate Crime Distribution of United States", subtitle = "Prob(log10(Hate Crimes)|Year)")

print(B.distr)

#probability-weighted
likelihood_B_A = t(prior_A * t(likelihood_B))
df_likelihood_B_A = data.frame(crime, likelihood_B_A)
colnames(df_likelihood_B_A) = c("crime", "2016", "2019")

melt_df_likelihood_B_A = melt(df_likelihood_B_A, id.vars = "crime", variable.name = "2016", value.name = "2019")
B.distr.A = ggplot(melt_df_likelihood_B_A, aes( x = crime, y = value, group = variable, colour = variable)) + 
  geom_line(aes(group = variable, colour = variable)) + 
  geom_vline(xintercept = example_crime)+ xlab("log10(Number of Hate Crimes)") +
  ylab("Probability Density") + ggtitle("Probability Weighted Hate Crime Distribution of United States", subtitle = "Prob(log10(Hate Crimes)|Year) * Prob(Year)")

print(B.distr.A)

#calculate posterior probability
conditional_likelihood = prior_A *dnorm(example_crime, mean_skew, sd_skew)
conditional_likelihood = array(conditional_likelihood, dim = c(1,2))
colnames(conditional_likelihood) = c("2016", "2019")

conditional_likelihood

marginal_prob = sum(conditional_likelihood)

marginal_prob

posterior_prob_year = conditional_likelihood/marginal_prob

posterior_prob_year

#when is p(A|B) = p(not A|B),ambiguous

#find where likelihoods are equal, distributions intersect

prob_2016 = c()
prob_2019 = c()
c_2016 = c()
c_2019 = c()

for(i in 1:length(melt_df_likelihood_B$crime)){
  
  if(melt_df_likelihood_B$variable[i] == 2016){
    prob_2016 = append(prob_2016, melt_df_likelihood_B$value[i])
    c_2016 = append(c_2016, melt_df_likelihood_B$crime[i])  
    }else if(melt_df_likelihood_B$variable[i] == 2019){
    prob_2019 = append(prob_2019, melt_df_likelihood_B$value[i])
    c_2019 = append(c_2019, melt_df_likelihood_B$crime[i])  
    }else{
    next
  }
}

index = 0;
for(i in 1:length(prob_2016)){
  if(prob_2016[i] - prob_2019[i] <= 0){
     index = i
     break
  }else{
      next
  }
}

#take average before and after distributions intersect, don't have exact point where they intersect
ambiguity_crimes = (c_2016[index-1] + c_2016[index])/2

ambiguity_crimes

#calculate posterior probability for log10(hate crimes) that gives ambiguity

conditional_likelihood = prior_A *dnorm(ambiguity_crimes, mean_skew, sd_skew)
conditional_likelihood = array(conditional_likelihood, dim = c(1,2))
colnames(conditional_likelihood) = c("2016", "2019")

conditional_likelihood

marginal_prob = sum(conditional_likelihood)

marginal_prob

posterior_prob_year = conditional_likelihood/marginal_prob

posterior_prob_year
