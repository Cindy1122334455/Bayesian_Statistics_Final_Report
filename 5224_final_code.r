---
# title: "Multilevel Bayesian Model for U.S. Labor Force Participation Rate"
# author: "Xiaotong Li(xl2788) & Fan Yang(fy2230) & Xiaotong Lin(xl2506)"

library(ggplot2)
library(lme4)
library(rstanarm)

###read file
setwd("/Users/tong/Desktop/Baye/Project")
# lfp <- read.csv("lfp.csv")
# lfp <- subset(lfp[,2:9])
state <- read.csv("state.csv") 
state <- as.data.frame(subset(state[,2:4]))
income <- read.csv("income.csv") 
income <- subset(income[,2:4])
state_income <- read.csv("state_income.csv")
state_income <- subset(state_income[,2:5])

## 1. Introduction
state<-read.csv('state.csv')
state<-as.data.frame(state)
ggplot(state, aes(x=state,y=lfpr)) + geom_smooth() + labs(y='lfp rate')

income<-read.csv('income.csv')
income <- as.data.frame(income)
ggplot(income, aes(x=income,y=lfpr)) + geom_smooth() + labs(y='lfp rate')


## 2 Multilevel Logistic Regression Model
#2.1 Fitting Logit Regression On Multiple Predictors


rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())
wage <- read.csv(file = "lfp.csv") 
wage<- as.data.frame(wage)

# State = NY(36)
# Toy model: will need to examine each state

state = 36
wage_new = wage[which(wage$state == state),]
ind =sample(1:dim(wage_new)[1], 15000)
wage_test = wage_new[ind[1:3000],]
wage_train1 = wage_new[ind[3001:6000],]
wage_train2 = wage_new[ind[6001:9000],]
wage_train3 = wage_new[ind[9001:12000],]
wage_train4 = wage_new[ind[12001:15000],]

income_group <- function(income){
  n = length(income)
  for (i in 1:n){
    if(income[i]<=4){
      income[i] <- 1
    } else if(income[i] >4 & income[i]<=10){
      income[i] <- 2
    } else if(income[i] >10 & income[i]<=20 ) {
      income[i] <-3
    } else if(income[i] >=20){
      income[i] <-4
    }}
  return(income)
}


# fit training data1 
wage_train = wage_train1
x1 = wage_train$sex
x2 = wage_train$white +1
x3 = wage_train$age +1
x4 = wage_train$skilled +1

x5 = income_group(wage_train$income)
y = wage_train$lfp

# prepare the test data

x1t = wage_test$sex
x2t = wage_test$white +1
x3t = wage_test$age +1
x4t = wage_test$skilled +1
x5t = income_group(wage_test$income)
yt = wage_test$lfp
Nt = length(yt)
# sum(is.na(x1))
# sum(is.na(x2))
# sum(is.na(x3))
# sum(is.na(x4))
# sum(is.na(x5))
# sum(is.na(y))

N = length(x1) ## // number of x
x1_N = length(unique(x1))## // number of categories in x
x2_N = length(unique(x2)) ##// number of categories in x2
x3_N = length(unique(x3))
x4_N = length(unique(x4))
x5_N = length(unique(x5))

data <- list(x1_N = x1_N, x2_N = x2_N,x3_N = x3_N,x4_N = x4_N,x5_N = x5_N,
             N = N,Nt = Nt, x1 = x1,x2 = x2,x3 = x3,x4 = x4,x5 = x5,x1t=x1t,
             x2t=x2t,x3t=x3t,x4t=x4t,x5t=x5t,
             y = y)

fit_multi1<-stan(file = "multi_logit3.stan",data=data, iter = 1000, chains=1, control = list(max_treedepth = 15))
fit_multi1_sum <- extract(fit_multi1)
print(fit_multi1)


# Now let's see how did we do about prediction on test

y_pred = apply(fit_multi1_sum$y_test, MARGIN = 2,FUN = mean)
y_pred_bin = ifelse(y_pred >=0.5,1,0)
y_pred_bin1 = y_pred_bin


# The success prediction rate
mean(y_pred_bin==yt)



fit_summary <- as.data.frame(summary(fit_multi1)$summary)

# Make vector of wanted parameter names
wanted_pars <- c(paste0("intercept"),paste0("x1_coeff[", 1:x1_N,"]"), paste0("x2_coeff[",1:x2_N,"]"),paste0("x3_coeff[",1:x3_N,"]"),
                 paste0("x5_coeff[",1:x5_N,"]"),paste0("x4_coeff[",1:x4_N,"]"),c("sigma_x1","sigma_x2","sigma_x3","sigma_x4","sigma_x5"))


# Get estimated and generating values for wanted parameters
estimated_values <- fit_summary[wanted_pars, c("mean", "2.5%", "97.5%")]

# Assesmble a data frame to pass to ggplot()
sim_df <- data.frame(parameter = factor(wanted_pars, rev(wanted_pars)), row.names = NULL)
sim_df$middle <- estimated_values[, "mean"] 
sim_df$lower <- estimated_values[, "2.5%"] 
sim_df$upper <- estimated_values[, "97.5%"] 


# Plot the posterior estimation or the parameters
ggplot(sim_df) + aes(x = parameter, y = middle, ymin = lower, ymax = upper) + 
  scale_x_discrete() + geom_abline(intercept = 0, slope = 0, color = "white") + 
  geom_linerange() + geom_point(size = 2) + labs(y = "Estimation", x = NULL) + 
  theme(panel.grid = element_blank()) + coord_flip()



# Another fitting method (vectorization)
library(rstan)
# Multi-logit Regression Fit
data6 <- matrix(NA,3000,6)
data6[,1]<-rep(1,3000)
data6[,2]<-scale(x1)
data6[,3]<-scale(x2)
data6[,4]<-scale(x3)
data6[,5]<-scale(x4)
data6[,6]<-scale(x5)

data7 <- matrix(NA,3000,6)
data7[,1]<-rep(1,3000)
data7[,2]<-scale(x1t)
data7[,3]<-scale(x2t)
data7[,4]<-scale(x3t)
data7[,5]<-scale(x4t)
data7[,6]<-scale(x5t)

y_prime=y+1
N = dim(data6)[1] ## // predictors
D = dim(data6)[2] ## // number of predictors
K = length(unique(y)) ##// number of categories
data <- list(N=N, D=D,K=K, x=data6, y = y_prime)
fit_multi1<-stan(file = "multi_logit.stan",data=data, iter = 200, chains=1)
print(fit_multi1)
fit_multi_sum = extract(fit_multi1)
beta = fit_multi_sum$beta


# Traning data 2
###################
# Same procedure for trainingdata2
# fit training data1 
wage_train = wage_train2
x1 = wage_train$sex
x2 = wage_train$white +1
x3 = wage_train$age +1
x4 = wage_train$skilled +1

x5 = income_group(wage_train$income)
y = wage_train$lfp

# prepare the test data

x1t = wage_test$sex
x2t = wage_test$white +1
x3t = wage_test$age +1
x4t = wage_test$skilled +1
x5t = income_group(wage_test$income)
yt = wage_test$lfp
Nt = length(yt)
# sum(is.na(x1))
# sum(is.na(x2))
# sum(is.na(x3))
# sum(is.na(x4))
# sum(is.na(x5))
# sum(is.na(y))

N = length(x1) ## // number of x
x1_N = length(unique(x1))## // number of categories in x
x2_N = length(unique(x2)) ##// number of categories in x2
x3_N = length(unique(x3))
x4_N = length(unique(x4))
x5_N = length(unique(x5))

data <- list(x1_N = x1_N, x2_N = x2_N,x3_N = x3_N,x4_N = x4_N,x5_N = x5_N,
             N = N,Nt = Nt, x1 = x1,x2 = x2,x3 = x3,x4 = x4,x5 = x5,x1t=x1t,
             x2t=x2t,x3t=x3t,x4t=x4t,x5t=x5t,
             y = y)

fit_multi1<-stan(file = "multi_logit3.stan",data=data, iter = 1000, chains=1, control = list(max_treedepth = 15))
fit_multi1_sum <- extract(fit_multi1)
print(fit_multi1)


# Now let's see how did we do about prediction on test

y_pred = apply(fit_multi1_sum$y_test, MARGIN = 2,FUN = mean)
y_pred_bin = ifelse(y_pred >=0.5,1,0)
y_pred_bin2 = y_pred_bin

# End of procedure for training data


# Training data 3
###################
# Same procedure for trainingdata3
# fit training data3 
wage_train = wage_train3
x1 = wage_train$sex
x2 = wage_train$white +1
x3 = wage_train$age +1
x4 = wage_train$skilled +1

x5 = income_group(wage_train$income)
y = wage_train$lfp

# prepare the test data

x1t = wage_test$sex
x2t = wage_test$white +1
x3t = wage_test$age +1
x4t = wage_test$skilled +1
x5t = income_group(wage_test$income)
yt = wage_test$lfp
Nt = length(yt)
# sum(is.na(x1))
# sum(is.na(x2))
# sum(is.na(x3))
# sum(is.na(x4))
# sum(is.na(x5))
# sum(is.na(y))

N = length(x1) ## // number of x
x1_N = length(unique(x1))## // number of categories in x
x2_N = length(unique(x2)) ##// number of categories in x2
x3_N = length(unique(x3))
x4_N = length(unique(x4))
x5_N = length(unique(x5))

data <- list(x1_N = x1_N, x2_N = x2_N,x3_N = x3_N,x4_N = x4_N,x5_N = x5_N,
             N = N,Nt = Nt, x1 = x1,x2 = x2,x3 = x3,x4 = x4,x5 = x5,x1t=x1t,
             x2t=x2t,x3t=x3t,x4t=x4t,x5t=x5t,
             y = y)

fit_multi1<-stan(file = "multi_logit3.stan",data=data, iter = 1000, chains=1, control = list(max_treedepth = 15))
fit_multi1_sum <- extract(fit_multi1)
print(fit_multi1)


# Now let's see how did we do about prediction on test

y_pred = apply(fit_multi1_sum$y_test, MARGIN = 2,FUN = mean)
y_pred_bin = ifelse(y_pred >=0.5,1,0)
y_pred_bin3 = y_pred_bin

# End of procedure for training data 3


# Traning data 4
###################
# Same procedure for trainingdata4
# fit training data4 
wage_train = wage_train4
x1 = wage_train$sex
x2 = wage_train$white +1
x3 = wage_train$age +1
x4 = wage_train$skilled +1

x5 = income_group(wage_train$income)
y = wage_train$lfp

# prepare the test data

x1t = wage_test$sex
x2t = wage_test$white +1
x3t = wage_test$age +1
x4t = wage_test$skilled +1
x5t = income_group(wage_test$income)
yt = wage_test$lfp
Nt = length(yt)
# sum(is.na(x1))
# sum(is.na(x2))
# sum(is.na(x3))
# sum(is.na(x4))
# sum(is.na(x5))
# sum(is.na(y))

N = length(x1) ## // number of x
x1_N = length(unique(x1))## // number of categories in x
x2_N = length(unique(x2)) ##// number of categories in x2
x3_N = length(unique(x3))
x4_N = length(unique(x4))
x5_N = length(unique(x5))

data <- list(x1_N = x1_N, x2_N = x2_N,x3_N = x3_N,x4_N = x4_N,x5_N = x5_N,
             N = N,Nt = Nt, x1 = x1,x2 = x2,x3 = x3,x4 = x4,x5 = x5,x1t=x1t,
             x2t=x2t,x3t=x3t,x4t=x4t,x5t=x5t,
             y = y)

fit_multi1<-stan(file = "multi_logit3.stan",data=data, iter = 1000, chains=1, control = list(max_treedepth = 15))
fit_multi1_sum <- extract(fit_multi1)
print(fit_multi1)


# Now let's see how did we do about prediction on test

y_pred = apply(fit_multi1_sum$y_test, MARGIN = 2,FUN = mean)
y_pred_bin = ifelse(y_pred >=0.5,1,0)
y_pred_bin4 = y_pred_bin

# End of procedure for training data
##########################


# check the results for the prediction 

dat <- data.frame(
  Part = factor(c("Yes","No"), levels=c("Yes","No")),
  count = c(sum(yt==0), sum(y==1))
)


# For the testing data
q1 = ggplot(data=dat, aes(x=Part, y=count, fill=Part)) +
  geom_bar(stat="identity")+
  xlab("Whether the Person will Participate The Labor Force or Not")+
  ylab("Total Count (Using The test Data)")

# For training 1

dat <- data.frame(
  Part = factor(c("Yes","No"), levels=c("Yes","No")),
  count = c(sum(y_pred_bin1==0), sum(y_pred_bin1==1))
)
q2 = ggplot(data=dat, aes(x=Part, y=count, fill=Part)) +
  geom_bar(stat="identity")+
  xlab("Whether the Person will Participate The Labor Force or Not")+
  ylab("Total Count (Using The Training 1 Data)")

# For training 2

dat <- data.frame(
  Part = factor(c("Yes","No"), levels=c("Yes","No")),
  count = c(sum(y_pred_bin2==0), sum(y_pred_bin2==1))
)
q3 = ggplot(data=dat, aes(x=Part, y=count, fill=Part)) +
  geom_bar(stat="identity")+
  xlab("Whether the Person will Participate The Labor Force or Not")+
  ylab("Total Count (Using The Training 2 Data)")

# For training 3

dat <- data.frame(
  Part = factor(c("Yes","No"), levels=c("Yes","No")),
  count = c(sum(y_pred_bin3==0), sum(y_pred_bin3==1))
)
q4 =  ggplot(data=dat, aes(x=Part, y=count, fill=Part)) +
  geom_bar(stat="identity")+
  xlab("Whether the Person will Participate The Labor Force or Not")+
  ylab("Total Count (Using The Training 3 Data)")

# For training 4

dat <- data.frame(
  Part = factor(c("Yes","No"), levels=c("Yes","No")),
  count = c(sum(y_pred_bin4==0), sum(y_pred_bin4==1))
)
q5 =  ggplot(data=dat, aes(x=Part, y=count, fill=Part)) +
  geom_bar(stat="identity")+
  xlab("Whether the Person will Participate The Labor Force or Not")+
  ylab("Total Count (Using The Training 4 Data)")

grid.arrange(q1,q2,q3,q4,q5,ncol=3, top="The Prediction Comparing y_rep and The y_test")


# For training data1
# Plot the individual paramerters (particularly interested in the intercept and the coefficients)



# Intercept
color1 = 'purple'
x1 = fit_multi1_sum$intercept;
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p1= ggplot(data = data1,
           aes(x = index,
               y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Intercept")

# Beta1[,1]

x1 = fit_multi1_sum$x1_coeff[,1]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p2=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta1[,1]")

# Beta1[,2]

x1 = fit_multi1_sum$x1_coeff[,2]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p3=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta1[,2]")


# Beta2[,1]

x1 = fit_multi1_sum$x2_coeff[,1]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p4=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta2[,1]")

# Beta2[,2]

x1 = fit_multi1_sum$x2_coeff[,2]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p5=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta2[,2]")

# Beta3[,1]

x1 = fit_multi1_sum$x3_coeff[,1]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p6=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta3[,1]")

# Beta3[,2]

x1 = fit_multi1_sum$x3_coeff[,2]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p7=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta3[,2]")

# Beta3[,3]

x1 = fit_multi1_sum$x3_coeff[,3]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p8=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta3[,3]")

# Beta3[,4]

x1 = fit_multi1_sum$x3_coeff[,4]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p9=ggplot(data = data1,
          aes(x = index,
              y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta3[,4]")

# Beta4[,1]

x1 = fit_multi1_sum$x4_coeff[,1]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p10=ggplot(data = data1,
           aes(x = index,
               y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta4[,1]")

# Beta4[,2]

x1 = fit_multi1_sum$x4_coeff[,2]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p11=ggplot(data = data1,
           aes(x = index,
               y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta4[,1]")

# Beta5[,1]

x1 = fit_multi1_sum$x5_coeff[,1]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p12=ggplot(data = data1,
           aes(x = index,
               y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta5[,1]")

# Beta5[,2]

x1 = fit_multi1_sum$x5_coeff[,2]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p13=ggplot(data = data1,
           aes(x = index,
               y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta5[,2]")

# Beta5[,3]

x1 = fit_multi1_sum$x5_coeff[,3]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p14=ggplot(data = data1,
           aes(x = index,
               y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta5[,3]")

# Beta5[,4]

x1 = fit_multi1_sum$x5_coeff[,4]
index = seq(1:length(x1))
data1 = as.data.frame(cbind(x1, index))
p15=ggplot(data = data1,
           aes(x = index,
               y = x1)) + 
  geom_line(color=color1, size =0.5)+
  xlab("Iteration")+
  ylab("Beta5[,4]")





library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,
             p10,p11,p12,p13,p14,p15, ncol=3, top="The Posterior Estimation of Parameters")



coef <- cbind(fit_multi1_sum$x1_coeff[,1],fit_multi1_sum$x1_coeff[,2],fit_multi1_sum$x2_coeff[,1],fit_multi1_sum$x2_coeff[,2],fit_multi1_sum$x3_coeff[,1],fit_multi1_sum$x3_coeff[,2],fit_multi1_sum$x3_coeff[,3],fit_multi1_sum$x3_coeff[,4],fit_multi1_sum$x4_coeff[,1],fit_multi1_sum$x4_coeff[,2],fit_multi1_sum$x5_coeff[,1],fit_multi1_sum$x5_coeff[,2],fit_multi1_sum$x5_coeff[,3],fit_multi1_sum$x5_coeff[,4],fit_multi1_sum$x6_coeff[,1],fit_multi1_sum$x6_coeff[,2],fit_multi1_sum$x6_coeff[,3],fit_multi1_sum$x6_coeff[,4],fit_multi1_sum$x7_coeff[,1],fit_multi1_sum$x7_coeff[,2],fit_multi1_sum$x7_coeff[,3],fit_multi1_sum$x7_coeff[,4])
index = seq(1:nrow(coef))
coef = as.data.frame(cbind(index,coef))


ggplot(coef, aes(index)) + 
  geom_line(aes(y = coef[,2], colour = "sex1")) + 
  geom_line(aes(y = coef[,3], colour = "sex2")) + 
  geom_line(aes(y = coef[,4], colour = "white1")) + 
  geom_line(aes(y = coef[,5], colour = "white2")) + 
  geom_line(aes(y = coef[,6], colour = "age1")) + 
  geom_line(aes(y = coef[,7], colour = "age2")) + 
  geom_line(aes(y = coef[,8], colour = "age3")) + 
  geom_line(aes(y = coef[,9], colour = "age4")) + 
  geom_line(aes(y = coef[,10], colour = "skill1")) + 
  geom_line(aes(y = coef[,11], colour = "skill2")) + 
  geom_line(aes(y = coef[,12], colour = "income1")) + 
  geom_line(aes(y = coef[,13], colour = "income2")) + 
  geom_line(aes(y = coef[,14], colour = "income3")) + 
  geom_line(aes(y = coef[,15], colour = "income4")) + 
  geom_line(aes(y = coef[,16], colour = "state1")) + 
  geom_line(aes(y = coef[,17], colour = "state2")) + 
  geom_line(aes(y = coef[,18], colour = "state3")) + 
  geom_line(aes(y = coef[,19], colour = "state4")) + 
  geom_line(aes(y = coef[,20], colour = "year1")) + 
  geom_line(aes(y = coef[,21], colour = "year2")) + 
  geom_line(aes(y = coef[,22], colour = "year3")) + 
  geom_line(aes(y = coef[,23], colour = "year4"))  + labs(y='Coefficient')





## 3 Expansion Model
#Model 1: Varying intercept model with no predictors (Variance components model)
  
M1_stanlmer <- stan_lmer(formula = lfpr ~ 1 + (1 | state),
                         data = state,
                         seed = 349)


#Posterior medians and posterior median absolute deviations
print(M1_stanlmer, digits = 2)


#Posterior means, posterior standard deviations, 95% credible intervals and Monte Carlo errors
summary(M1_stanlmer,
        pars = c("(Intercept)", "sigma", "Sigma[state:(Intercept),(Intercept)]"),
        probs = c(0.025, 0.975),
        digits = 2)

# Extract the posterior draws for all parameters
sims <- as.matrix(M1_stanlmer)
dim(sims)

para_name <- colnames(sims)
# para_name


##Obtaining means, standard deviations, medians and 95% credible intervals.

# Obtain state-level varying intercept a_j
# draws for overall mean
mu_a_sims <- as.matrix(M1_stanlmer,
                       pars = "(Intercept)")
# draws for statel-level error
u_sims <- as.matrix(M1_stanlmer,
                    regex_pars = "b\\[\\(Intercept\\) state\\:")
# draws for states' varying intercepts
a_sims <- as.numeric(mu_a_sims) + u_sims
# Obtain sigma_y and sigma_alpha^2
# draws for sigma_y
s_y_sims <- as.matrix(M1_stanlmer,
                      pars = "sigma")
# draws for sigma_alpha^2
s__alpha_sims <- as.matrix(M1_stanlmer,
                           pars = "Sigma[state:(Intercept),(Intercept)]")



# Compute mean, SD, median, and 95% credible interval of varying intercepts
# Posterior mean and SD of each alpha
a_mean <- apply(X = a_sims,
                MARGIN = 2,
                FUN = mean)
a_sd <- apply(X = a_sims,
              MARGIN = 2,
              FUN = sd)
# posterior mean
# posterior SD
# Posterior median and 95% credible interval
a_quant <- apply(X = a_sims,
                 MARGIN = 2,
                 FUN = quantile,
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")
# Combine summary statistics of posterior simulation draws
a_df <- data.frame(a_mean, a_sd, a_quant)
round(head(a_df), 2)


# Sort dataframe containing an estimated alpha's mean and sd for every school
a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])  # a vector of school rank
# Plot school-level alphas's posterior mean and 95% credible interval
ggplot(data = a_df,
       aes(x = a_rank,
           y = a_mean)) +
  geom_pointrange(aes(ymin = Q2.5,
                      ymax = Q97.5),
                  position = position_jitter(width = 0.1,
                                             height = 0)) +
  geom_hline(yintercept = mean(a_df$a_mean),
             size = 0.5,
             col = "red") +
  scale_x_continuous("Rank",
                     breaks = seq(from = 0,
                                  to = 80,
                                  by = 5)) +
  scale_y_continuous(expression(paste("varying intercept, ", alpha[j]))) +
  theme_bw( base_family = "serif")



##Making comparisons between individual states
# The difference between the two school averages (school #6 and #36)
state_diff <- a_sims[, 6] - a_sims[, 36]

# Investigate differences of two distributions
mean <- mean(state_diff)
sd <- sd(state_diff)
quantile <- quantile(state_diff, probs = c(0.025, 0.50, 0.975))
quantile <- data.frame(t(quantile))
names(quantile) <- c("Q2.5", "Q50", "Q97.5")
diff_df <- data.frame(mean, sd, quantile)
round(diff_df, 6)

state_diff <- state_diff*100
# Histogram of the differences
ggplot(data = data.frame(state_diff),
       aes(x = state_diff)) +
  geom_histogram(color = "black",
                 fill = "gray",
                 binwidth = 0.25) +
  scale_x_continuous("Labor Force Participation Rate diffence between two states: California & New York (*100)",
                     breaks = seq(from = 3,
                                  to = 8,
                                  by = 0.5)) +
  geom_vline(xintercept = c(mean(state_diff),
                            quantile(state_diff,
                                     probs = c(0.025, 0.975))),
             colour = "red",
             linetype = "longdash") +
  geom_text(aes(5.39, 30, label = "mean = 0.0539"),
            color = "red",
            size = 4) +
  geom_text(aes(7, 50, label = "SD = 0.0060"),
            color = "blue",
            size = 4) +
  theme_bw( base_family = "serif")


prop.table(table(a_sims[, 6] > a_sims[, 36]))


plot(M1_stanlmer, "rhat")
plot(M1_stanlmer, "ess")



# Model 2: Adding a student-level predictor

temp <- sample(1:33794, 50)
new_state_income <- state_income[temp,]

M2_stanlmer <- stan_lmer(formula = lfpr ~ state + (1 | income),
                         data = new_state_income ,
                         prior = normal(location = 0,
                                        scale = 100,
                                        autoscale = FALSE),prior_intercept = normal(location = 0,scale = 100,autoscale = FALSE),seed = 349)

prior_summary(object = M2_stanlmer)

M2_stanlmer


# Model 3: Allowing for varying slopes across schools

M3_stanlmer <- stan_lmer(formula = lfpr ~ state + (1 + state | income),
                         data = new_state_income,
                         seed = 349)
prior_summary(object = M3_stanlmer)

M3_stanlmer

  