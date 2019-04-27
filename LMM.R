library(data.table)
t_prior_int <- rstanarm::student_t(
  df = 2,
  location = 0,
  scale = 5)
# Student t prior for the regression coefficients
t_prior_beta <- rstanarm::student_t(
  df = 2,
  location = 0,
  scale = 2.5)

# Student t prior for the linear predictor (standard deviation)
t_prior_sigma <- rstanarm::student_t(
  df = 2,
  location = 0,
  scale = 2.5)
# function to use to link the coefficients to the output
#   normal distribution family with identity link function
#   see ?family for more options
gen_fam <- gaussian(link="identity")
# Decomposed prior for the group level covariance matrix
cov_prior <- rstanarm::decov(
  regularization = 2,
  concentration = 2,
  shape = 1,
  scale = 1)
# 
# fit.main.no_opts <- stan_glmer(
#   formula = `Percent face` ~ age + age*age + (1 | SubjectID),
#   data = cbsst)
#### PLOTTING
##just for visualization
#par(mfrow = c(1:2))
cbsst$StimType_name <- factor(cbsst$StimType, levels = c(0,1), labels = c("Charlie Brown", "Sesame Street"))
cbsst$rx_name <- factor(cbsst$RX, levels = c(0,1), labels = c("Low Risk", "High Risk"))
ggplot(cbsst,aes(x=age_centered, y=face_sal, color=sub_id)) + facet_grid(StimType_name ~ rx_name) + ylab("Percent Fixation, face OVER sal") + geom_line(aes(group=sub_id)) + geom_point() + guides(color = "none")+ coord_cartesian(xlim=c(0,13), ylim=c(0,100)) 
ggplot(cbsst,aes(x=age_centered, y=ave_face, color=sub_id)) + facet_grid(StimType_name ~ rx_name) + ylab("Percent Fixation, Face") + geom_line(aes(group=sub_id)) + geom_point() + guides(color="none") + coord_cartesian(xlim=c(0,15), ylim=c(0,100)) 
ggplot(cbsst,aes(ave_face)) + geom_dotplot()
ggplot(cbsst,aes(x=age_centered, y=psal, color=sub_id)) + facet_grid(StimType_name ~ rx_name) + ylab("Percent Fixation, Saliency") + geom_line(aes(group=sub_id)) + geom_point() + guides(color="none") + coord_cartesian(xlim=c(1,15), ylim=c(0,100)) 
# library(readxl)
# CBSST_salience <- read_excel("~/Desktop/Projects/ETdataFiles/spreadsheets/07-27-17_CB_salience.xlsx", sheet = "combinedCBSST")
# cbsst <- as.data.table(CBSST_salience)
# cbsst_2 <- cbsst[, .(pface = `Percent face`, age, age_sq  = age*age, rx, StimType, sub_id = as.factor(SubjectID))]
# table_show(cbsst_2)
# 
#USING REGULAR
fm.age2_RX_HLM <- lme(pface ~ (age_centered + age_sq)*StimType*RX, 
                      random =~age_centered|sub_id, 
                      # weight=varPower(form=~fitted(.)),
                      #correlation=corCAR1(),
                      data=cbsst)
summary(fm.age2_RX_HLM)
coef(fm.age2_RX_HLM)



  
# cbbst <- as.data.table(CBSST_salience)
# cbbst <- cbbst[, .(pface = `Percent face`, age, age_sq  = age*age, sub_id = as.factor(SubjectID))]
fit.main.no_opts <- rstanarm::stan_glmer(
  formula = pface ~ StimType + RX + age_centered + (age_centered | sub_id),
  warmup = 100,
  iter = 150,
  chains=1,
  cores=1,
  adapt_delta = 0.95,
  data = cbsst)
summary(fit.main.no_opts)
fit.main.basic <- rstanarm::stan_glmer(
  formula = pface ~ (age_centered + age_sq)*StimType*RX + (age_centered | sub_id),
  iter = 200,
  cores=1,
  adapt_delta = 0.95,
  data = cbsst)
print(fit.main.basic, digits=3)
print(fit.main.no_opts, digits=3)

fit.main_noage_2 <- rstanarm::stan_glmer(
  formula = pface ~ age*StimType*RX + (age | sub_id),
  iter = 150,
  cores=1,
  adapt_delta = 0.95,
  data = cbsst)
print(fit.main_noage_2, digits=3)
print(fit.main_noage_2, digits=3)

#### This is the main model with age, age^2 and RX. no effect of stim and no interaction. 
fit.main <- stan_glmer(
  formula = pface ~ (age + age_sq)* RX*StimType + (1 + age | sub_id),
  data = cbsst,
  iter = 10000,
  warm = 5000,
  thin = 1,
  chains = 4,
  family = gen_fam,
  prior_intercept = t_prior_int,
  prior = t_prior_beta,
  prior_aux = t_prior_sigma,
  prior_covariance = cov_prior)
print(fit.main, digits=3)
pp_check(fit.main)
#### This is the main model with age, age^2 and RX also with random age^2
fit.sal <- stan_glmer(
  formula = psal ~ age* RX + (1 + age | sub_id),
  data = cbsst,
  iter = 10000,
  warm = 5000,
  thin = 1,
  chains = 4,
  family = gen_fam,
  prior_intercept = t_prior_int,
  prior = t_prior_beta,
  prior_aux = t_prior_sigma,
  prior_covariance = cov_prior)
print(fit.sal, digits=3)
pp_check(fit.sal)

fit.sal_SN <- stan_glmer(
  formula = psal ~ age*RX*SN_HRvsLR_SMA + (age | sub_id),
  data = cbsst,
  iter = 10000,
  warm = 5000,
  thin = 1,
  chains = 4,
  family = gen_fam,
  prior_intercept = t_prior_int,
  prior = t_prior_beta,
  prior_aux = t_prior_sigma,
  prior_covariance = cov_prior)
print(fit.sal_SN, digits=3)
pp_check(fit.sal_SN)

#### This is the main model with age, age^2 and RX and StimType 
fit.main_no_stim <- stan_glmer(
  formula = pface ~ (age+age_sq) * RX + (1 + age | sub_id),
  data = cbsst,
  iter = 10000,
  warm = 5000,
  thin = 1,
  chains = 4,
  family = gen_fam,
  prior_intercept = t_prior_int,
  prior = t_prior_beta,
  prior_aux = t_prior_sigma,
  prior_covariance = cov_prior)
print(fit.main_no_stim, digits=3)



### Let's check the fit between stim and random slopes
loo.basic <- loo(fit.main_stim)
loo.basic<- loo(fit.main.no_opts)
loo.basic_bySTIM<- loo(fit.main.basic)
loo.sal_Stim_SN <- loo(fit.sal_Stim_SN)
loo.sal_SN <- loo(fit.sal_SN)
loo.main <- loo(fit.main_stim)
loo.alt1 <- loo(fit.main_stim_random_slope)
print(loo.basic)
print(loo.main)
print(loo.alt1)

main_v_alt1 <- compare_models(loo.basic, loo.basic_no_age_sq)
print(main_v_alt1)
loo_ci(main_v_alt1, prob=0.95)
#If the difference is negative the first model is preferred.
#If positive then the second model is preferred (based on the order in which they are given in the compare function).

#####checking for salience model
loo.basic_sal <- loo(fit.sal)
loo.basic_sal_stim <- loo(fit.sal_Stim)
print(loo.basic_sal)
print(loo.basic_sal_stim)

main_v_alt1 <- compare_models(loo.main, loo.main_noage_2)
print(main_v_alt1)
loo_ci(main_v_alt1, prob=0.95)

### let's look at specific variables
post_age <- posterior_interval(fit.main.basic, prob=0.95, pars='age_centered') 
post_StimType <- posterior_interval(fit.main.basic, prob=0.95, pars='StimType') 
RX <- posterior_interval(fit.main.basic, prob=0.95, pars='RX')
RXbyAge <- posterior_interval(fit.main.basic, prob=0.95, pars='age_centered:RX')
RXbyStimType <-posterior_interval(fit.main.basic, prob=0.95, pars='StimType:RX')
ageByStim<-posterior_interval(fit.sal_Stim, prob=0.95, pars='age_centered:StimType')
ageStimRX <-posterior_interval(fit.main.basic, prob=0.95, pars='age_centered:StimType:RX')


bayes_p <- 1-mean(post_age > 0)
print(bayes_p)
#now let's look at if we need to split by sex

females <- cbsst[sex=="F", ]
males <- cbsst[sex=="M", ]

just_girls <-posterior_predict(fit.main_stim, newdata=females)
just_boys <- posterior_predict(fit.main_stim, newdata=males)
yhat_girls <- apply(just_girls, 1, mean)
yhat_boys <- apply(just_boys, 1, mean)
girls_vs_boys <- yhat_girls-yhat_boys
bayes_p <- 1-mean(girls_vs_boys >0)
##
age_RX <- posterior_interval(fit.alt, prob=0.95, pars='age_sq:RX') 
loo.main <- loo(fit.main)
loo.alt <- loo(fit.main_stim)

## let's check some of our variables...are they significant??
#age
bayes_age <- posterior_interval(fit.main_stim, prob=0.95, pars='age')
print(bayes_age)
#stim 
bayes_stim<-posterior_interval(fit.main_stim, prob=0.95, pars='StimType')
print(bayes_stim)
#RX
bayes_RX<-posterior_interval(fit.main_stim, prob=0.95, pars='RX')
print(bayes_RX)
#age by RX
bayes_age_rx<-posterior_interval(fit.main_stim, prob=0.95, pars='age:RX')
print(bayes_age_rx)

bayes_p <- 1-mean(bayes_age_rx > 0)
print(bayes_p)
#age by RX by StimType
bayes_age_rx_stim<-posterior_interval(fit.main_stim, prob=0.95, pars='age:RX:StimType')
print(bayes_age_rx_stim)
post_pred <- posterior_predict(fit.main_stim)
post_pred_sal <- posterior_predict(fit.alt1)

####making saliency graphs
pred_age_sal <- seq(2.5, 12.5,0.1)
n <- length(pred_age_sal)
new_data_sal <- data.frame(age=rep(pred_age_sal, 4), RX=c(rep(0,n), rep(0,n), rep(1,n), rep(1,n)))

# get predicted values
pred_post_sal <- posterior_predict(fit.alt1, newdata=new_data,
                               re.form=NA)

# get means of predictions
pred_means <- colMeans(pred_post_sal)
# get medians by running median on each column of pred_post
pred_medians <- apply(pred_post_sal, 2, median)
# get 5th and 95th quantile for each column
pred_lower <- apply(pred_post_sal, 2, quantile, probs=0.05)
pred_upper <- apply(pred_post_sal, 2, quantile, probs=0.95)

# add predictions to new_data for plotting
new_data$pred <- pred_means
new_data$median <- pred_medians
new_data$lower <- pred_lower
new_data$upper <- pred_upper


# plot
ggplot(new_data, aes(y=median, x=age,
                     color=factor(RX),
                     group=factor(RX))) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(RX)),
              alpha=0.3)


###using posterior preditions
# create a vector of age from 2.5 to 12. 5 by 1
pred_age <- seq(2.5, 12.5, .1)
# creat a squared age vector
pred_age_sq <- pred_age^2
# get the length of these vectors above
n <- length(pred_age)
# create a new dataset for prediction, using age and age_sq
#  vectors above, and crossing RX=0/1 and StimType=0/1
new_data <- data.frame(age=rep(pred_age, 4), age_sq=rep(pred_age_sq, 4),
                       RX=c(rep(0,n), rep(0,n), rep(1,n), rep(1,n)),
                       StimType=c(rep(0,n), rep(1,n), rep(0,n), rep(1,n)))

# get predicted values
pred_post <- posterior_predict(fit.main_stim, newdata=new_data,
                               re.form=NA)

# get means of predictions
pred_means <- colMeans(pred_post)
# get medians by running median on each column of pred_post
pred_medians <- apply(pred_post, 2, median)
# get 5th and 95th quantile for each column
pred_lower <- apply(pred_post, 2, quantile, probs=0.05)
pred_upper <- apply(pred_post, 2, quantile, probs=0.95)

# add predictions to new_data for plotting
new_data$pred <- pred_means
new_data$median <- pred_medians
new_data$lower <- pred_lower
new_data$upper <- pred_upper


# plot
ggplot(new_data, aes(y=median, x=age,
                     color=factor(StimType),
                     group=factor(StimType))) +
  facet_wrap(~RX) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(StimType)),
              alpha=0.4)
  
  
print(fit.main.basic, digits=3)
coef(fit.main.basic)
ranef(fit.main.basic)

print(fit.sal_Stim, digits=3)
coef(fit.sal_Stim)
ranef(fit.sal_Stim)

