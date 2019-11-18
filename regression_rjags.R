require(rjags)
require(runjags)
require(bayesplot)

color_scheme_set("brightblue")
bayesplot_theme_set()

############ data prep ##############
# Load data and fill NAs.
setwd('C:/Users/johnd/OneDrive/Documents/TDI_project')
data <- read.csv('counties_person_hours_areas.csv')

# Remove those with 0 electrical fires recorded
data <- data[!is.na(data$acreage_fires),]

# Examine distribution of outcome variable.
hist(data$personhours)
hist(log(data$personhours + 0.01), breaks = 15)
data$personhours.log <- log(data$personhours + 0.01)

# Examine correlations
cor(data[,2:5]) # all below |0.15|

# define data in list form
model.data <- as.list(data[,c(2,3,5)])

# number of observations
model.data$n <- nrow(data)

# hyperparameters
model.data$mu.beta <- 0
model.data$tau.beta <- 0.1 

model.data$lb.sigma.y <- 0
model.data$ub.sigma.y <- 10


############ model ##############
model.modstring <- "model{
  # likelihood
  for (i in 1:n){
    personhours.log[i] ~ dnorm(mu[i], tau.y)
    
    mu[i] <- beta.zero +
             beta.count * (count_fires[i] - mean(count_fires[])) +    
             beta.acreage * (acreage_fires[i] - mean(acreage_fires[]))
  }
    # priors
      sigma.y ~ dunif(lb.sigma.y, ub.sigma.y)
      tau.y <- pow(sigma.y, -2)
  
  beta.zero ~ dnorm(mu.beta, tau.beta)
  beta.count ~ dnorm(mu.beta, tau.beta)
  beta.acreage ~ dnorm(mu.beta, tau.beta)
  }"

############# simulations ##############
# JAGS model definition and initialization
model.model <- jags.model(textConnection(model.modstring), 
                           data = model.data,
                           n.chains = 3)

update(model.model, 1000)

# sampling
model.vars <- c("sigma.y", "beta.zero", 
                 "beta.count", 
                 "beta.acreage")

model.samples <- coda.samples(model.model, variable.names = model.vars, 
                               n.iter = 50000)

########## MCMC diagnostics ############
# MCMC diagnostics
gelman.diag(model.samples)
mce.ratio <- min(summary(model.samples)$statistics[,2] / 
                    summary(model.samples)$statistics[,4] )

mcmc_trace(model.samples, pars=model.vars[1:(length(model.vars)-1)])

params <- as.mcmc(model.samples[[1]],
                  model.samples[[2]],
                  model.samples[[3]], chains=TRUE)

# autocorrelation and effective sample size for betas 
par(mfrow = c(3,4))
for (i in 1:ncol(params)){
  ess <- round(effectiveSize(params)[i])
  title <- paste0(model.vars[i], ", ess:", ess)
  autocorr.plot(params[,i], main=title, auto.layout = FALSE) 
}

####### residual analysis #######

# calcuation of fitted values
n <- nrow(data)

# coefficients
res.mat <- as.matrix(model.samples[,1:2])
n.samples <- nrow(res.mat)

intercept.res.mat <- as.matrix(model.samples[,3]) 
coeffs <- (cbind(intercept.res.mat, res.mat))
sigma.y <- as.matrix(model.samples[,4])

# data
covs <- data[,c(2,3)]
covs <- covs[,order(names(covs))]
covs <- as.matrix(covs) # 480 x 9
covs <- scale(covs, scale=FALSE)
intercepts <- as.matrix(rep(1,n))# 480 x 15

design <- cbind(intercepts, covs)
fittedvalues <- design %*% t(coeffs)

# hat matrix
H <- design %*% solve((t(design) %*% design)) %*% t(design) 


##### calcuation of studentized residuals
# studentised residual distributions matrix: 15,000 (5k * 3 chains) by 480 
studentisedresid <- matrix(0, nrow=n, ncol=n.samples) # initialize
for(sample in 1:n.samples){ # for each sample
  for(observation in 1:n){ # for each original observation
    studentisedresid[observation, sample] <- 
      (data$personhours.log[observation] - fittedvalues[observation, sample]) /
      ((sigma.y[sample]) * sqrt(1 - diag(H)[observation]))
  }
}

# posterior mean of studentised residuals
studentisedresidm <- numeric(n)
for(i in 1:n){
  studentisedresidm[i] <- mean(studentisedresid[i,])
}

# posterior mean fitted values
fittedvaluesm = numeric(n)
for(i in 1:n){
  fittedvaluesm[i]=mean(fittedvalues[i,])
}

# put fitted, resids, council, an year in data frame
resid.stud.groups <- data.frame(fitted = fittedvaluesm,
                                resid = studentisedresidm)

# resid vs index
resid.index <- qplot(seq_along(studentisedresidm), resid, 
                      data = resid.stud.groups,
                      xlab='observation index',
                      ylab='posterior mean studentized residual') + 
  theme_light() +
  theme(legend.position="none") + 
  ylim(-3.5, 3.5)
resid.index

# resid vs fitted
resid.fit <- qplot(fitted, resid, data=resid.stud.groups,
                    xlab='fitted value',
                    ylab='posterior mean studentized residual') +
  theme_light() +
  theme(legend.position="none") 

resid.fit

#QQ-plot
qq <- ggplot(data=resid.stud.groups, aes(sample = resid)) + theme_light()
qq <- qq + stat_qq() + stat_qq_line(color='maroon',lwd=1)
qq


####### inference ########
# backtransformation
back.params <- exp(params)

areas <- mcmc_dens(back.params[,1:2])+ vline_at(1, color='black', alpha=.9)
areas

dev.copy2pdf(file = 'posterior_areas.pdf', width = 10, height = 4)
