library(EnvStats)
library(bbmle)

data <- c(0.003, 0.012, 0.180, 0.253, 0.394,
          0.430, 0.491, 0.743, 1.066, 1.126, 
          1.303, 1.508, 1.740, 4.757, 5.376, 
          5.557, 7.236, 7.465, 8.054, 14.938)

#1. Estimasi Parameter
#   distribusi uniform

parunif <- eunif(data, method = 'mle')
parunif$parameters

unif_a <- parunif$parameters['min']
unif_b <- parunif$parameters['max']

#   distribusi eksponensial

parexp <- eexp(data, method = 'mle')
parexp$parameters

exp_lmd <- parexp$parameters['rate']

#   distribusi lognormal

parlnorm <- elnorm(data, method = 'mle')
parlnorm$parameters

lnorm_mu <- parlnorm$parameters['meanlog']
lnorm_sg <- parlnorm$parameters['sdlog']


#2. CDF
#   distribusi empiris

plot(ecdf(data), xlim = c(0,15), ylim = c(0,1),
     xlab = 'x: ground-up loss (dalam juta dollar)',
     main = '')
plotemp <- recordPlot()
title('Distribusi Empiris')

#   distribusi uniform

#      plot tunggal
x <- seq(-1,16,0.02)
plot(x, punif(x, unif_a, unif_b, lower.tail = TRUE), type = 'l',
     xlim = c(0,15), ylim = c(0,1),
     xlab = 'x: ground-up loss (dalam juta dollar)',
     ylab = 'F(x)',
     main = 'Distribusi Uniform')

#      plot perbandingan empiris
plotemp
lines(x, punif(x, unif_a, unif_b, lower.tail = TRUE), 
      col = 'blue', lwd = 2)
title('Perbandingan Empiris - Uniform')

#   distribusi eksponensial

#      plot tunggal
plot(x, pexp(x, exp_lmd, lower.tail = TRUE), type = 'l',
     xlim = c(0,15), ylim = c(0,1),
     xlab = 'x: ground-up loss (dalam juta dollar)',
     ylab = 'F(x)',
     main = 'Distribusi Eksponensial')

#      plot perbandingan empiris
plotemp
lines(x, pexp(x, exp_lmd, lower.tail = TRUE), col = 'pink', lwd = 2)
title('Perbandingan Empiris - Eksponensial')

#   distribusi lognormal

#      plot tunggal
plot(x, plnorm(x, lnorm_mu, lnorm_sg, lower.tail = TRUE), type = 'l',
     xlim = c(0,15), ylim = c(0,1),
     xlab = 'x: ground-up loss (dalam juta dollar)',
     ylab = 'F(x)',
     main = 'Distribusi Lognormal')

#      plot perbandingan empiris
plotemp
lines(x, plnorm(x, lnorm_mu, lnorm_sg, lower.tail = TRUE), col = 'blue', lwd = 2)
title('Perbandingan Empiris - Lognormal')


#3. Uji Kecocokan Model (KS)

#   distribusi uniform
ks.test(data, 'punif', unif_a, unif_b)

#p-value < alpha = 0.05
#kesimpulan: H0 ditolak
#distribusi uniform tidak memodelkan data dengan baik

#   distribusi eksponensial
ks.test(data, 'pexp', exp_lmd)

#p-value > alpha = 0.05
#kesimpulan: H0 tidak ditolak
#distribusi eksponensial memodelkan data dengan baik

#   distribusi lognormal
ks.test(data, 'plnorm', lnorm_mu, lnorm_sg)

#p-value > alpha = 0.05
#kesimpulan: H0 tidak ditolak
#distribusi lognormal memodelkan data dengan baik


#ekstra: menghitung AIC
library(survival)
library(flexsurv)

#model eksponensial
fitexp <- flexsurvreg(Surv(data)~1, 
                      dist = 'exponential')
fitexp$AIC

#model lognormal
fitlnorm <- flexsurvreg(Surv(data)~1, 
                        dist = 'lognormal')
fitlnorm$AIC


#Didapat AIC eksponensial lebih rendah dr AIC lognormal
#Kesimpulan: distribusi eksponensial lebih cocok untuk memodelkan data

#note: definisi AIC adalah -2lnL+2p