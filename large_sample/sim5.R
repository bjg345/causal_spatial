#homogeneous linear, noisy confounding

library(boot)

source('sc_functions.R')
source('params.R')
args <- commandArgs(trailingOnly = T)
id <- as.numeric(args[1])
method <- args[2]

out.file = file.path(method, paste0('sim5_', id, '.rds'))
if(file.exists(out.file)) quit()

set.seed(id)
s <- matrix(ncol = 2, runif(2*n, -1, 1))
C <- sin(2*pi*s[,1]*s[,2]) + s[,1] + s[,2] + rnorm(n)
X <- C^3 + rnorm(n, sd = 5)
Y <- 5*C + X + rnorm(n)

dat <- cbind(X, Y, s)


res <- sc_cont(dat = dat, method = method, shift = 1)

saveRDS(res, out.file)

