#flat confounder

library(boot)

source('sc_functions.R')
source('params.R')
args <- commandArgs(trailingOnly = T)
id <- as.numeric(args[1])
method <- args[2]

out.file = file.path(method, paste0('sim0_', id, '.rds'))
if(file.exists(out.file)) quit()

set.seed(id)
s <- matrix(ncol = 2, runif(2*n, -1, 1))
C <- s[,1] + s[,2]
X <- C + rnorm(n, sd = 5)
Y <- 3*C + X + rnorm(n)

dat <- cbind(X, Y, s)

res <- sc_cont(dat = dat, method = method, shift = 1)

saveRDS(res, out.file)

