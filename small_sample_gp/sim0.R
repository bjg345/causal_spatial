#flat confounder

library(boot)

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

library(BRISC)

mod = BRISC_estimation(coords = s, x=cbind(1, X), y = Y)

res <- BRISC_bootstrap(mod, n_boot = nboot, h = ncores)

saveRDS(list(est = mod$Beta[2], se = sd(res$boot.Beta[,2]), boot = res$boot.Beta[,2]), out.file)

