library(boot)
library(tidyverse)

nruns = 250

getRes = function(method, sim){
	
  tru = ifelse(sim == 4, 2.431, 1)

	runs = lapply(1:nruns, function(i) readRDS(file.path(method, paste0('sim', sim, '_', i, '.rds')))$boot)

  est.raw = sapply(runs, function(x) x$t0)
	est.cor = sapply(runs, function(x) 2*x$t0 - mean(x$t)	)

 	bias.raw = mean(est.raw) - tru
	bias.cor = mean(est.cor) - tru
	mse.raw = mean((est.raw - tru)^2)
	mse.cor = mean((est.cor - tru)^2)
  
	cis = lapply(runs, function(x) boot.ci(x, type = c('basic', 'perc', 'norm')))
	cis.basic = lapply(cis, function(x) x$basic[4:5])
	cis.perc = lapply(cis, function(x) x$perc[4:5])
	cis.norm.cor = lapply(cis, function(x) x$norm[2:3])
  cis.norm.raw = lapply(1:nruns, function(i) cis[[i]]$norm[2:3] - (runs[[i]]$t0 - mean(runs[[i]]$t)))
	

	
	cov.basic = (sapply(1:nruns, function(i) cis.basic[[i]][1] <= tru & cis.basic[[i]][2] >= tru)) %>% mean()
  cov.perc = (sapply(1:nruns, function(i) cis.perc[[i]][1] <= tru & cis.perc[[i]][2] >= tru)) %>% mean()
  cov.norm.cor = (sapply(1:nruns, function(i) cis.norm.cor[[i]][1] <= tru & cis.norm.cor[[i]][2] >= tru)) %>% mean()
  cov.norm.raw = (sapply(1:nruns, function(i) cis.norm.raw[[i]][1] <= tru & cis.norm.raw[[i]][2] >= tru)) %>% mean()
  
  width.basic = (sapply(1:nruns, function(i) cis.basic[[i]][2]- cis.basic[[i]][1] )) %>% mean()
  width.perc = (sapply(1:nruns, function(i) cis.perc[[i]][2]- cis.perc[[i]][1] )) %>% mean()
  width.norm.cor = (sapply(1:nruns, function(i) cis.norm.cor[[i]][2]- cis.norm.cor[[i]][1] )) %>% mean()
  width.norm.raw = (sapply(1:nruns, function(i) cis.norm.raw[[i]][2]- cis.norm.raw[[i]][1] )) %>% mean()
	
  res = matrix(ncol = 12, nrow = 1, c(bias.raw, bias.cor, mse.raw, mse.cor, cov.basic, width.basic, cov.perc, width.perc, cov.norm.raw, width.norm.raw,
   cov.norm.cor, width.norm.cor))
   
   return(res)
}

sims = c('simple', 'random effect', 'heterogeneous', 'nonlinear', 'noisy confounding', 'less noisy confounding', 'smooth exposure')

tab = crossing(as.character(1:7), c('spline3', 'spline_double', 'bart_single', 'bart_double'))

tab = tab %>% add_column(bias.raw = NaN, bias.cor = NaN, mse.raw = NaN, mse.cor =NaN, cov.basic = NaN, width.basic = NaN, cov.perc = NaN, width.perc = NaN,
cov.norm.raw =NaN, width.norm.raw = NaN, cov.norm.cor = NaN, width.norm.cor = NaN)

for(j in 1:nrow(tab)){
  sim = tab[j,1] %>% as.integer()
  tab[j,1] = sims[sim]
  method = tab[j,2]
  if(sim == 7 & grepl('double', method) ) next 
  tab[j, 3:14] = getRes(method, sim)
  
}

colnames(tab)[1] = 'sim'
colnames(tab)[2] = 'method'

write.csv(tab, 'boot_res.csv', quote=F)
saveRDS(tab, 'boot_res.rds')
