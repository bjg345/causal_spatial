#summarize simulation results

library(tidyverse)
library(xtable)

names = c('Linear confounding', 'Simple effect', 'Random heterogeneous effect', 'Structural heterogeneous effect', 'Nonlinear effect', 'Noisy confounding', 'Less noisy confounding', 'Smooth exposure')
names.abbrev = c('linear', 'simple', 'rand.het', 'struct.het', 'nonlinear', 'noisy', 'less.noisy', 'smooth')

df = data.frame(sim=numeric(), size=as.character(), method=as.character(), bias=numeric(), sd=numeric(), mse=numeric(), coverage=numeric())

getRes = function(sim, size, method){
  
  if(size == 'large') jmax = 250 else jmax = 500
  
  tru = ifelse(sim == 4, 2.431, 1) ##true effects; 2.431 calculated by simulation
  if(method == 'gp') {
    dir = paste0(size, '_sample_gp')
    out = lapply(1:jmax, function(j) readRDS(file.path(dir, method, paste0('sim', sim, '_', j, '.rds'))))
    bias = -tru + sapply(out, function(x) x$est) %>% mean()
    sd = sapply(out, function(x) x$est) %>% sd()
    mse = mean( (tru - sapply(out, function(x) x$est))^2 )
    coverage = sapply(out, function(x) between(tru, x$est-1.96*x$se, x$est+1.96*x$se)) %>% mean()
    return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage))
  }
  
  
  else if (method %in% c('spline', 'svc_mle', 'gp3', 'gp3_double', 'spline3', 'spline_double')) {
    dir = paste0(size, '_sample')
    if((grepl('double', method)) & sim==7) return(NA)
    if(!grepl('spline', method) & size == 'large') return(NA)
    if(method == 'spline'){
      out = lapply(1:jmax, function(j) readRDS(file.path(dir, method, paste0('sim', sim, '_', j, '.rds'))))
      bias = -tru + sapply(out, function(x) x$est) %>% mean()
      sd = sapply(out, function(x) x$est) %>% sd()
      mse = mean( (tru - sapply(out, function(x) x$est))^2 )
      coverage = sapply(out, function(x) between(tru, x$est-1.96*x$se, x$est+1.96*x$se)) %>% mean()
      return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage))
    }
    else{
      out = sapply(1:jmax, function(j) readRDS(file.path(dir, method, paste0('sim', sim, '_', j, '.rds'))))
      bias = -tru + out %>% mean()
      sd = out %>% sd()
      mse = mean( (tru - out)^2 )
      coverage = NA
      return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage))
      }
  }
  
  
  else{ 
    dir = file.path(paste0(size, '_sample_parallel'))
    if((grepl('double', method)) & sim==7) return(NA)
    out = lapply(1:jmax, function(j) readRDS(file.path(dir, method, paste0('sim', sim, '_', j, '.rds'))))
    bias = -tru + sapply(out, function(x) x$est) %>% mean()
    sd = sapply(out, function(x) x$est) %>% sd()
    mse = mean( (tru - sapply(out, function(x) x$est))^2 )
    coverage = sapply(out, function(x) between(tru, x$est-1.96*x$se, x$est+1.96*x$se)) %>% mean()
    return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage))
  }
  
  
  
}

for(sim in 0:7){
  
  for(size in c('small', 'large')){
    
    for(method in c('gp', 'gp3', 'gp3_double', 'svc_mle', 'spline',
 'bart_double', 'bart_single', 'gsem', 'RSR','rf', 'spatial_plus',
                    'spline3',
'spline_double','spline_interaction')){
      
      res = getRes(sim, size, method)
      if(!identical(res, NA)) df = rbind(df, res)
    }
    
    
  }
  
  
}


method_order = c('RSR', 'spline', 'gp',
'gsem', 'spatial_plus', 'svc_mle', 'spline_interaction', 'gp3', 'spline3', 'rf', 'bart_single', 'gp3_double', 'spline_double', 'bart_double')
method_names_revised = c('RSR', 'spline', 'gp', 'gSEM', 'spatial+', 'svc_mle', 'spline_interaction', 'gp3', 'spline3', 'rf', 'BART',
'DML_gp3', 'DML_spline', 'DML_BART')

dir = 'tables'
if(!dir.exists(dir)) dir.create(dir)

for(i in 0:7){

	tab = filter(df, sim == i) %>% mutate(method = factor(method, levels = method_order)) %>%
   mutate(n = as.integer(10^(3+(size=='large')))) %>%
   arrange(n, method) %>%
   select(n, method, bias, sd, mse, coverage) 

    for(j in 1:nrow(tab)){
	if(is.na(tab$coverage[j])) next
	tab$coverage[j] = (tab$coverage[j] %>% as.numeric() *100) %>% round() %>% format() %>% paste0('%')
	}
   
   out = xtable(tab, digits=-3, math.style.exponents = T, display = c('s','s','s','g','g','g','s'))
	  
     print(out, file = file.path(dir, paste0('sim', names.abbrev[i+1], '_tab.txt')), include.rownames=F, math.style.exponents = T)

}

dir = 'figs'
if(!dir.exists(dir)) dir.create(dir)

for(i in 0:7){

	tab = filter(df, sim == i) %>% mutate(method = factor(method, levels = method_order)) %>%
   mutate(n = as.factor(as.integer(10^(3+(size=='large'))))) %>%
   arrange(n, method) %>%
   select(n, bias, method) 

    p = ggplot(tab, aes(method, abs(bias))) + geom_bar(aes(fill=n), position=position_dodge(preserve = "single"), stat = 'identity') +
     theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
     ylab('absolute bias') +
     ggtitle(names[i+1]) +
	scale_x_discrete(labels = method_names_revised)
    
    ggsave(file.path(dir, paste0('sim_', names.abbrev[i+1], '.png')), p)

}
