##make figures

library(tidyverse)
library(xtable)

names = c('Linear confounding', 'Simple effect', 'Random heterogeneous effect', 'Structural heterogeneous effect', 'Nonlinear effect', 'Noisy confounding', 'Less noisy confounding', 'Smooth exposure')
names.abbrev = c('linear', 'simple', 'rand.het', 'struct.het', 'nonlinear', 'noisy', 'less.noisy', 'smooth')

df = data.frame(sim=numeric(), size=as.character(), method=as.character(), bias=numeric(), sd=numeric(), mse=numeric(), coverage=numeric(), err = numeric())

getRes = function(sim, size, method){
  
  if(size == 'large') jmax = 250 else jmax = 500
  
  tru = ifelse(sim == 4, 2.431, 1) ## tru effects; 2.431 calculated by simulation
  if(method == 'gp') {
    dir = paste0(size, '_sample_gp')
    out = lapply(1:jmax, function(j) readRDS(file.path(dir, method, paste0('sim', sim, '_', j, '.rds'))))
    bias = -tru + sapply(out, function(x) x$est) %>% mean()
    sd = sapply(out, function(x) x$est) %>% sd()
    mse = mean( (tru - sapply(out, function(x) x$est))^2 )
    coverage = sapply(out, function(x) between(tru, x$est-1.96*x$se, x$est+1.96*x$se)) %>% mean()
    return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage, err = sapply(out, function(x) x$est)-tru))
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
      return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage, err = sapply(out, function(x) x$est)-tru))
    }
    else{
      out = sapply(1:jmax, function(j) readRDS(file.path(dir, method, paste0('sim', sim, '_', j, '.rds'))))
      bias = -tru + out %>% mean()
      sd = out %>% sd()
      mse = mean( (tru - out)^2 )
      coverage = NA
      return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage, err = out-tru))
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
    return(data.frame(sim=sim,size=size,method=method,bias=bias,sd=sd,mse=mse,coverage=coverage, err = sapply(out, function(x) x$est)-tru))
  }
  
  
  
}

for(sim in 0:7){
  
  for(size in c('large')){
    
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
'gsem', 'spatial_plus', 'spline_interaction', 'spline3', 'rf', 'bart_single', 'spline_double', 'bart_double')
method_names_revised = c('RSR', 'spline', 'gp', 'gSEM', 'spatial+', 'spline_interaction', 'spline3', 'rf', 'BART',
 'DML_spline', 'DML_BART')

dir = 'journal_figs'
if(!dir.exists(dir)) dir.create(dir)

for(i in 0:7){

	tab = filter(df, sim == i) %>% mutate(method = factor(method, levels = method_order)) %>%
   mutate(n = as.factor(as.integer(10^(3+(size=='large'))))) %>%
   arrange(n, method) %>%
   select(n, bias, method, err) 

    p = ggplot(tab, aes(x=method,y = err, fill=method)) + geom_boxplot() +
     theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
     ylab('estimation error')  +theme(text = element_text(size = 16))+
     ggtitle(names[i+1]) +
	scale_x_discrete(labels = method_names_revised) +
	theme(legend.position = "none")
    if(i==4) p = p+scale_y_reverse()
    ggsave(file.path(dir, paste0('sim_', names.abbrev[i+1], '.png')), p)

}
