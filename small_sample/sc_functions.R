sc_cont <- function(dat, ind=NULL, method, shift){
  switch(method, 
         "OLS" = sc_ols_cont(Y, X, s, shift),
         "spline" = sc_spline_cont(dat, shift),
         "spline_interaction" = sc_spline_interaction_cont(dat, ind, shift),
         "svc_mle" = sc_svc_mle_cont(dat, shift),
         "spatial_plus" = sc_spatial_plus_cont(dat, ind, shift),
         "gsem" = sc_gsem_cont(dat, ind, shift),
         "rf_single" = sc_rf_single_cont(dat, ind, shift),
         "gp" = sc_gp_cont(Y, X, s, shift),
	 "gp3" = sc_gp3_cont(dat, shift),
	 "spline3" = sc_spline3_cont(dat,shift),
	 "bart_single" = sc_bart_single_cont(dat, ind, shift),
	 "bart_double" = sc_bart_double_cont(dat, ind, shift),
	 "gp3_double" = sc_gp3_double_cont(dat, shift),
	 "spline_double" = sc_spline_double_cont(dat, shift)
         )
}

sc_ols_cont <- function(Y, X, s, shift){
  coef(lm(Y ~ X))[2]*shift
}

sc_spline_cont <- function(dat, shift){
  suppressMessages(require(mgcv))
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  mod = mgcv::gam(Y ~ X + s(lat,lon,k=200))
  est = mod$coefficients[2]*shift
  se = diag(vcov(mod))[2]^.5*shift
  return(list(est = est, se = se))
}

sc_spline_interaction_cont <- function(dat, ind, shift){
  suppressMessages(require(mgcv))
  dat = dat[ind,]
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  mod = mgcv::gam(Y ~ s(lat, lon, by= X, k=200) + s(lat, lon, k=200))
  df1 = data.frame(lat = lat, lon = lon, X = X)
  df2 = data.frame(lat = lat, lon = lon, X = X+shift)
  mean(predict(mod, newdata = df2) - predict(mod, newdata = df1))
}

sc_spline3_cont = function(dat, shift){

  require(mgcv)
  
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]

  mod = mgcv::gam(Y ~ s(lat, lon, X, k=min(1000, length(X)/2)  ))
  df1 = data.frame(lat = lat, lon = lon, X = X)
  df2 = data.frame(lat = lat, lon = lon, X = X+shift)
  mean(predict(mod, newdata = df2) - predict(mod, newdata = df1))


}




sc_svc_mle_cont <- function(dat, shift){
  suppressMessages(require(varycoef))

  X = dat[,1]
  Y = dat[,2]
  s = dat[,3:4]

  mod = varycoef::SVC_mle(Y, cbind(1, X), as.matrix(s))
  df1 = data.frame(s=s, X = X)
  df2 = data.frame(s=s, X = X+shift)
  mean(predict(mod, newlocs = s, newX = cbind(1, X+shift), newW = cbind(1, X+shift))$y.pred -
  predict(mod, newlocs = s, newX = cbind(1, X), newW = cbind(1, X))$y.pred)
  
}

sc_spatial_plus_cont <- function(dat, ind, shift){
  suppressMessages(require(mgcv))
  dat = dat[ind,]
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]

  X.res = X - mgcv::gam(X~s(lat,lon,k=200))$fitted.values 
  coef(mgcv::gam(Y ~ X.res + s(lat, lon,k=200)))[2]*shift
}

sc_gsem_cont <- function(dat, ind, shift){
  suppressMessages(require(mgcv))
  dat = dat[ind,]
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]

  X.res = X - mgcv::gam(X~s(lat,lon,k=200))$fitted.values 
  Y.res = Y - mgcv::gam(Y~s(lat,lon,k=200))$fitted.values 
  coef(lm(Y.res ~ X.res))[2]*shift
}

sc_rf_single_cont <- function(dat, ind, shift){
  suppressMessages(require(randomForest))
  dat = dat[ind,]
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]

  mod = randomForest(Y ~ X+lat+lon)
  
  df1 = data.frame(lat = lat, lon = lon, X = X)
  df2 = data.frame(lat = lat, lon = lon, X = X+shift)
  mean(predict(mod, newdata = df2) - predict(mod, newdata = df1))
}

sc_gp_cont <- function(dat, ind, shift){
  suppressMessages(require(BRISC))
  dat=dat[,ind]
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  mod = BRISC_estimation(coords = s, x=cbind(1, X), y = Y)
   return(mod$Beta[2])
}

sc_gp3_cont <- function(dat, shift){
  suppressMessages(require(RobustGaSP))
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  des = data.frame(lat = lat, lon = lon, X = X)
  mod = ppgasp(design = des, response = matrix(Y), nugget.est=T)
  df1 = data.frame(lat = lat, lon = lon, X = X)
  df2 = data.frame(lat = lat, lon = lon, X = X+shift)
  mean(predict(mod, df2)$mean - predict(mod, df1)$mean)
}

sc_bart_single_cont <- function(dat, ind, shift){
  suppressMessages(require(dbarts))
  dat <- dat[ind, ]

  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  
  df1 = data.frame(lat = lat, lon = lon, X = X)
  df2 = data.frame(lat = lat, lon = lon, X = X+shift)
  mod = bart(data.frame(lat = lat, lon = lon, X), y.train = Y, x.test = rbind(df2, df1))

  mean(mod$yhat.test.mean[1:n]) - mean(mod$yhat.test.mean[-(1:n)])
}



sc_spline_double_cont = function(dat, shift){
  
  require(mgcv)
  
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  
  mu = mean(Y)
  
  mumod = gam(Y ~ s(X, lat, lon, k=min(1000, length(X)/2)))
  mu.fitted = mumod$fitted.values
  mu.fitted.shift = predict(mumod, data.frame(X=X+shift, lat = lat, lon = lon))
  
  mu_qr = mu.fitted.shift
  
  pimod = gam(X ~ s(lat, lon, k=200))
  resid = X - pimod$fitted.values
  d_mod = density(resid)
  
  dens_est = function(x, lat, lon) approx(d_mod$x, d_mod$y, x - predict(pimod, data.frame(lat = lat, lon = lon)))$y
 
  
  lambda = function(x, lat, lon) dens_est(x-shift, lat, lon)/dens_est(x, lat, lon)
  mu_qipw = mean(lambda(X, lat, lon)*Y)/sum(lambda(X, lat, lon))
  
  comp_model = lm(Y ~ lambda(X,lat, lon) + offset(mu.fitted) + 0)
  gamma_hat = coef(comp_model)
  
  mean(mu.fitted.shift + gamma_hat*lambda(X+shift, lat, lon)) - mu
  

}

sc_gp3_double_cont = function(dat, shift){
  
  suppressMessages(require(RobustGaSP))
  suppressMessages(require(mgcv))
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  df1 = data.frame(lat = lat, lon = lon, X = X)
  df2 = data.frame(lat = lat, lon = lon, X = X+shift)
  mumod = ppgasp(design = df1, response = matrix(Y), nugget.est=T)
  
  mu.fitted =  predict(mumod, df1)$mean
  mu.fitted.shift = predict(mumod,df2)$mean
  
  mu_qr = mean(mu.fitted.shift)
  
  mu = mean(Y)  
  
   pimod = gam(X ~ s(lat, lon, k=200))
  resid = X - pimod$fitted.values
  d_mod = density(resid)
  
  dens_est = function(x, lat, lon) approx(d_mod$x, d_mod$y, x - predict(pimod, data.frame(lat = lat, lon = lon)))$y
 
  
  lambda = function(x, lat, lon) dens_est(x-shift, lat, lon)/dens_est(x, lat, lon)
  mu_qipw = sum(lambda(X, lat, lon)*Y)/sum(lambda(X, lat, lon))
  
  comp_model = lm(Y ~ lambda(X,lat, lon) + offset(mu.fitted) + 0)
  gamma_hat = coef(comp_model)
  
  mean(mu.fitted.shift + gamma_hat*lambda(X+shift, lat, lon)) - mu
  

}

sc_bart_double_cont = function(dat, ind, shift){
	  
  suppressMessages(require(dbarts))
  suppressMessages(require(mgcv))
  dat <- dat[ind, ]
  
  X = dat[,1]
  Y = dat[,2]
  lat = dat[,3]
  lon = dat[,4]
  
   
  df1 = data.frame(lat = lat, lon = lon, X = X)
  df2 = data.frame(lat = lat, lon = lon, X = X+shift)
  mumod = bart(data.frame(lat = lat, lon = lon, X), y.train = Y, x.test = rbind(df2, df1))
  
  mu.fitted.shift = mumod$yhat.test.mean[1:n]
  mu.fitted = mumod$yhat.test.mean[-(1:n)]
  mu_qr = mean(mu.fitted.shift)
  
  mu = mean(Y)
  
  pimod = gam(X ~ s(lat, lon, k=200))
  resid = X - pimod$fitted.values
  d_mod = density(resid)
  
  dens_est = function(x, lat, lon) approx(d_mod$x, d_mod$y, x - predict(pimod, data.frame(lat = lat, lon = lon)))$y
 
  
  lambda = function(x, lat, lon) dens_est(x-shift, lat, lon)/dens_est(x, lat, lon)
  mu_qipw = sum(lambda(X, lat, lon)*Y)/sum(lambda(X, lat, lon))
  
  comp_model = lm(Y ~ lambda(X,lat, lon) + offset(mu.fitted) + 0)
  gamma_hat = coef(comp_model)
  
  mean(mu.fitted.shift + gamma_hat*lambda(X+shift, lat, lon)) - mu

}


