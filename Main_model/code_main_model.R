library(dlnm);library(splines);library(mvmeta);

# convert to character and count the number of immediate-regions
city<- as.character(unique(data$subregion))
n<-length(city)

# format the dataset as list for iteration
data_list<-group_split(data,data$subregion,.keep = F)

## matrix and list for saveing cumulative associations
y_c<-matrix(NA,length(city),1)
S_c<-vector("list",length(city))

## matrix and list for saveing lag pattern
df_lag<-4## df for the lag dimension
y_1<-matrix(NA,length(city),df_lag,dimnames=list(city,paste("b",seq(1:df_lag),sep="")))
S_1<-vector("list",length(city))

for (i in 1:length(city)) {
  print(paste(i, city[i] ,"Start"),sep = "  ")
  
  data_city<-data_list[[i]]#
  print(nrow(data_city))
  
  data_city <- data_city[order(data_city$date),]
  data_city$time <- 1:nrow(data_city)
  
  data_city <- data_city[order(data_city$date),]
  data_city$time <- 1:nrow(data_city)
  
  data_city$rh07 <- runMean(data_city$rh,0:7)
  data_city$temp21 <- runMean(data_city$tmean,0:21)
  
  n_year <- 7*length(unique(data_city$year))
  
  ## crossbasis for PM2.5
  #cb<-crossbasis(data_city$ext_PM, lag = 14, argvar = list("lin"), arglag = list(fun = "integer"))
  ## "interger" is equal to use single lags
  ## For lag 
  cb<-crossbasis(data_city$ext_PM, lag = 14, argvar = list("lin"), arglag = list(fun = "ns",knots=logknots(14,df=df_lag,int=T)))
  
  mfirst <-glm(all.corr~cb+ns(time,n_year)+dow+holidays+ns(temp21,3)+ns(rh07,3),data=data_city,family = quasipoisson)
  
  # Save cumulative associations
  crall <- crossreduce(cb, model=mfirst,type = "overall",cen = 0)
  y_c[i,] <- coef(crall)
  S_c[[i]] <- vcov(crall)
  
  ## Save lag pattern(10 ug/m3 vesus 0ug/m3)
  c_lag <- crossreduce(cb, model=mfirst,type="var", value= 10,cen = 0)
  y_1[i,] <- coef(c_lag)
  S_1[[i]] <- vcov(c_lag)
  
}
##save the coef and vcov
coef_all<-list(y_c,S_c,y_1,S_1)

##meta regression
mv_1<-mvmeta(y_1~1,S_1,method = "reml")

##build crossbasiss for prediction
lag_seq <- do.call("onebasis",c(list(x=0:140/10),attr(cb,"arglag")))

##prediction using final models
Cp_cumulative<-crosspred(var_seq,coef = coef(mv_c),vcov = vcov(mv_c),model.link = "log",by=1,from =0,to=100,cen=0)
Cp_lag_all14<-crosspred(lag_seq,coef = coef(mv_1),vcov = vcov(mv_1),model.link = "log",at=0:140/10,cen = 0)

##plot result
plot(Cp_lag_all14,ylab="RR for all-cause",xlab="lag (days)")+title("Max lag = 14")
plot(Cp_cumulative,ylab="RR",xlab="Wildfire PM2.5")
