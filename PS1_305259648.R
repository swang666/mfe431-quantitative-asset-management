library(data.table)
library(ggplot2)
library(fBasics)
#question 1
df = read.csv('stock.csv', stringsAsFactors = F)
CRSP_Stocks= as.data.table(df)

PS1_Q1 = function(DT){

  DT = DT[SHRCD == 10 | SHRCD == 11,]
  DT = DT[EXCHCD == 1 | EXCHCD == 2 | EXCHCD == 3,]
  DT = DT[!is.na(PRC),]
  DT[, dlret:=as.numeric(DLRET)]
  DT[, ret:=as.numeric(RET)]
  DT[is.na(dlret), dlret:= 0]
  DT[is.na(ret), ret:= 0]
  DT[, cum_div_ret := (1 + dlret)*(1+ret) - 1]
  DT[, PRC := abs(PRC)]
  DT[, SHROUT := abs(SHROUT)]
  DT[, mktcap := PRC * SHROUT]
  DT[is.na(mktcap), mktcap:=0]
  DT[, mktcaplagged := shift(mktcap), by = 'PERMNO']
  DT[is.na(mktcaplagged), mktcaplagged := 0]
  DATE = sort(unique(DT$date))
  
  n = length(DATE)
  
  vwretd = rep(0, n-1)
  ewretd = rep(0, n-1)
  lag_mv = rep(0, n-1)
  for (i in 2:n){
    out = DT[date == DATE[i], .(date, PERMNO, mktcaplagged, cum_div_ret)]
    market_port = weighted.mean(out$cum_div_ret, out$mktcaplagged, na.rm = T)
    vwretd[i-1] = market_port
    ewretd[i-1] = mean(out$cum_div_ret)
    lag_mv[i-1] = sum(out$mktcaplagged)
  }
  
  portfolio = as.data.table(cbind(DATE[2:n], vwretd,ewretd, lag_mv/1000000))
  colnames(portfolio) = c('date', 'Stock_Vw_Ret', 'Stock_Ew_Ret', 'Stock_lag_MV')
  return(portfolio)
}



#question 2
Monthly_CRSP_Stocks = PS1_Q1(CRSP_Stocks)
FF_mkt = read.csv('ff3.csv', skip = 3, stringsAsFactors = F)
PS1_Q2 = function(Monthly_CRSP_Stocks, df2){
  df2 = df2[1:1110,]
  vwRmRf = Monthly_CRSP_Stocks[7:1116,Stock_Vw_Ret]-as.numeric(df2$RF)/100
  actualRmRf = as.numeric(df2$Mkt.RF)/100
  
  
  monthly_mean1 = mean(vwRmRf)
  monthly_mean2 = mean(actualRmRf)
  annual_mean1 = monthly_mean1 * 12
  annual_mean2 = monthly_mean2 * 12
  annual_stdev1 = sd(vwRmRf) * sqrt(12)
  annual_stdev2 = sd(actualRmRf) * sqrt(12)
  SR1 = annual_mean1/annual_stdev1
  SR2 = annual_mean2/annual_stdev2
  skew1 = skewness(vwRmRf)
  skew2 = skewness(actualRmRf)
  exkurt1 = kurtosis(vwRmRf) - 3
  exkurt2 = kurtosis(actualRmRf) - 3
  
  statstable = as.data.frame(rbind(c('annualized mean', annual_mean1, annual_mean2),
                                   c('annualized stdev', annual_stdev1, annual_stdev2),
                                   c('annualized SR', SR1, SR2),
                                   c('skewness', skew1, skew2),
                                   c('excess kurtosis', exkurt1, exkurt2)))
  colnames(statstable) = c('Rm-Rf', 'estimated', 'actual')
  return(statstable)
}
mat = PS1_Q2(Monthly_CRSP_Stocks, FF_mkt)

#question 3
PS1_Q3 = function(Monthly_CRSP_Stocks, df2){
  df2 = df2[1:1110,]
  vwRmRf = Monthly_CRSP_Stocks[7:1116,Stock_Vw_Ret]-as.numeric(df2$RF)/100
  actualRmRf = as.numeric(df2$Mkt.RF)/100
  cor = cor(vwRmRf, actualRmRf)
  result = as.data.frame(cbind(as.numeric(df2$X),vwRmRf-actualRmRf))
  #ggplot(data = result, aes(x = V1)) + geom_line(aes(y = V2))
  maxdiff = max(abs(result$V2))
  return(c(cor, maxdiff))
}

q3 = PS1_Q3(Monthly_CRSP_Stocks, FF_mkt)
