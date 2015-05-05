## Binary clustering functions
## Joan Verdu

#------------#
# Libraries  #
#------------#
if(!require("assertthat"))install.packages("assertthat")
if(!require("dplyr"))install.packages("dplyr")
if(!require("tidyr"))install.packages("tidyr")
if(!require("compiler"))install.packages("compiler")

#------------#
# Functions  #
#------------#

#' outlier
#' 
#' Function to calculate outliers from a series of returns and volatilities
#' @param return Numeric vector with values (returns) Mean return zero is assumed
#' @param volatility Numeric vector with values (volatilities), same time series as returns
#' @param stat Numeric Statistic parameter corresponding to the confidence level of a N(0,1) distribution
#' @param type Factor Type of outlier to calculate ('positive','negative','general')
#' @return Integer vector of outlier  condition (0 or 1)
#' @export
#' @import
#' #' 

outlier<-function(return,volatility, stat=1.645,type='general'){
    assert_that(type %in% c('general','negative','positive'))
    if (type=='positive') outlier<-(return/volatility)>stat
    else if (type=='negative') outlier<- (return/volatility)< -stat
    else outlier<- abs(return/volatility)> stat
    return (outlier)
}

#' cluster_outlier
#' 
#' Function to sum number of outliers on a given past days time window
#' @param outlier Integer vector of time series outliers (0 or 1)
#' @param days Numeric number of days of the cluster to sum up number of outliers
#' @param type Character 'Past' to consider past days (including today) or 'ahead' to consider future days
#' @return Integer vector of number of outliers in the time window
#' @export
#' @import
#' 

cluster_outlier<-function(outlier,days,type='past'){
    assert_that(type %in% c('past','ahead'))
    cluster<-rep(NA,length(outlier))
    if (type=='past'){
        for (i in days:length(outlier)){
            cluster[i]<-sum(outlier[(i-days+1):i])
        }
    } else if (type=='ahead'){
        for (i in 1:(length(outlier)-days)){
            cluster[i]<-sum(outlier[(i+1):(i+days)])
        }
    }
    return(cluster)
}

#' calc_signific
#' 
#' Function to calculate if differences are significant of ahead clustering probability
#' when considering a clustering a past outlier cluster
#' @param data_past Data Frame, each column is a ticker, each row the cu with fields date, vertex_id (ticker), and outlier (0 or 1)
#' @param data_ahead Numeric vector including number of days to check, and number of outliers in this days
#' @param level Numeric Significance level, both sides (by default 0.95, 95% both-sided)
#' @return A numeric vector with ticker value being 1 if significant probability change due to outlier cluster
#' @export
#' @import compiler
#' @examples
#' 
calc_signific<-function(data_past,data_ahead,level=0.95){
    # initial checks
    assert_that(dim(data_past)[1]>=dim(data_ahead)[1])
    assert_that(dim(data_past)[2]>=dim(data_ahead)[2])
    
    one_check<-function(one_ticker_past,one_ticker_ahead,level){
        data<-data.frame(past=as.vector(one_ticker_past),ahead=as.vector(one_ticker_ahead))
        names(data)<-c('past','ahead')
        data<-data[complete.cases(data),]
        table<-table(data)
        if (nrow(table)==2 & ncol(table)==2){
            n_clust<-sum(table[2,]) # observed past cluster outliers
            n_noclust<-sum(table[1,]) #observed past no-clusters
            p_clust<-table[2,2]/n_clust # probability of cluster outlier having observed a past outlier cluster
            p_noclust<-table[1,2]/n_noclust # probability of cluster outlier not having observed a past outlier cluster
            avg_p<-(p_clust*n_clust+p_noclust*n_noclust)/(n_clust+n_noclust) # weight avg
            # z statistic
            z<-(p_clust-p_noclust)/sqrt(avg_p*(1-avg_p)*((1/n_clust)+(1/n_noclust)))
            significant<-abs(z)>abs(qnorm((1-level)/2))
        } else significant<-FALSE # there are missing cases, can't compare
        
        return(significant)
    }
    
    one_check<-cmpfun(one_check) # compile to boost performance
    
    ntickers<-dim(data_past)[2]
    out_signific<-rep(NA,ntickers)
    for (i in 1:ntickers){
        out_signific[i]<-one_check(data_past[,i],data_ahead[,i],level)
    }
    assert_that(length(out_signific)==ntickers)
    return(out_signific)
}


#' out_increase
#' 
#' Function to calculate increase in mean daily probability ratio of
#' having an outlier for n_days_ahead, knowing today's outlier condition
#' @param data Data Frame with fields date, vertex_id (ticker), and outlier (0 or 1)
#' @param ahead Numeric vector including number of days to check, and number of outliers in this days
#' @param past_cluster Numeric vector including number of past days to check, and number of outliers 
#' @return A data frame including fields: 
#' vertex_id ticker code
#' out_past past outlier cluster condition (TRUE or FALSE)
#' ratio (increase probability ratio of outliers), 
#' signific (is significant the probability of future outlier clusters 
#' depending on past outlier cluster observed or not)
#' @export
#' @import 
#' @examples
#' 
# Function to calculate increase in mean daily probability ratio of
# having an outlier for n_days_ahead, knowing today's outlier condition
out_increase<-function(data,ahead,past_cluster){
    # initial checks
    assert_that(ahead[1]>=ahead[2])
    assert_that(past_cluster[1]>=past_cluster[2])
    
    data<-tbl_df(data)
    
    # generate mean expected probability of 1day ahead out-lier by ticker
    out_p<-data %>% group_by(vertex_id) %>% summarize(mean_p=mean(
        outlier, na.rm=T))
    # generate mean expected probability of number of outliers equal of more than the threshold
    for (i in 1:dim(out_p)[1]){ # do per ticker
        out_p$exp_cluster_p[i]<-sum(dbinom(ahead[2]:ahead[1],ahead[1],out_p$mean_p[i]))    
    }
    
    
    # spread outlier data.frame to have ticker in separate columns
    
    data2<-spread(data,vertex_id,outlier)
    # count number of days and tickers
    ndays<-nrow(data2)
    ntickers<-ncol(data2)-1
    
    # Generate ahead data
    data_ahead<-select(data2,-date)
    # ahead number of outliers in defined time window
    data_ahead<-apply(as.matrix(data_ahead),2,cluster_outlier,ahead[1],'ahead')
    # ahead clustering outlier condition based on defined threshold (number of outliers)
    data_ahead<-as.data.frame(data_ahead>=ahead[2])
    
    # Generate past outlier clustering
    data_past<-select(data2,-date)
    data_past<-apply(as.matrix(data_past),2,cluster_outlier,past_cluster[1],'past')
    data_past<-as.data.frame(data_past>=past_cluster[2])
    
    # Calculate significant differences as binomial distribution being past cluster
    # true or not
    level<-0.95 #significance level, both sides
    significance<-calc_signific(data_past,data_ahead,level)
    
    # format data
    data_ahead$date<-data2$date
    data_past$date<-data2$date
    data_ahead<-tbl_df(data_ahead)
    data_past<-tbl_df(data_past)
        
    
    data_ahead<-gather(data_ahead,vertex_id,out_ahead,-date,na.rm=T)
    data_past<-gather(data_past,vertex_id,out_past,-date,na.rm=T)
    
    # Generate mean probability increase with respect to expected clustering outlier probability
    # Join all data
    data<-left_join(data,data_ahead,c('date','vertex_id'))
    data<-left_join(data,data_past,c('date','vertex_id'))
    data<-data[complete.cases(data),] # remove head and tail where couldn't compute cluster
    
    # generate mean expected probability of outlier cluster ahead having observed today
    # a past cluster outlier or not, by ticker
    out_cluster<-data %>% group_by(vertex_id,out_past) %>% summarize(obs_cluster_p=mean(
        out_ahead, na.rm=T))
    
    # Create a common frame with all possibilities
    output<-data.frame(vertex_id=rep(out_p$vertex_id,each=2),out_past=rep(c(FALSE,TRUE),times=ntickers))
    
    # Compare to a priori probability of cluster outlier per ticker
    output<-left_join(output,out_p,'vertex_id')
    output<-left_join(output,out_cluster,c('vertex_id','out_past'))
    # Adding increase of probability
    output$ratio<-NA
    output$ratio<-as.vector(output$obs_cluster_p/output$exp_cluster_p)-1
    # Adding significant difference between past outlier cluster true or not 
    # in terms of future outlier cluster probability
    output$signific<-NA
    output$signific<-rep(significance,each=2)
    
    # Select increase ratios of future outliers clustering for past clustering cases
    #result<- out_cluster %>% group_by(vertex_id,out_past) %>% filter(out_past==TRUE) %>% select(ratio)  
    
    return(output)
}



