# outlier prediction,
# Joan Verdu


# libraries
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("dplyr"))install.packages("dplyr")
if(!require("tidyr"))install.packages("tidyr")
if(!require("gplots"))install.packages("gplots")
if(!require("RColorBrewer"))install.packages("RColorBrewer")

# Load data

data<-read.csv('heavytails_output.csv')
data<-data[,c('net_id','vertex_id','return','volatility')]

# Extract day
data$date<-substr(data$net_id,12,21)
data$date<-as.Date(data$date,"%Y-%m-%d")


#------------------------#
## Outlier calculation ###
#------------------------#

# Outlier classification
data$outlier<-abs(data$return/data$volatility)>1.645
# Days by ticker
n_days<-aggregate(date~vertex_id,data=data,function(x) length(unique(x)))
# Outliers by ticker
out<-aggregate(outlier~vertex_id,data=data,sum)
out$days<-n_days$date
# initialize hist mean p of outliers
out$meanp<-NA
for (i in 1:length(out$vertex_id)){
    subset<-subset(data,vertex_id==out$vertex_id[i],select=c(outlier,date))
    out$meanp[i]<-mean(subset$outlier,na.rm=T)
}

# spread outlier data.frame to have ticker in separate columns
data2<-tbl_df(data)
data2<-select(data2,vertex_id,date,outlier)
data2<-spread(data2,vertex_id,outlier)
nobs<-nrow(data2)
nticker<-ncol(data2)-1

#---------------------------------
### study volatility clustering at different time window vs mean probability
# count mean p in a sourrounding window of X days
#---------------------------------

data_base<-gather(data2,vertex_id,outlier,-date,na.rm=T)

### 1 day ahead, based on today's outlier info ###########

    data_1day<-data2
    data_1day[1:1030,2:(nticker+1)]<-data2[-1,2:(nticker+1)]
    data_1day[1031,2:(nticker+1)]<-rep(NA,nticker)
    data_1day<-gather(data_1day,vertex_id,mean_1d,-date,na.rm=T)
    
    data3<-left_join(data_base,data_1day,c('date','vertex_id'))
    # generate mean expected probability of 1day ahead outlier being today an outlier or not, 
    # by ticker
    out_1day<-data3 %>% group_by(vertex_id,outlier) %>% summarize(mean=mean(mean_1d,
                                                                        na.rm=T))
    # Adding a priori probability per ticker
    out_1day<-left_join(out_1day,out,'vertex_id')
    # Adding increase of probability
    out_1day$ratio<-(out_1day$mean/out_1day$meanp)-1
    # summary by today's outlier
    tapply(out_1day$ratio, out_1day$outlier.x, summary)
    # Increase of 20% on average!
    # Barplot
    ggplot(data=out_1day, aes(x=vertex_id, y=ratio, fill=outlier.x)) +
        geom_bar(stat="identity", position=position_dodge())+
        ggtitle('1 Day ahead variation of outlier probability \n 
                depending on today\'s outlying condition')+
        ylab('Probability increase of outlier tomorrow')+
        xlab('Ticker')+
        scale_fill_discrete(name="Outlier today")+
        coord_flip()


## Multiple days ahead, based on today's info #############

    fut_days<-seq(1,10,1)
    out_multiple<-data.frame(vertex_id=out_1day$vertex_id,
                             outlier_today=out_1day$outlier.x)
    quantiles<-matrix(NA,length(fut_days),5)
    summaries<-matrix(NA,length(fut_days),6)

    # Function to calculate increase in mean daily probability ratio of
    # having an outlier for n_days_ahead, knowing today's outlier condition
    out_increase<-function(data,data_base,outlier_base,n_days_ahead){
        ndays<-nrow(data)
        ntickers<-ncol(data)-1
        data_nahead<-data
        for (i in 1:(ndays-n_days_ahead)){ # n_days_ahead mean
            data_nahead[i,2:(ntickers+1)]<-colMeans(
                data[(i+1):(i+n_days_ahead), 2:(ntickers+1)],na.rm=T)
        }
    
        data_nahead[(ndays-n_days_ahead+1):ndays,2:(nticker+1)]<-matrix(NA,n_days_ahead,
                                                                      nticker)
        data_nahead<-gather(data_nahead,vertex_id,mean_ndays,-date,na.rm=T)
        
        data3<-left_join(data_base,data_nahead,c('date','vertex_id'))
        # generate mean expected probability of 1day ahead outlier being today an outlier or not, 
        # by ticker
        out_nday<-data3 %>% group_by(vertex_id,outlier) %>% summarize(mean=mean(
                                    mean_ndays, na.rm=T))
        # Adding a priori probability per ticker
        out_nday<-left_join(out_nday,outlier_base,'vertex_id')
        # Adding increase of probability
        out_nday$ratio<-as.vector(out_nday$mean/out_nday$meanp)-1
        return(out_nday$ratio)
    }

    for (i in fut_days){
        title<-paste0('ahead_',i)
        out_multiple[[title]]<-out_increase(data2,data_base,out,i)
        # summary if today's outlier is TRUE
        probs=c(0.05,0.25,0.5,0.75,0.9)
        this_summ<-tapply(out_multiple[[title]], as.numeric(out_multiple$outlier_today), summary)
        this_quan<-tapply(out_multiple[[title]], as.numeric(out_multiple$outlier_today), quantile,probs)
        summaries[i,]<-as.vector(this_summ[['1']])
        quantiles[i,]<-as.vector(this_quan[['1']])
        # Barplot
        plottitle<-paste0(i,' Days ahead variation of outlier probability \n 
                  depending on today\'s outlying condition')
        print(ggplot(data=out_multiple,aes(x=vertex_id, y=out_multiple[[title]], 
                         fill=outlier_today)) +
            geom_bar(stat="identity", position=position_dodge())+
            ggtitle(plottitle)+
            ylab('Probability increase of outlier tomorrow')+
            xlab('Ticker')+
            scale_y_continuous(limits = c(-0.75, 1.5))+
            scale_fill_discrete(name="Outlier today")+
            coord_flip())
        
    }

# Summary
    # Print summary of quantiles
    summaries<-as.data.frame(summaries)
    names(summaries)<-c('Min','Q1','Median','Mean','Q3','Max')
    summaries$days_ahead<-fut_days
    summaries
    quantiles<-as.data.frame(quantiles)
    names(quantiles)<-paste0('Q',probs)
    quantiles$days_ahead<-fut_days
    quantiles
    # Plot summary
    ggplot(summaries, aes(days_ahead)) + 
        geom_line(aes(y=Median), colour="blue") + 
        geom_ribbon(aes(ymin=Q1, ymax=Q3), fill='orange',alpha=0.2)+
        xlab('Days ahead')+
        ylab('Median, 1st and 3rd quantiles')+
        ggtitle('Increase in 1-day probability of outlier')
    # Plot confidence intervals
    ggplot(quantiles, aes(days_ahead)) + 
        geom_line(aes(y=Q0.5), colour="blue") + 
        geom_ribbon(aes(ymin=Q0.05, ymax=Q0.9), fill='orange',alpha=0.2)+
        xlab('Days ahead')+
        ylab('Median, 10% confidence interval')+
        ggtitle('Increase in 1-day probability of outlier')

    # Heat map
    heat<-as.matrix(out_multiple[,-c(1,2)])
    rownames(heat)<-out_multiple$vertex_id
    my_palette <- colorRampPalette(c("green", "white", "red"))(n = 20)
    heatmap.2(heat,Colv=NA,col=my_palette,cexRow=0.5)



