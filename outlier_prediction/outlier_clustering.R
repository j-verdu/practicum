### Outlier clustering
## Joan Verdu

#------------#
# libraries
#------------#
rm(list=ls())
if(!require("gplots"))install.packages("gplots")
if(!require("RColorBrewer"))install.packages("RColorBrewer")

if(!require("doSNOW"))install.packages("doSNOW")
if(!require("dplyr"))install.packages("dplyr")
if(!require("compiler"))install.packages("compiler")

source("../lib/binary_clustering.R")

#------------#
# Algorithm
#------------#


# Load data
data<-read.csv('../data/heavytails_output.csv')
data<-data[,c('net_id','vertex_id','return','volatility')]

# Extract day
data$date<-substr(data$net_id,12,21)
data$date<-as.Date(data$date,"%Y-%m-%d")

# Calculate outlier
outlier_type<-'general' #Possible valures: general, positive, negative
data$outlier<-outlier(data$return,data$volatility,stat=1.645,type=outlier_type)

# Remove unused fields
data<-subset(data,select=-c(net_id,return,volatility))

# calculate outlier increase by different ahead and past cluster conditions
#initialize results
out<-tbl_df(data.frame(vertex_id=rep(unique(data$vertex_id),each=2),
                out_past=rep(c(TRUE,FALSE),length(unique(data$vertex_id)))))

# Sample example
ahead<-c(1,1) # 20 days ahead, up to 3 outliers define outlier cluster
past_cluster<-c(1,1) # 20 past days (including today), up to 3 outliers define past cluster
title<-paste0('fut',ahead[1],'_',ahead[2],'past',past_cluster[1],'_',past_cluster[2])
calculate<-tbl_df(out_increase(data,ahead,past_cluster))
calculate<-calculate %>% select(vertex_id,out_past,ratio,signific) %>% 
        rename_(.dots=setNames(list('ratio'), title))
out<-left_join(out,calculate,c('vertex_id','out_past'))

# Parellel computation of all cases
# Prepare cases
# past window
n_outliers<-1:5
past_days_window<-c(5,20,100)
min_outliers<- rep(n_outliers,length(past_days_window))
past_days<- rep(past_days_window,each=length(n_outliers))
past_cluster<-data.frame(days=past_days,min_outliers=min_outliers)
# future window
n_fut_outl<-1 #consider only one outlier for future days
fut_days_window<- c(1,5,20) 
min_fut_outliers<- rep(n_fut_outl,length(fut_days_window))
fut_days<- rep(fut_days_window,each=length(n_fut_outl))
ahead<-data.frame(days=fut_days,min_outliers=min_fut_outliers)

cases_p<-dim(past_cluster)[1]
cases_f<-dim(ahead)[1]
# initialize parallelization
cores<-parallel::detectCores()
cluster = makeCluster(cores, type = "SOCK")
registerDoSNOW(cluster)
out<-tbl_df(data.frame(vertex_id=rep(unique(data$vertex_id),each=2),
                       out_past=rep(c(TRUE,FALSE),length(unique(data$vertex_id)))))

# compute in parallel all cases
results<- foreach(ah=1:cases_f, .combine = 'cbind', .packages=c("compiler","dplyr",
                "assertthat","tidyr")) %:% foreach(past = 1:cases_p) %dopar% {
    title<-paste0('fut',ahead[ah,1],'_',ahead[ah,2],
                  'past',past_cluster[past,1],'_',past_cluster[past,2])
    calculate<-tbl_df(out_increase(data,as.numeric(ahead[ah,]),as.numeric(past_cluster[past,])))
    calculate<-calculate %>% select(vertex_id,out_past,ratio,signific) %>% 
        rename_(.dots=setNames(list('ratio'), title))
    out2<-left_join(out,calculate,c('vertex_id','out_past'))
    index<-dim(out2)[2]
    out2[,c(index-1,index)]
}
stopCluster(cluster)

# Group results in single data frame
out_increase<-out
out_significance<-out
for (fut in 1:cases_f){
    for (past in 1:cases_p){
        aux<-as.data.frame(results[past,fut])
        out_increase<-data.frame(out_increase,aux[,1])
        names(out_increase)[length(out_increase)]<-names(aux)[1]
        out_significance<-data.frame(out_significance,aux[,2])
        names(out_significance)[length(out_significance)]<-names(aux)[2]
    }
}
# Refine names
names(out_increase)<-sub('result.[0-9]+.','',names(out_increase))
names(out_increase)<-sub('fut','f',names(out_increase))
names(out_increase)<-sub('past','p',names(out_increase))
names(out_increase)[2]<-'out_past'
names(out_significance)[-c(1,2)]<-paste0(names(out_increase)[-c(1,2)],'.sig')

# convert to dplyr table
out_increase<-tbl_df(out_increase)
out_significance<-tbl_df(out_significance)

## Plot increases when past outlier cluster is true, only when significant ##
out_increase<-out_increase %>% filter(out_past==T)
out_significance<-out_significance %>% filter(out_past==T)
# set non-significant cases to 0 value
out_increase_filt<-out_increase
out_increase_filt[,-c(1,2)]<-out_increase_filt[,-c(1,2)]*out_significance[,-c(1,2)]
#out_increase_filt[,-c(1,2)]<-apply(out_increase_filt[,-c(1,2)],2,
#                                   function (x) ifelse(x==0,NA,x))

# extract cases and plot
window_cases<-length(past_days_window)*length(fut_days_window)
past_out_cases<-length(n_outliers)
my_palette <- colorRampPalette(c("red", "white", "green"))(n = 29)
for (i in (1:window_cases)){
    index<-3+past_out_cases*(i-1)
    data_plot<-out_increase_filt[,c(index:(index+past_out_cases-1))]
    data_plot<-as.matrix(data_plot)
    rownames(data_plot)<-out_increase_filt$vertex_id
    # Plot heatmap
    title0<-colnames(data_plot)[1]
    title0<-substr(title0, 1, nchar(title0)-2)
    title<-sub('_1',' , ',title0)
    title<-sub('f','Future: ',title)
    title<-sub('p','Past: ',title)
    title0<-paste0(title0,outlier_type)
    colnames(data_plot)<-n_outliers
    plot_title<-paste0('Pr increase of 1+ Outlier w/r to past outlier cluster\n Time window (days) ',
                       title,'\nOutlier type: ',outlier_type)
    
    png(file=paste(title0,"png",sep="."),width = 800, height = 620)
        heatmap.2(data_plot,Colv=NA,na.rm=T,col=my_palette, dendrogram='row',
                  na.color='grey', cexCol=0.8, srtCol=0,
                  xlab="Past outliers (>=)",ylab="ticker",
                  symm=F,symkey=F,symbreaks=T,
                  main=plot_title)
        legend(0.8,1.1,'Not Available Data','grey',cex=0.7,bty='n')
        
    dev.off()
    
}
    