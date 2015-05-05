# outlier prediction,
# Joan Verdu

### REgression


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

# spread outlier data.frame to have ticker in separate columns
data<-tbl_df(data)
data<-select(data,vertex_id,date,outlier)
data<-spread(data,vertex_id,outlier)
nobs<-nrow(data)
nticker<-ncol(data)-1


data_base<-gather(data,vertex_id,outlier,-date,na.rm=T)

data_nahead<-data
    for (i in 1:(ndays-n_days_ahead)){ # n_days_ahead mean
        data_nahead[i,2:(ntickers+1)]<-colMeans(
            data[(i+1):(i+n_days_ahead), 2:(ntickers+1)],na.rm=T)
    }

data_nahead[(ndays-n_days_ahead+1):ndays,2:(nticker+1)]<-matrix(NA,n_days_ahead,
                                                                nticker)
