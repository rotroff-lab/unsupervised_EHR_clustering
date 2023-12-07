########## Unsupervised Methods Metrics #####################

library(data.table)
library(tidyverse)
library(dtwclust)
library(foreach)
library(doParallel)



metrics_function<-function(source_dir,a_file,original_file){
ret_df<-NULL
df<-"Read in afile (clustering results)"
ground_truth<-"Read in original file"
ground_truth<-ground_truth %>% group_by(Class,ID) %>% dplyr::summarise(Values=list(Value))
ret_df<-NULL
state_num<-1

for(i in 1:length(df)){
  temp_cl<-df[[i]]
  cluster_metrics<-cvi(temp_cl,b=factor(ground_truth$Class),type=c("ARI","RI"))
  temp_df<-data.frame(Metric=names(cluster_metrics),Value=cluster_metrics,
             Type=temp_cl@type,Distance=temp_cl@distance,Centroid=temp_cl@centroid,
             Random_Run=state_num,CentroidsNum=length(temp_cl@centroids),
             Cohort=ground_file,fileName=a_file)
  temp_df$Args<-list(unlist(temp_cl@args))
  ret_df<-rbind.data.frame(ret_df,temp_df)
  state_num<-state_num+1
  if(state_num>5){
    state_num<-1
  }
}

return(ret_df)
}

metrics_function(source_dir,a_file)





