# Centroid based algorithms #

library(dtwclust)
library(tidyverse)
library(data.table)
args<-commandArgs(T)


require("doParallel")
workers <- makeCluster(8L)
invisible(clusterEvalQ(workers, library("dtwclust")))
registerDoParallel(workers)

a_cohort<-"Simulated Dataset File Path"

p1<-Sys.time()
df<-fread(file.path(a_cohort),data.table = F)
traj_df<-df %>% group_by(Class,ID) %>% summarise(Trajectory=list(Value))

### Clustering Methods ###

num_classes<-sum(grepl("Class ",colnames(df)))
num_classes_overlap<-num_classes


##### File destinations ######

dest_dir<-"Output Dir"
dir.create(dest_dir,recursive = T)
models_dir<-"Models Dir"
dir.create(models_dir,recursive = T)

## PAM Clustering & Fuzzy Clustering

# Save models, save all configurations
distance_metrics<-c("dtw_basic","dtw_lb","lbk","lbi","sbd","gak","sdtw","L2")
window_sizes<-seq(2,15,3)
p2<-Sys.time()

for(a_method in distance_metrics){
  for(a_size in window_sizes){
    dir_name<-paste0(models_dir,"/pam_",a_method,"_",a_size,"/")
    dir.create(dir_name)
    fn1<-gsub(".csv",".rds",paste0(dir_name,sapply(strsplit(a_cohort,"/"),"[",5)))
    
    pc_par <- tsclust(traj_df$Spline_Trajectories, k = num_classes,
                  distance = a_method, centroid = "pam",
                  seed = 938,
                  control = partitional_control(nrep = 5L),
                  args=tsclust_args(dist = list(window.size=a_size)),
                  max.iter=25)
    names(pc_par)<-paste0(rep(paste0("k_",num_classes),each=5),"_",1:5)
    saveRDS(pc_par,fn1)
    
    
    dir_name<-paste0(models_dir,"/fuzzy_",a_method,"_",a_size,"/")
    dir.create(dir_name)
    fn2<-gsub(".csv",".rds",paste0(dir_name,sapply(strsplit(a_cohort,"/"),"[",5)))
    pc_par <- tsclust(traj_df$Spline_Trajectories, k = num_classes,
                      distance = a_method, type= "fuzzy",
                      seed = 938,
                      args=tsclust_args(dist = list(window.size=a_size)),
                      max.iter=25)
    saveRDS(pc_par,fn2)
    
  }
}
print("######Finished PAM and Fuzzy#########")
print(Sys.time()-p2)

## DBA Centroid and DTW distance
p2<-Sys.time()
distance_metrics<-c("dtw_basic","dtw_lb","lbk","lbi")
for(a_method in distance_metrics){
for(a_size in window_sizes){
  dir_name<-paste0(models_dir,"/dba_",a_method,"_",a_size,"/")
  dir.create(dir_name)
  fn3<-gsub(".csv",".rds",paste0(dir_name,sapply(strsplit(a_cohort,"/"),"[",5)))
  pc_par <- tsclust(traj_df$Spline_Trajectories, k = num_classes,
                  distance = a_method, centroid = "dba",
                  seed = 938,
                  control = partitional_control(nrep = 5L),
                  args=tsclust_args(dist = list(window.size=a_size)),
                  max.iter=25)
  names(pc_par)<-paste0(rep(paste0("k_",num_classes),each=5),"_",1:5)
  saveRDS(pc_par,fn3)
  
  }
}
print("######Finished DBA#########")
print(Sys.time()-p2)
## K-Shape Clustering
p2<-Sys.time()
dir_name<-paste0(models_dir,"/kshape/")
dir.create(dir_name)
fn4<-gsub(".csv",".rds",paste0(dir_name,sapply(strsplit(a_cohort,"/"),"[",5)))
pc_par <- tsclust(traj_df$Spline_Trajectories, k = num_classes,
                    distance = "sbd", centroid = "shape",
                    seed = 938,preproc = zscore,
                    control = partitional_control(nrep = 5L),
                  max.iter=25)
names(pc_par)<-paste0(rep(paste0("k_",num_classes),each=5),"_",1:5)
saveRDS(pc_par,fn4)


print("######Finished KShape#########")
print(Sys.time()-p2)

## Soft-DTW
p2<-Sys.time()
gamma_vals<-c(0.01,0.1,1,5,10,100)

for(gamma_val in gamma_vals){
  dir_name<-paste0(models_dir,"/softDTW_",gamma_val,"/")
  dir.create(dir_name)
  fn5<-gsub(".csv",".rds",paste0(dir_name,sapply(strsplit(a_cohort,"/"),"[",5)))

  pc_par <- tsclust(traj_df$Spline_Trajectories, k = num_classes,
                  distance = "sdtw", centroid = "sdtw_cent",
                  seed = 938,
                  control = partitional_control(nrep = 5L),
                  args=tsclust_args(dist = list(gamma=gamma_val)),
                  max.iter=25)
  names(pc_par)<-paste0(rep(paste0("k_",num_classes),each=5),"_",1:5)

  saveRDS(pc_par,fn5)

}
print("######Finished SoftDTW#########")
print(Sys.time()-p2)
print(Sys.time()-p1)


stopCluster(workers)
