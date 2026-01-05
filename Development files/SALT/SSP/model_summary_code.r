library(dplyr)

ssp.folder.names<-c("Status40%",
                    "Status20%",
                    "Status60%",
                    "Status20%_Scale40%_estCt",
                    "Status60%_Scale40%_estCt",
                    "Status40%_h_low",
                    "Status40%_h_hi",
                    "Status40%_M_low",
                    "Status40%_M_hi",
                    "Status40%_Scale40%_estCt_h_low",
                    "Status40%_Scale40%_estCt_h_hi",
                    "Status40%_Scale40%_estCt_M_low",
                    "Status40%_Scale40%_estCt_M_hi",
                    "Status_est_Scale40%_h_low",
                    "Status_est_Scale40%_h_ref",
                    "Status_est_Scale40%_h_hi",
                    "Status_est_Scale40%_M_low",
                    "Status_est_Scale40%_M_ref",
                    "Status_est_Scale40%_M_hi")

SSP_choices<-c(
  "Status 40%",
  "Status 20%",
  "Status 60%",
  "Status 20%, Scale estimate through catch",
  "Status 60%, Scale estimate through catch",
  "Steepness low",
  "Steepness high",
  "M low",
  "M high",
  "Scale estimate through catch, Steepness low",
  "Scale estimate through catch, Steepness high",
  "Scale estimate through catch, M low",
  "Scale estimate through catch, M high",
  "Estimate status, M low",
  "Estimate status, M reference",
  "Estimate status, M high",
  "Estimate status, Steepness low",
  "Estimate status, Steepness reference",
  "Estimate status, Steepness high")

Dir_SSP<-"C:/Users/Jason.Cope/Documents/Github/Stock-Assessment-Learning-Tool/SALT/SSP/Models"
SSP_mod_dir <-NULL
for(i in 1:(length(ssp.folder.names)))
{SSP_mod_dir[i] <-file.path(Dir_SSP,ssp.folder.names[i])}
ssp.mod.prep<-r4ss::SSgetoutput(
  dirvec=SSP_mod_dir,
  #  keyvec=1:length(SSP.model.list), 
  getcovar=FALSE
)
names(ssp.mod.prep)<-ssp.folder.names
ssp_summary<- SSsummarize(ssp.mod.prep)
save(ssp_summary,file="C:/Users/Jason.Cope/Documents/Github/Stock-Assessment-Learning-Tool/SALT/SSP/mod_summary.rds")

#Make catch history object
#Catches<-mapply(function(x) data.frame(Year=unique(ssp.mod.prep[[x]]$catch$Yr),Catches=ssp.mod.prep[[x]]$catch$dead_bio,Model=SSP_choices[x]),x=1:length(ssp.folder.names), SIMPLIFY = FALSE)

Catches<-list()
for(x in 1:length(ssp.mod.prep))
{
  Catches[[x]]<-ssp.mod.prep[[x]]$catch %>% 
    group_by(Yr) %>% 
    summarise(dead_bio = sum(dead_bio))
  Catches[[x]]$Model<-SSP_choices[x]
}

Catches<-do.call("rbind", Catches)       
save(Catches,file="C:/Users/Jason.Cope/Documents/Github/Stock-Assessment-Learning-Tool/SALT/SSP/Catches.rds")
