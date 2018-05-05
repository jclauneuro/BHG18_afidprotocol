# this script tries to organize all fiducial files in a directory and summarize the findings
rm(list=ls())

library(plyr)
library(digest)
library(reshape2)
## visualization of specific fid
library("plot3D")
library("plot3Drgl") # for interactive 3D plotting

citation(package='plyr')
packageVersion('plyr')

setwd('~/GitHub/BHG18_landmarkprotocol//input/input_fid')

df_raters <- data.frame(fid=integer(),X=double(),Y=double(),Z=double(),rater=factor(),
                        template=factor(),mri_type=factor(),session=integer(),date=integer(),
                        name=character(),description=character(),stringsAsFactors = FALSE)

csv_files <- list.files(".", "*.fcsv")

for (i in 1:length(csv_files)) {
  curr_split <- unlist(strsplit(csv_files[i],"_"))
  if (length(curr_split)>1) { # extract name and session data
    rater_template <- curr_split[1]
    rater_mri_type <- curr_split[2]
    rater_name <- curr_split[3]
    rater_session <- as.numeric(curr_split[4])
    rater_date <- as.numeric(unlist(strsplit(curr_split[5],"[.]"))[1])
  }

  curr_rater <- read.table(csv_files[i], header=FALSE, sep=",")
  df_rater <- data.frame(fid = 1:length(curr_rater$V1))

  df_rater <- cbind(df_rater,X=curr_rater[2],Y=curr_rater[3],Z=curr_rater[4],rater=rater_name,
                    template=rater_template,mri_type=rater_mri_type,
                    session=rater_session,date=rater_date,name=curr_rater[12],
                    description=curr_rater[13])
  
  df_rater <- rename(df_rater, c("V2"="X","V3"="Y","V4"="Z","V12"="name","V13"="description"))
  df_raters <- rbind(df_raters,df_rater)
  #print(i)
}

# Remapping levels
levels(df_raters$rater) # original; alphabetical
levels(df_raters$rater) <- 1:11 # rename raters from 1:11

#### Create study MEAN ########################################################################
ddply(df_raters, "fid", summarize, X=mean(X), Y=mean(Y), Z=mean(Z) ) # study mean

#### Gold-standard study MEAN ########################################################################
# Gold standard (mean from previous group)
setwd('~/GitHub/fid_study/BHG18/input/input_mean')
UHF_gold <- read.table('UHF_MEAN_no_outliers.fcsv', header=FALSE, sep=",")
df_gold <- data.frame(fid = 1:length(curr_rater$V1))
df_gold <- cbind(df_gold,X=UHF_gold[2],Y=UHF_gold[3],Z=UHF_gold[4],rater='GOLD',
                  template=rater_template,mri_type=rater_mri_type,
                  session=rater_session,date=rater_date,name=UHF_gold[12],
                  description=UHF_gold[13])
df_gold <- rename(df_gold, c("V2"="X","V3"="Y","V4"="Z","V12"="name","V13"="description"))

#### Recalculate rater errors ########################################################################

df_raters$xdist <- NA
df_raters$ydist <- NA
df_raters$zdist <- NA

df_raters$rater_error <- NA
df_raters$outlier <- NA

for (i in 1:dim(df_raters)[1]) {
        curr_rater <- df_raters[i,]
        mean_raters <- df_gold[curr_rater$fid,]

        xdist <- curr_rater$X - mean_raters$X
        ydist <- curr_rater$Y - mean_raters$Y
        zdist <- curr_rater$Z - mean_raters$Z
        
        rater_error <- sqrt( (curr_rater$X - mean_raters$X)^2 + (curr_rater$Y - mean_raters$Y)^2 + (curr_rater$Z - mean_raters$Z)^2 )
        
        df_raters[i,]$xdist <- xdist
        df_raters[i,]$ydist <- ydist
        df_raters[i,]$zdist <- zdist
        
        rater_error <- sqrt( (curr_rater$X - mean_raters$X)^2 + (curr_rater$Y - mean_raters$Y)^2 + (curr_rater$Z - mean_raters$Z)^2 )
        df_raters[i,]$rater_error <- rater_error
        df_raters[i,]$outlier <- (rater_error > 10) # focus on true outliers (1cm+) first (TODO: optimize threshold later)
        #print(rater_error)
}

##########################################################################################
# Summary Statistics
mean(df_raters$rater_error)
sd(df_raters$rater_error)
ddply(df_raters, "rater", summarize, mean=mean(rater_error), sd=sd(rater_error))

########### INDIVIDUAL RATER EXAMPLE ###############
# e.g. rater 4
df_curr_rater <- subset(df_raters, rater==4)[,c("fid","name","description","rater_error","outlier")]
df_curr_rater$description <- df_gold$description
mean(df_curr_rater$rater_error)
sd(df_curr_rater$rater_error)
df_curr_rater


