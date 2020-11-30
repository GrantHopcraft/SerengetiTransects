# add grass data

# load packages
library(tidyr)
library(dplyr)
library(forcats)

# load master
master<-read.csv('grass_master_20180407.csv')

#add date column to master

names(master)[1]<-"Y"
names(master)[5]<-"Transect"


master$Date<-paste(master$D,master$M,master$Y,sep='/')
master$Date<-as.Date(master$Date,format='%d/%m/%Y') 

# format master

master$Transect<-fct_collapse(master$Transect,
                                     AirstripOlduvai = c("AirSrip-Olduvai","AirStirp-Olduvai","Airstrip-olduvai","AirStrip-Olduvai","AirStrip-Olduvai"),
                                     BanagiLobo = c("banagi-lobo","Banagi-Lobo","Banagi-Lobo"),
                                     BarafuGolini = c("barafu-golini","Barafu-Golini","Barafu -Golini","Barafu-Golini"),
                                     BaundarySimba = c("Baundary-Simba","Baundary-SimbaKopje","Simba-Baundary","Baundary-SimbaKopjes","Simba-Baundary","Baundary-Simba","Baundary-SIMBA"),
                                     HippoPoolRongai = c("HipoPool-Rongai","hippo pull-rongai","Hippo pull-rongai","HippoPool-Rongai","hipopool-rongai","HipoPool-Rongai"),
                                     SeroneraIkomaGate = c("seonera-ikoma gate","sero-ikoma gate","Sero-IkomaGate","Seronera-IkomaGate","Ser-IkomaGate","Sero-Musabi","Sero-IkomaGate"), 
                                     SeroneraMusabi = c("sero-musabi","Sero-Musabi","Seronera-Musabi","sero-Musabi"), 
                                     SopaBridgeJunction = c("Sopa bridge-junction","sopabridge-junction","SopaBridge-Junction","SopaBridge-Juction","SopBridge-Junction","SopaBridge-Junction"),
                                     SWRCBarafu = c("Swrc-barafu","Swrc-Barafu","SWRC-BARAFU","Swrc-Barafu"))



# clean new data

path<-"C:\\Users\\giova\\Desktop\\new_grass_data"


# this will create a list of all .csv files names present in the specified folder
filelist<-list.files(path, full.names = TRUE,pattern=".csv$")

# makes a list of the dataframes
datalist<-lapply(filelist,FUN = read.csv, header=TRUE)

for (i in 1:length(datalist)){
  
  
  datalist[[i]]$Date<-paste(datalist[[i]][,3],datalist[[i]][,2],datalist[[i]][,1],sep='/')
  
  datalist[[i]]$Date<-as.Date(datalist[[i]]$Date,format='%d/%m/%Y')
  
  names(datalist[[i]])<-names(master) # change the names to the master ones 
  
  datalist[[i]]$Transect<-as.factor(datalist[[i]]$Transect)
  
  datalist[[i]][c(1:3,6:7)]<-lapply(datalist[[i]][c(1:3,6:7)],as.numeric) #grass height as numeric
  
  datalist[[i]]$Transect<-fct_collapse(datalist[[i]]$Transect,
                                       AirstripOlduvai = c("AirSrip-Olduvai","AirStirp-Olduvai","Airstrip-olduvai","AirStrip-Olduvai","AirStrip-Olduvai"),
                                       BanagiLobo = c("banagi-lobo","Banagi-Lobo","Banagi-Lobo"),
                                       BarafuGolini = c("barafu-golini","Barafu-Golini","Barafu -Golini","Barafu-Golini"),
                                       BaundarySimba = c("Baundary-Simba","Baundary-SimbaKopje","Simba-Baundary","Baundary-SimbaKopjes","Simba-Baundary","Baundary-Simba","Baundary-SIMBA"),
                                       HippoPoolRongai = c("HipoPool-Rongai","hippo pull-rongai","Hippo pull-rongai","HippoPool-Rongai","hipopool-rongai","HipoPool-Rongai"),
                                       SeroneraIkomaGate = c("seonera-ikoma gate","sero-ikoma gate","Sero-IkomaGate","Seronera-IkomaGate","Ser-IkomaGate","Sero-Musabi","Sero-IkomaGate"), 
                                       SeroneraMusabi = c("sero-musabi","Sero-Musabi","Seronera-Musabi","sero-Musabi"), 
                                       SopaBridgeJunction = c("Sopa bridge-junction","sopabridge-junction","SopaBridge-Junction","SopaBridge-Juction","SopBridge-Junction","SopaBridge-Junction"),
                                       SWRCBarafu = c("Swrc-barafu","Swrc-Barafu","SWRC-BARAFU","Swrc-Barafu"))
}


# bind together the new data
newdata<-do.call(rbind,datalist)
newdata<-newdata[order(newdata$Date),]

# bind it to master
newmaster<-rbind(newdata,master)

# check that it works 
nrow(newmaster)
nrow(newdata)+nrow(master)

# order it by date
newmaster<-newmaster[order(newmaster$Date),]

#remove potential row duplicates before saving the file on disk
newmaster<-newmaster[!duplicated(newmaster),]

# save the new master as a csv
write.csv(newmaster,file ='grass_master_07072020.csv',row.names = FALSE)

