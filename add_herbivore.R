# Add herbivore transect data to master

# load packages
library(tidyr)
library(dplyr)
library(forcats)

# load master
master<-read.csv('first_clean_master_11052020.csv')
str(master)

#make sure master file dates are as dates
master$Date<-paste(master$Day,master$Month,master$Year,sep='/')
master$Date<-as.Date(master$Date,format='%d/%m/%Y')


# clean new data 

path<-"C:\\Users\\giova\\Desktop\\new_herbiv_data"

# this will create a list of all .csv files names present in the specified folder
filelist<-list.files(path, full.names = TRUE,pattern=".csv$")

# makes a list of the dataframes
datalist<-lapply(filelist,FUN = read.csv, header=TRUE)


for (i in 1:length(datalist)){
  
  names(datalist[[i]])<-names(master) # change the names of the columns to the correct ones based on the master 
  
  datalist[[i]][,c(2:4,7:21)]<-lapply(datalist[[i]][,c(2:4,7:21)],as.numeric) # make columns numeric
  
  datalist[[i]]$Date<-paste(datalist[[i]]$Day,datalist[[i]]$Month,datalist[[i]]$Year,sep='/')
  
  datalist[[i]]$Animal.sp<-as.factor(datalist[[i]]$Animal.sp) #animal species as factor
  
  datalist[[i]]$Date<-as.Date(datalist[[i]]$Date,format='%d/%m/%Y')
  
  datalist[[i]]$Transect<-fct_collapse(datalist[[i]]$Transect,
                                       AirstripOlduvai = c("AirSrip-Olduvai","AirStirp-Olduvai","Airstrip-olduvai","AirStrip-Olduvai"),
                                       BanagiLobo = c("banagi-lobo","Banagi-Lobo"),
                                       BarafuGolini = c("barafu-golini","Barafu-Golini","Barafu -Golini"),
                                       SimbaBoundary = c("Baundary-Simba","Baundary-SimbaKopje","Simba-Baundary","BaundarySimba"),
                                       HippoPoolRongai = c("HipoPool-Rongai","hippo pull-rongai","Hippo pull-rongai","HippoPool-Rongai"),
                                       SeroneraIkomaGate = c("seonera-ikoma gate","sero-ikoma gate","Sero-IkomaGate","Seronera-IkomaGate","Sero-Ikomagate"), 
                                       SeroneraMusabi = c("sero-musabi","Sero-Musabi","Seronera-Musabi","Sero- Musabi"), 
                                       SopaBridgeJunction = c("Sopa bridge-junction","sopabridge-junction","SopaBridge-Junction","SopaBridge-Juction"),
                                       SWRCBarafu = c("Swrc-barafu","Swrc-Barafu","SWRC-BARAFU","SWRC-Barafu")) # this part deals with spelling mistakes
  
  # If there are new variations of spellings in the data from field staff, then add them in here.  
}

# bind all new data together
newdata<-do.call(rbind,datalist)

# replace NAs with 0s
newdata[7:21][is.na(newdata[7:21])]=0

# here we bind the newdata to the older master
newmaster<-rbind(newdata,master)

# check that it works - if the rows do not match something is wrong
nrow(newmaster)
nrow(newdata)+nrow(master)
str(newmaster)

N<-nrow(newmaster)

for (i in 1:N){ 
  if (newmaster$Total[i]== 0) {
    
    newmaster$Total[i]<-(newmaster$F.New.born[i]+newmaster$F.1.4.[i]+newmaster$F.1.2.[i]+newmaster$F.Ad[i]+
                           newmaster$M.1.4.[i]+newmaster$M.1.2.[i]+newmaster$M.Ad[i]+newmaster$Un.id.sex_Adults[i])}
  if (newmaster$Total[i]>=1) {
    newmaster$Un.id.sex_Adults[i]<-newmaster$Total[i] - sum(newmaster$F.New.born[i],newmaster$F.1.4.[i],newmaster$F.1.2.[i],
                                                            newmaster$F.Ad[i],newmaster$M.1.4.[i],
                                                            newmaster$M.1.2.[i],newmaster$M.Ad[i])}}

#remove potential row duplicates before saving the file on disk
newmaster<-newmaster[!duplicated(newmaster),]

# order it by date
newmaster<-newmaster[order(newmaster$Date),]

# save the new master as a csv, make sure it doesn't add an extra id column
write.csv(newmaster,file ='herbiv_master_07072020.csv',row.names = FALSE)
