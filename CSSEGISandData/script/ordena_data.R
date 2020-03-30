library(dplyr)
library(RCurl)
library(ggplot2)
library("rjson")
library(jsonlite)
library("plyr")
library("dplyr")
#
x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
tabla_confirmed <- read.csv(text=x,header=TRUE,sep=",")
tabla_confirmed$Province.State <- gsub(" ",".",as.character(tabla_confirmed$Province.State))
tabla_confirmed$Province.State <- gsub(",","",as.character(tabla_confirmed$Province.State))
tabla_confirmed$Country.Region <- gsub(" ",".",as.character(tabla_confirmed$Country.Region))
tabla_confirmed$Country.Region <- gsub(",","",as.character(tabla_confirmed$Country.Region))
tabla_confirmed$region <- ifelse(tabla_confirmed$Province.State!="",paste(tabla_confirmed$Province.State,"_",tabla_confirmed$Country.Region,sep=""),paste("All_",tabla_confirmed$Country.Region,sep=""))
#
tabla_confirmed_T<-as.data.frame(t(tabla_confirmed[,6:ncol(tabla_confirmed)-1]))
colnames(tabla_confirmed_T) <- c(tabla_confirmed$region)
#
tablaFechas <- as.data.frame(rownames(tabla_confirmed_T))
colnames(tablaFechas) <- c("DATE")
rownames(tablaFechas) <- tablaFechas$DATE
tablaFechas$DATE <- as.Date(as.character(tablaFechas$DATE), format="X%m.%d.%y")
#
tabla_confirmed_T <- merge(tabla_confirmed_T,tablaFechas,by="row.names")
tabla_confirmed_T <- tabla_confirmed_T[order(tabla_confirmed_T$DATE),]
tabla_confirmed_T$Row.names <- NULL
rownames(tabla_confirmed_T) <- NULL
#
regions <- tabla_confirmed$region
#length(regions[-1])
defSet <- select(tabla_confirmed_T,"DATE",regions[1])
colnames(defSet)[2] <- "CASES"
defSet$REGION <- regions[1]
for(i in regions[-1]){
    addSet <- select(tabla_confirmed_T,"DATE",i)
    colnames(addSet)[2] <- "CASES"
    addSet$REGION <- i
    defSet <- rbind(defSet,addSet)
}
countries <- select(tabla_confirmed,"Country.Region","Lat","Long","region")
colnames(countries)[4] <- "REGION"
defSet <- merge(countries,defSet,by="REGION")
#
defSet_countries <- aggregate(CASES ~ DATE + Country.Region, data=defSet, sum)
#
defSet_world <- aggregate(CASES ~ DATE, data=defSet, sum)
#
png("../images/confirmed_world.png",width = 700, height = 400)
g <- ggplot(defSet_world, aes(x=DATE, y=(CASES),color="World")) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 confirmed cases") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
tabla_deaths <- read.csv(text=x,header=TRUE,sep=",")
tabla_deaths$Province.State <- gsub(" ",".",as.character(tabla_deaths$Province.State))
tabla_deaths$Province.State <- gsub(",","",as.character(tabla_deaths$Province.State))
tabla_deaths$Country.Region <- gsub(" ",".",as.character(tabla_deaths$Country.Region))
tabla_deaths$Country.Region <- gsub(",","",as.character(tabla_deaths$Country.Region))
tabla_deaths$region <- ifelse(tabla_deaths$Province.State!="",paste(tabla_deaths$Province.State,"_",tabla_deaths$Country.Region,sep=""),paste("All_",tabla_deaths$Country.Region,sep=""))
#
tabla_deaths_T<-as.data.frame(t(tabla_deaths[,6:ncol(tabla_deaths)-1]))
colnames(tabla_deaths_T) <- c(tabla_deaths$region)
#
tablaFechas <- as.data.frame(rownames(tabla_deaths_T))
colnames(tablaFechas) <- c("DATE")
rownames(tablaFechas) <- tablaFechas$DATE
tablaFechas$DATE <- as.Date(as.character(tablaFechas$DATE), format="X%m.%d.%y")
#
tabla_deaths_T <- merge(tabla_deaths_T,tablaFechas,by="row.names")
tabla_deaths_T <- tabla_deaths_T[order(tabla_deaths_T$DATE),]
tabla_deaths_T$Row.names <- NULL
rownames(tabla_deaths_T) <- NULL
#
regions <- tabla_deaths$region
#length(regions[-1])
defSet_D <- select(tabla_deaths_T,"DATE",regions[1])
colnames(defSet_D)[2] <- "CASES"
defSet_D$REGION <- regions[1]
for(i in regions[-1]){
    addSet <- select(tabla_deaths_T,"DATE",i)
    colnames(addSet)[2] <- "CASES"
    addSet$REGION <- i
    defSet_D <- rbind(defSet_D,addSet)
}
countries <- select(tabla_deaths,"Country.Region","Lat","Long","region")
colnames(countries)[4] <- "REGION"
defSet_D <- merge(countries,defSet_D,by="REGION")
#
defSet_D_countries <- aggregate(CASES ~ DATE + Country.Region, data=defSet_D, sum)
#
defSet_D_world <- aggregate(CASES ~ DATE, data=defSet_D, sum)
#
png("../images/deaths_world.png",width = 700, height = 400)
g <- ggplot(defSet_D_world, aes(x=DATE, y=(CASES),color="World")) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 confirmed deaths") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
####
#
x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
tabla_recovered <- read.csv(text=x,header=TRUE,sep=",")
tabla_recovered$Province.State <- gsub(" ",".",as.character(tabla_recovered$Province.State))
tabla_recovered$Province.State <- gsub(",","",as.character(tabla_recovered$Province.State))
tabla_recovered$Country.Region <- gsub(" ",".",as.character(tabla_recovered$Country.Region))
tabla_recovered$Country.Region <- gsub(",","",as.character(tabla_recovered$Country.Region))
tabla_recovered$region <- ifelse(tabla_recovered$Province.State!="",paste(tabla_recovered$Province.State,"_",tabla_recovered$Country.Region,sep=""),paste("All_",tabla_recovered$Country.Region,sep=""))
#
tabla_recovered_T<-as.data.frame(t(tabla_recovered[,6:ncol(tabla_recovered)-1]))
colnames(tabla_recovered_T) <- c(tabla_recovered$region)
#
tablaFechas <- as.data.frame(rownames(tabla_recovered_T))
colnames(tablaFechas) <- c("DATE")
rownames(tablaFechas) <- tablaFechas$DATE
tablaFechas$DATE <- as.Date(as.character(tablaFechas$DATE), format="X%m.%d.%y")
#
tabla_recovered_T <- merge(tabla_recovered_T,tablaFechas,by="row.names")
tabla_recovered_T <- tabla_recovered_T[order(tabla_recovered_T$DATE),]
tabla_recovered_T$Row.names <- NULL
rownames(tabla_recovered_T) <- NULL
#
regions <- tabla_recovered$region
#length(regions[-1])
defSet_R <- select(tabla_recovered_T,"DATE",regions[1])
colnames(defSet_R)[2] <- "CASES"
defSet_R$REGION <- regions[1]
for(i in regions[-1]){
    addSet <- select(tabla_recovered_T,"DATE",i)
    colnames(addSet)[2] <- "CASES"
    addSet$REGION <- i
    defSet_R <- rbind(defSet_R,addSet)
}
countries <- select(tabla_recovered,"Country.Region","Lat","Long","region")
colnames(countries)[4] <- "REGION"
defSet_R <- merge(countries,defSet_R,by="REGION")
#
defSet_R_countries <- aggregate(CASES ~ DATE + Country.Region, data=defSet_R, sum)
#
defSet_R_world <- aggregate(CASES ~ DATE, data=defSet_R, sum)
#
png("../images/recovered_world.png",width = 700, height = 400)
g <- ggplot(defSet_R_world, aes(x=DATE, y=(CASES),color="World")) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 recovered cases") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
##
#
table_C <- defSet_countries
colnames(table_C)[3] <- "CONFIRMED"
table_D <- defSet_D_countries
colnames(table_D)[3] <- "DEATHS"
table_R <- defSet_R_countries
colnames(table_R)[3] <- "RECOVERED"
table_CD <- table_C %>% right_join(table_D, by=c("DATE","Country.Region"))
table_CDR <- table_CD %>% right_join(table_R, by=c("DATE","Country.Region"))
#
write_json(table_CDR, "../files/COVID19_world.json")
write.csv(table_CDR, file = "../files/COVID19_world.csv", row.names = FALSE)
#
list_of_countries_of_interest <- read.csv("dic_countries.csv",header=TRUE)
#
##
#
defSet_selected_countries_SA <- merge(table_CDR,list_of_countries_of_interest[which(list_of_countries_of_interest$CONTINENT=="SouthAmerica"),],by="Country.Region")
#
png("../images/confirmed_SouthAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_SA[which(defSet_selected_countries_SA$CONFIRMED!=0),], aes(x=DATE, y=(CONFIRMED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 confirmed cases South America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/deaths_SouthAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_SA[which(defSet_selected_countries_SA$DEATHS!=0),], aes(x=DATE, y=(DEATHS), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 deaths South America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/recovered_SouthAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_SA[which(defSet_selected_countries_SA$RECOVERED!=0),], aes(x=DATE, y=(RECOVERED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 recovered cases South America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
##
#
defSet_selected_countries_EU <- merge(table_CDR,list_of_countries_of_interest[which(list_of_countries_of_interest$CONTINENT=="Europe"),],by="Country.Region")
#
png("../images/confirmed_Europe.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_EU[which(defSet_selected_countries_EU$CONFIRMED!=0),], aes(x=DATE, y=(CONFIRMED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 confirmed cases Europe") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/deaths_Europe.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_EU[which(defSet_selected_countries_EU$DEATHS!=0),], aes(x=DATE, y=(DEATHS), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 deaths Europe") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/recovered_Europe.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_EU[which(defSet_selected_countries_EU$RECOVERED!=0),], aes(x=DATE, y=(RECOVERED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 recovered cases Europe") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
##
#
defSet_selected_countries_As <- merge(table_CDR,list_of_countries_of_interest[which(list_of_countries_of_interest$CONTINENT=="Asia"),],by="Country.Region")
#
png("../images/confirmed_Asia.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_As[which(defSet_selected_countries_As$CONFIRMED!=0),], aes(x=DATE, y=(CONFIRMED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 confirmed cases Asia") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/deaths_Asia.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_As[which(defSet_selected_countries_As$DEATHS!=0),], aes(x=DATE, y=(DEATHS), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 deaths Asia") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/recovered_Asia.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_As[which(defSet_selected_countries_As$RECOVERED!=0),], aes(x=DATE, y=(RECOVERED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 recovered cases Asia") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
##
#
defSet_selected_countries_NA <- merge(table_CDR,list_of_countries_of_interest[which(list_of_countries_of_interest$CONTINENT=="NorthAmerica"),],by="Country.Region")
#
png("../images/confirmed_NorthAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_NA[which(defSet_selected_countries_NA$CONFIRMED!=0),], aes(x=DATE, y=(CONFIRMED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 confirmed cases North America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/deaths_NorthAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_NA[which(defSet_selected_countries_NA$DEATHS!=0),], aes(x=DATE, y=(DEATHS), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 deaths North America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/recovered_NorthAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_NA[which(defSet_selected_countries_NA$RECOVERED!=0),], aes(x=DATE, y=(RECOVERED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 recovered cases North America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
##
#
defSet_selected_countries_CA <- merge(table_CDR,list_of_countries_of_interest[which(list_of_countries_of_interest$CONTINENT=="CentralAmerica"),],by="Country.Region")
#
png("../images/confirmed_CentralAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_CA[which(defSet_selected_countries_CA$CONFIRMED!=0),], aes(x=DATE, y=(CONFIRMED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 confirmed cases Central America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/deaths_CentralAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_CA[which(defSet_selected_countries_CA$DEATHS!=0),], aes(x=DATE, y=(DEATHS), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 deaths Central America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
png("../images/recovered_CentralAmerica.png",width = 700, height = 400)
g <- ggplot(defSet_selected_countries_CA[which(defSet_selected_countries_CA$RECOVERED!=0),], aes(x=DATE, y=(RECOVERED), color=Country.Region)) 
g <- g + geom_line(size = 1) + geom_point(size = 2) 
g <- g + labs(color="",x = "", y = "") 
g <- g + theme_classic()
g <- g + ggtitle("COVID-19 recovered cases Central America") + theme(plot.title = element_text(hjust = 0.5))
g <- g + theme(axis.line = element_line(colour = 'black', size = 1),axis.text= element_text(face="bold", colour="black"),panel.grid.major = element_line(colour="grey", size = (0.5)))
#g <- g + theme(legend.position="top",panel.grid.major = element_line(colour="grey", size = (0.5)))
g <- g + scale_y_continuous(trans='log10')
#g <- g + scale_y_continuous(labels = function(x) paste0(x/1000, " k",sep=" "))
#g <- g + scale_x_continuous(labels = function(x) paste0("S ",x))
g
dev.off()
#
