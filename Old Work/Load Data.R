#library(lubridate)

for(i in c("2005_2014", 2015, 2016)) {
  wd=getwd()
  
  for(j in c("accidents", "casualties", "vehicles")) {
    file=paste(wd, "Data",paste0("DfTRoadSafety_", j, "_", i,".csv"), sep="/")
    assign(paste0(j, ".", i), read.csv(file, head=TRUE))
  }
}

accidents = rbind(accidents.2005_2014, accidents.2015, accidents.2016)
casualties = rbind(casualties.2005_2014, casualties.2015[,-16], casualties.2016[,-16])
vehicles = rbind(vehicles.2005_2014, vehicles.2015[,-23], vehicles.2016[,-23])

for(i in 2009:2016) {
  wd=getwd()
  file=paste(wd, "Data",paste0("DfTRoadSafety_MakeModel_", i,".csv"), sep="/")
  assign(paste0("MakeModel.", i), read.csv(file, head=TRUE))
}

file=paste(wd, "Data",paste0("DfTRoadSafety_MakeModel_2004_2008.csv"), sep="/")
MakeModel.2004_2008=read.csv(file, head=TRUE)

sum(names(MakeModel.2004_2008) != names(MakeModel.2009))
names(MakeModel.2004_2008)=names(MakeModel.2009)
names(MakeModel.2014)=names(MakeModel.2009)
names(MakeModel.2015)=names(MakeModel.2009)
names(MakeModel.2016)=names(MakeModel.2009)

MakeModel = rbind(MakeModel.2004_2008, MakeModel.2009, MakeModel.2010, MakeModel.2011, MakeModel.2012, MakeModel.2013, MakeModel.2014,
                  MakeModel.2015, MakeModel.2016)

#new = new.accidents[year(as.Date(new.accidents$Date, "%d/%m/%Y"))>2008,]
