library("forcats")
#This code will load the data, create factors, create features from vehicles and casualty files, and separate train/test/holdout.

#load(paste(getwd(),"datafiles","accidents.rda",sep="/"))


for(i in c("2005_2014", "2015", "2016")) {
  wd=wd=getwd()

  for(j in c("accidents", "casualties", "vehicles")) {
    file=paste(wd, "Data",paste0("DfTRoadSafety_", j, "_", i,".csv"), sep="/")
    assign(paste0(j, ".", i), read.csv(file, head=TRUE))
  }
}


colnames(accidents.2005_2014)[1] <- "Accident_Index"
colnames(casualties.2005_2014)[1] <- "Accident_Index"
colnames(vehicles.2005_2014)[1] <- "Accident_Index"

accidents = rbind(accidents.2005_2014, accidents.2015, accidents.2016)
casualties = rbind(casualties.2005_2014, casualties.2015[,-16], casualties.2016[,-16])
vehicles = rbind(vehicles.2005_2014, vehicles.2015[,-23], vehicles.2016[,-23])

# for(i in 2009:2016) {
#   wd=getwd()
#   file=paste(wd, "Data",paste0("DfTRoadSafety_MakeModel_", i,".csv"), sep="/")
#   assign(paste0("MakeModel.", i), read.csv(file, head=TRUE))
# }
# 
# file=paste(wd, "Data",paste0("DfTRoadSafety_MakeModel_2004_2008.csv"), sep="/")
# MakeModel.2004_2008=read.csv(file, head=TRUE)
# 
# sum(names(MakeModel.2004_2008) != names(MakeModel.2009))
# names(MakeModel.2004_2008)=names(MakeModel.2009)
# names(MakeModel.2010)=names(MakeModel.2009)
# names(MakeModel.2011)=names(MakeModel.2009)
# names(MakeModel.2012)=names(MakeModel.2009)
# names(MakeModel.2013)=names(MakeModel.2009)
# names(MakeModel.2014)=names(MakeModel.2009)
# names(MakeModel.2015)=names(MakeModel.2009)
# names(MakeModel.2016)=names(MakeModel.2009)
# 
# MakeModel = rbind(MakeModel.2004_2008, MakeModel.2009, MakeModel.2010, MakeModel.2011, MakeModel.2012, MakeModel.2013, MakeModel.2014,MakeModel.2015, MakeModel.2016)




#Clean up old files



rm(accidents.2005_2014)
rm(accidents.2015)
rm(accidents.2016)

rm(casualties.2005_2014)
rm(casualties.2015)
rm(casualties.2016)

rm(vehicles.2005_2014)
rm(vehicles.2015)
rm(vehicles.2016)

# rm(MakeModel.2004_2008)
# rm(MakeModel.2009)
# rm(MakeModel.2010)
# rm(MakeModel.2011)
# rm(MakeModel.2012)
# rm(MakeModel.2013)
# rm(MakeModel.2014)
# rm(MakeModel.2015)
# rm(MakeModel.2016)



##Start Data Cleaning


accidents$Date <- as.Date(accidents$Date, "%d/%m/%Y")
# Extract the month
accidents$month <- format(accidents$Date, format="%B")
accidents$month <- as.factor(accidents$month)
accidents$month <- factor(accidents$month,levels=month.name)
#summary(accidents$month)



# Extract the day of the week
accidents$day <- format(accidents$Date, format="%A")
accidents$day <- factor(accidents$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                        labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
#summary(accidents$day)



# Add an hour band variable
accidents$Time <- gsub("[ [:punct:]]", "" , accidents$Time) # remove punctuation from 'Time' variable
accidents$Time <- gsub("(\\d\\d)(\\d\\d)", "\\1:\\2", accidents$Time) # add a colon after 2nd value
accidents$hour<- as.POSIXlt(accidents$Time, format="%H:%M")$hour # add a new column called 'hour' with hours band





# Relabel the 'Accident_Severity' categories
accidents$sev <- factor(accidents$Accident_Severity,
                        levels= c(1,2,3),
                        labels= c("Fatal", "Serious", "Slight"))

#accidents$fatal <- fct_collapse(accidents$sev,
#                                Yes = c("Fatal"),
#                                No = c("Serious", "Slight")
#)
accidents$fatal <- factor(ifelse(accidents$sev=="Fatal",1,2), levels=c(1,2), labels=c("Yes","No"))

#accidents$Serious <- fct_collapse(accidents$sev,
#                                  Yes = c("Serious"),
#                                  No = c("Fatal", "Slight")
#)
accidents$Serious <- factor(ifelse(accidents$sev=="Serious",2,3), levels=c(2,3), labels=c("Yes","No"))

accidents$Slight <- fct_collapse(accidents$sev,
                                 Yes = c("Slight"),
                                 No = c("Serious", "Fatal")
)

accidents$Accident_Severity <- NULL



# Relabel the X1st_Road_Class
accidents$X1st <- factor(accidents$X1st_Road_Class,
                         levels= c(1,2,3, 4, 5, 6),
                         labels= c("Motorway", "A(M)", "A", "B", "C", "Unclassified"))
accidents$X1st_Road_Class <- NULL
##levels(accidents$X1st)



# Relabel the Road_Type
accidents$RdType <- factor(accidents$Road_Type,
                           levels= c(1,2,3,6,7,9,12,-1),
                           labels= c("Roundabout", "One way street", "Dual carriageway", "Single carriageway", "Slip road", "Unknown", "One way street/Slip Road", "Data missing or out of range"))
accidents$Road_Type <- NULL
##levels(accidents$RdType)



# Relabel the Junction_Detail
accidents$JuncDetail <- factor(accidents$Junction_Detail,
                               levels= c(0,1,2,3,5,6,7,8,9,-1),
                               labels= c("Not at junction or within 20 metres", "Roundabout", "Mini-roundabout", "T or staggered junction", "Slip road", "Crossroads", "More than 4 arms (not roundabout)", "Private drive or entrance", "Other junction", "Data missing or out of range"))
accidents$Junction_Detail <- NULL
##levels(accidents$JuncDetail)



# Relabel the Junction_Control
accidents$JuncCtrl <- factor(accidents$Junction_Control,
                             levels= c(0,1,2,3,4,-1),
                             labels= c("Not at junction or within 20 metres", "Authorised person", "Auto traffic signal", "Stop sign", "Give way or uncontrolled", "Data missing or out of range"))
accidents$Junction_Control <- NULL
##levels(accidents$JuncCtrl)




# Relabel the X2nd_Road_Class
accidents$X2nd <- factor(accidents$X2nd_Road_Class,
                         levels= c(1,2,3, 4, 5, 6),
                         labels= c("Motorway", "A(M)", "A", "B", "C", "Unclassified"))
accidents$X2nd_Road_Class <- NULL
##levels(accidents$X2nd)




# Relabel the Pedestrian_Crossing.Human_Control
accidents$PedXingHC <- factor(accidents$Pedestrian_Crossing.Human_Control,
                              levels= c(0,1,2,-1),
                              labels= c("None within 50 metres", "Control by school crossing patrol", "Control by other authorised person", "Data missing or out of range"))
accidents$Pedestrian_Crossing.Human_Control <- NULL
##levels(accidents$PedXingHC)




# Relabel the Pedestrian_Crossing.Physical_Facilities
accidents$PedXingPF <- factor(accidents$Pedestrian_Crossing.Physical_Facilities,
                              levels= c(0,1,4,5,7,8,-1),
                              labels= c("No physical crossing facilities within 50 metres", "Zebra", "Pelican, puffin, toucan or similar non-junction pedestrian light crossing", "Pedestrian phase at traffic signal junction", "Footbridge or subway", "Central refuge", "Data missing or out of range"))
accidents$Pedestrian_Crossing.Physical_Facilities <- NULL
##levels(accidents$PedXingPF)



# Relabel the Light_Conditions
accidents$LightCond <- factor(accidents$Light_Conditions,
                              levels= c(1,4,5,6,7,-1),
                              labels= c("Daylight", "Darkness - lights lit", "Darkness - lights unlit", "Darkness - no lighting", "Darkness - lighting unknown", "Data missing or out of range"))
accidents$Light_Conditions <- NULL
###fct_count(accidents$LightCond)

accidents$Light <- fct_collapse(accidents$LightCond,
                                Yes = c("Daylight", "Darkness - lights lit"),
                                No = c("Darkness - lights unlit", "Darkness - no lighting", "Darkness - lighting unknown", "Data missing or out of range"),
                                UNK = c("Data missing or out of range"))
accidents$LightCond <- NULL
##fct_count(accidents$Light)





# Relabel the Weather_Conditions
accidents$WeathCond <- factor(accidents$Weather_Conditions,
                              levels= c(1,2,3,4,5,6,7,8,9,-1),
                              labels= c("Fine no high winds", "Raining no high winds", "Snowing no high winds", "Fine + high winds", "Raining + high winds", "Snowing + high winds", "Fog or mist", "Other", "Unknown", "Data missing or out of range"))
accidents$Weather_Conditions <- NULL
##levels(accidents$WeathCond)

accidents$Wind <- fct_collapse(accidents$WeathCond,
                               Yes = c("Fine + high winds", "Raining + high winds", "Snowing + high winds"),
                               No = c("Fine no high winds", "Raining no high winds", "Snowing no high winds", "Fog or mist", "Other"),
                               UNK = c("Unknown", "Data missing or out of range"))
##fct_count(accidents$Wind)


accidents$Fine <- fct_collapse(accidents$WeathCond,
                               Yes = c("Fine no high winds","Fine + high winds"),
                               No = c("Raining no high winds", "Snowing no high winds", "Raining + high winds", "Snowing + high winds", "Fog or mist", "Other"),
                               UNK = c("Unknown", "Data missing or out of range"))
##fct_count(accidents$Fine)

accidents$Rain <- fct_collapse(accidents$WeathCond,
                               Yes = c("Raining no high winds","Raining + high winds"),
                               No = c("Fine no high winds","Snowing no high winds", "Fine + high winds", "Snowing + high winds", "Fog or mist", "Other"),
                               UNK = c("Unknown", "Data missing or out of range"))
##fct_count(accidents$Rain)

accidents$Snow <- fct_collapse(accidents$WeathCond,
                               Yes = c("Snowing no high winds", "Snowing + high winds"),
                               No = c("Fine no high winds", "Raining no high winds", "Fine + high winds", "Raining + high winds", "Fog or mist", "Other"),
                               UNK = c("Unknown", "Data missing or out of range"))
##fct_count(accidents$Snow)

accidents$Fog <- fct_collapse(accidents$WeathCond,
                              Yes = c("Fog or mist"),
                              No = c("Fine no high winds", "Raining no high winds", "Snowing no high winds", "Fine + high winds", "Raining + high winds", "Snowing + high winds", "Other"),
                              UNK = c("Unknown", "Data missing or out of range"))
accidents$WeathCond <- NULL
##fct_count(accidents$Fog)





# Relabel the Road_Surface_Condition
accidents$RoadSurface <- factor(accidents$Road_Surface_Condition,
                                levels= c(1,2,3,4,5,6,7,-1),
                                labels= c("Dry", "Wet or damp", "Snow", "Frost or ice", "Flood over 3cm deep", "Oil or diesel", "Mud", "Data missing or out of range"))
accidents$Road_Surface_Condition <- NULL
##levels(accidents$RoadSurface)

accidents$DryRd <- fct_collapse(accidents$RoadSurface,
                                Yes = c("Dry"),
                                No = c("Wet or damp", "Snow", "Frost or ice", "Flood over 3cm deep", "Oil or diesel", "Mud"),
                                UNK = c("Data missing or out of range")
)
##fct_count(accidents$DryRd)

accidents$WetRd <- fct_collapse(accidents$RoadSurface,
                                Yes = c("Wet or damp"),
                                No = c("Dry", "Snow", "Frost or ice", "Flood over 3cm deep", "Oil or diesel", "Mud"),
                                UNK = c("Data missing or out of range")
)
##fct_count(accidents$WetRd)

accidents$SnowRd <- fct_collapse(accidents$RoadSurface,
                                 Yes = c("Snow"),
                                 No = c("Dry", "Wet or damp", "Frost or ice", "Flood over 3cm deep", "Oil or diesel", "Mud"),
                                 UNK = c("Data missing or out of range")
)
##fct_count(accidents$SnowRd)

accidents$IceRd <- fct_collapse(accidents$RoadSurface,
                                Yes = c("Frost or ice"),
                                No = c("Dry", "Wet or damp", "Snow", "Flood over 3cm deep", "Oil or diesel", "Mud"),
                                UNK = c("Data missing or out of range")
)
##fct_count(accidents$IceRd)

accidents$FloodRd <- fct_collapse(accidents$RoadSurface,
                                  Yes = c("Flood over 3cm deep"),
                                  No = c("Dry", "Wet or damp", "Snow", "Frost or ice", "Oil or diesel", "Mud"),
                                  UNK = c("Data missing or out of range")
)
#fct_count(accidents$FloodRd)

accidents$OilRd <- fct_collapse(accidents$RoadSurface,
                                Yes = c("Oil or diesel"),
                                No = c("Dry", "Wet or damp", "Snow", "Frost or ice", "Flood over 3cm deep", "Mud"),
                                UNK = c("Data missing or out of range")
)
#fct_count(accidents$OilRd)

accidents$MudRd <- fct_collapse(accidents$RoadSurface,
                                Yes = c("Mud"),
                                No = c("Dry", "Wet or damp", "Snow", "Frost or ice", "Flood over 3cm deep", "Oil or diesel"),
                                UNK = c("Data missing or out of range")
)
accidents$RoadSurface <- NULL
#fct_count(accidents$MudRd)







# Relabel the Special_Conditions_at_Site
accidents$SpecialCond <- factor(accidents$Special_Conditions_at_Site,
                                levels= c(0,1,2,3,4,5,6,7,-1),
                                labels= c("None", "Auto traffic signal - out", "Auto signal part defective", "Road sign or marking defective or obscured", "Roadworks", "Road surface defective", "Oil or diesel", "Mud", "Data missing or out of range"))
accidents$Special_Conditions_at_Site <- NULL
#fct_count(accidents$SpecialCond)

accidents$SiteIssue_Signal <- fct_collapse(accidents$SpecialCond,
                                           Yes = c("Auto traffic signal - out", "Auto signal part defective"),
                                           No = c("None", "Road sign or marking defective or obscured", "Roadworks", "Road surface defective", "Oil or diesel", "Mud"),
                                           UNK = c("Data missing or out of range"))

accidents$SiteIssue_Sign <- fct_collapse(accidents$SpecialCond,
                                         Yes = c("Road sign or marking defective or obscured"),
                                         No = c("None", "Auto traffic signal - out", "Auto signal part defective", "Roadworks", "Road surface defective", "Oil or diesel", "Mud"),
                                         UNK = c("Data missing or out of range"))

accidents$SiteIssue_Construction <- fct_collapse(accidents$SpecialCond,
                                                 Yes = c("Roadworks"),
                                                 No = c("None", "Auto traffic signal - out", "Auto signal part defective", "Road sign or marking defective or obscured", "Road surface defective", "Oil or diesel", "Mud"),
                                                 UNK = c("Data missing or out of range"))

accidents$SiteIssue_PotHole <- fct_collapse(accidents$SpecialCond,
                                            Yes = c("Road surface defective"),
                                            No = c("None", "Auto traffic signal - out", "Auto signal part defective", "Road sign or marking defective or obscured", "Roadworks", "Oil or diesel", "Mud"),
                                            UNK = c("Data missing or out of range"))

accidents$SiteIssue_Mud <- fct_collapse(accidents$SpecialCond,
                                        Yes = c("Mud"),
                                        No = c("None", "Auto traffic signal - out", "Auto signal part defective", "Road sign or marking defective or obscured", "Roadworks", "Road surface defective", "Oil or diesel"),
                                        UNK = c("Data missing or out of range"))

accidents$SiteIssue_Oil <- fct_collapse(accidents$SpecialCond,
                                        Yes = c("Oil or diesel"),
                                        No = c("None", "Auto traffic signal - out", "Auto signal part defective", "Road sign or marking defective or obscured", "Roadworks", "Road surface defective", "Mud"),
                                        UNK = c("Data missing or out of range"))
accidents$SpecialCond <- NULL





# Relabel the Carriageway_Hazards
accidents$Hazards <- factor(accidents$Carriageway_Hazards,
                            levels= c(0,1,2,3,4,5,6,7,-1),
                            labels= c("None","Vehicle load on road", "Other object on road", "Previous accident", "Dog on road", "Other animal on road", "Pedestrian in carriageway - not injured", "Any animal in carriageway (except ridden horse)", "Data missing or out of range"))
accidents$Carriageway_Hazards <- NULL
#fct_count(accidents$Hazards)

accidents$Hazard_Animal <- fct_collapse(accidents$Hazards,
                                        Yes = c("Dog on road", "Other animal on road", "Any animal in carriageway (except ridden horse)"),
                                        No = c("None","Vehicle load on road", "Other object on road", "Previous accident", "Pedestrian in carriageway - not injured"),
                                        UNK = c("Data missing or out of range"))

accidents$Hazard_Ped <- fct_collapse(accidents$Hazards,
                                     Yes = c("Pedestrian in carriageway - not injured"),
                                     No = c("None","Vehicle load on road", "Other object on road", "Previous accident", "Dog on road", "Other animal on road", "Any animal in carriageway (except ridden horse)"),
                                     UNK = c("Data missing or out of range"))

accidents$Hazard_Object <- fct_collapse(accidents$Hazards,
                                        Yes = c("Vehicle load on road", "Other object on road"),
                                        No = c("None","Previous accident", "Dog on road", "Other animal on road", "Pedestrian in carriageway - not injured", "Any animal in carriageway (except ridden horse)"),
                                        UNK = c("Data missing or out of range"))

accidents$Hazard_PrevAcc <- fct_collapse(accidents$Hazards,
                                         Yes = c("Previous accident"),
                                         No = c("None","Vehicle load on road", "Other object on road", "Dog on road", "Other animal on road", "Pedestrian in carriageway - not injured", "Any animal in carriageway (except ridden horse)"),
                                         UNK = c("Data missing or out of range"))
accidents$Hazards <- NULL
#fct_count(accidents$Hazard_PrevAcc)






# Relabel the Urban_or_Rural_Area
accidents$UrbanRural <- factor(accidents$Urban_or_Rural_Area,
                               levels= c(1,2,3),
                               labels= c("Urban", "Rural", "Unallocated"))
accidents$Urban_or_Rural_Area <- NULL
#levels(accidents$UrbanRural)



#Factor creation fromthe vehicle file



#Pedal cycle, Motorcycle (all sizes), Electric motorcycle
vehicles$vehMotoInd <- as.numeric(vehicles$Vehicle_Type %in% c(1,2,3,4,5,23,97))
#Tram
vehicles$vehTramInd <- as.numeric(vehicles$Vehicle_Type %in% c(18))
#All towing except None and Unknown
vehicles$vehTowingInd <- as.numeric(vehicles$Towing_and_Articulation %in% c(1,2,3,4,5))
#U-turn, Turning Left, Turning Right
vehicles$vehTurningInd <- as.numeric(vehicles$Vehicle_Manoeuvre %in% c(6,7,9))
#Parked, Waiting to go - held up, Slowing or stopping, Waiting to turn left, Waiting to turn left, Waiting to turn right
vehicles$vehStoppedInd <- as.numeric(vehicles$Vehicle_Manoeuvre %in% c(2,3,4,8,10))
#All skidding and overturning except None and Missing
vehicles$vehSkiddingInd <- as.numeric(vehicles$Skidding_and_Overturning %in% c(1,2,3,4,5))
#All skidding and overturning except None and Missing
vehicles$vehSkiddingInd <- as.numeric(vehicles$Skidding_and_Overturning %in% c(1,2,3,4,5))
#All vehicles leaving carriageway
vehicles$vehLeavingInd <- as.numeric(vehicles$Vehicle_Leaving_Carriageway %in% c(1,2,3,4,5,6,7,8))
#Front point of impact
vehicles$vehFrontImpInd <- as.numeric(vehicles$X1st_Point_of_Impact %in% c(1))
#Front point of impact
vehicles$vehBackImpInd <- as.numeric(vehicles$X1st_Point_of_Impact %in% c(2))
#Front point of impact
vehicles$vehSideImpInd <- as.numeric(vehicles$X1st_Point_of_Impact %in% c(3,4))
#Was Vehicle Left Hand Drive
vehicles$vehLeftDrInd <- as.numeric(vehicles$Was_Vehicle_Left_Hand_Drive. %in% c(3,4))

library("dplyr")

#summarize the vehicle file by Accident_Index and create an index per accident using max
veh.temp <- group_by(vehicles, Accident_Index)
vehicle.lookup <- summarize(veh.temp, vehMotoInd = max(vehMotoInd), vehTramInd = max(vehTramInd),
                            vehTowingInd=max(vehTowingInd), vehTurningInd=max(vehTurningInd),
                            vehStoppedInd=max(vehStoppedInd), vehSkiddingInd=max(vehSkiddingInd),
                            vehLeavingInd=max(vehLeavingInd), vehFrontImpInd=max(vehFrontImpInd),
                            vehBackImpInd=max(vehBackImpInd), vehSideImpInd=max(vehSideImpInd),
                            vehLeftDrInd=max(vehLeftDrInd))
#head(vehicle.lookup)

#create factor versions of variables
vehicle.lookup$vehMotoInd <- factor(vehicle.lookup$vehMotoInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehTramInd <- factor(vehicle.lookup$vehTramInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehTowingInd <- factor(vehicle.lookup$vehTowingInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehTurningInd <- factor(vehicle.lookup$vehTurningInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehStoppedInd <- factor(vehicle.lookup$vehStoppedInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehSkiddingInd <- factor(vehicle.lookup$vehSkiddingInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehLeavingInd <- factor(vehicle.lookup$vehLeavingInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehFrontImpInd <- factor(vehicle.lookup$vehFrontImpInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehBackImpInd <- factor(vehicle.lookup$vehBackImpInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehSideImpInd <- factor(vehicle.lookup$vehSideImpInd,levels= c(0,1),labels= c("None", "One+"))
vehicle.lookup$vehLeftDrInd <- factor(vehicle.lookup$vehLeftDrInd,levels= c(0,1),labels= c("None", "One+"))


#left join on accidents file to pull in all of the vehicle indicators
accidents <- merge(accidents,vehicle.lookup,by="Accident_Index", all.x=TRUE)

#Clean up temporary data frames
rm(vehicle.lookup)
rm(veh.temp)


casualty.lookup=casualties%>%
group_by(Accident_Index)%>%
mutate(child_in_casualties=min(ifelse(Age_of_Casualty<0,3,ifelse(Age_of_Casualty<18,1,2))))%>%
distinct(Accident_Index, child_in_casualties)

casualty.lookup$child_in_casualties=factor(casualty.lookup$child_in_casualties, levels=c(1,2,3), labels=c("YES","NO","UNK"))
#fct_count(casualty.lookup$child_in_casualties)

#left join on accidents file to pull in casualty indicators
accidents <- merge(accidents,casualty.lookup,by="Accident_Index", all.x=TRUE)

#Clean up temporary data frames
rm(casualty.lookup)

#create holdout data, test data, and train data
holdout.data=accidents[accidents$Date>='2015-01-01',]
non.holdout=accidents[accidents$Date<'2015-01-01',]

set.seed(123)
train = sample(1:nrow(non.holdout),nrow(non.holdout)*.7,rep=FALSE) #train on a 70% sample

train.data = non.holdout[train,]
test.data = non.holdout[-train,]
rm(non.holdout)
rm(casualties)
rm(vehicles)
#names(accidents)

saveRDS(train.data, file = "datafiles/train.data.rds")
saveRDS(test.data, file = "datafiles/test.data.rds")
saveRDS(holdout.data, file = "datafiles/holdout.data.rds")