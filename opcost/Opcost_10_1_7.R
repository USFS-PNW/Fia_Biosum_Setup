# OpCost 10.1.7 July 4, 2026
# Current code maintainers: Jeremy Fried of USDA Forest Service PNW Research Station and Daniel Swann, Oak Ridge Institute for Science Education
#
# Today’s OpCost consists of an R script, OpCost_10_1_7.R, tightly integrated with an SQLite database, opcost_ref.db, that contains all of the harvest system definitions, 
# harvest machine equations and unit costs. It is designed and maintained for use as part of the BioSum modeling framework for exploring biophysical and economic outcomes
# and implications of alternative forest management trajectories. OpCost is open source, available for customization and improvement, and it is the responsibility of users
# to determine its fitness of purpose for their intended use—no warranties for this code are claimed.
#
# The foundation of this code was written by research specialist Carlin Starrs in 2018 while at University of California, Berkeley. 
# It was a complete re-write of code begun by Conor Bell, while he was research assistant at University of Idaho circa 2014.
#
# OpCost was inspired by and builds on the ideas in the Fuel Reduction Cost  Simulator and STHARVEST Excel macro-based analysis frameworks for estimating logging costs for fuel treatment,
# developed by Roger Fight, Bruce Hartsough, Peter Noordijk and others in the early 2000s.
#
# Several BioSum analysts have stepped up to update, repair and enhance OpCost over the years:
#
#   Sara Loreno, natural resource analyst at Ecotrust maintained and enhanced the code from 2018-2019.
#
#   Jeremy Fried, research forester at USDA Forest Service PNW Research Station, updated the code circa 2019 to add a tethered harvesting system based on the work of Joshua Petitmermet.
#
#   Sebastian Busby, research fellow with Oak Ridge Institute for Science Education, updated code in 2023 to read from and write to SQLite databases when BioSum transitioned away from MS Access circa 2023-2026
#
#   Daniel Swann, research fellow with Oak Ridge Institute for Science Education, updated code in 2026 to address:
#     1.	significant bugs with respect to how estimates of different equations and harvest tree size classes were combined that caused sometimes drastic underestimates of operations costs;
#     2.	corrections needed for representations of some machine equations and dropping of a few that were not appropriate for the harvest systems for which they were defined;
#     3.	planning for a future version (11.0.0) that will interface differently with BioSum, in terms of parameters passed, to more accurately reflect the volume and biomass processing demands,
#         in terms of harvest machine time, in the modeling of operations cost.
#
# Much appreciation is owed to analysts who took on the responsibility for maintaining and extending the capabilities of OpCost.
#
# Documentation of OpCost and how it can be customized is available as part of the BioSum Users Guide available online at www.biosum.info.
#
# A published reference that describes an outdated version of this script can be found at:
# Conor K. Bell; Robert F. Keefe; Jeremy S. Fried. 2017. OpCost: an open-source
# system for estimating costs of stand-level forest operations. Gen. Tech. Rep.
# PNW-GTR-960. Portland, OR: USDA For. Serv. PN Res. Sta. 23 p.  https://research.fs.usda.gov/treesearch/55324
#
# Note that the stand-alone operation described in that reference, which
# makes use of the R-shiny interface, is not operational in this version, or any version since ~2018.
# Note: This code depends on the RSQLite package which is currently available only
# for R version 4.0.0 or better
#
#
#####Initial package loading
#####Automatically install if package missing
#
#remove.packages("RSQLite")

packages = "RSQLite"
package_check <- require(RSQLite)

if (package_check == "FALSE") {
 install.packages(packages, type = "binary", repos="http://cran.r-project.org", dependencies = TRUE)
  } else {
    print("RSQLite is already installed")
}

require(RSQLite)


###################### STAND ALONE PATH DIRECTORIES ################################
#These will be used if no pathways are provided from the command line (args string length = 0)

opcost.ref <- "C:/Users/DanielSwann/Box/BioSumBox/Development/OpCost2018/2026_update/R_and_DB_changes/opcost_ref_DSedits.db" #set the location of the opcost_ref.db you'd like to use
opcost.input <- "C:/Users/DanielSwann/Box/BioSumBox/Development/OpCost2018/2026_update/R_and_DB_changes/CR_P001_20260121_1110_10_1_6.db" #input database

###set the output location database
opcost.output <- "C:/Users/DanielSwann/Box/BioSumBox/Development/OpCost2018/2026_update/R_and_DB_changes/CR_P001_20260121_1110_10_1_6_output.db"

###set output for the graphs (optional)
#graph.output <- "C:/Users/SebastianBusby/Desktop/OpCost Update SQLite/graph/"


#####################################################################################
##LOAD DATA


##############OLD MS Access#####################
{
#args=(commandArgs(TRUE))

#print(args[1])

#if (length(args)>0) {
#  con<-odbcConnectAccess2007(args[1])
#  print("odbc Connection:OK")
#  m<-data.frame(sqlFetch(con, "opcost_input", as.is=TRUE))
#  print("m data.frame opcost_input SqlFetch:OK")
  
#  odbcCloseAll()
  
#  ref2 <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", args[2])
  
#  con2 <- odbcDriverConnect(ref2)
  
#  opcost_equation_ref<- sqlFetch(con2, "opcost_equation_ref", as.is = TRUE)
  
#  opcost_cost_ref <- sqlFetch(con2, "opcost_cost_ref", as.is = TRUE)
  
#  opcost_harvestequation_ref <- sqlFetch(con2, "opcost_harvestequation_ref", as.is = TRUE)
  
#  opcost_harvestsystem_ref <- sqlFetch(con2, "opcost_harvestsystem_ref", as.is = TRUE)
  
#  odbcCloseAll()
  
####
}

##############NEW SQLite#######################
  
  #Sets the driver type
  sqlite <- dbDriver("SQLite")
  
  ##This is used to pass R the file name / location string via command line
  ##The string needs to be inside parentheses to work properly e.g: "C:/pathway"
  args=(commandArgs(TRUE))
  print("pathway to input database")
  print(args[1])
  print("pathway to ref database")
  print(args[2])
  
  #uses command line provided pathway to connect to Opcost input database
  #writes data table "opcost_input" to new dataframe called "m"
  
  if (length(args)>0) {
    con<-dbConnect(sqlite,args[1]) 
    print("odbc Connection:OK")
    m<-data.frame(dbReadTable(con, "opcost_input", as.is=TRUE))
    print("m data.frame opcost_input SqlFetch:OK")
    
    dbDisconnect(con)#
    
 #uses command line provided pathway to connect to Opcost ref database, writes ref tables to new dataframes
    con2 <- dbConnect(sqlite,args[2]) 
 
    
    opcost_equation_ref<- dbReadTable(con2, "opcost_equation_ref", as.is = TRUE)
    
    opcost_cost_ref <- dbReadTable(con2, "opcost_cost_ref", as.is = TRUE)
    
    opcost_harvestequation_ref <- dbReadTable(con2, "opcost_harvestequation_ref", as.is = TRUE)
    
    opcost_harvestsystem_ref <- dbReadTable(con2, "opcost_harvestsystem_ref", as.is = TRUE)
    
    dbDisconnect(con2)
  
    #### If no pathway is passed from command line (arg string length = 0), loads the data instead from pathways defined by the user above
} else {

  opcost.ref.location <- opcost.ref
  opcost.input.location <- opcost.input

  ###set the output location database
  opcost.output.location <-opcost.output

  ###set output for the graphs (optional)
  #graph.directory <- graph.output
  
  #Connect to the Opcost_Input 
  conn <- dbConnect(sqlite, opcost.input.location)
  m<-data.frame(dbReadTable(conn, "opcost_input", as.is=TRUE))

  dbDisconnect(conn)

  #Connect to Opcost_Ref database using
  conn <- dbConnect(sqlite, opcost.ref.location)

  opcost_equation_ref<- dbReadTable(conn, "opcost_equation_ref", as.is = TRUE)

  opcost_cost_ref <- dbReadTable(conn, "opcost_cost_ref", as.is = TRUE)

  opcost_harvestequation_ref <- dbReadTable(conn, "opcost_harvestequation_ref", as.is = TRUE)

  opcost_harvestsystem_ref <- dbReadTable(conn, "opcost_harvestsystem_ref", as.is = TRUE)

  dbDisconnect(conn)
}


#####################################################################################
##START CSV LOAD BLOCK
#####BRING IN REFERENCE TABLES -- 
#THIS IS FOR IF YOU'RE NOT USING THE ACCESS DATABASE VERSION ABOVE AND NEED TO BRING IN THE CSVS######
# setwd("G:/Dropbox/Carlin/GitHub/Fia_Biosum_Scripts/OPCOST")
# opcost_equation_ref <- read.csv("opcost_equation_ref.csv")
# opcost_units <- read.csv("opcost_units.csv")
# opcost_cost_ref <- read.csv("opcost_cost_ref.csv")
# opcost_harvestsystem_ref <- read.csv("opcost_harvestsystem_ref.csv")
# opcost_ideal_ref <- read.csv("opcost_ideal_ref.csv")
##END CSV LOAD BLOCK
#####################################################################################

#Set "NaN' to NA
m[m == "NaN"] <- NA #convert "NaN" values to NA


####Calculate additional variables

#Average twitchVol (merchantable volume) in feet or meters cubed
m$twitchVol_ct_ft <- ((m$Chip.trees.MerchAsPctOfTotal/100)*m$Chip.trees.average.volume.ft3.)
m$twitchVol_ct <- ((m$Chip.trees.MerchAsPctOfTotal/100)*m$Chip.trees.average.volume.ft3.)*.0283168

m$twitchVol_sl_ft <- ((m$Small.log.trees.MerchAsPctOfTotal/100)*m$Small.log.trees.average.volume.ft3.)
m$twitchVol_sl <- ((m$Small.log.trees.MerchAsPctOfTotal/100)*m$Small.log.trees.average.volume.ft3.)*.0283168

m$twitchVol_ll_ft <- ((m$Large.log.trees.MerchAsPctOfTotal/100)*m$Large.log.trees.average.vol.ft3.)
m$twitchVol_ll <- ((m$Large.log.trees.MerchAsPctOfTotal/100)*m$Large.log.trees.average.vol.ft3.)*.0283168

#2026 Propose eliminating the following block and instead incorporating tpa in the opcost eqn table where needed
#e.g., HAR04S Hours per acre conversion column changes from totalVol_sl/result to (twitchVol_sl*sl_tpa)/result 
#PROVIDED THAT HAR04S is actually intended to use merch vol not total vol


#totalVol is the (Trees Per Acre * Average Tree Volume (ft3)))
#calculate for all size bins
#convert to metric
m$totalVol_ll <- (m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3.)* 0.0283168
m$totalVol_sl <- (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.)* 0.0283168
m$totalVol_ct <- (m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3.)* 0.0283168
m$totalVol <- (m$totalVol_ll + m$totalVol_sl + m$totalVol_ct)

#2026 would like to replace this with QMD_CT calculated and passed as a variable by BioSum
#then remove row below and sub QMD_CT for dbh_ct wherever it appears in OpCost
#dbh calculated from twitchVol
#calculate for chip trees
# m$dbh_ct <- sqrt(((m$Chip.trees.average.volume.ft3.) + 8.4166)/.2679)
# Temporary replacement function to predict QMD for chip trees based on average total volume
m$dbh_ct <- 3.87*(m$Chip.trees.average.volume.ft3.^0.3547)

#distBetween Trees is calculated by taking all size classes trees per acre (feet)
m$distBetweenTrees <- (sqrt((43560/(m$Small.log.trees.per.acre+ m$Large.log.trees.per.acre+ m$Chip.tree.per.acre))/pi))*2 
m$distBetweenTrees_ll <- (sqrt((43560/(m$Large.log.trees.per.acre))/pi))*2 
m$distBetweenTrees[m$Large.log.trees.per.acre == 0 & m$Small.log.trees.per.acre == 0 &  m$Chip.tree.per.acre == 0] <- NA
m$distBetweenTrees_ll[m$Large.log.trees.per.acre == 0] <- NA

#2026 Unclear to JSF what these is.na statements do
#2026 DS comment: They set all instances of NA for log.trees.average.density.lbs.ft3. to 0, 
# but I feel like this is unnecessary, my 4FRI dataset has no NAs for these vars as it seems like BioSum
# is already setting any NAs to 0 in the OpCost input table before it's passed to OpCost
#totalWeight is average of non-zero large and small log density values (lbs/ft3) * totalVol (ft3/acre) to get (lbs/acre)
is.na(m$Large.log.trees.average.density.lbs.ft3.) <- m$Large.log.trees.average.density.lbs.ft3.==0
is.na(m$Small.log.trees.average.density.lbs.ft3.) <- m$Small.log.trees.average.density.lbs.ft3.==0

#2026 remove the all 4 lines in this block-- 1st 3 are never used except for to calculate total weight-- 
#totalWeight calculation here is incorrect (multiplies small log cu ft and large log average density)
#let BioSum calculate and pass MerchWtLbs_SL and MerchWtLbs_CT and MerchWtLbs_LL (needed for Tethered anyway)
#and then can calculate totalWeight inline in the equationref as the sum of these for helicopter systems, 
#which is the only usage for this in pounds of ALL boles to be harvested, including bark
#2026 DS comment: Makes sense to me, I think for the 1st round of this though, bark will not be on the boles as that won't be split out yet until TVBC step 2.

m$MerchWtLbs_SL <- m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.*m$Small.log.trees.average.density.lbs.ft3. * (m$Small.log.trees.MerchAsPctOfTotal/100)
m$MerchWtLbs_CT <- m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3. * m$CHIPS.Average.Density..lbs.ft3. * (m$Chip.trees.MerchAsPctOfTotal/100)
m$MerchWtLbs_LL <- m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3. * (m$Large.log.trees.MerchAsPctOfTotal/100)

m$TotTreeWtLbs_SL <- m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3.*m$Small.log.trees.average.density.lbs.ft3.
m$TotTreeWtLbs_CT <- m$Chip.tree.per.acre * m$Chip.trees.average.volume.ft3. * m$CHIPS.Average.Density..lbs.ft3.
m$TotTreeWtLbs_LL <- m$Large.log.trees.per.acre * m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3.



#does this replace any NAs with zeros?
#2026 DS comment: yes
m[is.na(m)] <- 0

#ChipFeedstockWeight is sum of trees per acre of small logs and large logs assigned to the chip bine times volume and density

#2026 This should definitely be calculated and passed by BioSum as weight of percent of SL and LL bole weight that is specified for bioenergy
#i.e., tops, and the branches if in a harvest system that collects those, and 100% of the cull trees
#that could be passed as this name
#2026 DS comment: Makes sense to me
m$ChipFeedstockWeight_CT  <- ifelse(grepl("WT", m$Harvesting.System), 
                                    m$ChipFeedstockWeight_CT <- (m$Chip.tree.per.acre * 
                                                                m$Chip.trees.average.volume.ft3. * m$CHIPS.Average.Density..lbs.ft3.),
                                    m$ChipFeedstockWeight_CT <- (m$Chip.tree.per.acre * (m$Chip.trees.MerchAsPctOfTotal/100) * 
                                                                m$Chip.trees.average.volume.ft3. * m$CHIPS.Average.Density..lbs.ft3.))

m$ChipFeedstockWeight_SL  <- ifelse(grepl("WT", m$Harvesting.System), 
                                 m$ChipFeedstockWeight_SL <- (m$Small.log.trees.per.acre * (m$Small.log.trees.ChipPct_Cat2_4/100) * 
                                                             m$Small.log.trees.average.volume.ft3. * m$Small.log.trees.average.density.lbs.ft3.),
                                 m$ChipFeedstockWeight_SL <- (m$Small.log.trees.per.acre * (m$Small.log.trees.ChipPct_Cat1_3/100) * 
                                                             m$Small.log.trees.average.volume.ft3. * m$Small.log.trees.average.density.lbs.ft3.))

m$ChipFeedstockWeight_LL  <- ifelse(grepl("WT", m$Harvesting.System), 
                                    m$ChipFeedstockWeight_LL <- (m$Large.log.trees.per.acre * (m$Large.log.trees.ChipPct_Cat2/100) * 
                                                                m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3.),
                                    m$ChipFeedstockWeight_LL <- (m$Large.log.trees.per.acre * (m$Large.log.trees.ChipPct_Cat1_3_4/100) * 
                                                                m$Large.log.trees.average.vol.ft3. * m$Large.log.trees.average.density.lbs.ft3.))

###Tethered Harvester equations - see Petitmermet harvest cost model doc for reference

#2026 AS stated several lines above, Let BioSum calculate and pass MerchWtLbs_SL and MerchWtLbs_CT and MerchWtLbs_LL
#Tethered total weight in lbm converted to tonnes
# m$TetheredTotalWeight <- (m$Small.log.trees.per.acre * m$Small.log.trees.average.volume.ft3. * m$Small.log.trees.average.density.lbs.ft3.)*0.0005

m$MerchWtTonnes_ct <- 0.000453592 * m$MerchWtLbs_CT
m$MerchWtTonnes_sl <- 0.000453592 * m$MerchWtLbs_SL
m$MerchWtTonnes_ll <- 0.000453592 * m$MerchWtLbs_LL


m$TotTreeTonnes_ct <- 0.000453592 * m$TotTreeWtLbs_CT
m$TotTreeTonnes_sl <- 0.000453592 * m$TotTreeWtLbs_SL
m$TotTreeTonnes_ll <- 0.000453592 * m$TotTreeWtLbs_LL

# m$MerchWtLbs_Harvester <- m$MerchWtLbs_CT + m$MerchWtLbs_SL
# m$MerchWtTonnes_Harvester <- 0.000453592 * m$MerchWtLbs_Harvester

#2026 change to m$MerchWtTonnes_Harvester <- 0.000453592 * (m$MerchWtLbs_CT + m$MerchWtLbs_SL)
#2026 drop m$MerchWtLbs_Harvester assignment (not needed or used)

#2026 MerchWtLbs_LL will now be calculated by BioSum and passed so can drop that assignment row below
###Tethered Forwarder Equations - see Petitmermet harvest cost model doc for reference
#2026 DS comment: MerchWtTonnes_Forwarder isn't used in the tethered models it would seem, is that what you see too?
# it looks like that variable only gets used in the SKID_08 hours per acre conversion, which is only used in ground-based mechanized or manual WT or log.
#does that mean it should be calculated earlier to avoid confusion, or folded into eqn_ref? 

# m$MerchWtTonnes_Forwarder_ct <- m$MerchWtLbs_CT * 0.000453592
# m$MerchWtTonnes_Forwarder_sl <- m$MerchWtLbs_SL * 0.000453592
# m$MerchWtTonnes_Forwarder_ll <- m$MerchWtLbs_LL * 0.000453592

#2026  Variable calculated in line below appears never to be used
#2026 DS comment: I can see no usage for this either
# m$TPA_CSL <- m$Small.log.trees.per.acre + m$Large.log.trees.per.acre + m$Chip.tree.per.acre

#2026 combine into one assignment as MerchWtLbs_Loader is never used elsewhere, or work into equation_ref?
# m$MerchWtLbs_Loader <- m$MerchWtLbs_LL + m$MerchWtLbs_SL
# m$MerchWtTonnes_Loader <- m$MerchWtLbs_Loader * 0.000453592


# #2026 These 3 are used for the weight based portion of forwarder cost calculation; would be more consistent to put these in eqn_ref
# #and probably put them on separate lines, one per OC size class
# #2026 DS comment: It looks like FT_wt_SL are FT_wt_LL are using MerchWtLbs_CT, I think that should be changed to MerchWtLbs_SL or MerchWtLbs_LL respectively
# m$FT_wt_CT <- with(m,ifelse(Chip.tree.per.acre > 0, exp(1.0613+0.8841*log(Chip.tree.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Chip.tree.per.acre)),0))
# m$FT_wt_SL <- with(m,ifelse(Small.log.trees.per.acre > 0, exp(1.0613+0.8841*log(Small.log.trees.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Small.log.trees.per.acre)),0))
# m$FT_wt_LL <- with(m, ifelse(Large.log.trees.per.acre > 0, exp(1.0613+0.8841*log(Large.log.trees.per.acre)+0.6714*log(MerchWtLbs_CT * 0.000453592/Large.log.trees.per.acre)),0))


print("variables calculated")

#####CALCULATE HOURS PER ACRE######
#calculate_hpa calculates harvest time in hours per acre from the imported Opcost_Input table. 
#It incorporates opcost_equation_ref, opcost_modifiers, opcost_units to get a final hours per acre value 
#(as in it includes conversion from the original equation to hours per acre and 
#any other adjustments that get made as per opcost_modifiers). The function parameters are: 

#Arguments:
#data - The opcost input data
#equation.ID - the ID of the equation in the Equation.ID column

#Example: calculate_hpa(data = m, equation.ID = "TETH_01")

calculate_hpa <- function(data, equation.ID) {
  og_data_rows <- nrow(data) #get original number of rows in data
  og_data_columns <- ncol(data) #get original number of columns in data
  
  equation_ref <- opcost_equation_ref #if a different ref is not specified, use opcost_equation_ref
  #units <- opcost_units #if units is not specified, use opcost_units
  
  row <- equation_ref[equation_ref$EquationID == equation.ID,] #get the row from equation_ref with the specified equation ID 
  
  #stop function if there are duplicates of equation.ID
  if(nrow(row) > 1){
    stop("Duplicate equation ID") #this stops function if the equation ID is not unique
  } 
  
  if(nrow(row) == 0) {
    stop("Reference not found") #this stops the function if the equation.ID is not found in ref
  }
  
  if(row$Units == "" || is.na(row$Units)) { 
    #If units is not populated, use the equation as is from opcost_equation_ref
    equation <- row$Equation
  } else {
    #convert units if Unit column is populated. This references opcost_units to add the unit conversion to the equation in opcost_equation_ref
    equation<- gsub("result", paste0("(",row$Equation,")"), row$HoursPerAcreConversion)
  }
  
  if(row$LimitStatement == "" || is.na(row$LimitStatement)) {  #for harvest methods with limits
    equation.statement <- parse(text = paste0("with(data,", equation, ")"))
    data[,ncol(data) + 1] <- eval(equation.statement) #calculate hours per acre value based on equation 
  } else {
    #Evaluate equation for data subset based on Limit.Statement
    limit.statement <- parse(text = paste0("with(data,", row$LimitStatement, ")"))
    equation.statement <- parse(text = paste0("with(data[",limit.statement, ",],", equation, ")"))
    data[eval(limit.statement),ncol(data) + 1] <- eval(equation.statement)
  }
  
  if(ncol(data) > og_data_columns) {
    names(data)[ncol(data)] <- paste0(row$EquationID) #add column and name it based on the name/anaylsis/ID with "premod"
  } else {
    stop("Equation not calculated")
  }
  
  return(data)
  
}

test<-calculate_hpa(data = m, equation.ID = "SKID_08C")

#####COMPARE HOURS PER ACRE CALCULATIONS BY MACHINE AND GET MEAN######
#compute_harvest_system_equations runs calculate_hpa() for all equations in an machine type (e.g. "Skidder"),
#puts them in a single table, and calculates the mean of all equation results for the machine type. 

#Arguments:
#data - The opcost input data
#harvest_system - Harvesting system to compute using equations specified in opcost_harvestsystem_ref (case sensitive). Note this will only run for equations specified in the "Equation.ID" column
#allCols - TRUE/FALSE - If true, shows original data columns as well as cost and mean values. If FALSE, output is stand ID with equation results and mean
#meansonly - TRUE/FALSE - If true, shows only the calculated mean values and not individual equation values.

#Example: compute_harvest_system_equations(data = m, harvesting_system = "Shovel Logging", allCols = TRUE)

compute_harvest_system_equations <- function(data, harvest_system, allCols, meansonly) {
  #add in default values for function parameters
  if(missing(allCols)) {
    allCols <- FALSE
  }
  
  data1 = data
  
  equation_ref <- opcost_equation_ref
  harvestequation_ref <- opcost_harvestequation_ref
  
  harvest.system <- harvestequation_ref[harvestequation_ref$Method == harvest_system,]
  
  harvest.system$Equations <- paste(harvest.system$BC, harvest.system$Cut, harvest.system$Move, harvest.system$Chip, 
                                    harvest.system$Load, harvest.system$Extra, sep=",")
  
  harvest.system$Equations <- gsub("NA,","",harvest.system$Equations)#remove NA, values
  harvest.system$Equations <- gsub("NA","",harvest.system$Equations)#remove NA values
  
  #create a list of equations
  harvest.system2 <- strsplit(as.character(harvest.system$Equations), split = ",")
  harvest.system2 <- data.frame(Equation.ID = unique(trimws(unlist(harvest.system2))))#create dataframe containing equation IDs 
  
  # DS comment: making changes starting here ======================================================================================================
  #attach machine to cost Equation.ID
  # DS added Size variable
  harvest.system2 <- merge(equation_ref[, c("EquationID", "Size","Machine")], harvest.system2, by.x="EquationID", by.y="Equation.ID") 
  #rename column to "Machine"
  names(harvest.system2)[names(harvest.system2) =='x'] <- "Machine"
  
  #create list of equations 
  harvest.equations <- harvest.system2$EquationID
  
  #grab the rows from equation_ref  that match the equations in harvest.equations
  ref <- equation_ref[equation_ref$EquationID %in% harvest.equations,] 
  #create list
  ref_list <- ref[,"EquationID"]
  
  
  #create new dataframe containing stand values of input data
  newdata <- data.frame(data1$Stand)
  #rename column to stand
  names(newdata) <- c("Stand")
  
  for (i in 1:length(ref_list)) { #use calculate_hpa to get hours per acre for each equation.ID
    output <- calculate_hpa(data = data1, equation.ID = ref_list[i])
    newdata <- merge(newdata, output[, c(1, ncol(output))], by = "Stand")
  }
  
  cleaned.newdata <- do.call(data.frame,lapply(newdata, function(x) replace(x, is.infinite(x),NA))) #replace Inf values with NA
  cleaned.newdata <- do.call(data.frame,lapply(cleaned.newdata, function(x) replace(x, x == 0,NA))) #replace zero values with NA
  cleaned.newdata <- do.call(data.frame,lapply(cleaned.newdata, function(x) replace(x, x == "NaN", NA))) #replace NaN values with NA
  
  data_premean <- cleaned.newdata
  
  data <- merge(data1, cleaned.newdata)
  
  data2 <- data
  
  machines <- unique(equation_ref$Machine) #get unique machine.cost values
  #DS added this to get unique sizes
  sizes <- unique(equation_ref$Size) 
  
  #DS made changes to this for loop
  for (j in 1:length(machines)) {
    tot_machine_hpa <- 0
    machine.eqs.all <- harvest.system2$EquationID[harvest.system2$Machine == machines[j]]
    if(length(machine.eqs.all) == 0) {
      next
    }
    
    for (k in 1:length(sizes)) {
      machine.eqs <- harvest.system2$EquationID[harvest.system2$Machine == machines[j] & harvest.system2$Size == sizes[k]]
      machine.cols <- which(grepl(paste(machine.eqs, collapse = "|"), colnames(data2)))
      if(length(machine.eqs) == 0) {
        next
      }
      if(length(machine.eqs) > 1) {
        treetype_hpa <- rowMeans(data2[, machine.cols], na.rm=TRUE) #calculate mean of machine hours per acre values by OpCost tree type
      } else {
        treetype_hpa <- data2[,machine.cols]
      }
      
      treetype_hpa[is.nan(treetype_hpa)] <- 0
      treetype_hpa[is.na(treetype_hpa)] <- 0
      tot_machine_hpa <- treetype_hpa+tot_machine_hpa
      
      
    }
    
    tot_machine_hpa[tot_machine_hpa == 0] <- NA
    data2[,ncol(data2)+1] <- tot_machine_hpa
    names(data2)[ncol(data2)] <- paste0(harvest_system,".mean", ".", machines[j], "_HPA")
  }
  
  means <- sum(grepl('_HPA', data2))
  data.means <- data2[,c(1, (ncol(data)-means+1): ncol(data2))]
  data.means[is.na(data.means)] <- NA
  
  data <- merge(data2, data.means, all.x = TRUE)[, union(names(data2), names(data.means))]
  data[is.na(data)] <- NA
  
  ############################################################################################# 
  #Error Trapping
  
  limit.test <-aggregate(ref$LimitStatement, list(ref$Machine, ref$Size), paste, collapse=" | ")
  
  limit.test <-limit.test[ which(limit.test$x != "NA"),]
  limit.test <-limit.test[!grepl("NA", limit.test$x),]
  
  # Dan added If else statement here to allow for occasions when every machine in harvest system has at least 1 equation with an NA for the limit statement
  # And therefore, it can get an estimated HPA
  if (nrow(limit.test) > 0) {
    limit.test$x <-paste0("(", limit.test$x, ")")
    
    limit.test.statement <-paste(unlist(limit.test$x), collapse =" & ")
    
    limit.test.statement <- paste0("with(data, (", limit.test.statement, ")==FALSE)")
    
    data$limit.flag <- eval(parse(text=limit.test.statement))
  } else {
    data$limit.flag <- "FALSE"
  }
  # DS comment: end of DS changes  =========================================
  
  drop <- subset(data, limit.flag=="TRUE", select = "Stand")
  
  data <- data[!(data$Stand %in% drop$Stand),]
  
  ############################################################################################# 
  
  if(allCols == FALSE) { #if allCols = FALSE
    b <- ncol(data)-(ncol(cleaned.newdata)+ncol(data.means))
    data <- data[,c(1, (b):ncol(data))] #remove old data columns (keep Stand column)
  }
  
  if(meansonly == TRUE) {
    data <- data.means
  }
  return(list(data, drop))
}

test2_new <- compute_harvest_system_equations(data = m, harvest_system = "Cable Manual Log", allCols = T, meansonly = F)
test2_new <- as.data.frame(test2_new[1])


#####ESTIMATE COST######
#The estimate_cost function takes reference tables opcost_cost_ref, opcost_harvestsystem_ref, 
#and estimates costs by harvest system for all aspects of the harvest. This function relies on the 
#all_harvesting_systems() and compute_harvest_system_equations() functions. The output is a data frame
#in a list. 

#Arguments:
#harvest_system - the name of the harvest system to estimate costs for. 
#data - variable name for Opcost_Input data 
#cost - Cost values the user wants to use to calculate costs. 
#This corresponds to the name of each column in opcost_cost_ref. Defaults to "Default.CPH"

#Example: estimate_cost(harvest_system = "Cable Manual Log", data = m, cost = "DefaultCPH")
estimate_cost <- function(harvest_system, data, cost) {
  #add in default values for function parameters
  if(missing(cost)){
    cost <- "DefaultCPH"
  }
  
  #bring in reference tables
  cost_ref <- opcost_cost_ref
  harvestsystem_ref <- opcost_harvestsystem_ref
  
  vars <- c("Machine", cost, "MoveInCostMultiplier", "MoveInCostLowBoy")
  cost_ref2 <- cost_ref[vars]
  names(cost_ref2)[2] <- "Cost"
  
  costestimate_ref <- merge(cost_ref2, harvestsystem_ref, all.y = TRUE) #merge cost_ref values with harvest_system_ref
  
  if (any(is.na(costestimate_ref$DefaultCPH))) {
    warning("A Harvesting.System was removed; cost per hour value missing for a machine")
  }
  
  costestimate_ref <- costestimate_ref[!is.na(costestimate_ref$Cost),]#remove harvest systems where a cost per hour value is not given
  
  run_costestimate <- compute_harvest_system_equations(data, harvest_system, allCols = FALSE, meansonly = TRUE)
  costestimate <- run_costestimate[[1]]
  drop <- run_costestimate[[2]]

  costestimate[is.na(costestimate)] <- 0
  
  #pull in lowboy calculation parameters
  pattern <- c("Stand", "Move_In_Hours", "Harvest_area_assumed_acres", "Percent.Slope")
  lowboy.data <- data[,c(which(grepl(paste0(pattern, collapse = "|"),names(data))))]#changed data to m
  
  costestimate <- merge(costestimate, lowboy.data, by = "Stand")
  
  system.cpa1 <- data.frame()
  
  harvest.cost <- costestimate_ref[costestimate_ref$HarvestingSystem == harvest_system,]
  
  for (j in 1:nrow(harvest.cost)) {
    system_HPA_col <- which(colnames(costestimate) %in% paste0(harvest.cost$HarvestingSystem, ".mean.", harvest.cost$Machine[j], "_HPA"))
    if(length(system_HPA_col)!= 1) {
      stop("Harvest per acre column name duplicated or missing")
    }
    
    #calculate harvest cost _cpa for each machine
    Percent.Slope <- costestimate$Percent.Slope
    costestimate[,ncol(costestimate) + 1] <- costestimate[system_HPA_col] * eval(parse(text=paste0(harvest.cost$Cost[j])))
    names(costestimate)[ncol(costestimate)] <- paste0(harvest.cost$Machine[j], "_CPA")
    
    #calculate set up move in costs for each machine
    costestimate[,ncol(costestimate) + 1] <- eval(parse(text=paste0(harvest.cost$Cost[j])))
    names(costestimate)[ncol(costestimate)] <- paste0(harvest.cost$Machine[j], "_miCPA")
    cph.col.name <- names(costestimate)[ncol(costestimate)] 
    
    #calculate set up move in costs
    Harvest_area_assumed_acres <- costestimate$Harvest_area_assumed_acres
    costestimate[,ncol(costestimate) + 1] <- eval(parse(text = paste0(harvest.cost$MoveInCostMultiplier[j])))
    mic.col.name <- paste0(harvest.cost$Machine[j], "_MIC.Multiplier")
    names(costestimate)[ncol(costestimate)] <- as.character(mic.col.name)
    
    costestimate[,ncol(costestimate) + 1] <- costestimate[,c(which(grepl(mic.col.name, names(costestimate))))] * costestimate[,c(which(grepl(cph.col.name, names(costestimate))))]
    names(costestimate)[ncol(costestimate)] <- paste0(harvest.cost$Machine[j], "_Set.Up.Cost")
    
    #calculate lowboy move in costs
    Move_In_Hours <- costestimate$Move_In_Hours
    costestimate[,ncol(costestimate) + 1] <- eval(parse(text = paste0(harvest.cost$MoveInCostLowBoy[j])))
    names(costestimate)[ncol(costestimate)] <- paste0(harvest.cost$Machine[j], "_MIC.Lowboy")
  }
  
  #calculate total machine CPA
  chip.col <- which(grepl("Chipper_CPA",names(costestimate)))
  cpa.cols <- which(grepl("_CPA",names(costestimate)))
  cpa.cols.wo.chip <- cpa.cols[!cpa.cols %in% chip.col]
  
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(cpa.cols.wo.chip)], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.NoChip.Machine_CPA")
  
  #calculate set up move in cost
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(which(grepl("_Set.Up.Cost",names(costestimate))))], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.Non.LB.Cost")
  #calculate total lowboy cost
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(which(grepl("_MIC.Lowboy",names(costestimate))))], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.LB.Cost")
  #calculate total move in cost
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c("Total.Non.LB.Cost", "Total.LB.Cost")], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total.Move.In.Cost")
  
  pattern <- c("Total.NoChip.Machine_CPA", "Total.Move.In.Cost", "Chipper_CPA")
  costestimate[,ncol(costestimate) + 1] <- rowSums(costestimate[,c(which(grepl(paste0(pattern, collapse = "|"),names(costestimate))))], na.rm = TRUE)
  names(costestimate)[ncol(costestimate)] <- paste0("Total_CPA")
  
  pattern <- c("Total.NoChip.Machine_CPA", "Total.Move.In.Cost", "Total_CPA", "Chipper_CPA")
  system.cpa <- costestimate[,c(1,which(grepl(paste0(pattern, collapse = "|"),names(costestimate))))]

  return(list(system.cpa, drop))
  
}


#####CALCULATE HARVEST COSTS######
#calculate_costs_for_input runs estimate_cost() for the harvest system specified in the input dataset (via
#the Harvesting.System column) and outputs the total machine cost per acre, the total move in cost per acre, 
#the lowboy cost per acre, and the total cost per acre (i.e. the sum of the other values). The optimal parameter
#allows you to specify whether the data has been run through the optimal_harvesting.system() function yet; if 
#so, setting optimal to TRUE will calculate the cost for the optimal harvest system as well. 

#Arguments:
#data - Input data to be used. 
#optimal - TRUE/FALSE - specify whether the dataset has an Optimal.Harvest.System column or not (i.e. the dataset has been run through optimal_harvesting.system)

#Example: calculate_costs_for_input(data = m)

calculate_costs_for_input <- function(data) {
  
  #data <- m
  unique.harvesting.systems <- unique(data$Harvesting.System)
  unique.harvesting.systems <- unique.harvesting.systems[!is.na(unique.harvesting.systems)]
  
  mylist <- vector(mode="list", length=length(unique.harvesting.systems))
  droplist <- vector(mode="list", length=length(unique.harvesting.systems))
  
  for(i in 1:length(unique.harvesting.systems)) {
    system <- data[data$Harvesting.System == unique.harvesting.systems[i],]
    system2 <- suppressWarnings(estimate_cost(unique.harvesting.systems[i], system))
    mylist[[i]] <- system2[[1]]
    droplist[[i]] <- system2[[2]]
  }
  
  data2 <- Reduce(rbind, mylist)
  data2 <- merge(data2, data[, c("Stand", "Harvesting.System")], by="Stand")
  
  drop <- Reduce(rbind, droplist)
  drop <- merge(drop, data[, c("Stand", "Harvesting.System", "RxCycle", "RxPackage", "Rx", "biosum_cond_id")], by="Stand")
  
  return(list(data2, drop))
}

run_output <- calculate_costs_for_input(m)
output <- run_output[[1]]

drop <- run_output[[2]]

if (nrow(drop)>0) {
drop$error <- "Machine Limit Exceeded"
}

output <- output[!(output$Stand %in% drop$Stand),]


opcost_output <- data.frame("stand" = output$Stand, 
                            "harvest_cpa" = output$Total_CPA, 
                            "chip_cpa" = output$Chipper_CPA, 
                            "assumed_movein_cpa" = output$Total.Move.In.Cost,
                            "harvest_system" = output$Harvesting.System, 
                            "RxPackage_Rx_RxCycle" = substr(output$Stand, 26, 32), 
                            "biosum_cond_id" = substr(output$Stand, 1, 25),
                            "RxPackage" = substr(output$Stand, 26, 28), 
                            "Rx" = substr(output$Stand, 29, 31), 
                            "RxCycle" = substr(output$Stand, 32, 32))
                            
opcost_errors <- data.frame("stand" = drop$Stand, 
                            "harvest_system" = drop$Harvesting.System,
                            "error_message" = drop$error,
                            "biosum_cond_id" = drop$biosum_cond_id, 
                            "RxPackage" = drop$RxPackage, 
                            "Rx" = drop$Rx,
                            "RxCycle" = drop$RxCycle)



print("Calculations Complete")
############################ OUTPUT ##################################

#If output file pathway is passed from command line, then write files to pathways
print("Attempting to write outputs, see file pathway below")
print(args[1])
if (length(args!=0)) {
  con<-dbConnect(sqlite,args[1]) 
  dbWriteTable(con, "OpCost_Output", as.data.frame(opcost_output), overwrite = TRUE,
               field.types = c(stand = "CHAR(50)", harvest_cpa = "DOUBLE", chip_cpa = "DOUBLE", assumed_movein_cpa = "DOUBLE",
                               harvest_system = "CHAR(40)", RxPackage_Rx_RxCycle = "CHAR(10)", biosum_cond_id = "CHAR(50)", 
                               RxPackage = "CHAR(3)", Rx = "CHAR(3)", RxCycle = "CHAR(1)"))
  if (nrow(opcost_errors)>0)
  dbWriteTable(con, "OpCost_Errors", as.data.frame(opcost_errors), overwrite = TRUE,
               field.types = c(stand = "CHAR(50)", harvest_system = "CHAR(50)", error_message = "CHAR(255)",
                               biosum_cond_id = "CHAR(50)", RxPackage = "CHAR(3)", Rx = "CHAR(3)", RxCycle = "CHAR(1)"))
  dbDisconnect(con)
} else {
  #If output file pathway is not passed from command line, then write files to pathways defined by user at beginning of script
  conn<-dbConnect(sqlite,opcost.output.location) 
  dbWriteTable(conn, "OpCost_Output", as.data.frame(opcost_output), overwrite = TRUE,
               field.types = c(stand = "CHAR(50)", harvest_cpa = "DOUBLE", chip_cpa = "DOUBLE", assumed_movein_cpa = "DOUBLE",
                               harvest_system = "CHAR(40)", RxPackage_Rx_RxCycle = "CHAR(10)", biosum_cond_id = "CHAR(50)", 
                               RxPackage = "CHAR(3)", Rx = "CHAR(3)", RxCycle = "CHAR(1)"))
  if (nrow(opcost_errors)>0)
  dbWriteTable(conn, "OpCost_Errors", as.data.frame(opcost_errors), overwrite = TRUE,
               field.types = c(stand = "CHAR(50)", harvest_system = "CHAR(50)", error_message = "CHAR(255)",
                               biosum_cond_id = "CHAR(50)", RxPackage = "CHAR(3)", Rx = "CHAR(3)", RxCycle = "CHAR(1)"))
  dbDisconnect(conn)
}
print("Processing Complete")

# ###################################################################################
# ###CREATE ANALYSIS GRAPHICS###
# packages <- c("reshape2", "ggplot2", "dplyr", "data.table", "plyr")
# 
# package.check <- lapply(packages, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, repos="http://cran.r-project.org", dependencies = TRUE)
#     library(x, character.only = TRUE)
#   }
# })
#
# 
# #MAKE SURE YOUR WORKING DIRECTORY IS SET TO WHERE YOU WANT THE GRAPHICS TO SAVE###
# #The code below will save it to your project directory in a new folder called "opcost_graphics"
# #MAKE SURE YOUR WORKING DIRECTORY IS SET TO WHERE YOU WANT THE GRAPHICS TO SAVE###
# #The code below will save it to your project directory in a new folder called "opcost_graphics"
# #project.directory <- "C:/Users/sloreno/Opcost/opcost_graphics" #change to your project directory
# setwd(graph.directory)
# 
# graph_analyses_machine <- function(data) {
#   ref <- opcost_equation_ref
#   ###SUBSET TO ONLY INCLUDE SPECIFIC EQUATIONS/MACHINES###
#   #This is a good spot to subset out certain equations
#   #for example, to remove specific equations, you would change ref to:
#   #ref <- opcost_equation_ref[!opcost_equation_ref$Equation.ID %in% c("FB_06", "FB_04", "FB_01"),]
#   #you can remove additional equations by adding to the list after the c().
#   #You can use the same method to remove machines:
#   #ref <- opcost_equation_ref[!opcost_equation_ref$Machine %in% c("Feller Buncher"),]
#   #This will help avoid corrupting any of the tables as ref is not stored
#   #in the global environment when the function is run
#   ref$Machine.size <- paste0(ref$Machine, "_", ref$Size)
#   unique.machines <- unique(ref$Machine.size)
#   folder <- file.path(graph.directory, paste(format(Sys.time(), "%Y%m%d%H%M%S"), "machine_analysis", sep = "_"))
#   dir.create(folder, showWarnings = FALSE)
#   setwd(folder)
# 
# 
#   for (i in 1:length(unique.machines)) {
#     values <- ref[as.character(ref$Machine.size) == as.character(unique.machines[i]),]
#     values <- values[values$Equation != "",]
#     mylist <- vector(mode="list", length=nrow(values))
#     name.vector <- as.character()
#     for (j in 1:nrow(values)) {
#       values.data <- calculate_hpa1 <-calculate_hpa(data = data, equation.ID = values$EquationID[j])
#       mylist[[j]] <- values.data
#       name.vector[j] <- paste0(values$EquationID[j])
#     }
#     values.data <- Reduce(merge, mylist)
#     names(values.data)[(ncol(values.data) + 1 - length(name.vector)):ncol(values.data)] <- name.vector
#     b <- ncol(values.data)
#     a <- ncol(data) + 1
#     df2 <- melt(values.data, id.vars = c(a:b), measure.vars = names(values.data)[a:b])
#     n <- ncol(df2)
#     df2 <- df2[complete.cases(df2[n]),]
#     df3 <- df2[,c(n-1, n)]
#     df4 <- df3 %>% group_by(variable) %>% tally()
#     df2 <- merge(df4, df3, by = "variable")
#     df2$variable <- gsub(unique.machines[i],"",df2$variable)
#     df4$variable <- gsub(unique.machines[i],"",df4$variable)
#     df5 <- merge(df2, values[, c("EquationID", "Machine.size")], by.x="variable", by.y="EquationID")#Turn your 'treatment' column into a character vector
#     graph <- ggplot(df5, aes(variable, value, fill =  variable)) +
#       geom_boxplot() +
#       labs(x=unique.machines[i], y="Hours Per Acre") +
#       facet_wrap(  ~ Machine.size)
#     ggsave(filename = paste0(unique.machines[i], ".png"),graph, device = "png", width = ifelse(nrow(df4)*1.3 > 6, nrow(df4)*1.3, 6))
#   }
#   setwd(graph.directory)
# }
# 
# graph_analyses_machine(m)
# 
# graph_analyses_harvest_system <- function(data) {
#   ref <- opcost_harvestsystem_ref
#   unique.harvest.system <- unique(ref$HarvestingSystem)
#   folder <- file.path(graph.directory, paste(format(Sys.time(), "%Y%m%d%H%M%S"), "harvest_analysis", sep = "_"))
#   dir.create(folder, showWarnings = FALSE)
#   setwd(folder)
# 
#   for (i in 1:length(unique.harvest.system)) {
#     pattern <- c(" ", "-", "/") #use data frame names as the harvest system names vector
#     filename1 <- gsub(paste0(pattern, collapse = "|"),".", unique.harvest.system[i])
#     run.values.data <- compute_harvest_system_equations(data = data, harvest_system = unique.harvest.system[i], meansonly = FALSE)
#     values.data<-run.values.data[[1]]
#     #values.data <- compute_harvest_system_equations(data = m, harvest_system = "Cable CTL", meansonly = FALSE)
#     # pattern <- c("TETH_01", "TETH_02", "TETH_03")
#     # values.data$TETH <- rowSums(values.data[,c(which(grepl(paste0(pattern, collapse = "|"), names(values.data))))], na.rm = TRUE)
#     # values.data <- values.data[,-c(which(grepl(paste0(pattern, collapse = "|"), names(values.data))))]
#     #values.data <- values.data[,-c(which(grepl("mean", names(values.data))))]
#     values.data <- values.data[, !grepl("mean", colnames(values.data))]
#     values.data <- values.data[, !grepl("FT_wt", colnames(values.data))]
#     values.data <- values.data[1:(length(values.data)-1)]
#     b <- ncol(values.data)
#     a <- 2
#     df2 <- melt(values.data, id.vars = c(1), measure.vars = names(values.data)[a:b])
#     n <- ncol(df2)
#     df2 <- df2[complete.cases(df2[n]),]
#     df3 <- df2[,c(n-1, n)]
#     df4 <- df3 %>% group_by(variable) %>% tally()
#     df2 <- merge(df4, df3, by = "variable")
#     # df2$variable <- gsub(unique.machines[i],"",df2$variable)
#     # df4$variable <- gsub(unique.machines[i],"",df4$variable)
#     # ylim1 <- boxplot.stats(df2$value)$stats[c(1, 5)]
#     graph <- ggplot(df2, aes(variable, value)) + geom_boxplot() + labs(x=unique.harvest.system[i], y="Hours Per Acre") +
#       scale_x_discrete(labels = paste(df4$variable, df4$n, sep = "\n"))
#       #scale_y_continuous(limits = c(0,120), breaks=c(0, 20, 40, 60, 80, 100, 120))
#     ggsave(filename = paste0(filename1, ".png"),graph, device = "png", width = ifelse(nrow(df4)*1.1 > 6, nrow(df4)*1.1, 6))
#   }
#   setwd(graph.directory)
# }
# 
# graph_analyses_harvest_system(m)


