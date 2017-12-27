#####Initial package loading
require(RODBC) || install.packages(RODBC)

####Load Data



args=(commandArgs(TRUE))

print(args)
con<-odbcConnectAccess2007(args)
print("odbc Connection:OK")
m<-data.frame(sqlFetch(con, "opcost_input", as.is=TRUE))
print("m data.frame opcost_input SqlFetch:OK")
M<-data.frame(m)
print("M data.frame:OK")

## Edit idealTable and chipTable: 1 creates or updates the ideal table and 0 does not.

idealTable<-1

chipTable<-1

#####Run Data



prescribe<-function(m,m3){ifelse((m$"Harvesting.System")=="Cable Manual WT", yarderEst(m,m3),
                                 ifelse((m$"Harvesting.System")=="Ground-Based Manual WT", sawSkidEst(m,m3),
                                        ifelse((m$"Harvesting.System")=="Ground-Based Mech WT", fbSkidEst(m,m3),
                                               ifelse((m$"Harvesting.System")=="Ground-Based CTL", harForEst(m,m3), 
                                                      ifelse((m$"Harvesting.System")=="Helicopter CTL", heliHarEst(m,m3),
                                                             ifelse((m$"Harvesting.System")=="Cable Manual WT/Log", yarderSawEst(m,m3),
                                                                    ifelse((m$"Harvesting.System")=="Cable Manual Log", yarderSawLogEst(m,m3),
                                                                           ifelse((m$"Harvesting.System")=="Ground-Based Manual Log", sawSkidLogEst(m,m3),
                                                                                  ifelse((m$"Harvesting.System")=="Helicopter Manual WT", heliSawEst(m,m3),
                                                                                         ifelse((m$"Harvesting.System")=="Cable CTL", yarderHarEst(m,m3),
                                                                                              ifelse((m$"Harvesting.System")=="Shovel Logging", shovelEst(m,m3),  1)))))))))))
}

m3<-data.frame(row.names=c("fellerBuncher","forwarder","harvester","grappleSkidderLarge",
                           "cableSkidderLarge","wheelFellerBuncher","crawlerFellerBuncher","yarder","slideboom","chainsaw","chipper","helicopter","shovel"),values=c(210.10,170.00,180.00,110.00,149.80,205.10,185.60,299.00,200.00,90.00,100.00,1500,186.00))

## The above costs are assuming 40 hour week with stand operating and ownership costs

dbh<-function(m){
  sqrt((twitchVol(m)+8.4166)/.2679)
}

dbh.cm<-function(m){
  dbh(m)*2.54
}

treesRemoved<-function(m){
  (m$"Small.log.trees.per.acre"+
     m$"Large.log.trees.per.acre"+
     m$"Chip.tree.per.acre")
}

bcTpa<-function(m){
  m$"BrushCutTPA"  
}

bcVol<-function(m){
  m$"BrushCutAvgVol"
}

chipTrees<-function(m){
  ifelse(m$"Harvesting.System"=="Ground-Based Mech WT"|m$"Harvesting.System"=="Ground-Based Manual WT"|m$"Harvesting.System"=="Shovel Logging",
         m$Small.log.trees.ChipPct_Cat2_4+m$Large.log.trees.ChipPct_Cat2+m$Chip.tree.per.acre,
         ifelse(m$"Harvesting.System"=="Cable Manual WT",
                m$Small.log.trees.ChipPct_Cat2_4+m$Large.log.trees.ChipPct_Cat1_3_4+m$Chip.tree.per.acre,
                ifelse(m$"Harvesting.System"=="Cable Manual WT/Log",
                       m$Small.log.trees.ChipPct_Cat5+m$Large.log.trees.ChipPct_Cat5+m$Chip.tree.per.acre,
                       ifelse(m$"Harvesting.System"=="Helicopter CTL"|m$"Harvesting.System"=="Ground-Based CTL"|m$"Harvesting.System"=="Cable CTL"|m$"Harvesting.System"=="Ground-Based Manual Log",
                              m$Small.log.trees.ChipPct_Cat1_3+m$Large.log.trees.ChipPct_Cat1_3_4+m$Chip.tree.per.acre,
                              ifelse(m$"Harvesting.System"=="Cable Manual Log"|m$"Harvesting.System"=="Helicopter Manual WT",
                                     m$Small.log.trees.ChipPct_Cat1_3+m$Large.log.trees.ChipPct_Cat1_3_4+m$Chip.tree.per.acre,NA)))))
}

sppgrp<-function(m){
  ifelse((m$"Small.log.trees.hardwood.percent"+m$"Large.log.trees.hardwood.percent")>1, 0, 1)
}

distBetweenTrees<-function(m){
  (sqrt((43560/(m$"Small.log.trees.per.acre"+
                  m$"Large.log.trees.per.acre"+
                  m$"Chip.tree.per.acre"))/pi))*2
} ## Feet between trees

twitchVol<-function(m){
  ((m$"Large.log.trees.per.acre"*m$"Large.log.trees.average.vol.ft3.")+(m$"Small.log.trees.per.acre"*m$"Small.log.trees.average.volume.ft3.")+(m$"Chip.tree.per.acre"*m$"Chip.trees.average.volume.ft3."))/
    (m$"Large.log.trees.per.acre"+m$"Small.log.trees.per.acre"+m$"Chip.tree.per.acre")
}

twitchVol2<-function(m){
  ((m$"Large.log.trees.per.acre"*(m$"Large.log.trees.average.vol.ft3."+(
    m$Large.log.trees.average.vol.ft3.*(.01*m$Large.log.trees.MerchAsPctOfTotal))))+(
      m$"Small.log.trees.per.acre"*(m$"Small.log.trees.average.volume.ft3."+(
        m$Small.log.trees.average.volume.ft3.*(.01*m$Small.log.trees.MerchAsPctOfTotal))))+(
          m$"Chip.tree.per.acre"*m$"Chip.trees.average.volume.ft3."))/
    (m$"Large.log.trees.per.acre"+m$"Small.log.trees.per.acre"+m$"Chip.tree.per.acre")
}

## twitchVol<-function(m){ifelse(m$Harvesting.System=="Ground-Based CTL"|
##                                m$Harvesting.System=="Helicopter CTL"|
##                                m$Harvesting.System=="Cable CTL",twitchVol3(m),ifelse(
##                                  m$Harvesting.System=="Cable Manual WT"|
##                                    m$Harvesting.System=="Ground-Based Manual WT"|
##                                    m$Harvesting.System=="Ground-Based Mech WT"|
##                                    m$Harvesting.System=="Cable Manual WT/Log"|
##                                    m$Harvesting.System=="Cable Manual Log"|
##                                    m$Harvesting.System=="Ground-Based Manual Log"|
##                                    m$Harvesting.System=="Helicopter Manual WT"|
##                                    m$Harvesting.System=="Shovel Logging",twitchVol2(m),NaN))}

twitchVolM<-function(m){
  twitchVol(m)*0.0283168
}


twitchWeight<-function(m){
  twitchDF<-data.frame(m[,"Large.log.trees.average.density.lbs.ft3."],
                       m[,"Small.log.trees.average.density.lbs.ft3."])
  twitchDF[twitchDF==0]<-NA
  avgDensity<-rowMeans(twitchDF, na.rm=TRUE)
  avgDensity*twitchVol(m)
}

totalWeight<-function(m){
  twitchDF<-data.frame(m[,"Large.log.trees.average.density.lbs.ft3."],
                       m[,"Small.log.trees.average.density.lbs.ft3."])
  twitchDF[twitchDF==0]<-NA
  avgDensity<-rowMeans(twitchDF, na.rm=TRUE)
  avgDensity*totalVol(m)
}

cordsPerAcre<-function(m){
  ((m$"Large.log.trees.per.acre"*m$"Large.log.trees.average.vol.ft3.")+(m$"Small.log.trees.per.acre"*m$"Small.log.trees.average.volume.ft3."))/128
}

totalVol<-function(m){
  (m$"Large.log.trees.per.acre"*m$"Large.log.trees.average.vol.ft3.")+(m$"Small.log.trees.per.acre"*m$"Small.log.trees.average.volume.ft3.")
}

totalVolM<-function(m){
  totalVol(m)*0.0283168
}

## Equations

behjouSaw<-function(m){
  -2.80+(0.051*(25.4*dbh(m)))+(0.039*(distBetweenTrees(m)/3.28084))
} ## Min/Tree

klepacSaw=function(m){
  24.796+0.331419*(dbh(m)^2)
} ## Seconds/Tree

ghafSaw<-function(m){
  -1.582+(0.099*dbh.cm(m))
} ##Min/Tree

hartsoughSaw<-function(m){
  0.1+0.0111*(dbh(m)^1.496)
} ##Min/Tree

kluenderSaw<-function(m){
  (0.016*((dbh(m)*2.54)^1.33))*
    (distBetweenTrees(m)^0.083)*(.5^0.196)
} ## Min/Tree

spinelliSaw<-function(m){
  30.04 + 0.2*((dbh(m))^2)+ 8.3
}

akaySaw<-function(m){
  56.62*(log(twitchVolM(m)))+322.09
}


hansillChip<-function(m){
  2.32+(-0.42*1.79)+(1.83*dbh.cm(m))
} ## Sec/Tree

hansillChip2<-function(m){
  2.4+(-0.32*1.4)+(1.3*dbh.cm(m))
}

boldingChip<-function(m){
  0.001*(totalVol(m)*25)
}


adebayoHar<-function(m){
  30.04 + 0.2*((dbh(m))^2)+ 8.3
} ## centi-Minutes/Acre

boldingHar=function(m){
  .1765521+(0.003786057*dbh(m))+(4.936639*m$"Percent.Slope"*(sqrt(treesRemoved(m))))
} ##****

hieslHarCord2=function(m){
  exp((-0.826+.309*(dbh(m)))+0.386*sppgrp(m))
} ## Cords/PMH

karhaHar<-function(m){
  0.288+(0.1004*(m$"Percent.Slope"/0.001))+(-0.00008*(twitchVolM(m)/0.001)^2)
}  ##m^3/PMH

karhaHar2<-function(m){
  0.181*m$"Percent.Slope"+(0.1315*(twitchVolM(m)*1000))
}

karhaHar3<-function(m){
  0.422+(0.1126*(twitchVolM(m)*1000))
}

klepacSkid<-function(m){
  abs(
    (0.0076197*m$"One.way.Yarding.Distance"-0.6073183873)+
      (0.00735*(distBetweenTrees(m)*4)+0.5438016119)+
      (0.0121066*m$"One.way.Yarding.Distance"-1.651069636)
  )
} ## 3 compounded functions

klepacHar=function(m){
  24.796+0.331419*(dbh(m)^2)
} ## Seconds/Tree

drewsHar<-function(m){
  21.139+72.775*(totalVol(m))
}

jirousekHar<-function(m){
  60.711*(twitchVolM(m)^0.6545)
} ## m^3/PMH

klepacHar<-function(m){
  abs(
    (0.0076197*m$"One.way.Yarding.Distance"-0.6073183873)+
      (0.00735*(distBetweenTrees(m)*4)+0.5438016119)+
      (0.0121066*m$"One.way.Yarding.Distance"-1.651069636)
  )
}


## Feller Buncher

hartsoughFB<-function(m){
  0.324+0.00138*(dbh(m)^2)
} ##minutes per tree

akayFB<-function(m){
  56.62*(log(twitchVolM(m)))+322.09
}

stokesFB<-function(m){
  2.80*(m$"One.way.Yarding.Distance")^0.574
}

drewsFB<-function(m){
  21.139+72.775*(totalVol(m))
}

dykstraFB<-function(m){
  2.39219+0.0019426*(m$"Percent.Slope")+
    (m$"One.way.Yarding.Distance")+0.030463*(treesRemoved(m))
}

boldingFB=function(m){
  .1765521+(0.003786057*dbh(m))+(4.936639*m$"Percent.Slope"*(sqrt(treesRemoved(m))))
} 

karhaFB<-function(m){
  0.422+(0.1126*(twitchVolM(m)*1000))
}

hieslFB=function(m){
  exp((-0.826+.309*(dbh(m)))+0.386*sppgrp(m))
}

behjouFB<-function(m){
  -2.80+(0.051*(25.4*dbh(m)))+(0.039*(distBetweenTrees(m)/3.28084))
}

adebayoFB<-function(m){
  30.04 + 0.2*((dbh(m))^2)+ 8.3
}

## Skidder

hieslGrapCord=function(m){
  exp(1.754*(-0.0005*m$"One.way.Yarding.Distance"))+(0.755*twitchVol(m))
} ##Cords/PMH

akaySkid<-function(m){
  -0.1971+(1.1287*5)+((0.0045*twitchVolM(m)*5))+(0.0063*(m$"One.way.Yarding.Distance"*2))
}

ghafSkid<-function(m){
  13.027+(0.035*(m$"One.way.Yarding.Distance"*2))+(0.847*(m$"Percent.Slope"))+(0.551*(twitchVolM(m)*5))
}

akaySkid2<-function(m){
  -0.1971+(1.1287*5)+((0.0045*twitchVolM(m)*5))+(0.0063*(m$"One.way.Yarding.Distance"*2))
}

akaySkid2<-function(m){
  0.012*(m$"One.way.Yarding.Distance"^-0.399)*(dbh(m)^2.041)*(5^0.766)
}

kluenderSkid<-function(m){
  (0.017*((m$"One.way.Yarding.Distance"^-0.574))*2)*(dbh.cm(m)^2.002)
}

boldingSkid<-function(m){
  .1761+(0.00357*dbh(m))+(4.93*m$"Percent.Slope"*(sqrt(treesRemoved(m))))
}

fisherSkid=function(m){
  2.374+(0.00841141*(m$"One.way.Yarding.Distance"))+(0.72548570*(1.35))
}

##Forwarder

jirousekFor<-function(m){
  abs(-7.6881*log((m$"One.way.Yarding.Distance"*0.3048))+64.351)
}

boldingFor<-function(m){
  .1761+(0.00357*dbh(m))+(4.93*m$"Percent.Slope"*(sqrt(treesRemoved(m))))
}

jirousekFor2<-function(m){
  10.5193*(m$"One.way.Yarding.Distance"^(24.9181/m$"One.way.Yarding.Distance"))
}

jirousekFor3<-function(m){
  17.0068*(m$"One.way.Yarding.Distance"^(13.2533/m$"Percent.Slope"))
}

kluenderFor<-function(m){
  (0.017*((m$"One.way.Yarding.Distance"^-0.574))*2)*(dbh.cm(m)^2.002)
}

fisherFor=function(m){
  2.374+(0.00841141*(m$"One.way.Yarding.Distance"))+(0.72548570*(1.35))
}

dykstraFor<-function(m){
  2.39219+0.0019426*(m$"Percent.Slope")+
    (m$"One.way.Yarding.Distance")+0.030463*(treesRemoved(m))
}

iffFor<-function(m){
  1.054+.00234*(m$One.way.Yarding.Distance)+0.01180*(97)+0.980*(
    treesRemoved(m))+.00069*(totalWeight(m))
}

## Yarder

fisherYarder=function(m){
  2.374+(0.00841141*(m$"One.way.Yarding.Distance"))+(0.72548570*(1.35))
} ## Turn Time Minutes

curtisYarding<-function(m){
  23.755 + (2.7716*(1.5))-(0.63694*(m$"One.way.Yarding.Distance"))
} ## Logs/Hour

curtisYarding2<-function(m){
  11.138+(7.1774*(1.5))-(0.59976*(m$"One.way.Yarding.Distance"))
} ## Logs/Hour

dykstraYarding<-function(m){
  2.39219+0.0019426*(m$"Percent.Slope")+
    (m$"One.way.Yarding.Distance")+0.030463*(treesRemoved(m))
}

aulerichYard3<-function(m){
  1.210+0.009*(m$One.way.Yarding.Distance)+0.015*(
    m$One.way.Yarding.Distance)+0.253*(treesRemoved(m))
}

iffYard<-function(m){
  1.054+.00234*(m$One.way.Yarding.Distance)+0.01180*(97)+0.980*(
    treesRemoved(m))+.00069*(totalWeight(m))
}

aulerichYard2<-function(m){
  1.925+0.002*(m$One.way.Yarding.Distance)+0.017*(
    m$One.way.Yarding.Distance)+0.909*(treesRemoved(m))
}

aulerichYard<-function(m){
  0.826+0.006*(m$One.way.Yarding.Distance)+0.032*(
    m$One.way.Yarding.Distance)+0.897*(treesRemoved(m))
}

## Slide Boom Processor

hartsoughSlide<-function(m){
  0.141+0.0298*dbh(m)
}

ghafLoad<-function(m){
  23.297/twitchVolM(m)
}

suchomelSlide<-function(m){
  twitchVolM(m)/19.8
}



## Helicopter Yarding

flattenHeli<-function(m){
  40.737274+(0.0168951*m$"One.way.Yarding.Distance")+((totalWeight(m)/12)*2.052894)+(22.658839)
}

dykstraHeli<-function(m){
  1.3721152+(0.0126924*m$"Percent.Slope")+(0.00246741*m$"One.way.Yarding.Distance")+
    (0.031200*(3))+(0.000060987*(totalVol(m)/12))-(0.000050837*(totalVol(m)/36))
}

curtisHeli<- function(m){ 
  23.755 + 2.7716*treesRemoved(m)-0.63694*(m$"One.way.Yarding.Distance")
}

akayHeli<-function(m){
  1.3721152+(0.0126924*m$"Percent.Slope")+(0.00246741*m$"One.way.Yarding.Distance")+
    (0.031200*(3))+(0.000060987*(totalVol(m)/12))-(0.000050837*(totalVol(m)/36))
}


############## Time Per Acre Converstions

ghafSkidTime<-function(m){
  ifelse(m$"Percent.Slope"<45, totalVolM(m)/(twitchVolM(m)*30)*ghafSkid(m)/60, NA)
}

mechBC<-function(m){
  ifelse(is.na(m$BrushCutTPA), 0, ifelse(m$BrushCutTPA>0, ifelse(m$BrushCutAvgVol<4, m$BrushCutTPA/(10*60), m$BrushCutTPA/(5*60)), 0))
} ##Hours/Acre

adebayoHarTime<-function(m){
  adebayoHar(m)*treesRemoved(m)/60/60
} ## Hours/Acre

hansillChip2Time<-function(m){
  ifelse(dbh.cm(m)<76, ((chipTrees(m)*hansillChip2(m))/60), NA)
} ##Hours/Acre

curtisYardingTime<-function(m){
  ifelse(m$"One.way.Yarding.Distance"<10,treesRemoved(m)/curtisYarding(m),NA)
} ## Hours/Acre

manBC2<-function(m){
  ifelse(is.na(m$BrushCutTPA), 0, ifelse(m$BrushCutTPA>0 ,ifelse(m$BrushCutAvgVol<4, m$BrushCutTPA/(60), m$BrushCutTPA/(2*60)),0))
}

curtisYarding2Time<-function(m){
  ifelse(m$"One.way.Yarding.Distance"<10,treesRemoved(m)/curtisYarding2(m),NA)
} ## Hours/Acre

fisherYarderTime<-function(m){
  ((((m$"Small.log.trees.per.acre"+m$"Large.log.trees.per.acre")/1.35)*fisherYarder(m))/120)
} ##  Hours/Acre

kluenderSkidTime<-function(m){
  ifelse(m$"Percent.Slope"<45, kluenderSkid(m), NA)
}

behjouSawTime<-function(m){
  ifelse(dbh.cm(m)>40.00, ((treesRemoved(m)*behjouSaw(m))/60), NA)
}

dykstraHeliTime<-function(m){
  ifelse((twitchWeight(m)<2900),
         (((((totalVol(m)/3)*(m$"Large.log.trees.per.acre"+m$"Small.log.trees.per.acre"))/190)*dykstraHeli(m))/60),
         NA)
} ##Hours/Acre

flattenHeliTime<-function(m){
  ifelse((twitchWeight(m)<2900), flattenHeli(m)/3600, NA)
} ## Hours/Acre

ghafSawTime<-function(m){
  ifelse(dbh.cm(m)>25, ((ghafSaw(m)*treesRemoved(m))/180), NA)
}

hansillChipTime<-function(m){
  ifelse(dbh.cm(m)<76, ((chipTrees(m)*hansillChip(m))/60/60), NA)
} ## Hours/ Acre

hartsoughSawTime<-function(m){
  (hartsoughSaw(m)*treesRemoved(m))/60
}

jirousekFor3Time<-function(m){
  ifelse(m$"Percent.Slope"<45, totalVolM(m)/jirousekFor3(m), NA)
}

boldingChipTime<-function(m){
   (boldingChip(m))/60
}

akaySkidTime<-function(m){
  ifelse(m$"One.way.Yarding.Distance"<3500, akaySkid(m)*.4, NA)
}

jirousekFor2Time<-function(m){
  ifelse(m$"Percent.Slope"<45, totalVolM(m)/jirousekFor2(m), NA)
}

hansillChip2Time<-function(m){
  ifelse(dbh.cm(m)<76, ((chipTrees(m)*hansillChip2(m))/60), NA)
}

kluenderSawTime<-function(m){
  (kluenderSaw(m)*treesRemoved(m))/60
}

curtisHeliTime<-function(m){
  ifelse(dbh.cm(m)>20, curtisHeli(m)/60, NA)
}

hieslGrapTime<-function(m){
  cordsPerAcre(m)/hieslGrapCord(m)
} ##Hours/Acre

hieslHarTime<-function(m){
  cordsPerAcre(m)/hieslHarCord2(m)
} ##Hours/Acre

boldingForTime<-function(m){
  ifelse(m$"Percent.Slope"<0,  totalVolM(mslope)/boldingFor(m), NA)
}

karhaHar3Time<-function(m){
  ifelse(twitchVolM(m)<40, (totalVolM(m))/karhaHar3(m), NA)
}



ghafLoadTime<-function(m){
  ifelse(twitchVolM(m)<3, ghafLoad(m)/60, NA)
}

klepacSkidTime<-function(m){
  ifelse(m$"Percent.Slope"<0, treesRemoved(m)/(klepacSkid(m)*15), NA)
}

jirousekHarTime<-function(m){
  ifelse(twitchVolM(m)<1.4, (totalVolM(m)/jirousekHar(m)), NA)
} ##Hours/Acre

jirousekForTime<-function(m){
  totalVolM(m)/jirousekFor(m)
} ##Hours/Acre

klepacHarTime<-function(m){
  ((klepacHar(m)*treesRemoved(m))/60)/60
} ## Hours/Acre

stokesFBTime<-function(m){
  (ifelse(m$"Percent.Slope"<45, stokesFB(m)/60, NA))
} ##Hours/Acre

akayFBTime<-function(m){
  ifelse(twitchVolM(m)<.2, (totalVolM(m))/akayFB(m), NA)
}

karhaHarTime<-function(m){
  ifelse(twitchVolM(m)<.0, (totalVol(m)*0.0283168)/karhaHar(m), NA)
} ## Hours/Acre

hartsoughFBTime<-function(m){
  (hartsoughFB(m)*treesRemoved(m))/60
} ##Hours/Acre

hartsoughSlideTime<-function(m){
  hartsoughSlide(m)*30
} ##Hours/Acre

suchomelSlideTime<-function(m){
  suchomelSlide(m)
} ##Hours/Acre

karhaHar2Time<-function(m){
  ifelse(twitchVolM(m)<40, (totalVolM(m))/karhaHar2(m), NA)
} ##Hours/Acre



############################### Cycle Time Analysis

## Chipping Analysis

chipDF<-function(m){
  data.frame(
    hansillChipTime(m),
    hansillChip2Time(m)^.8,
    boldingChipTime(m))
}

chipTime<-function(m){
  rowMeans(chipDF(m), na.rm=TRUE)
}

chipTime2<-function(m){ifelse(chipTime(m)=="NaN",0,chipTime(m))
}

## Feller Buncher Analysis

fbDF<-function(m){
  data.frame(
    hartsoughFBTime(m),
    stokesFBTime(m),
    akayFBTime(m))
}

fbTime<-function(m){
  fbAvg(m)
}


fbAvg<-function(m){
  ifelse(fbTime2(m)=="NaN", mechBC(m), fbTime2(m)+mechBC(m))
}

fbTime2<-function(m){
  rowMeans(fbDF(m), na.rm=TRUE)
}
## Slideboom Processor

slideDF<-function(m){
  data.frame(
    ifelse(is.nan(suchomelSlideTime(m)),0,suchomelSlideTime(m)),
    ifelse(is.nan(ghafLoadTime(m)),0,ghafLoadTime(m)),
    ifelse(is.nan(hartsoughSlideTime(m)),0,hartsoughSlideTime(m))
  )
}

slideTime<-function(m){
  rowMeans(slideDF(m), na.rm=TRUE)
}

## Forwarder Analysis

forDF<-function(m){
  data.frame(
    jirousekForTime(m),
    jirousekFor2Time(m),
    boldingForTime(m),
    jirousekFor3Time(m))
}

forTime<-function(m){
  rowMeans(forDF(m), na.rm=TRUE)
}

## Skidder Analysis

skidderDF<-function(m){
  data.frame(
    hieslGrapTime(m),
    akaySkidTime(m),
    kluenderSkidTime(m),
    ghafSkidTime(m),
    klepacSkidTime(m))
}

skidderTime<-function(m){
  rowMeans(skidderDF(m), na.rm=TRUE)
}

## Manual

sawDF<-function(m){
  data.frame(
    behjouSawTime(m),
    ghafSawTime(m),
    hartsoughSawTime(m),
    kluenderSawTime(m))
}

sawTime2<-function(m){
 ifelse((m$"Harvesting.System")=="Cable Manual WT/Log",
        (m$"Small.log.trees.per.acre"+m$"Large.log.trees.per.acre")*0.013333+(rowMeans(sawDF(m), na.rm=TRUE)),
        ifelse((m$"Harvesting.System")=="Ground-Based Manual Log"|(m$"Harvesting.System")=="Shovel Logging",
               ((m$"Small.log.trees.per.acre"+m$"Large.log.trees.per.acre")*0.013333+(rowMeans(sawDF(m), na.rm=TRUE))),
               (rowMeans(sawDF(m), na.rm=TRUE))))
 }

sawAvg<-function(m){
  ifelse(sawTime2(m)=="NaN", manBC2(m), sawTime2(m)+manBC2(m))
}

sawTime<-function(m){
  sawAvg(m)}

## Hours/Acre

## Harvester

harvesterDF<-function(m){
  data.frame(
    klepacHarTime(m),
    hieslHarTime(m),
    adebayoHarTime(m),
    karhaHarTime(m),
    jirousekHarTime(m))
}


harTime2<-function(m){
  rowMeans(harvesterDF(m), na.rm=TRUE)
}

harAvg<-function(m){
  ifelse(harTime2(m)=="NaN", mechBC(m), harTime2(m)+mechBC(m))
}

harTime<-function(m){
  harAvg(m)
}

## Helicopter Yarding Analysis

heliDF<-function(m){
  data.frame(
    flattenHeliTime(m),
    dykstraHeliTime(m),
    curtisHeliTime(m))
}

heliTime<-function(m){
  abs(rowMeans(heliDF(m), na.rm=TRUE)*1.2)
}

## Cable Yarding Analysis

yarderDF<-function(m){
  data.frame(
    fisherYarderTime(m)^.85,
    abs(curtisYardingTime(m)),
    abs(curtisYarding2Time(m)))
}

yarderTime<-function(m){
  rowMeans(yarderDF(m), na.rm=TRUE)
}

## Analizing Time Conversions and Price Regression

highYardingTime<-function(m){
  ifelse(yarderTime(m)>sawTime(m), yarderTime(m), 
         ifelse(sawTime(m)>yarderTime(m), sawTime(m), 1))
} ## Returns the limiting time/acre

highGroundHarSkidTime<-function(m){
  ifelse(skidderTime(m)>harTime(m), skidderTime(m), ifelse(harTime(m)>skidderTime(m), harTime(m), 1))
} ## Returns the limiting time

highGroundFBSkidTime<-function(m){
  ifelse(skidderTime(m)>fbTime(m), skidderTime(m), ifelse(fbTime(m)>skidderTime(m), fbTime(m), 1))
} ## Returns the limiting time

highGroundCTLTime<-function(m){
  ifelse(forTime(m)>harTime(m), forTime(m), ifelse(harTime(m)>forTime(m), harTime(m), 1))
}

yarderEst<-function(m,p){
  rowSums(cbind((yarderTime(m)*p["yarder",]),(sawTime(m)*p["chainsaw",]),(slideTime(m)*p["slideboom",]),((chipTime(m)*.2)*p["chipper",])),na.rm = TRUE)
}

yarderSawEst<-function(m,p){
  rowSums(cbind((yarderTime(m)*p["yarder",]),(sawTime(m)*p["chainsaw",]),(slideTime(m)*p["slideboom",]),((chipTime(m)*.15)*p["chipper",])),na.rm=TRUE)
}

sawSkidEst<-function(m,p){
  rowSums(cbind(((skidderTime(m)*p["grappleSkidderLarge",])),(sawTime(m)*p["chainsaw",]),(slideTime(m)*p["slideboom",]),((chipTime(m)*.1)*p["chipper",])),na.rm = TRUE)
}

sawSkidLogEst<-function(m,p){
  rowSums(cbind((skidderTime(m)*p["grappleSkidderLarge",]*2.5),(sawTime(m)*p["chainsaw",]*2.5),(slideTime(m)*p["slideboom",]),((chipTime(m)*.1)*p["chipper",])),na.rm=TRUE)
}

harForEst<-function(m,p){
  rowSums(cbind((((forTime(m)*.75)+2)*p["forwarder",]),((harTime(m)*p["harvester",])*5),(slideTime(m)*p["slideboom",]),((chipTime(m)*.2)*p["chipper",])),na.rm = TRUE)
}

harSkidEst<-function(m,p){
  rowSums(cbind((harTime(m)*p["harvester",]),(skidderTime(m)*p["grappleSkidderLarge",]),(slideTime(m)*p["slideboom",]),((chipTime(m)*.1)*p["chipper",])),na.rm = TRUE)
}

heliHarEst<-function(m,p){
  rowSums(cbind((heliTime(m)*p["helicopter",]),(harTime(m)*p["harvester",])),na.rm = TRUE)
}

yarderHarEst<-function(m,p){
  rowSums(cbind((yarderTime(m)*p["yarder",]),(harTime(m)*p["harvester",]),((chipTime(m)*.15)*p["chipper",])),na.rm = TRUE)
}

fbSkidEst<-function(m,p){
  rowSums(cbind((fbTime(m)*p["fellerBuncher",]),((skidderTime(m)*1.5*p["grappleSkidderLarge",])*1.3),(slideTime(m)*p["slideboom",]),((chipTime(m)*.15)*p["chipper",])),na.rm = TRUE)
}

yarderSawLogEst<-function(m,p){
  rowSums(cbind((yarderTime(m)*p["yarder",]),(sawTime(m)*p["chainsaw",]),(slideTime(m)*p["slideboom",]),((chipTime(m)*.5)*p["chipper",])),na.rm = TRUE)
}

heliSawEst<-function(m,p){
  rowSums(cbind((heliTime(m)*p["helicopter",]),(sawTime(m)*p["chainsaw",])),na.rm = TRUE)
}

shovelEst<-function(m,p){
  rowSums(cbind((fbTime(m)*p["shovel",]),(sawTime(m)*p["fellerBuncher",]),(slideTime(m)*p["slideboom",]),((chipTime(m)*.15*p["chipper",]))),na.rm = TRUE)
}


################ Cost Data Frames

idahoCost<-data.frame(row.names=c("fellerBuncher","forwarder","harvester","grappleSkidderLarge","cableSkidderLarge","wheelFellerBuncher","crawlerFellerBuncher","yarder","slideboom","chainsaw","chipper"),values=c(
  210.10,170.00,180.00,110.00,149.80,205.10,185.60,299.00,200.00,90.00,100.00))

washingtonCost<-data.frame(row.names=c("fellerBuncher","forwarder","harvester","grappleSkidderLarge","cableSkidderLarge","wheelFellerBuncher","crawlerFellerBuncher","yarder","slideboom","chainsaw","chipper"),values=c(190.10,170.00,170.00,155.00,190.00,195.10,180.60,305.00,200.00,90.00,100.00))

oregonCost<-data.frame(row.names=c("fellerBuncher","forwarder","harvester","grappleSkidderLarge","cableSkidderLarge","wheelFellerBuncher","crawlerFellerBuncher","yarder","slideboom","chainsaw","chipper"),values=c(175.10,180.00,175.00,150.00,150.90,190.10,162.60,303.00,200.00,90.00,100.00))

############### Common Harvest Costs

yarderPrice<-function(m){
  m["yarder",]+m["chainsaw",]
}

groundMechHarSkidPrice<-function(m){
  m["harvester",]+(m["grappleSkidderLarge",])
}

groundMechFBSkidPrice<-function(m){
  m["fellerBuncher",]+(m["grappleSkidderLarge",])
}

groundCTLPrice<-function(m){
  m["forwarder",]+m["harvester",]
}

exPriceFBSkid<-idahoCost["fellerBuncher",]+(idahoCost["grappleSkidderLarge",])

exPriceHarSkid<-idahoCost["harvester",]+(idahoCost["grappleSkidderLarge",])

TyardingPrice<-function(m){
  yarderPrice(m)*highYardingTime(m)
}

TgroundMechHarSkidPrice<-function(m){
  exPriceHarSkid*highGroundHarSkidTime(m)
}

TgroundMechFBSkidPrice<-function(m){
  exPriceFBSkid*highGroundFBSkidTime(m)
}

TgroundCTLPrice<-function(m){
  forTime(m)*groundCTLPrice(idahoCost)
}

############### Prescribed Analysis 

prescribedEq<-function(m){
  ifelse((m$"Harvesting.System")=="Cable Manual WT", highYardingTime(m),
         ifelse((m$"Harvesting.System")=="Ground-Based Manual WT", highGroundHarSkidTime(m),
                ifelse((m$"Harvesting.System")=="Ground-Based Mech WT", highGroundFBSkidTime(m),
                       ifelse((m$"Harvesting.System")==30500, highGroundCTLTime(m), 1))))
}

labelEq1<-function(m){
  ifelse((m$"Harvesting.System")=="Cable Manual WT", "Cable Manual WT", 
         ifelse((m$"Harvesting.System")=="Ground-Based Manual WT", "Ground-Based Manual WT",
                ifelse((m$"Harvesting.System")=="Ground-Based Mech WT", "Ground-Based Mech WT",
                       ifelse((m$"Harvesting.System")=="Ground-Based CTL", "Ground-Based CTL",
                              ifelse((m$"Harvesting.System")=="Helicopter CTL", "Helicopter CTL", 
                                     ifelse((m$"Harvesting.System")=="Cable Manual WT/Log", "Cable Manual WT/Log",
                                            ifelse((m$"Harvesting.System")=="Cable Manual Log", "Cable Manual Log",
                                                   ifelse((m$"Harvesting.System")=="Ground-Based Manual Log", "Ground-Based Manual Log", 
                                                          ifelse((m$"Harvesting.System")=="Helicopter Manual WT", "Helicopter Manual WT", 
                                                                 ifelse((m$"Harvesting.System")=="Cable CTL", "Cable CTL",
                                                                        ifelse((m$"Harvesting.System")=="Shovel Logging", "Shovel Logging",  1)))))))))))
}

cNames<-function(m){colnames(m)<-c("Stand ID", "Treatment Cost", "FVS OpCost Treatment Selection")}





############## Ideal Analysis


mic<-function(x){
  (m3[x,]*.35)*((30/25)+(30/45))
}



slopeEq<-function(m){
  ifelse((m$"Percent.Slope")>45.001, highYardingTime(m), idealGround(m))
}

harFBChoice<-function(m){
  ifelse(TgroundMechHarSkidPrice(m)>TgroundMechFBSkidPrice(m), highGroundFBSkidTime(m),
         ifelse(TgroundMechFBSkidPrice(m)>TgroundMechHarSkidPrice(m), highGroundHarSkidTime(m), 1))
}

idealGround<-function(m){
  ifelse((harFBChoice(m)*groundMechHarSkidPrice(idahoCost))>TgroundCTLPrice(m), harFBChoice(m),
         ifelse((harFBChoice(m)*groundMechHarSkidPrice(idahoCost))<TgroundCTLPrice(m), highGroundCTLTime(m), 1))
}

print("896: OK")

mIdealSubset<-subset(m, Small.log.trees.average.volume.ft3.<3 & Small.log.trees.average.volume.ft3.>.0001)

mIdeal<-m[! rownames(m) %in% rownames(mIdealSubset),]

mIdeal2<-data.frame(m[row.names(mIdealSubset),])



labelEq2<-function(m,p){ifelse(m$Percent.Slope>45,ifelse(
  m$"Harvesting.System"=="Cable Manual WT"|m$"Harvesting.System"=="Cable Manual WT/Log",ifelse(
    yarderEst(m,m3)<yarderSawEst(m,m3),yarderEst(m,m3),yarderSawEst(m,m3)),ifelse(
      yarderSawLogEst(m,m3)<yarderHarEst(m,m3)&yarderSawEst(m,m3),yarderSawLogEst(m,m3),ifelse(
        yarderHarEst(m,m3)<yarderSawEst(m,m3),yarderHarEst(m,m3),yarderSawEst(m,m3)))),ifelse(
          m$"Harvesting.System"=="Ground-Based CTL"|m$"Harvesting.System"=="Ground-Based Manual Log",ifelse(
            harForEst(m,m3)<sawSkidEst(m,m3),harForEst(m,m3),sawSkidEst(m,m3)),ifelse(
              harSkidEst(m,m3)<fbSkidEst(m,m3) & shovelEst(m,m3),harSkidEst(m,m3),ifelse(fbSkidEst(m,m3)<shovelEst(m,m3),fbSkidEst(m,m3),shovelEst(m,m3)))))
  }




print("labelEq2: OK")

labelEq3<-function(m,p){
  ifelse(m$Percent.Slope>45,ifelse(
    m$"Harvesting.System"=="Cable Manual WT"|m$"Harvesting.System"=="Cable Manual WT/Log",ifelse(
      yarderEst(m,m3)<yarderSawEst(m,m3),"Cable Manual WT","Cable Manual WT/Log"),ifelse(
        yarderSawLogEst(m,m3)<yarderHarEst(m,m3)&yarderSawEst(m,m3),"Cable Manual Log",ifelse(
          yarderHarEst(m,m3)<yarderSawEst(m,m3),"Cable CTL","Cable Manual WT/Log"))),ifelse(
            m$"Harvesting.System"=="Ground-Based CTL"|m$"Harvesting.System"=="Ground-Based Manual Log",ifelse(
              harForEst(m,m3)<sawSkidEst(m,m3),"Ground-Based CTL","Ground-Based Manual WT"),ifelse(
                harSkidEst(m,m3)<fbSkidEst(m,m3) & shovelEst(m,m3),"Ground-Based CTL", ifelse(fbSkidEst(m,m3)< shovelEst(m,m3),"Ground-Based Mech WT","Shovel Logging"))))
}

print("labelEq3: OK")




mim<-function(m,m3){ifelse((m$"Harvesting.System")=="Cable Manual WT", mic("yarder")+mic("chainsaw"),
                           ifelse((m$"Harvesting.System")=="Ground-Based Manual WT", mic("chainsaw")+mic("grappleSkidderLarge"),
                                  ifelse((m$"Harvesting.System")=="Ground-Based Mech WT", mic("fellerBuncher")+mic("grappleSkidderLarge"),
                                         ifelse((m$"Harvesting.System")=="Ground-Based CTL", mic("forwarder")+mic("harvester"),
                                                ifelse((m$"Harvesting.System")=="Helicopter CTL", mic("helicopter"),
                                                       ifelse((m$"Harvesting.System")=="Cable Manual WT/Log", mic("yarder")+mic("chainsaw"),
                                                              ifelse((m$"Harvesting.System")=="Cable Manual Log", mic("yarder")+mic("chainsaw"),
                                                                     ifelse((m$"Harvesting.System")=="Ground-Based Manual Log", mic("chainsaw")+mic("grappleSkidderLarge"),
                                                                            ifelse((m$"Harvesting.System")=="Helicopter Manual WT", mic("helicopter"),
                                                                                   ifelse((m$"Harvesting.System")=="Cable CTL", mic("chainsaw")+mic("yarder"),
                                                                                          ifelse((m$"Harvesting.System")=="Shovel Logging", mic("chainsaw")+mic("shovel"),  1)))))))))))
}

print("MIM: OK")

miclb<-function(m){ifelse(m$"Harvesting.System"=="Cable Manual WT"|
                            m$"Harvesting.System"=="Ground-Based Manual WT"|
                            m$"Harvesting.System"=="Cable Manual WT/Log"|
                            m$"Harvesting.System"=="Cable Manual Log"|
                            m$"Harvesting.System"=="Ground-Based Manual Log"|
                            m$"Harvesting.System"=="Helicopter Manual WT"|
                            m$"Harvesting.System"=="Shovel Logging", (((m$"Move_In_Hours"*100)*2)/m$"Harvest_area_assumed_acres"),
                          ifelse(m$"Harvesting.System"=="Ground-Based Mech WT"|
                                   m$"Harvesting.System"=="Ground-Based CTL"|
                                   m$"Harvesting.System"=="Helicopter CTL"|
                                   m$"Harvesting.System"=="Cable CTL",(((m$"Move_In_Hours"*100)*3)/m$"Harvest_area_assumed_acres"),0))}

print("MICLB: OK")







chippingCost2<-function(m,p){ifelse((m$"Harvesting.System")=="Cable Manual WT", ((chipTime2(m)*.203)*p["chipper",]),
                                    ifelse((m$"Harvesting.System")=="Ground-Based Manual WT", ((chipTime2(m)*.21)*p["chipper",]),
                                           ifelse((m$"Harvesting.System")=="Ground-Based Mech WT"|(m$"Harvesting.System")=="Shovel Logging", ((chipTime2(m)*.19)*p["chipper",]),
                                                  ifelse((m$"Harvesting.System")=="Ground-Based CTL", ((chipTime2(m)*.13)*p["chipper",]),
                                                         ifelse((m$"Harvesting.System")=="Helicopter CTL", ((chipTime2(m)*.13)*p["chipper",]),
                                                                ifelse((m$"Harvesting.System")=="Cable Manual WT/Log", ((chipTime2(m)*.15)*p["chipper",]),
                                                                       ifelse((m$"Harvesting.System")=="Cable Manual Log", ((chipTime2(m)*.13)*p["chipper",]),
                                                                              ifelse((m$"Harvesting.System")=="Ground-Based Manual Log", ((chipTime2(m)*.131)*p["chipper",]),
                                                                                     ifelse((m$"Harvesting.System")=="Helicopter Manual WT", ((chipTime2(m)*.2)*p["chipper",]),
                                                                                            ifelse((m$"Harvesting.System")=="Cable CTL", ((chipTime2(m)*.1)*p["chipper",]), "NAN"))))))))))
}


chippingCost3<-function(m,p){ifelse((m)=="Cable Manual WT", ((chipTime2(m)*.203)*p["chipper",]),
                                    ifelse((m)=="Ground-Based Manual WT", ((chipTime2(m)*.21)*p["chipper",]),
                                           ifelse((m)=="Ground-Based Mech WT"|(m$"Harvesting.System")=="Shovel Logging", ((chipTime2(m)*.19)*p["chipper",]),
                                                  ifelse((m)=="Ground-Based CTL", ((chipTime2(m)*.13)*p["chipper",]),
                                                         ifelse((m)=="Helicopter CTL", ((chipTime2(m)*.13)*p["chipper",]),
                                                                ifelse((m)=="Cable Manual WT/Log", ((chipTime2(m)*.15)*p["chipper",]),
                                                                       ifelse((m)=="Cable Manual Log", ((chipTime2(m)*.13)*p["chipper",]),
                                                                              ifelse((m)=="Ground-Based Manual Log", ((chipTime2(m)*.131)*p["chipper",]),
                                                                                     ifelse((m)=="Helicopter Manual WT", ((chipTime2(m)*.2)*p["chipper",]),
                                                                                            ifelse((m)=="Cable CTL", ((chipTime2(m)*.1)*p["chipper",]), "NAN"))))))))))
}

idealTest<-function(m,m3){ifelse(m$Percent.Slope>45,
                                  ifelse(yarderEst(m,m3)<yarderSawEst(m,m3)&yarderSawLogEst(m,m3)&yarderHarEst(m,m3),yarderEst(m,m3),
                                         ifelse(yarderSawEst(m,m3)<yarderSawLogEst(m,m3) & yarderHarEst(m,m3),yarderSawEst(m,m3),
                                                ifelse(yarderSawLogEst(m,m3)<yarderHarEst(m,m3),yarderSawLogEst(m,m3),yarderHarEst(m,m3)))),
                                  ifelse(sawSkidEst(m,m3)<fbSkidEst(m,m3)&harForEst(m,m3)&sawSkidLogEst(m,m3)&shovelEst(m,m3), sawSkidEst(m,m3),
                                         ifelse(fbSkidEst(m,m3)<harForEst(m,m3)&sawSkidLogEst(m,m3)&shovelEst(m,m3), fbSkidEst(m,m3), ifelse(harForEst(m,m3)<sawSkidLogEst(m,m3)&shovelEst(m,m3), harForEst(m,m3),ifelse(sawSkidLogEst(m,m3)<shovelEst(m,m3),sawSkidLogEst(m,m3),shovelEst(m,m3))))))}

idealTest2<-function(m,m3){ifelse(m$Percent.Slope>45,
                                  ifelse(yarderEst(m,m3)<yarderSawEst(m,m3)&yarderSawLogEst(m,m3)&yarderHarEst(m,m3),"Cable Manual WT",
                                         ifelse(yarderSawEst(m,m3)<yarderSawLogEst(m,m3) & yarderHarEst(m,m3),"Cable Manual WT/Log",
                                                ifelse(yarderSawLogEst(m,m3)<yarderHarEst(m,m3),"Cable Manual Log","Cable CTL"))),
                                  ifelse(sawSkidEst(m,m3)<fbSkidEst(m,m3)&harForEst(m,m3)&sawSkidLogEst(m,m3)&shovelEst(m,m3), "Ground-Based Manual WT",
                                         ifelse(fbSkidEst(m,m3)<harForEst(m,m3)&sawSkidLogEst(m,m3)&shovelEst(m,m3), "Ground-Based Mech WT", 
                                                ifelse(harForEst(m,m3)<sawSkidLogEst(m,m3)&shovelEst(m,m3), "Ground-Based CTL",ifelse(sawSkidLogEst(m,m3)<shovelEst(m,m3),"Ground-Based Manual Log","Shovel Logging")))))
}



idealMIM<-function(m,m3){ifelse((idealTest2(m,m3))=="Cable Manual WT", mic("yarder")+mic("chainsaw"),
                                ifelse((idealTest2(m,m3))=="Ground-Based Manual WT", mic("chainsaw")+mic("grappleSkidderLarge"),
                                       ifelse((idealTest2(m,m3))=="Ground-Based Mech WT", mic("fellerBuncher")+mic("grappleSkidderLarge"),
                                              ifelse((idealTest2(m,m3))=="Ground-Based CTL", mic("forwarder")+mic("harvester"),
                                                     ifelse((idealTest2(m,m3))=="Helicopter CTL", mic("helicopter"),
                                                            ifelse((idealTest2(m,m3))=="Cable Manual WT/Log", mic("yarder")+mic("chainsaw"),
                                                                   ifelse((idealTest2(m,m3))=="Cable Manual Log", mic("yarder")+mic("chainsaw"),
                                                                          ifelse((idealTest2(m,m3))=="Ground-Based Manual Log", mic("chainsaw")+mic("grappleSkidderLarge"),
                                                                                 ifelse((idealTest2(m,m3))=="Helicopter Manual WT", mic("helicopter"),
                                                                                        ifelse((idealTest2(m,m3))=="Cable CTL", mic("chainsaw")+mic("yarder"),
                                                                                               ifelse((idealTest2(m,m3)=="Shovel Logging"),mic("shovel")+mic("chainsaw"), 1)))))))))))
}

mIdeal3<-mIdeal

mIdeal3$Harvesting.System<-idealTest2(m,m3)

idealLB<-function(m,m3){ifelse(idealTest2(m,m3)=="Cable Manual WT"|
                                 idealTest2(m,m3)=="Ground-Based Manual WT"|
                                 idealTest2(m,m3)=="Cable Manual WT/Log"|
                                 idealTest2(m,m3)=="Cable Manual Log"|
                                 idealTest2(m,m3)=="Ground-Based Manual Log"|
                                 idealTest2(m,m3)=="Helicopter Manual WT"|
                                 idealTest2(m,m3)=="Shovel Logging", (((m$"Move_In_Hours"*100)*2)/m$"Harvest_area_assumed_acres"),
                               ifelse(idealTest2(m,m3)=="Ground-Based Mech WT"|
                                        idealTest2(m,m3)=="Ground-Based CTL"|
                                        idealTest2(m,m3)=="Helicopter CTL"|
                                        idealTest2(m,m3)=="Cable CTL",(((m$"Move_In_Hours"*100)*3)/m$"Harvest_area_assumed_acres"),0))}

idealCost<-function(m,m3){idealTest(m,m3)+idealMIM(m,m3)+idealLB(m,m3)}




############# Selection


m5<-function(m,m3){
  prescribe(m,m3)+mim(m,m3)+miclb(m)
}

print("M5: OK")

idealCost2<-function(m5,m,m3){
  ifelse(m5(m,m3)<idealCost(m,m3), m5(m,m3),idealCost(m,m3))
}

mIdeal5<-function(m5,m,m3){
  ifelse(m5(m,m3)<idealCost(m,m3), labelEq1(m),idealTest2(m,m3))
}

m5.1<-round(m5(m,m3), 2)

m6<-data.frame(m$"Stand", m$"YearCostCalc", m5.1, as.numeric(chippingCost2(m,m3)), miclb(m), labelEq1(m),
               m$"RxPackage_Rx_RxCycle", m$"biosum_cond_id", m$"RxPackage", m$"Rx", m$"RxCycle")

print("M6: OK")

m7<-m6

colnames(m7)<-c("stand", "rx_year", "harvest_cpa","chip_cpa", "assumed_movein_cpa", "harvest_system", "RxPackage_Rx_RxCycle", "biosum_cond_id","RxPackage","Rx","RxCycle")

exp2<-subset(m7, harvest_cpa<1 | harvest_cpa=="NA" | harvest_cpa=="Inf")


m16<-data.frame(m$"Stand", m$"YearCostCalc", mIdeal5(m5,m,m3), round(idealCost2(m5,m,m3),2), ifelse(mIdeal5(m5,m,m3)==labelEq1(m),"TRUE","FALSE"),
                idealLB(m,m3),as.numeric(chippingCost2(mIdeal3,m3)),m$"RxPackage_Rx_RxCycle",
                m$"biosum_cond_id", m$"RxPackage", m$"Rx", m$"RxCycle")

print("m16:OK")

colnames(m16)<-c("stand", "rx_year", "harvest_system", "harvest_cpa", "Matches Original System?","ideal_assumed_movein_cpa","ideal_chip_cpa", "RxPackage_Rx_RxCycle",
                 "biosum_cond_id", "RxPackage", "Rx", "RxCycle")

m16<-m16[! m16$harvest_cpa %in% NA,]

row.names(m16)<-1:nrow(m16)

m17<-subset(m16, harvest_cpa<1 | harvest_system=="<NA>")

sqlSave(con, m17, tablename="OpCost_Ideal_Errors",append = TRUE)

print("m17: OK")

exp21<-data.frame(m[row.names(exp2),],exp2$harvest_cpa)

exp20<-ifelse(exp21$exp2.harvest_cpa=="Inf" & exp21$One.way.Yarding.Distance==0, "Zeroed Yarding Distance",
              ifelse(exp21$exp2.harvest_cpa=="Inf" & exp21$Small.log.trees.average.volume.ft3.==0 & exp21$Large.log.trees.average.vol.ft3.==0, "Zeroed Volume: Twitch",
                     ifelse(exp21$exp2.harvest_cpa=="NaN" & exp21$Small.log.trees.average.volume.ft3.==0 & exp21$Large.log.trees.average.vol.ft3.==0 & exp21$Harvesting.System=="Ground-Based Mech WT", "Completely Zeroed Logs:GB",
                            ifelse(exp21$exp2.harvest_cpa=="NaN" & exp21$Small.log.trees.average.volume.ft3.==0 & exp21$Large.log.trees.average.vol.ft3.==0 & exp21$Harvesting.System=="Cable Manual WT", "Completely Zeroed Logs:CB",
                                   ifelse(exp21$exp2.harvest_cpa<0 & exp21$Harvesting.System=="Ground-Based Mech WT", "Check Skidding Times",
                                          ifelse(exp21$exp2.harvest_cpa<0 & exp21$Harvesting.System=="Cable Manual WT", "Check Short Yarding Distances",
                                                 ifelse(exp21$exp2.harvest_cpa<0 & exp21$Harvesting.System=="Helicopter CTL", "Insufficient Trees", "Oh Yay, a new one!")))))))

exp22<-data.frame(exp2,exp20)


colnames(exp22)<-c("stand", "rx_year", "harvest_cpa","chip_cpa", "assumed_movein_costs", "harvest_system","RxPackage_Rx_RxCycle",
                   "biosum_cond_id", "RxPackage", "Rx", "RxCycle", "error_message")

m9<-m7[! rownames(m7) %in% rownames(exp2),]

print("m9:OK")

m10<-m9[! m9$stand %in% NA,]

row.names(m10)<-1:nrow(m10)

print("m10Row:OK")

sqlSave(con, exp22, tablename="OpCost_Errors",append = TRUE)

print("exp22:SAVED")

sqlSave(con, m10, tablename="OpCost_Output", safer=FALSE)

print("sqlSave:ok")

ifelse(idealTable==1,sqlSave(con, m16, tablename="OpCost_Ideal_Output", safer=FALSE),print("Ideal Table Not Created or Updated"))

print("m10:OK")

odbcCloseAll()