##Functions for Data Analysis: at least for plotting 

#PerSiteAnalys
library(plyr)

##############################################################################
##########################          HEIGHT          ##########################    
##############################################################################

#instead of just raw growth, now you have percent growth
PercGrowth_byTree <- function(datasets, years, sites){

  Growthrtdata <- list()
  iter = 0
  
  for (sitedata in datasets){
    iter = iter + 1
    
    sitedata <- sitedata[!is.na(sitedata$Height),] #because those will be useless anyway
    st <- as.vector(sitedata$Site[1])
    sitedata$Year = as.numeric(sitedata$Year)
    sitedata$Height = as.numeric(sitedata$Height)
    sitedata$MSP <- as.numeric(sitedata$MSP)
    sitedata$Slope <- as.numeric(sitedata$Slope)
    sitedata$Aspect <- as.numeric(sitedata$Aspect)
    
    ##Growth Rate
    column_names <- c("Site", "Ecoregion", "Source", "Family", "Block", "Tree", "TotGrowth", "NumDieback", "AmntDieback", "Dist", 
                      "ElevDist", "Slope", "Aspect", "eFFP", "MSP", "DD.5.sp")
    GrowthRatemat = matrix(NA, nrow = length(unique(sitedata$Family))*length(unique(sitedata$Block))*length(unique(sitedata$Tree)), 
                           ncol = length(column_names))
    colnames(GrowthRatemat) = column_names
    
    row_ind <- 0 #so you know where to put the data in the matrix 
    yrs <- unique(sitedata$Year)
    
    last_yr <- max(yrs[!is.na(yrs)])
    first_yr <- min(yrs[!is.na(yrs)])
    
    for (parent in unique(sitedata$Family)){
      
      areadat <- sitedata[sitedata$Family == parent,]
      
      for (block in unique(areadat$Block)){
        
        blockdat <- areadat[areadat$Block== block,]
        for (tree in unique(blockdat$Tree)){ #have to deal with multiple trees 
          row_ind <- row_ind + 1
          treedat <- blockdat[blockdat$Tree== tree,]
          
          if (NA %in% treedat$Condition){ #get rid of trees with unknown condition
            Nanind <- match(NA, treedat$Condition)
            treedat <- treedat[-Nanind,]
          }
          
          if (NA %in% treedat$Height){ #get rid of trees with unknown heights
            Nanind <- match(NA, treedat$Height)
            treedat <- treedat[-Nanind,]
          }
          
          treedat <- treedat[treedat$Condition != 4, ] # get rid of dead trees for height analysis
          chng <- 0
          
          
          if (sum(!is.na(unique(treedat$Year))) > 1){
            
            
            if (last_yr %in% treedat$Year & first_yr %in% treedat$Year){
              pre_post <- 100*((treedat$Height[treedat$Year== last_yr] - treedat$Height[treedat$Year== first_yr])/treedat$Height[treedat$Year== first_yr])
            } else {
              pre_post <- NA
            }
            
            N_dbk <- 0
            dbk <- 0
            
            
          #Still editing
            for (yr in unique(treedat$Year[!is.na(treedat$Year)])){
              
              if ((yr - 1)  %in% treedat$Year){
                chng = 100*((treedat$Height[treedat$Year== yr] - treedat$Height[treedat$Year== (yr - 1)])/treedat$Height[treedat$Year== (yr - 1)])
              } else if ((yr - 2) %in% treedat$Year){
                chng = 100*((treedat$Height[treedat$Year== yr] - treedat$Height[treedat$Year== (yr - 2)])/(2*treedat$Height[treedat$Year== (yr - 2)]))
              } else {
                chng <- 0
              }
              
              if (chng <0){
                N_dbk <- N_dbk + 1
                dbk <- dbk + chng
              }
            } #Year loop
            
          } else {
            pre_post <- NA
            N_dbk <- 0
            dbk <- 0
          }
          
          
          
          ##NEED TO AVERAGE AT THIS LEVEL
          GrowthRatemat[row_ind, 1] = st  
          GrowthRatemat[row_ind, 2] = as.vector(areadat$Ecoregion[1])
          GrowthRatemat[row_ind, 3] = as.vector(areadat$Source[1])
          GrowthRatemat[row_ind, 4] = parent
          GrowthRatemat[row_ind, 5] = block
          GrowthRatemat[row_ind, 6] = tree
          GrowthRatemat[row_ind, 7] = pre_post
          GrowthRatemat[row_ind, 8] = N_dbk
          GrowthRatemat[row_ind, 9] = abs(dbk)
          GrowthRatemat[row_ind, 10] = as.numeric(areadat$Distance[1])
          GrowthRatemat[row_ind, 11] = as.numeric(areadat$Slope[1])
          GrowthRatemat[row_ind, 12] = as.numeric(areadat$Aspect[1])
          GrowthRatemat[row_ind, 13] = as.numeric(areadat$ElevDist[1])
          GrowthRatemat[row_ind, 14] = as.numeric(areadat$eFFP[1])
          GrowthRatemat[row_ind, 15] = as.numeric(areadat$MSP[1])
          GrowthRatemat[row_ind, 16] = as.numeric(areadat$DD.5.sp[1])
          
          
          
          
        } # Tree loop
        
        
      } #Block loop
      
      
    } #parent loop
    
    Growthrt <- as.data.frame(GrowthRatemat)
    
    Growthrt[,5:16] <- sapply(Growthrt[,5:16],as.vector) #unclear why this is necessary
    Growthrt[,5:16] <- sapply(Growthrt[,5:16],as.numeric)
    
    Growthrtdata[[iter]] <- Growthrt[!is.na(Growthrt$Family),]
  }
  
  names(Growthrtdata) = names(datasets)

  return(Growthrtdata)


}




IndivHeights_byTree <- function(datasets, years, sites){
  Growthrtdata <- list()
  iter = 0
  
  for (sitedata in datasets){
    iter = iter + 1
    
    sitedata <- sitedata[!is.na(sitedata$Height),] #because those will be useless anyway
    st <- as.vector(sitedata$Site[1])
    sitedata$Year = as.numeric(sitedata$Year)
    sitedata$Height = as.numeric(sitedata$Height)
    sitedata$MSP <- as.numeric(sitedata$MSP)
    sitedata$Slope <- as.numeric(sitedata$Slope)
    sitedata$Aspect <- as.numeric(sitedata$Aspect)
    
    ##Growth Rate
    column_names <- c("Site", "Ecoregion", "Source", "Family", "Block", "Tree", "TotGrowth", "NumDieback", "AmntDieback", "Dist", 
                      "ElevDist", "Slope", "Aspect", "DD.5.sp", "eFFP", "MSP")
    GrowthRatemat = matrix(NA, nrow = length(unique(sitedata$Family))*length(unique(sitedata$Block))*length(unique(sitedata$Tree)), 
                           ncol = length(column_names))
    colnames(GrowthRatemat) = column_names
    
    row_ind <- 0 #so you know where to put the data in the matrix 
    yrs <- unique(sitedata$Year)
    
    last_yr <- max(yrs[!is.na(yrs)])
    first_yr <- min(yrs[!is.na(yrs)])
    
    for (parent in unique(sitedata$Family)){
      
      areadat <- sitedata[sitedata$Family == parent,]
      
      for (block in unique(areadat$Block)){
        
        blockdat <- areadat[areadat$Block== block,]
        for (tree in unique(blockdat$Tree)){ #have to deal with multiple trees 
          row_ind <- row_ind + 1
          treedat <- blockdat[blockdat$Tree== tree,]
          
          if (NA %in% treedat$Condition){ #get rid of trees with unknown condition
            Nanind <- match(NA, treedat$Condition)
            treedat <- treedat[-Nanind,]
          }
          
          if (NA %in% treedat$Height){ #get rid of trees with unknown heights
            Nanind <- match(NA, treedat$Height)
            treedat <- treedat[-Nanind,]
          }
          
          treedat <- treedat[treedat$Condition != 4, ] # get rid of dead trees for height analysis
          chng <- 0
          
          
          if (sum(!is.na(unique(treedat$Year))) > 1){
            
            
            if (last_yr %in% treedat$Year & first_yr %in% treedat$Year){
              pre_post <- treedat$Height[treedat$Year== last_yr] - treedat$Height[treedat$Year== first_yr]
            } else {
              pre_post <- NA
            }
          
            N_dbk <- 0
            dbk <- 0
            for (yr in unique(treedat$Year[!is.na(treedat$Year)])){
              
              if ((yr - 1)  %in% treedat$Year){
                chng = treedat$Height[treedat$Year== yr] - treedat$Height[treedat$Year== (yr - 1)]
              } else if ((yr - 2) %in% treedat$Year){
                chng = (treedat$Height[treedat$Year== yr] - treedat$Height[treedat$Year== (yr - 2)])/2
              } else {
                chng <- 0
              }
              
              if (chng <0){
                N_dbk <- N_dbk + 1
                dbk <- dbk + chng
              }
            } #Year loop
            
          } else {
            pre_post <- NA
            N_dbk <- 0
            dbk <- 0
          }
            

          
          ##NEED TO AVERAGE AT THIS LEVEL
            GrowthRatemat[row_ind, 1] = st  
            GrowthRatemat[row_ind, 2] = as.vector(areadat$Ecoregion[1])
            GrowthRatemat[row_ind, 3] = as.vector(areadat$Source[1])
            GrowthRatemat[row_ind, 4] = parent
            GrowthRatemat[row_ind, 5] = block
            GrowthRatemat[row_ind, 6] = tree
            GrowthRatemat[row_ind, 7] = pre_post
            GrowthRatemat[row_ind, 8] = N_dbk
            GrowthRatemat[row_ind, 9] = abs(dbk)
            GrowthRatemat[row_ind, 10] = areadat$Distance[1]
            GrowthRatemat[row_ind, 11] = areadat$ElevDist[1]
            GrowthRatemat[row_ind, 12] = areadat$Slope[1]
            GrowthRatemat[row_ind, 13] = areadat$Aspect[1]
            GrowthRatemat[row_ind, 14] = areadat$DD.5.sp[1]
            GrowthRatemat[row_ind, 15] = areadat$eFFP[1]
            GrowthRatemat[row_ind, 16] = areadat$MSP[1]
            
            

          
        } # Tree loop
        

      } #Block loop
      
      
    } #parent loop
    
     Growthrt <- as.data.frame(GrowthRatemat)
    
    Growthrt[,5:16] <- sapply(Growthrt[,5:16],as.vector) #unclear why this is necessary
    Growthrt[,5:16] <- sapply(Growthrt[,5:16],as.numeric)
  
    Growthrtdata[[iter]] <- Growthrt[!is.na(Growthrt$Family),]
  }
  
  names(Growthrtdata) = names(datasets)
  
  return(Growthrtdata)
}



##############################################################################
##########################         CONDITION        ##########################    
##############################################################################

IndivCond <- function(datasets, sites){
  Conddata <- list()
  st_ind = 0
  for (sitedata in datasets){
    st_ind = st_ind + 1
    st = sites[st_ind]
    
    numrow <- (length(na.omit(unique(sitedata$Family))) * length(na.omit(unique(sitedata$Year))) *
                 length(na.omit(unique(sitedata$Block))))
      #number of families and years and blocks
    srcecond <- as.data.frame(matrix(0, nrow=numrow, ncol=15)) #before 0 was set to 0.0001
    colnames(srcecond) <- c("Block", "Family", "Source","Year","Dead", "Total", "PercDead", "Ecoregion", "Dist", 
                            "Slope", "Aspect", "ElevDist", "DD.5.sp", "eFFP", "MSP")
    
                                
    iter = 0
    for (fam in na.omit(unique(sitedata$Family))){
      famdata <- sitedata[sitedata$Family == fam,]
      famdata <- famdata[!is.na(famdata$Family),]
      
      for (block in na.omit(unique(sitedata$Block))){
        
        blockdata <- famdata[famdata$Block==block,]

        for (yr in na.omit(unique(famdata$Year))){
          iter = iter + 1
          yrdat <- famdata[famdata$Year == yr,]
          
          #If there is no data for that Family in that Year
          if (length(na.omit(yrdat$Condition)) == 0){
            next
          } 
          
          countdat <- as.data.frame(table(na.omit(yrdat$Condition)))
          
          #Block, family, year, and total number of trees
          srcecond[iter, 1] = block
          srcecond[iter, 2] = fam
          srcecond[iter, 3] <- famdata[1,4]
          srcecond[iter, 4] = yr
          total = sum(countdat[,2])
          srcecond[iter, 6] = total
          
          #Figure out how many there are in condition 4 (in this case)
          for (cond in 1:length(countdat[,1])){
            numcond = as.numeric(as.vector(countdat[cond, 1]))
            if (numcond == 4){
              srcecond[iter, 5] = countdat[cond,2]
            }
            
          } #end countmatrix loop
          
          #Percent dead
          srcecond[iter, 7] = srcecond[iter, 5]/total
          
          #Other variables
          eco <- famdata$Ecoregion[1]
          srcecond[iter, 8] = eco 
          
          ##Distance variables
          srcecond[iter, 9] = famdata$Distance[1]
          srcecond[iter, 10] = famdata$Slope[1]
          srcecond[iter, 11] = famdata$Aspect[1]
          srcecond[iter, 12] = famdata$ElevDist[1]
          
          #Climate Variables
          srcecond[iter, 13] = famdata$DD.5.sp[1]
          srcecond[iter, 14] = famdata$eFFP[1]
          srcecond[iter, 15] = famdata$MSP[1]
  
        } # end year loop
      } #End block loop
    } #end family loop
    
    ##Get rid of data that you can't use
    srcecond = srcecond[srcecond$Family!=0,]
    srcecond = srcecond[srcecond$Family!= "?",]
    
    Conddata[[st_ind]] <- srcecond[!is.na(srcecond$Family),]
  }
  
  names(Conddata) = names(datasets)
  return(Conddata)
  
}

##Just looks at overall mortality instead of by year
IndivCond_final <- function(datasets, sites){
  Conddata <- list()
  st_ind = 0
  for (sitedata in datasets){
    st_ind = st_ind + 1
    st = sites[st_ind]
    
    numrow <- (length(na.omit(unique(sitedata$Family)))*
                 length(na.omit(unique(sitedata$Block))))
    #number of families and years and blocks
    srcecond <- as.data.frame(matrix(0, nrow=numrow, ncol=15)) #before 0 was set to 0.0001
    colnames(srcecond) <- c("Block", "Family", "Source", "Dead", "Total", "PercDead", "Ecoregion", "Dist", 
                            "Slope", "Aspect", "ElevDist", "eFFP", "DD.5.sp", "MSP", "Site")
    
    
    iter = 0
    for (fam in na.omit(unique(sitedata$Family))){
      famdata <- sitedata[sitedata$Family == fam,]
      famdata <- famdata[!is.na(famdata$Family),]
      
      for (block in na.omit(unique(sitedata$Block))){
        dead_tree_nums <- c()
        
        blockdata <- famdata[famdata$Block==block,]
        blockdata <- blockdata[!is.na(blockdata$Block),]
        total <- length(na.omit(unique(blockdata$Tree)))
        
        prev_alive <- c() # just to have something to start with
        
        for (yr in na.omit(unique(blockdata$Year))){
          
          yrdat <- blockdata[blockdata$Year == yr,]
          
          #If there is no data for that Family in that Year
          if (length(na.omit(yrdat$Condition)) == 0){
            next
          } 

          ###Dead Trees - Need to avoid double-counting
          deadtr <- yrdat$Tree[yrdat$Condition == 4]
          alivetr <- yrdat$Tree[yrdat$Condition != 4]
          
          if (length(deadtr) == 0){ #no dead trees to take into account
            prev_alive <- alivetr #to help you check to see if trees disappear
            
            next 
          }
          
          if (length(prev_alive) != 0){ #check for trees that disappear
            for(tr in prev_alive){
              if(tr %in% deadtr || tr %in% alivetr){
                
                next #no problem
              } else {
                deadtr <- append(deadtr, tr) #this means the tree
                #disappeared so we're going to treat it as dead
              }
            }
          }
          
          for (dtr in deadtr){
            if (!(dtr %in% dead_tree_nums)){
              
              if (dtr %in% alivetr){ #if it is also listed as alive in a year
                next #we are going to ignore the dead one, assuming it was a mistake
              } else {
                dead_tree_nums <- append(dead_tree_nums, dtr) #if it has not already been counted
              }
              
            }
            
            
            
          }
          prev_alive <- alivetr #to help you check to see if trees disappear
          
        } # end year loop
        
        iter = iter + 1 #so you can skip if you need to
        
        srcecond[iter, 1] = block
        srcecond[iter, 2] = fam
        srcecond[iter, 3] <- famdata[1,4]
        total = length(unique(blockdata$Tree)) #number of trees that were there
        srcecond[iter, 5] = total
        srcecond[iter, 15] <- st
        
        srcecond[iter, 4] <- length(dead_tree_nums)
        #Percent dead
        srcecond[iter, 6] = srcecond[iter, 4]/total
        
        #Other variables
        eco <- as.vector(famdata$Ecoregion[1])
        srcecond[iter, 7] = eco 
        
        ##Distance variables
        srcecond[iter, 8] = as.numeric(famdata$Distance[1])
        srcecond[iter, 9] = as.numeric(famdata$Slope[1])
        srcecond[iter, 10] = as.numeric(famdata$Aspect[1])
        srcecond[iter, 11] = as.numeric(famdata$ElevDist[1])
        
        #Climate Variables
        srcecond[iter, 12] = as.numeric(famdata$eFFP[1])
        srcecond[iter, 13] = as.numeric(famdata$DD.5.sp[1])
        srcecond[iter, 14] = as.numeric(famdata$MSP[1])
        
        
      } #End block loop
    } #end family loop
    
    ##Get rid of data that you can't use
    srcecond = srcecond[srcecond$Family!=0,]
    srcecond = srcecond[srcecond$Family!= "?",]
    
    Conddata[[st_ind]] <- srcecond[!is.na(srcecond$Family),]
  }
  
  names(Conddata) = names(datasets)
  return(Conddata)
  
}

##Just looks at overall mortality instead of by year
IndivCond_finalpt <- function(datasets, sites){
  
  Conddata <- list()
  st_ind = 0
  for (sitedata in datasets){
    st_ind = st_ind + 1
    st = sites[st_ind]
    
    numrow <- (length(na.omit(unique(sitedata$Family)))*
                 length(na.omit(unique(sitedata$Block)))*length(na.omit(unique(sitedata$Tree))))
    #number of families and years and blocks
    srcecond <- as.data.frame(matrix(0, nrow=numrow, ncol=13)) #before 0 was set to 0.0001
    colnames(srcecond) <- c("Block", "Family", "Source", "Dead", "Ecoregion", "Dist", 
                            "Slope", "Aspect", "ElevDist", "DD.5.sp", "eFFP", "MSP", "Site")
    
    
    iter = 0
    for (fam in na.omit(unique(sitedata$Family))){ #iterate through the families
      if (fam == ""){
        next
      }
      famdata <- sitedata[sitedata$Family == fam,]
      famdata <- famdata[!is.na(famdata$Condition),]
      
      for (block in na.omit(unique(sitedata$Block))){ #iterate through the blocks
        
        blockdata <- famdata[famdata$Block==block,]
        blockdata <- blockdata[!is.na(blockdata$Block),]
        
        for (tree in na.omit(unique(blockdata$Tree))){
          
          treedata <- blockdata[blockdata$Tree==tree,]
          treedata <- treedata[!is.na(treedata$Condition),]
          
          max_yr <- max(unique(treedata$Year))
          max_cond <- treedata$Condition[treedata$Year == max_yr]
          min_yr <- min(unique(treedata$Year))
          min_cond <- treedata$Condition[treedata$Year == min_yr]
          
          if (length(min_cond) > 1){
            print(paste("Warning: Tree", tree, "from block", block, "from family", fam, "at site", st, "has more than one data point for", min_yr))
            if (4 %in% min_cond){
              min_cond <- min_cond[is.na(match(min_cond, 4))] #get rid of the dead trees
              
              if(length(min_cond) > 1){
                print(paste("Error for Tree", tree, "from block", block, "from family", fam))
              } else if (length(min_cond) == 0){
                min_cond <- 4
              }
            }
          }
          
          if (length(max_cond) > 1){
            print(paste("Warning: Tree", tree, "from block", block,"from family", fam, "at site", st, "has more than one data point for", max_yr))
            next #don't know what to do about this so just skip it
          }
          
          if (max_cond == 4){ #the ones that are dead by the end
            if (min_cond == 4){ #that means it started out dead, which shouldn't count
              dead <- NA 
            } else {
              dead <- 1
            }
            
          } else {
            dead <- 0
          }
          
          #This is lower down so you can skip a tree if need be
          iter = iter + 1
          
          #Basic Info
          srcecond[iter, 1] = block
          srcecond[iter, 2] = fam
          srcecond[iter, 3] <- famdata[1,4]
          
          #Dead
          srcecond[iter, 4] = dead
          #Other variables
          eco <- famdata$Ecoregion[1]
          srcecond[iter, 5] = eco 
          
          ##Distance variables
          srcecond[iter, 6] = famdata$Distance[1]
          srcecond[iter, 7] = famdata$Slope[1]
          srcecond[iter, 8] = famdata$Aspect[1]
          srcecond[iter, 9] = famdata$ElevDist[1]
          
          #Climate Variables
          srcecond[iter, 10] = famdata$DD.5.sp[1]
          srcecond[iter, 11] = famdata$eFFP[1]
          srcecond[iter, 12] = famdata$MSP[1]
          
          #Site
          srcecond[iter, 13] = st  
        } # end tree loop

      } #End block loop
    } #end family loop
    
    ##Get rid of data that you can't use
    srcecond = srcecond[srcecond$Family!=0,]
    srcecond = srcecond[srcecond$Family!= "?",]
    
    Conddata[[st_ind]] <- srcecond[!is.na(srcecond$Family),]
  }
  
  names(Conddata) = names(datasets)
  return(Conddata)
  
}
##############################################################################
##########################          LEADERS         ##########################    
##############################################################################

IndivLeads <- function(datasets, sites){
  Leaddata <- list()
  st_ind = 0
  for (sitedata in datasets){
    st_ind = st_ind + 1
    st = sites[st_ind]
    
    numrow <- length(unique(sitedata$Name)) * length(unique(sitedata$Year)) #number of sources and years
    srcelead <- as.data.frame(matrix(0, nrow=numrow, ncol=9))
    colnames(srcelead) <- c("Source", "Year", "Lead1", "Lead2", "Lead3", "Total", "SrcAvgDist", 
                            "Ecoregion", "EcoAvgDist")
    
    iter = 0
    for (fam in na.omit(unique(sitedata$Source))){
      if (fam == ''){
        next
      }
      famdata <- na.omit(sitedata[sitedata$Source == fam,])
      famdata <- famdata[famdata$cond != 4,] #remove dead trees from analysis
      for (yr in na.omit(unique(famdata$Year))){
        iter = iter + 1
        yrdat <- famdata[famdata$Year == yr,]
        countdat <- as.data.frame(table(yrdat$Leaders))
        srcelead[iter, 1] = fam
        srcelead[iter, 2] = yr
        total = sum(countdat[,2])
        srcelead[iter, 6] = total
        for (lead in 1:length(countdat[,1])){
          numlead = as.numeric(as.vector(countdat[lead, 1]))
          nums = countdat[lead,2]
          if (nums == 0){ #to deal with plotting errors
            nums = 0.0001
          }
          srcelead[iter, (numlead+2)] = nums 
        }
        
        srcelead[iter, 7] = mean(na.omit(famdata$Distance[famdata$Source==fam]))
        srcelead[iter, 8] = famdata$Ecoregion[1]
        srcelead[iter, 9] = mean(na.omit(sitedata$Distance[sitedata$Ecoregion==famdata$Ecoregion[1]]))
      }
    }
    #Order the sources by average distance to the planting location
    srcelead$Source <- factor(srcelead$Source, levels = unique(srcelead$Source[order(srcelead$SrcAvgDist)]))
    srcelead$Ecoregion <- factor(srcelead$Ecoregion, levels = unique(srcelead$Ecoregion[order(srcelead$EcoAvgDist)]))
    
    Leaddata[[st_ind]] <- srcelead
  }
  
  names(Leaddata) = names(datasets)
  return(Leaddata)
  
}

##########################################################################################################
##########################################################################################################
WelchsorWMW <- function(dataset, groupind, dind1, dind2=0, ttype="Welch"){
  
  grps <- unique(dataset[,groupind])
  #remove NaNs
  if ("0" %in% grps){
    Nanind <- match("0", grps)
    grps <- grps[-Nanind]
  } else if (NA %in% grps){
    Nanind <- match(NA, grps)
    grps <- grps[-Nanind]
  }
 
  othergrps <- grps
  if (ttype == "Welch"){
    numcol = 5
  } else {
    numcol = 3
  }
  comparisons <- matrix(0, nrow = sum(1:(length(grps)-1)), ncol = numcol)
  if (numcol == 5){
    colnames(comparisons) = c("Groups", "Statistic", "Pval", "Lwr","Upr")
  } else {
    colnames(comparisons) = c("Groups", "Statistic", "Pval")
  }
  comp_ind = 0
  tol = 0.01 #tolerance of deviation that is still considered constant
  for (grp in grps){
    grp_ind <- match(grp, grps)
    grp_ind2 <- match(grp, othergrps)
    if (dind2 ==0){ #aka, there is no value to divide by 
      maingrpdat <- na.omit(dataset[dataset[, groupind]==grp,dind1])
    } else{
      maingrpdat <- na.omit(dataset[dataset[, groupind]==grp,dind1]/dataset[dataset[, groupind]==grp,dind2])
    }
    othergrps <- othergrps[-grp_ind2]
    
    if (abs(max(maingrpdat) - min(maingrpdat)) < tol){
      next
    }
    
    for (ogrp in othergrps){
      comp_ind = comp_ind + 1
      if (dind2 ==0){
        ogrpdat <- na.omit(dataset[dataset[, groupind]==ogrp,dind1])
      } else {
        ogrpdat <- na.omit(dataset[dataset[, groupind]==ogrp,dind1]/dataset[dataset[, groupind]==ogrp,dind2])
      }
      if (abs(max(ogrpdat) - min(ogrpdat)) < tol){
        comparisons[comp_ind, 1] = paste(grp, "-", ogrp)
        comparisons[comp_ind, 2] = NA
        comparisons[comp_ind, 3] = NA
      } else{
        if (ttype == "Welch"){
          w <- t.test(maingrpdat, ogrpdat)
          comparisons[comp_ind, 4] = w$conf.int[1]
          comparisons[comp_ind, 5] = w$conf.int[2]
          
        } else if (ttype == "wmw"){
          w <- wilcox.test(maingrpdat, ogrpdat)
        }
        
        comparisons[comp_ind, 1] = paste(grp, "-", ogrp)
        comparisons[comp_ind, 2] = w$statistic
        comparisons[comp_ind, 3] = w$p.value
      }

    } 
  }
  comparisons = as.data.frame(comparisons)
  comparisons[,2:5] <- sapply(comparisons[,2:3],as.numeric)
  
  #If you want to get the confidence interval: http://www.stat.umn.edu/geyer/old03/5102/notes/rank.pdf
  
  return(comparisons)
}


#########################################################################
#######       For making the sig outputs                          #######   
#######                                                           #######
#########################################################################




SigDecomp <- function(comparisons, LabelList, ResStorage, splitter, labels = "inmat"){
  
  if (is.null(rownames(comparisons))){
    totlen <- 1
    if (is.na(comparisons[1])){
      skip <- TRUE
    } else {
      skip <- FALSE
    }
  } else {
    totlen <- length(comparisons[,1])
    if (totlen == 0){
      skip <- TRUE
    } else {
      skip <- FALSE
    }
  
  }
  
  for (ro in 1:totlen){ ### MAKE A FUNCTION FOR THIS
  
    if (skip){
      break
    }
    
    if (labels == "inmat"){
      rolab = comparisons[ro,1]
      
    } else if (labels == "row") {
      if (totlen > 1){
        rolab = rownames(comparisons)[ro]
      } else {
        rolab = comparisons[[5]]
      }
    }
    splitsrc <- unlist(strsplit(rolab, splitter))
    src1 <- match(splitsrc[1], LabelList)
    src2 <- match(splitsrc[2], LabelList)
    
    if (totlen > 1){
      if (comparisons[ro, 2] > 0){
        ResStorage[src1, src2] = ResStorage[src1, src2] + 1
        ResStorage[src2, src1] = ResStorage[src2, src1] - 1
      } else if (comparisons[ro, 2] < 0){
        ResStorage[src1, src2] = ResStorage[src1, src2] - 1
        ResStorage[src2, src1] = ResStorage[src2, src1] + 1
      }
    } else {
      if (comparisons[2] > 0){
        ResStorage[src1, src2] = ResStorage[src1, src2] + 1
        ResStorage[src2, src1] = ResStorage[src2, src1] - 1
      } else if (comparisons[2] < 0){
        ResStorage[src1, src2] = ResStorage[src1, src2] - 1
        ResStorage[src2, src1] = ResStorage[src2, src1] + 1
      }
    }
    
  }
  return(ResStorage)
}

