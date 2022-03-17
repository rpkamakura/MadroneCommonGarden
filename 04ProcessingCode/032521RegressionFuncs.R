##Regression function
library(nlme)
library(lme4)
library(GLMMadaptive)

##would be great if you can figure out the random and weighted part so you could use one function

MultReg1 <- function(df, ParamData_ind, DepVar_ind, types, rand1, family = "linear"){
  
  ##Add the binomial part to this so you can deal with GR vs mort
  
  ctrl <- lmeControl(opt='optim')
  
  dep_dat <- df[, DepVar_ind]
  rand1_dat <- df[, rand1]
  
  l_pres <- FALSE
  e_pres <- FALSE
  
  
  if ("L" %in% types){
    l_ind <- match("Dist", colnames(df)) #which one is the linear one
    l_listInd <- match(l_ind, ParamData_ind)
    l_dat <- df[,l_ind] #get the dist data
    l_pres <- TRUE
  }
  
  if ("E" %in% types){
    e_ind <- match("Ecoregion", colnames(df)) #which one it is in the parameter list
    e_listInd <- match(e_ind, ParamData_ind)
    e_dat <- df[,e_ind] #get the ecoregion data 
    e_pres<- TRUE
  }
  
  if (length(ParamData_ind) == 1){###########################################################
    param_dat <- df[,ParamData_ind]
    
    
    if (l_pres){ #linear
      
      if (family == "linear"){ #see if there is a second random variable
        reg <- lme(dep_dat ~ l_dat, random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
      
      } else if (family == "binomial"){ #if you do have a second random variable 
          
        reg <- glmer(dep_dat ~ l_dat + (1|rand1_dat), family="binomial")
      } 
      
    } else if (e_pres) {
      if (family == "linear") { #see if there is a second random variable
        reg <- lme(dep_dat ~ e_dat, random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") { #if you do have a second random variable 
        
        reg <- glmer(dep_dat ~ e_dat + (1|rand1_dat), family="binomial")
      } 
    
    } else {
      #Make quadratic
      param_dat <- df[, ParamData_ind]
      param2_dat <- param_dat^2
      
      #Regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ param_dat + param2_dat, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ param_dat + (1|rand1_dat), family="binomial")
      }
    } #type if statement
    
  } else if (length(ParamData_ind) == 2){ ###########################################################
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg <- lme(dep_dat ~ e_dat + l_dat, random = ~1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg <- glmer(dep_dat ~ e_dat + l_dat + (1|rand1_dat), family="binomial")
        }
      
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        param_ind = ParamData_ind[-c(l_listInd)]
        param_dat <- df[,param_ind] #get the other parameter data
        param2_dat <- param_dat^2
        
        if (family == "linear") {
          reg <- lme(dep_dat ~ l_dat + param_dat + param2_dat, random = ~1|rand1_dat,  
                      na.action = na.omit, control = ctrl, method="ML")
        
        } else if (family == "binomial") {

          reg <- glmer(dep_dat ~ l_dat + param_dat + param2_dat + (1|rand1_dat),family="binomial")
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
      #Have ecoregion but no linear variables
    } else if (e_pres){
      
      #Non-ecoregion data
      param_ind <- ParamData_ind[-c(e_listInd)]
      param_dat <- df[,param_ind] #get the other parameter data
      param2_dat <- param_dat^2
      
      if (family == "linear") {
        reg <- lme(dep_dat ~ e_dat + param_dat + param2_dat, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
    
        reg <- glmer(dep_dat ~ e_dat + param_dat + param2_dat + (1|rand1_dat), family="binomial")
      
        } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + (1|rand1_dat),  
                   family="binomial")
        
      } #end random variable loop
      
      
    } #end quadratic loop
  } else if (length(ParamData_ind) == 3){ ###########################################################
    
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_ind <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[, q_ind]
        paramA2_dat <- paramA_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat, random = ~1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + (1|rand1_dat),  
                     family="binomial")
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        
        if (family == "linear") {
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat + (1|rand1_dat),  
                     family="binomial")
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]

      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      
      if (family == "linear") {
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + (1|rand1_dat),  
                   family="binomial")
        
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat, random = ~1|rand1_dat,  na.action = na.omit, 
                   control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + paramC2_dat +
                       (1|rand1_dat), family="binomial")
        
      } #end random variable loop
      
      
    } #end quadratic loop
    
  } else if (length(ParamData_ind) == 4){ ###########################################################
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]

        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + (1|rand1_dat),  
                     family="binomial")
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]

        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        
        if (family == "linear") {
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + paramC2_dat, random = ~1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + paramC2_dat + (1|rand1_dat),  
                     family="binomial")
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]

      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      
      if (family == "linear") {
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat, random = ~ 1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + (1|rand1_dat),  
                   family="binomial")
        
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat, random = ~ 1|rand1_dat,  na.action = na.omit, 
                   control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat + (1|rand1_dat), family="binomial")
        
      } #end random variable loop
      
      
    } #end quadratic loop
    
  } else if(length(ParamData_ind) == 5){###########################################################
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]

        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat, random = ~ 1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + (1|rand1_dat), family="binomial")
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]

        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        
        if (family == "linear") {
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat, random = ~ 1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + (1|rand1_dat), family="binomial")
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]

      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      
      if (family == "linear") {
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat, random = ~ 1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + (1|rand1_dat), family="binomial")
        
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, 
                   random = ~ 1|rand1_dat,  na.action = na.omit,control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + (1|rand1_dat), family="binomial")
        
      } #end random variable loop
      
      
    } #end quadratic loop
  } else if(length(ParamData_ind) == 6){###########################################################
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        paramD_dat <- df[,q_inds[4]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat, random = ~ 1|rand1_dat,
                     na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + (1|rand1_dat),  
                     family="binomial")
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        
        if (family == "linear") {
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, random = ~ 1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + (1|rand1_dat),  
                     family="binomial")
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      paramE_dat <- df[,q_inds[5]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      
      if (family == "linear") {
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, random = ~ 1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + (1|rand1_dat),  
                   family="binomial")
        
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      paramF_dat <- df[,ParamData_ind[6]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat +
                     paramF_dat + paramF2_dat, random = ~ 1|rand1_dat,  na.action = na.omit,control = ctrl, 
                   method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + (1|rand1_dat), 
                   family="binomial")
        
      } #end random variable loop
      
      
    } #end quadratic loop
  } else if(length(ParamData_ind) == 7){ ###########################################################
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]

        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, 
                     random = ~ 1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + (1|rand1_dat), 
                      family="binomial")
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        paramF_dat <- df[,q_inds[6]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        paramF2_dat <- paramF_dat^2
        
        if (family == "linear") {
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + 
                       paramF2_dat, random = ~ 1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat+ 
                         (1|rand1_dat), family="binomial")
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      paramE_dat <- df[,q_inds[5]]
      paramF_dat <- df[,q_inds[6]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      
      if (family == "linear") {
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat 
                   + paramF2_dat, random = ~ 1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + (1|rand1_dat),  
                   family="binomial")
        
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      paramF_dat <- df[,ParamData_ind[6]]
      paramG_dat <- df[,ParamData_ind[7]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      paramG2_dat <- paramG_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat +
                     paramF_dat + paramF2_dat + paramG_dat + paramG2_dat, random = ~ 1|rand1_dat,  
                   na.action = na.omit,control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + paramG_dat + 
                     paramG2_dat + (1|rand1_dat), family="binomial")
        
      } #end random variable loop
      
      
    } #end quadratic loop
  } else if(length(ParamData_ind) == 8){ ###########################################################
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        paramF_dat <- df[,q_inds[6]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        paramF2_dat <- paramF_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat, 
                     random = ~ 1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + 
                         (1|rand1_dat), family="binomial")
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        paramF_dat <- df[,q_inds[6]]
        paramG_dat <- df[,q_inds[7]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        paramF2_dat <- paramF_dat^2
        paramG2_dat <- paramG_dat^2
        
        if (family == "linear") {
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + 
                       paramF2_dat + paramG_dat + paramG2_dat, random = ~ 1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + 
                         paramG_dat + paramG2_dat + (1|rand1_dat), family="binomial")
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      paramE_dat <- df[,q_inds[5]]
      paramF_dat <- df[,q_inds[6]]
      paramG_dat <- df[,q_inds[7]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      paramG2_dat <- paramG_dat^2
      
      if (family == "linear") {
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat 
                   + paramF2_dat + paramG_dat + paramG2_dat, random = ~ 1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + 
                       paramG_dat + paramG2_dat + (1|rand1_dat), family="binomial")
        
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      paramF_dat <- df[,ParamData_ind[6]]
      paramG_dat <- df[,ParamData_ind[7]]
      paramH_dat <- df[,ParamData_ind[8]]
      
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      paramG2_dat <- paramG_dat^2
      paramH2_dat <- paramH_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat +
                     paramF_dat + paramF2_dat + paramG_dat + paramG2_dat + paramH_dat + paramH2_dat, random = ~ 1|rand1_dat,  
                   na.action = na.omit,control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + paramG_dat + 
                     paramG2_dat  + paramH_dat + paramH2_dat + (1|rand1_dat), family="binomial")
        
      } #end random variable loop
      
      
    } #end quadratic loop
  }
  
  
  return(reg)

} 

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

## Option 2, which should allow you to get the r2 values the way their code is written
MultReg2 <- function(df, ParamData_ind, DepVar_ind, types, rand1, rand2=NULL, family="linear"){
  
  ctrl <- lmeControl(opt='optim')
  
  dep_dat <- df[, DepVar_ind]
  rand1_dat <- df[, rand1]
  
  if (!is.null(rand2)){
    rand2_dat <- df[, rand2]
  }
  
  
  l_pres <- FALSE
  e_pres <- FALSE

  
  if ("L" %in% types){
    l_ind <- match("Dist", colnames(df)) #which one is the linear one
    l_listInd <- match(l_ind, ParamData_ind)
    l_dat <- df[,l_ind] #get the dist data
    l_pres <- TRUE
  }
  
  if ("E" %in% types){
    e_ind <- match("Ecoregion", colnames(df)) #which one it is in the parameter list
    e_listInd <- match(e_ind, ParamData_ind)
    e_dat <- df[,e_ind] #get the ecoregion data 
    e_pres<- TRUE
  }
  
  if (length(ParamData_ind) == 1){###########################################################
    param_dat <- df[,ParamData_ind]
    
    
    if (l_pres){ #linear
      
      if (family == "linear"){ #see if there is a second random variable
        reg_df <- data.frame(dep_dat, l_dat, rand1_dat)
        reg <- lme(dep_dat ~ l_dat, random = ~1|rand1_dat, data = reg_df, na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") { #if you do have a second random variable 
        reg_df <- data.frame(dep_dat, l_dat, rand1_dat)
        
        reg <- glmer(dep_dat ~ l_dat + (1|rand1_dat), data = reg_df, 
                   family="binomial")
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, l_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ l_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      }
      
    } else if (e_pres) {
      if (family == "linear") { #see if there is a second random variable
        reg_df <- data.frame(dep_dat, e_dat, rand1_dat)
        reg <- lme(dep_dat ~ e_dat, data = reg_df,random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") { #if you do have a second random variable 
        
        reg_df <- data.frame(dep_dat, e_dat, rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + (1|rand1_dat), data = reg_df, family="binomial"(link = "log"))
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      }
      
    } else {
      #Make quadratic
      param_dat <- df[, ParamData_ind]
      param2_dat <- param_dat^2
      
      #Regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, param_dat, param2_dat, rand1_dat)
        reg <- lme(dep_dat ~ param_dat + param2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, param_dat, param2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ param_dat + param2_dat + (1|rand1_dat), data = reg_df, 
                   family="binomial")
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, param_dat, param2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~  param_dat + param2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      }
    } #type if statement
    
  } else if (length(ParamData_ind) == 2){ ###########################################################
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg_df <- data.frame(dep_dat, e_dat, l_dat, rand1_dat)
          reg <- lme(dep_dat ~ e_dat + l_dat, data = reg_df, random = ~1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          
          reg_df <- data.frame(dep_dat, e_dat, l_dat, rand1_dat)
          reg <- glmer(dep_dat ~ e_dat + l_dat+ (1|rand1_dat), data = reg_df ,  
                     family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, e_dat, l_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~  e_dat + l_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        param_ind = ParamData_ind[-c(l_listInd)]
        param_dat <- df[,param_ind] #get the other parameter data
        param2_dat <- param_dat^2
        
        ##if you decide to rescale
        #p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, paramF_dat, paramG_dat)
        #numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
        #p_list[,numcols] <- scale(p_list[,numcols])
        
        if (family == "linear") {
          
          reg_df <- data.frame(dep_dat, l_dat, param_dat, param2_dat, rand1_dat)
          reg <- lme(dep_dat ~ l_dat + param_dat + param2_dat, data = reg_df, random = ~1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          
          reg_df <- data.frame(dep_dat, l_dat, param_dat, param2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ l_dat + param_dat + param2_dat+ (1|rand1_dat), data = reg_df ,  
                     family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, l_dat, param_dat, param2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ l_dat + param_dat + param2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
      #Have ecoregion but no linear variables
    } else if (e_pres){
      
      #Non-ecoregion data
      param_ind <- ParamData_ind[-c(e_listInd)]
      param_dat <- df[,param_ind] #get the other parameter data
      param2_dat <- param_dat^2
      
      if (family == "linear"){
        reg_df <- data.frame(dep_dat, e_dat, param_dat, param2_dat, rand1_dat)
        reg <- lme(dep_dat ~ e_dat + param_dat + param2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, e_dat, param_dat, param2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + param_dat + param2_dat+ (1|rand1_dat), data = reg_df ,  
                   family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, param_dat, param2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat + param_dat + param2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat+ (1|rand1_dat), data = reg_df ,  
                   family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
    } #end quadratic loop
  } else if (length(ParamData_ind) == 3){ ###########################################################
    
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_ind <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[, q_ind]
        paramA2_dat <- paramA_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, rand1_dat)
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat, data = reg_df, random = ~1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat+ (1|rand1_dat), data = reg_df ,  
                     family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        
        if (family == "linear") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, data = reg_df, random = ~1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat+ (1|rand1_dat), data = reg_df ,  
                     family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + (1|rand1_dat), data = reg_df ,  
                   family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat, data = reg_df, random = ~1|rand1_dat,  na.action = na.omit, 
                   control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + paramC2_dat + (1|rand1_dat),
                   data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                             paramC_dat + paramC2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
    } #end quadratic loop
    
  } else if (length(ParamData_ind) == 4){ ###########################################################
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, data = reg_df, random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + (1|rand1_dat), data = reg_df,  
                     family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        
        if (family == "linear") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + paramC2_dat, data = reg_df, random = ~1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + paramC2_dat  + (1|rand1_dat), data = reg_df,  
                     family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + paramC2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + (1|rand1_dat), 
                     data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, paramD_dat, paramD2_dat, rand1_dat)
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat, data = reg_df, random = ~1|rand1_dat,  na.action = na.omit, 
                   control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, paramD_dat, paramD2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                             paramC_dat + paramC2_dat + paramD_dat + paramD2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      }#end random variable loop
      
      
    } #end quadratic loop
    
  } else if(length(ParamData_ind) == 5){###########################################################
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               rand1_dat)
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat, data = reg_df, random = ~1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               rand1_dat)
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                                rand1_dat)
          reg <- mixed_model(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                               paramC_dat + paramC2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        
        if (family == "linear") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, rand1_dat)
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat, data = reg_df, random = ~1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                               paramC2_dat + paramD_dat + paramD2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, rand1_dat)
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                           + paramC2_dat + paramD_dat + paramD2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, data = reg_df, 
                   random = ~1|rand1_dat,  na.action = na.omit,control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + (1|rand1_dat), data = reg_df, 
                   family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat,rand1_dat)
        reg <- mixed_model(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                             paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, 
                           random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
    } #end quadratic loop
  } else if(length(ParamData_ind) == 6){###########################################################
    
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        paramD_dat <- df[,q_inds[4]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, rand1_dat)
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat, data = reg_df, random = ~1|rand1_dat,
                     na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                               paramC_dat + paramC2_dat + paramD_dat + paramD2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        }
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        
        if (family == "linear") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, data = reg_df, random = ~1|rand1_dat,  
                     na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + (1|rand1_dat), data = reg_df,  
                     family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                               paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      paramE_dat <- df[,q_inds[5]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + (1|rand1_dat), data = reg_df,  
                   family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                           + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      paramF_dat <- df[,ParamData_ind[6]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             rand1_dat)
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat +
                     paramF_dat + paramF2_dat, data = reg_df, random = ~1|rand1_dat,  na.action = na.omit,control = ctrl, 
                   method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             rand1_dat)
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + (1|rand1_dat), data = reg_df, 
                   family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                             paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat+
                             paramF_dat + paramF2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      }#end random variable loop
      
      
    } #end quadratic loop
  } else if(length(ParamData_ind) == 7){ ###########################################################
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, data = reg_df, 
                     random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        } else if (family == "binomial") { #with the second random variable
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + (1|rand1_dat), 
                       data = reg_df, family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, rand1_dat)
          reg <- mixed_model(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                               paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        }  
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        paramF_dat <- df[,q_inds[6]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        paramF2_dat <- paramF_dat^2
        
        if (family == "linear") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                               rand1_dat)
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + 
                       paramF2_dat, data = reg_df, random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                               rand1_dat)
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat +
                        (1|rand1_dat), data = reg_df, family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                               rand1_dat)
          reg <- mixed_model(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                               paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + 
                               paramF2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        }  #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      paramE_dat <- df[,q_inds[5]]
      paramF_dat <- df[,q_inds[6]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             rand1_dat)
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat 
                   + paramF2_dat, data = reg_df, random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + (1|rand1_dat), 
                     data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                           + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat 
                           + paramF2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      paramF_dat <- df[,ParamData_ind[6]]
      paramG_dat <- df[,ParamData_ind[7]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      paramG2_dat <- paramG_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             paramG_dat, paramG2_dat, rand1_dat)
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat +
                     paramF_dat + paramF2_dat + paramG_dat + paramG2_dat + (1|rand1_dat), data = reg_df,   
                     family="binomial")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             paramG_dat, paramG2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + paramG_dat + 
                     paramG2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             paramG_dat, paramG2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                             paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat+
                             paramF_dat + paramF2_dat+ paramG_dat + paramG2_dat, random = ~1|rand1_dat, 
                           data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
    } #end quadratic loop
    
  } else if(length(ParamData_ind) == 8){ ###########################################################
    if (l_pres){ #if there is a linear variable in there
      
      if (e_pres){ #if you have a linear var and a categorical var basically 
        q_inds <- ParamData_ind[-c(l_listInd, e_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]]
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        paramF_dat <- df[,q_inds[6]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        paramF2_dat <- paramF_dat^2
        
        if (family == "linear") { #with one random variable
          #Do the regression
          reg_df <- data.frame(dep_dat, e_dat, l_dat,paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, rand1_dat)
          reg <- lme(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat, data = reg_df, 
                     random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        
          } else if (family == "binomial") { #with the second random variable
          reg_df <- data.frame(dep_dat, e_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat,rand1_dat)
          reg <- glmer(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat +
                       paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + 
                         paramF2_dat + (1|rand1_dat), data = reg_df, family="binomial")
          } else if (family == "zeroinfl"){
            
            reg_df <- data.frame(dep_dat, e_dat, l_dat,paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                                 paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, rand1_dat)
            reg <- mixed_model(dep_dat ~ e_dat + l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                                 paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat, 
                               random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
          } 
        
        
      } else { #if you have the linear variable but not the categorical one
        
        #Non-linear data
        q_inds <- ParamData_ind[-c(l_listInd)]
        
        paramA_dat <- df[,q_inds[1]] #get the other parameter data
        paramB_dat <- df[,q_inds[2]] 
        paramC_dat <- df[,q_inds[3]] 
        paramD_dat <- df[,q_inds[4]]
        paramE_dat <- df[,q_inds[5]]
        paramF_dat <- df[,q_inds[6]]
        paramG_dat <- df[,q_inds[7]]
        
        paramA2_dat <- paramA_dat^2
        paramB2_dat <- paramB_dat^2
        paramC2_dat <- paramC_dat^2
        paramD2_dat <- paramD_dat^2
        paramE2_dat <- paramE_dat^2
        paramF2_dat <- paramF_dat^2
        paramG2_dat <- paramG_dat^2
        
        if (family == "linear") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, paramG_dat, paramG2_dat, 
                               rand1_dat)
          reg <- lme(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + 
                       paramF2_dat + paramG_dat + paramG2_dat, data = reg_df, random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
          
        } else if (family == "binomial") {
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, paramG_dat, paramG2_dat,
                               rand1_dat)
          reg <- glmer(dep_dat ~ l_dat + paramA_dat + paramA2_dat  + paramB_dat + paramB2_dat  + paramC_dat + 
                       paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + 
                         paramG_dat + paramG2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        } else if (family == "zeroinfl"){
          
          reg_df <- data.frame(dep_dat, l_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                               paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, paramG_dat, paramG2_dat,
                               rand1_dat)
          reg <- mixed_model(dep_dat ~ l_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat  + paramC_dat + 
                               paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + 
                               paramF2_dat + paramG_dat + paramG2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
        } #end random variable loop
        
        
      } #end loop that includes a linear variable
      
    } else if (e_pres){ #Have ecoregion but no linear variables
      
      #Non-linear data
      q_inds <- ParamData_ind[-c(e_listInd)]
      
      paramA_dat <- df[,q_inds[1]] #get the other parameter data
      paramB_dat <- df[,q_inds[2]] 
      paramC_dat <- df[,q_inds[3]] 
      paramD_dat <- df[,q_inds[4]]
      paramE_dat <- df[,q_inds[5]]
      paramF_dat <- df[,q_inds[6]]
      paramG_dat <- df[,q_inds[7]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      paramG2_dat <- paramG_dat^2
      
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, paramG_dat, paramG2_dat,
                             rand1_dat)
        reg <- lme(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                   + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat 
                   + paramF2_dat + paramG_dat + paramG2_dat, data = reg_df, random = ~1|rand1_dat, na.action = na.omit, control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, paramG_dat, paramG2_dat,
                             rand1_dat)
        reg <- glmer(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + 
                     paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + 
                       paramG_dat + paramG2_dat + (1|rand1_dat), data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, e_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, paramG_dat, paramG2_dat,
                             rand1_dat)
        reg <- mixed_model(dep_dat ~ e_dat + paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat 
                           + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat + paramF_dat 
                           + paramF2_dat + paramG_dat + paramG2_dat, random = ~1|rand1_dat, data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
      
    } else { #all quadratic
      
      paramA_dat <- df[,ParamData_ind[1]]
      paramB_dat <- df[,ParamData_ind[2]]
      paramC_dat <- df[,ParamData_ind[3]]
      paramD_dat <- df[,ParamData_ind[4]]
      paramE_dat <- df[,ParamData_ind[5]]
      paramF_dat <- df[,ParamData_ind[6]]
      paramG_dat <- df[,ParamData_ind[7]]
      paramH_dat <- df[,ParamData_ind[8]]
      
      paramA2_dat <- paramA_dat^2
      paramB2_dat <- paramB_dat^2
      paramC2_dat <- paramC_dat^2
      paramD2_dat <- paramD_dat^2
      paramE2_dat <- paramE_dat^2
      paramF2_dat <- paramF_dat^2
      paramG2_dat <- paramG_dat^2
      paramH2_dat <- paramH_dat^2
      
      #Do the regression
      if (family == "linear") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             paramG_dat, paramG2_dat, paramH_dat, paramH2_dat, rand1_dat)
        reg <- lme(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                     paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat +
                     paramF_dat + paramF2_dat + paramG_dat + paramG2_dat + paramH_dat + paramH2_dat, data = reg_df, random = ~1|rand1_dat,  
                   na.action = na.omit,control = ctrl, method="ML")
        
      } else if (family == "binomial") {
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             paramG_dat, paramG2_dat, paramH_dat, paramH2_dat, rand1_dat)
        reg <- glmer(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + paramC_dat + paramC2_dat + 
                     paramD_dat + paramD2_dat  + paramE_dat + paramE2_dat + paramF_dat + paramF2_dat + paramG_dat + 
                     paramG2_dat + paramH_dat + paramH2_dat  + (1|rand1_dat), data = reg_df, family="binomial")
        
      } else if (family == "zeroinfl"){
        
        reg_df <- data.frame(dep_dat, paramA_dat, paramA2_dat, paramB_dat, paramB2_dat, paramC_dat, paramC2_dat, 
                             paramD_dat, paramD2_dat, paramE_dat, paramE2_dat, paramF_dat, paramF2_dat, 
                             paramG_dat, paramG2_dat, paramH_dat, paramH2_dat, rand1_dat)
        reg <- mixed_model(dep_dat ~ paramA_dat + paramA2_dat + paramB_dat + paramB2_dat + 
                             paramC_dat + paramC2_dat + paramD_dat + paramD2_dat + paramE_dat + paramE2_dat+
                             paramF_dat + paramF2_dat+ paramG_dat + paramG2_dat+ paramH_dat + paramH2_dat, random = ~1|rand1_dat, 
                           data = reg_df, family = hurdle.lognormal(), zi_fixed = ~1, iter_EM = 0)
      } #end random variable loop
      
      
    } #end quadratic loop
  }
  
  return(list(reg_df, reg))
  
} 
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

##All Linear
MultReg3 <- function(df, ParamData_ind, DepVar_ind, rand1, family = "linear", priorw = NA){
  
  ctrl <- lmeControl(opt='optim')
  
  dep_dat <- df[, DepVar_ind]
  rand1_dat <- as.factor(df[, rand1])
  
  if (!is.na(priorw)){
    ntrials <- df[, priorw]
  }
  
  
  if (length(ParamData_ind) == 1){###########################################################
    param_dat <- df[,ParamData_ind]
    
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, param_dat, rand1_dat)
      reg <- lme(dep_dat ~ param_dat, random = ~1|rand1_dat, data = reg_df, na.action = na.omit, control = ctrl, method="ML")
    } else if (family == "binomial") {
      reg_df <- data.frame(dep_dat, param_dat, rand1_dat, ntrials)
      
      reg <- glmer(dep_dat ~ param_dat + (1|rand1_dat), data = reg_df, family="binomial", 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)
    } 
    
    
    
  } else if (length(ParamData_ind) == 2){ ###########################################################
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    
    
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, control = ctrl, method="ML")
    
      } else if (family == "binomial") {
      
      #for rescaling
      p_list <- data.frame(paramA_dat, paramB_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)
      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + (1|rand1_dat), data = reg_df, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)
    }
  } else if (length(ParamData_ind) == 3){ ###########################################################
    
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    paramC_dat <- df[,ParamData_ind[3]]
    
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, paramC_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat + paramC_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, 
                 control = ctrl, method="ML")
    } else{
      
      #rescaling
      p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)
      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + (1|rand1_dat), data = reg_df, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials) 
    }
    
    
  
    
  } else if (length(ParamData_ind) == 4){ ###########################################################
    
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    paramC_dat <- df[,ParamData_ind[3]]
    paramD_dat <- df[,ParamData_ind[4]]
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, paramC_dat, paramD_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, 
                 control = ctrl, method="ML")
    } else{
      
      #rescaling
      p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat, paramD_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)
      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + (1|rand1_dat), data = reg_df, family="binomial",
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)
    }
    
    
        
    
  } else if(length(ParamData_ind) == 5){###########################################################
    
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    paramC_dat <- df[,ParamData_ind[3]]
    paramD_dat <- df[,ParamData_ind[4]]
    paramE_dat <- df[,ParamData_ind[5]]
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, 
                 control = ctrl, method="ML")
    } else{
      #rescaling
      p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)
      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + (1|rand1_dat), data = reg_df, 
                   family="binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)
    }
    
    
    
  } else if(length(ParamData_ind) == 6){###########################################################
    
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    paramC_dat <- df[,ParamData_ind[3]]
    paramD_dat <- df[,ParamData_ind[4]]
    paramE_dat <- df[,ParamData_ind[5]]
    paramF_dat <- df[,ParamData_ind[6]]
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, paramF_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + paramF_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, 
                 control = ctrl, method="ML")
    } else{
      #rescaling
      p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, paramF_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)
      
      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + paramF_dat + (1|rand1_dat), data = reg_df, 
                   family="binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)
    }
    
    
    
  } else if(length(ParamData_ind) == 7){ ###########################################################
    
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    paramC_dat <- df[,ParamData_ind[3]]
    paramD_dat <- df[,ParamData_ind[4]]
    paramE_dat <- df[,ParamData_ind[5]]
    paramF_dat <- df[,ParamData_ind[6]]
    paramG_dat <- df[,ParamData_ind[7]]
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, 
                           paramF_dat, paramG_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + 
                   paramF_dat + paramG_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, 
                 control = ctrl, method="ML")
      
    } else{
      #rescaling
      p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, paramF_dat, paramG_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)
      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + paramF_dat + paramG_dat + 
                   (1|rand1_dat), data = reg_df,family="binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)
    }
    
    
  } else if (length(ParamData_ind) == 8){
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    paramC_dat <- df[,ParamData_ind[3]]
    paramD_dat <- df[,ParamData_ind[4]]
    paramE_dat <- df[,ParamData_ind[5]]
    paramF_dat <- df[,ParamData_ind[6]]
    paramG_dat <- df[,ParamData_ind[7]]
    paramH_dat <- df[,ParamData_ind[8]]
    
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, 
                           paramF_dat, paramG_dat, paramH_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + 
                   paramF_dat + paramG_dat + paramH_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, 
                 control = ctrl, method="ML")
    } else{
      #rescaling
      p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, paramF_dat,
                           paramG_dat, paramH_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)

      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + paramF_dat + paramG_dat + 
                     paramH_dat + (1|rand1_dat), data = reg_df,family="binomial", 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)  
    }
    
    
  } else if (length(ParamData_ind) == 9){
    paramA_dat <- df[,ParamData_ind[1]]
    paramB_dat <- df[,ParamData_ind[2]]
    paramC_dat <- df[,ParamData_ind[3]]
    paramD_dat <- df[,ParamData_ind[4]]
    paramE_dat <- df[,ParamData_ind[5]]
    paramF_dat <- df[,ParamData_ind[6]]
    paramG_dat <- df[,ParamData_ind[7]]
    paramH_dat <- df[,ParamData_ind[8]]
    paramI_dat <- df[,ParamData_ind[9]]
    
    
    if (family == "linear") {
      reg_df <- data.frame(dep_dat, paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, 
                           paramF_dat, paramG_dat, paramH_dat, paramI_dat, rand1_dat)
      reg <- lme(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + 
                   paramF_dat + paramG_dat + paramH_dat, data = reg_df, random = ~ 1|rand1_dat,  na.action = na.omit, 
                 control = ctrl, method="ML")
    } else{
      #rescaling
      p_list <- data.frame(paramA_dat, paramB_dat, paramC_dat, paramD_dat, paramE_dat, paramF_dat, paramG_dat,
                           paramH_dat, paramI_dat)
      numcols <- c(1:length(p_list[1,]))[sapply(p_list[1,], is.numeric)]
      p_list[,numcols] <- scale(p_list[,numcols])
      
      
      reg_df <- data.frame(dep_dat, p_list, rand1_dat, ntrials)

      reg <- glmer(dep_dat ~ paramA_dat + paramB_dat + paramC_dat + paramD_dat + paramE_dat + paramF_dat + paramG_dat + 
                     paramH_dat + paramI_dat + (1|rand1_dat), data = reg_df,family="binomial", 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), weights = ntrials)
    }
    
    
  }
  
  return(list(reg_df, reg))
  
}
