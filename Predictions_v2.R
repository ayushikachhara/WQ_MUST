
PredictConcentrations = function(catch.area, LU.breakdown, SC.opts, SWT.opts, SWT.fracs){
  
  # catch.area = catchment area (m2)
  # LU.breakdown = proportion of catchment area in each landuse type
  # SC.opts = source control options selected for each landuse type
  # SWT.opts = stormwater treatment option selected for each landuse type
  # SWT.fracs = proportion of landuse type area to which the selected stormwater treatment option applies
  
  # ----------------------------------------------------------------------------------------------------
  # FUNCTION DEFINITIONS ###############################################################################
  #-----------------------------------------------------------------------------------------------------
  
  ReadLanduseYields = function(landuse){
    
    # Read landuse yields from file for a single landuse type
    
    fname = landuse.lookup$Yield.filename[which(landuse.lookup$Key==landuse)]
    landuse.yields = read.table(file=paste(getwd(),"/Input data/",metal,"/",fname,".csv",sep=""), header=TRUE, sep=",", skip=1, stringsAsFactors=FALSE)
    names(landuse.yields) = c("BAU","MSC","LI","MSC_LI")
    
    return(landuse.yields)
  }
  
  #-----------------------------------------------------------------------------------------------------
  
  ReadLRFs = function(SWTopt){
    
    # Read load reduction factors from file for a single stormwater treatment option
    
    fname = SWT.lookup$LRF.filename[which(SWT.lookup$Key==SWTopt)]
    LRFs = read.table(file=paste(getwd(),"/Input data/",metal,"/",fname,".csv",sep=""), header=TRUE, sep=",", skip=1, stringsAsFactors=FALSE)
    names(LRFs) = "LRF"
    
    return(LRFs)
  }
  
  #-----------------------------------------------------------------------------------------------------
  
  BootstrapReplicate = function(bs.lm, bs.s, catch.lnYield){
    
    # Function description...
    
    bs.s.sample = sample(bs.s, size=length(bs.lm$residuals), replace=TRUE)          # sample adjusted residuals
    bs.lnConc.sample = fitted(bs.lm) + bs.s.sample                                  # predicted log-transformed catchment concentrations with sampled residuals
    lnYield = model.frame(bs.lm)$lnYield                                            # log-transformed catchment yields
    
    bs.lm.sample = lm(bs.lnConc.sample~lnYield)                                     # fit linear regression model
    
    bs.leverage = influence(bs.lm.sample)$hat                                       # create adjusted residuals
    bs.res.adj = residuals(bs.lm.sample)/sqrt(1-bs.leverage)
    bs.res.adj = bs.res.adj - mean(bs.res.adj)
    
    b0 = coef(bs.lm)["(Intercept)"] - coef(bs.lm.sample)["(Intercept)"]
    b1 = coef(bs.lm)["lnYield"] - coef(bs.lm.sample)["lnYield"]
    
    bs.e.sample = b0 + b1*catch.lnYield + sample(bs.res.adj, size=1)                # calculate prediction error
    return(unname(bs.e.sample))
  }
  
  #-----------------------------------------------------------------------------------------------------
  
  PredictionReplicate = function(N.bsIterations){
    
    # Function to predict a single value for a given concentration statistic (median, 95th percentile) and
    # use bootstrapping to return a set of prediction errors. Returns the set of concentrations given by
    # the predicted value + prediction errors.
    
    # N.bsIterations = number of bootstrap iterations to assess the prediction error
    
    # Sample from regression models
    
    lm.sample = sample(lm.all, 1, replace=TRUE)[[1]]
    
    # Sample from yields and LRFs for each landuse type
    
    LU.data.sample = LU.data
    LU.data.sample$yield = 0
    LU.data.sample$LRF = 0
    
    for (landuse in landuses){
      
      SCopt = LU.data[landuse,"SC.opt"]
      LU.data.sample[landuse,"yield"] = sample(landuse.yields.all[[landuse]][,SCopt], size=1, replace=TRUE)
      
      SWTopt = LU.data[landuse,"SWT.opt"]
      if (SWTopt == "None"){
        LU.data.sample[landuse,"LRF"] = 0
      } else{
        LU.data.sample[landuse,"LRF"] = sample(LRFs.all[[SWTopt]][,"LRF"], size=1, replace=TRUE)
      }
    }
    
    
    # Calculate catchment yield and predict concentration
    
    LU.load.treated = catch.area * with(LU.data.sample, Area.frac*SWT.frac*yield*(1-LRF))                                 # untreated load from each landuse type (g/yr)
    LU.load.untreated = catch.area * with(LU.data.sample, Area.frac*(1-SWT.frac)*yield)                                   # treated load from each landuse type (g/yr)
    
    catch.yield.sample = sum(LU.load.treated + LU.load.untreated, na.rm=FALSE)/catch.area                                 # total catchment yield (g/m2/yr)
    catch.lnYield.sample = log(catch.yield.sample)                                                                        # log-transformed catchment yield
    
    catch.lnConc.sample = predict(lm.sample, newdata=data.frame(lnYield=catch.lnYield.sample))                            # predicted log-transformed concentration
    
    # Create adjusted residuals for bootstrapping
    
    leverage = influence(lm.sample)$hat
    res.adj = residuals(lm.sample)/sqrt(1-leverage)
    s.sample = res.adj - mean(res.adj)
    
    # Do bootstrapping
    
    lnErr.all.sample = replicate(n=N.bsIterations, BootstrapReplicate(lm.sample, s.sample, log(catch.yield.sample)))      # prediction error for each iteration
    
    catch.lnConc.all.sample = catch.lnConc.sample + lnErr.all.sample                                                      # predicted log-transformed concentration for each iteration
    return(catch.lnConc.all.sample)
  }
  
  # ----------------------------------------------------------------------------------------------------
  # MAIN CALCULATION ###################################################################################
  #-----------------------------------------------------------------------------------------------------
  
  set.seed(12344321)
  
  metals = c("Cu","Zn")                                                                                                                         # metals to run calculations for
  stats = c("Median","Pctile95")                                                                                                                # concentration statistics to run calculations for
  
  landuse.lookup = read.table(file=paste(getwd(),"/Input data/LanduseLookup.csv",sep=""), header=TRUE, sep=",", stringsAsFactors=FALSE)         # read in landuse type information
  landuses = landuse.lookup$Key                                                                                                                 # landuse types
  N.landuses = length(landuses)                                                                                                                 # number of landuse types
  
  SC.lookup = read.table(file=paste(getwd(),"/Input data/SourceControlLookup.csv",sep=""), header=TRUE, sep=",", stringsAsFactors=FALSE)        # read in metal source control option information
  SCopts = SC.lookup$Key                                                                                                                        # metal source control options
  N.SCopts = length(SCopts)                                                                                                                     # number of metal source control options
  
  SWT.lookup = read.table(file=paste(getwd(),"/Input data/TreatmentLookup.csv",sep=""), header=TRUE, sep=",", stringsAsFactors=FALSE)           # read in stormwater treatment option information
  SWTopts = SWT.lookup$Key                                                                                                                      # stormwater treatment options
  N.SWTopts = length(SWTopts)                                                                                                                   # number of stormwater treatment options
  
  # Generate landuse summary dataframe
  
  LU.data = merge(LU.breakdown, SC.opts, by="Key")                                                                        # determine source control option for each landuse type
  LU.data$SC.opt = "BAU"
  LU.data$SC.opt[LU.data$MSC] = "MSC"
  LU.data$SC.opt[LU.data$LI] = "LI"
  LU.data$SC.opt[LU.data$MSC & LU.data$LI] = "MSC_LI"
  LU.data = LU.data[,c("Key","Area.frac","SC.opt")]
  
  LU.data = merge(LU.data, SWT.opts, by="Key")                                                                            # determine stormwater treatment option for each landuse type
  LU.data$SWT.opt = "None"
  LU.data$SWT.opt[LU.data$Conv] = "Conv"
  LU.data$SWT.opt[LU.data$GI] = "GI"
  LU.data = LU.data[,c("Key","Area.frac","SC.opt","SWT.opt")]
  
  LU.data = merge(LU.data, SWT.fracs, by="Key")                                                                           # fraction of landuse area treated for each landuse type
  names(LU.data) = c("Key","Area.frac","SC.opt","SWT.opt","SWT.frac")
  
  row.names(LU.data) = LU.data$Key
  LU.data = LU.data[,!(names(LU.data) %in% c("Key"))]
  
  # Run calculations
  
  N.outer = 100                                                                                                # number of bootstrap iterations for the predicted concentration (outer bootstrap)
  N.inner = 10                                                                                              # number of bootstrap iterations for the prediction error (inner bootstrap)
  
  results = as.data.frame(matrix(NA,nrow=N.outer*N.inner,ncol=0))
  
  for (metal in metals){
    
    landuse.yields.all = lapply(landuses, function(x) ReadLanduseYields(x))                                    # read in yields for each landuse type and source control option
    names(landuse.yields.all) = landuses
    
    LRFs.all = lapply(SWTopts, function(x) ReadLRFs(x))                                                        # read in load reduction factors for each stormwater treatment option
    names(LRFs.all) = SWTopts
    
    for (stat in stats){
      
      lm.all = readRDS(paste(getwd(),"/Input data/",metal,"/RegressionModels",stat,".RData",sep=""))           # read in regression models
      
      catch.lnConc.all = replicate(n=N.outer, PredictionReplicate(N.inner))
      catch.conc.all = exp(catch.lnConc.all)
      
      metal.stat = paste(metal,stat,sep="_")
      results[,metal.stat] = as.vector(catch.conc.all)
    }
  }
  
  return(results)
}


