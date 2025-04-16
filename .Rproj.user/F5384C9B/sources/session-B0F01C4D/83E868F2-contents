#' @useDynLib TwinFish, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL


# Charlotte, last updated April 2025
# University of Western Australia
# Spatial fish population model package

#' Run the RCPP code with specified number of years and simulations
#'
#' @param NSIM Number of simulations you want to run (Integer)
#' @param BurnInPop Population you end up with at the end of your burn in period (3D array, columns are months, rows are cells, layers are ages)
#' @param MaxYear The total number of years you want your model to run i.e. 1950-2000 would be 51 years (Integer)
#' @param MaxAge How old the species lives for (Integer)
#' @param CatchAge Integer age at which fish become legal size for catching, if no MLL set to 1 (Integer)
#' @param MaxCell The total number of cells in the model (Integer)
#' @param NatMort The natural mortality per time step for your species (Integer)
#' @param BHa Beverton-Holt parameter alpha (Double)
#' @param BHb Beverton-Holt paramter beta (Double)
#' @param PF Proportion of females in the population i.e. 0.5 means 50:50 males and females - this is important for calculating spawning biomass (Double)
#' @param AdultMove Probability of a fish moving from one cell to any other cell in the model in one time step (Matrix - rows and columns represent cells so should be same as MaxCell)
#' @param Mature Probability that a fish of a given age (in years) in mature in each month of the year (Matrix - rows are fish age in years, columns months)
#' @param Weight Expected weight of a fish of a given age (in years) in a given month (Matrix - rows are fish age in years, columns are months)
#' @param Settlement Probability recruit will settle in a specific cell in the model (Vector - total number of values should be same as total number of cells)
#' @param Selectivity Probability of a fish of a given age (in years) being caught and retained in a given month in a given year, this needs to have the same number of years as the model even if selectivity stays the same every year (3D array - rows are fish age in years, columns are months, layers are years)
#' @param FishingMort Expected fishing mortality in each cell of the model, in each month, in each year (3D array - rows are cells, columns are months, layers are years of the model)
#' 
#' @return list object with all outputs of model including ages per cell, numbers per cell, catch
#' @export

RunPopModel <- function(NSIM, BurnInPop, MaxYear, CatchAge,
                        MaxAge, MaxCell, NatMort, BHa, BHb, PF, AdultMove, Mature, Weight, Settlement, 
                        Selectivity, FishingMort){
  
  SIM.N.Cell <- list()
  SIM.N.Catches <- list()
  SIM.Age.Catches <- list()
  SIM.Weight.Catches <- list()
  
  ## Save all information by simulation
  Sim.Ages <- array(0, dim=c(MaxAge, MaxYear, NSIM))
  
  for (SIM in 1:NSIM){ # Simulation loop
    
    ## To save population information
    PopTotal <- array(0, dim=c(MaxCell, 12, MaxYear)) 
    Pop.Total.Cell <- array(0, dim=c(MaxCell, MaxYear))
    
    ## To save catch information
    age.catch <- array(0, dim=c(12, MaxAge, MaxYear))
    catch.by.cell <- array(0, dim=c(MaxCell, MaxYear))
    catch.by.age <- array(0, dim=(c(MaxAge, MaxYear)))
    catch.by.weight <- array(0, dim=(c(MaxCell, MaxYear)))
    
    #### SET UP INITIAL POPULATION ####
    
    print(paste0("SIM", sep=" ", SIM))
    
    YearlyTotal <- BurnInPop #readRDS(paste0(model.name, sep="_", "BurnInPop_High_M"))
    Effort <- FishingMort
    
    for (YEAR in 0:(MaxYear-1)){ # Start of model year loop, starts at 0 because things are indexed from 0 in C++
      
      print(YEAR+1) # Printing the year index in a value that R understands and I think is more intuitive
      
      ## Loop over all the Rcpp functions in the model
      ModelOutput <- RunModelfunc_cpp(YEAR, MaxAge, MaxYear, MaxCell, NatMort, BHa, BHb, PF, AdultMove, Mature, Weight, Settlement, 
                                      YearlyTotal, Selectivity, Effort)
      
      ## Get outputs from the model
      # Have to add 1 to all YEAR when adding to R objects because the loop is now starting at 0
      ## Abundance in different areas
      
      PopTotal[ , ,YEAR+1] <- rowSums(ModelOutput$YearlyTotal[,,1:MaxAge], dim=2) # This flattens the matrix to give you the number of fish present in the population each month in each cell, with layers representing the year
      # Total[YEAR+1,1] <- sum(PopTotal[ , 12, YEAR+1])
      
      # Total pop in each cell at the end of the year
      Pop.Total.Cell[ ,YEAR+1] <- PopTotal[,12,YEAR+1]
      
      ## Catch data
      monthly.catch <- ModelOutput$month_catch
      age.catch[,,YEAR+1] <- colSums(ModelOutput$month_catch) #This is the number of fish in each age class caught in each month
      
      catch.by.cell[,YEAR+1] <- rowSums(monthly.catch[,,CatchAge:MaxAge], dims=1) # Number of legal size fish caught in each cell 
      catch.by.age[,YEAR+1] <- colSums(age.catch[,,YEAR+1]) # number of fish caught by the end of the year in each age class
      
      monthly.catch.weight <- ModelOutput$month_catch_weight
      catch.by.weight[ ,YEAR+1] <- rowSums(monthly.catch.weight[,,CatchAge:MaxAge], dims=1)
      
      Sim.Ages[ ,YEAR+1,SIM] <- colSums(ModelOutput$YearlyTotal[ ,12,1:MaxAge]) #Number of fish present in age group at the end of the year
    } # End of model year loop
    
    ## Population
    SIM.N.Cell[[SIM]] <- Pop.Total.Cell
    
    ## Catches
    SIM.N.Catches[[SIM]] <- catch.by.cell # Catches in each cell
    SIM.Age.Catches[[SIM]] <- catch.by.age # Catches by age in each month of the year in each year
    SIM.Weight.Catches[[SIM]] <- catch.by.weight
    
  }
  
  Results <- list(Sim.Ages, 
                  SIM.N.Cell, 
                  SIM.N.Catches, 
                  SIM.Age.Catches, 
                  SIM.Weight.Catches)
  
  
  return(Results)
  
}

