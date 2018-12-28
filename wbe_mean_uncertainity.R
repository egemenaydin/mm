mean_uncertainity <- function(concentration = NULL, observations = NULL, 
                              flow = NULL, population = NULL, 
                              US = 0.1, UC = 0.10, UF = 0.05, UP = 0.2){
        
        UT_approx_formula <- sqrt((US/sqrt(observations))^2+UC^2+UF^2+UP^2)
        
        n_MC <- 100000
        
        sd_concentration <- UC*concentration
        sd_sampling <- US/sqrt(observations)
        sd_flow <- UF*flow
        sd_population <- UP*population
        
        repeat_MC <- function()
        {
                sampling_unc_MC <- rnorm(n_MC, 1, sd = sd_sampling)
                chemical_analysis_MC <- rnorm(n_MC, concentration, sd = sd_concentration)
                flow_MC <- rnorm(n_MC, population, sd = sd_flow)
                population_MC <- rnorm(n_MC, population, sd = sd_population)
                
                loads_MC <- sampling_unc_MC*(chemical_analysis_MC*flow_MC)/population_MC
                
                UT_MC <- sd(loads_MC)/mean(loads_MC)
                return(UT_MC)
        }
        
        UT_MC_vec <- rep(NA, 1)
        
        for (i in 1:length(UT_MC_vec))
        {
                UT_MC_vec[i] <- repeat_MC()
        }
        return(mean(UT_MC_vec))
}