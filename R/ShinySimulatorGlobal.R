#help functions

#' Parameter to set the maximum number of pareto slices
#'
#' @return The the maximum number of Pareto Slices.
max_number_of_pareto_slices = 5

#' Random Pareto generator
#'
#' @param n Number of values to generate.
#' @param alpha A positive real number. Alpha parameter of the Pareto distribution.
#' @param x_m A positive real number. The minimum value for the Pareto distribution.
#' @return A vector of \code{n} random Pareto variables with parameters \code{alpha} and \code{x_m}.
rpareto <- function(n, alpha, x_m) x_m / runif(n)^(1/alpha)

#' Apply severity cap function
#'
#' @param claims A vector of Claims.
#' @param severity_cap_boolean A variable that if true, the function will cap the claims, otherwise will just return them.
#' @param severity_cap_amount The claim cap value.
#' @return If \code{severity_cap_boolean} is true, then will return the minimum of \code{severity_cap_amount} or \code{claims} otherwise will return \code{claims}. The operation is vectorised.
apply_severity_cap <- function(claims, severity_cap_boolean, severity_cap_amount){
  if(severity_cap_boolean){
    claims <- ifelse(claims>severity_cap_amount, severity_cap_amount, claims)
  }
  return(claims)
}

#' A vector with the reinsurance structure options
#'
#' @return The reinsurance structure options
reinsurance_structures_options <- c('No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer')

#' Apply a deductible and limit to claims
#'
#' @param gross_claims_data A vector of Claims.
#' @param reinsurance_structure The chosen reinsurance structure. Options are: 'No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer'.
#' @param deductible The deductible of the reinsurance structure.
#' @param limit The limit of the reinsurance structure.
#' @return The ceded claims for the structure, with the chosen deductible and limit.
#' @export
#' @examples
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 40, 20)
#' apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 10, 30)
apply_deductible_limit <- function(gross_claims_data, reinsurance_structure, deductible, limit){
  if(reinsurance_structure=='No Reinsurance Structure'){return(gross_claims_data)}
  else if (reinsurance_structure=='Unlimited Layer'){return(ifelse(gross_claims_data<deductible,0, gross_claims_data-deductible))}
  else if (reinsurance_structure=='Limited Layer'){return(ifelse(gross_claims_data<deductible, 0, ifelse(gross_claims_data-deductible>limit, limit, gross_claims_data-deductible)))}
  else if (reinsurance_structure=='Exclude Layer'){return(gross_claims_data - ifelse(gross_claims_data<deductible, 0, ifelse(gross_claims_data-deductible>limit, limit, gross_claims_data-deductible)))}
}

#' The class of the distribution objects
#'
distributionClass <- setClass("distributionClass", slots = c(
  distrID="character"
  ,distr_label="character"
  ,paramIDs="character"
  ,param_labels="character"
  ,param_min_values="numeric"
  ,param_max_values="numeric"
  ,simulate_func = "function"
))

#' A vector with the frequency distribution objects
#'
#' @return The frequency distribution objects.
freq_dist_options <- c(
  Poisson=distributionClass(
    distrID='Poisson'
    ,distr_label='Poisson'
    ,paramIDs=c("lamda")
    ,param_labels=c("lamda")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpois(n = number_of_simulations, lambda = parameters[1])
    )}
  )
  ,Negative_Binomial=distributionClass(
    distrID='Negative_Binomial'
    ,distr_label='Negative Binomial'
    ,paramIDs=c("r", "beta")
    ,param_labels=c("r", "beta")
    ,param_min_values=c(0,0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpois(n = number_of_simulations, lambda = rgamma(n =number_of_simulations, shape = parameters[1], scale = parameters[2]))
    )}
  )
  ,Binomial=distributionClass(
    distrID='Binomial'
    ,distr_label='Binomial'
    ,paramIDs=c("n", "p")
    ,param_labels=c("n", "p")
    ,param_min_values=c(0,0)
    ,param_max_values=c(NA,1)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rbinom(n = number_of_simulations, size = parameters[1], prob = parameters[2])
    )}
  )
  ,Fixed_number_of_Counts=distributionClass(
    distrID='Fixed_number_of_Counts'
    ,distr_label='Fixed number of Counts'
    ,paramIDs=c("FixedNumberOfCounts")
    ,param_labels=c("FixedNumberOfCounts")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rep(parameters[1], number_of_simulations)
    )}
  )
)

#' A data frame with the frequency distribution parameter placeholders
#'
#' @return The frequency distribution parameter placeholders.
freq_dist_parameter_placeholders <- data.frame(
  param_number = 1:max(sapply(freq_dist_options, function(x) length(x@paramIDs)))
  ,param_id = paste0("freq_param_", 1:max(sapply(freq_dist_options, function(x) length(x@paramIDs))))
)

#' A vector with the severity distribution objects
#'
#' @return The severity distribution objects.
sev_dist_options <- c(
  Normal=distributionClass(
    distrID='Normal'
    ,distr_label='Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("mu", "sigma")
    ,param_min_values=c(0, 0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rnorm(n = number_of_simulations, mean = parameters[1], sd = parameters[2])
    )}
  )
  ,LogNormal=distributionClass(
    distrID='LogNormal'
    ,distr_label='Log-Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("mu", "sigma")
    ,param_min_values=c(0,0)
    ,param_max_values=c(NA,1)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rlnorm(n = number_of_simulations, meanlog = parameters[1], sdlog = parameters[2])
    )}
  )
  ,Gamma=distributionClass(
    distrID='Gamma'
    ,distr_label='Gamma'
    ,paramIDs=c("rate", "scale")
    ,param_labels=c("rate", "scale")
    ,param_min_values=c(0,0)
    ,param_max_values=c(NA,1)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rgamma(n = number_of_simulations, rate = parameters[1], scale = parameters[2])
    )}
  )
  ,Exponential=distributionClass(
    distrID='Exponential'
    ,distr_label='Exponential'
    ,paramIDs=c("rate")
    ,param_labels=c("rate")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rexp(n = number_of_simulations, rate = parameters[1])
    )}
  )
  ,Pareto=distributionClass(
    distrID='Pareto'
    ,distr_label='Pareto'
    ,paramIDs=c("alpha", "x_m")
    ,param_labels=c("alpha", "x_m")
    ,param_min_values=c(0, 0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpareto(n = number_of_simulations, alpha = parameters[1], x_m = parameters[2])
    )}
  )
  ,Fixed_Severity=distributionClass(
    distrID='Fixed_Severity'
    ,distr_label='Fixed Severity'
    ,paramIDs=c("Fixed_sev_amount")
    ,param_labels=c("Fixed Severity Amount")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rep(parameters[1], number_of_simulations)
    )}
  )
)

#' A data frame with the severity distribution parameter placeholders
#'
#' @return The severity distribution parameter placeholders.
sev_dist_parameter_placeholders <- data.frame(
  param_number = 1:max(sapply(sev_dist_options, function(x) length(x@paramIDs)))
  ,param_id = paste0("sev_param_", 1:max(sapply(sev_dist_options, function(x) length(x@paramIDs))))
)

#' A function to simulate frequency - severity of insurance claims. The function applies severity cap, reinsurance structure for each and every loss claim, reinsurance structure for each and aggregate claims. The function allows for piecewise pareto slices.
#'
#' @param numOfSimulations The number of simulations to run.
#' @param freq_params A vector of the frequency distribution parameters.
#' @param sev_params A vector of the severity distribution parameters.
#' @param seedSetBinary True if there is a fixed seed, otherwise false.
#' @param seedValue The seed value.
#' @param freqDistr The frequency distribution. Options are as per the freq_dist_options.
#' @param sevDistr The severity distribution. Options are as per the sev_dist_options.
#' @param paretoSlice True if there is Pareto slicing.
#' @param pareto_slice_times The number of Pareto slices.
#' @param slice_pareto_alphas A vector of Pareto slices' aphla parameters.
#' @param slice_pareto_x_ms A vector of Pareto slices' x_m parameters.
#' @param sevCapBinary True if there is a severity cap.
#' @param sev_cap_amount The severity cap amount.
#' @param reinsuranceStructureEEL The chosen reinsurance structure for each and every loss claim.
#' @param reinsurance_structure_eel_dedctible_amount The deductible for each and every loss reinsurance structure.
#' @param reinsurance_structure_eel_limit_amount The limit for each and every loss reinsurance structure.
#' @param reinsuranceStructureAL The chosen reinsurance structure for aggregate claims.
#' @param reinsurance_structure_al_dedctible_amount The deductible for aggregate reinsurance structure.
#' @param reinsurance_structure_al_limit_amount The limit for aggregate reinsurance structure.
#' @param reinsuranceStructureLimitedReinstatements True if there is a limit in reinstatements, otherwise false.
#' @param reinsuranceStructureReinstatementLimit The reinstatement limit.
#' @param multiprocessing True if multiprocessing is used, otherwise false.
#' @return A data frame with claims counts, ceded claims and the number of reinstatements used.
#' @export
simulate_function <- function(
    numOfSimulations,
    freq_params,
    sev_params,
    seedSetBinary,
    seedValue,
    freqDistr,
    sevDistr,
    paretoSlice,
    pareto_slice_times,
    slice_pareto_alphas,
    slice_pareto_x_ms,
    sevCapBinary,
    sev_cap_amount,
    reinsuranceStructureEEL,
    reinsurance_structure_eel_dedctible_amount,
    reinsurance_structure_eel_limit_amount,
    reinsuranceStructureAL,
    reinsurance_structure_al_dedctible_amount,
    reinsurance_structure_al_limit_amount,
    reinsuranceStructureLimitedReinstatements,
    reinsuranceStructureReinstatementLimit,
    multiprocessing
  ){
  #set custom seed
  if(seedSetBinary){set.seed(seedValue)}
  #initiate data and simulate counts
  data <- data.frame(claim_counts = freq_dist_options[[freqDistr]]@simulate_func(
    number_of_simulations = numOfSimulations
    ,parameters = freq_params
  ))

  #predefine parameters in a simulate severity function
  #simulate claims from the chosen distribution and parammeters
  simulate_individual_severities_parametrised <- function (claim_counts){

    claims = sev_dist_options[[sevDistr]]@simulate_func(
      number_of_simulations = claim_counts
      ,parameters = sev_params
    )
    #apply pareto slices
    if(paretoSlice){
      for(j in 1:pareto_slice_times){
        claims = ifelse(
          claims > slice_pareto_x_ms[j]
          ,rpareto(n = claim_counts, alpha = slice_pareto_alphas[j], x_m = slice_pareto_x_ms[j])
          ,claims
        )
      }
    }
    #apply severity cap
    claims = apply_severity_cap(
      claims
      ,severity_cap_boolean = sevCapBinary
      ,severity_cap_amount = sev_cap_amount
    )
    #apply EEL deductible
    claims = apply_deductible_limit(
      claims
      ,reinsurance_structure = reinsuranceStructureEEL
      ,deductible = reinsurance_structure_eel_dedctible_amount
      ,limit = reinsurance_structure_eel_limit_amount
    )
    #sum individual claims and return them
    claims = sum(claims)
    return(claims)
  }

  #simulate individual severities, apply severity cap, apply reinsurance structure EEL and take a sum of individual claims
  if(multiprocessing){plan(multisession)}
  data$total_claims <- unlist(
    if(multiprocessing){
      future_lapply(
        future.seed = T
        ,data$claim_counts
        ,function(z) {simulate_individual_severities_parametrised(z)}
      )
    } else {
      lapply(
        data$claim_counts
        ,function(z) {simulate_individual_severities_parametrised(z)}
      )
    }
  )
  if(multiprocessing){plan(sequential)}
  #apply reinstatements
  if(reinsuranceStructureEEL  %in% c('Limited Layer')){
    if(reinsuranceStructureLimitedReinstatements){
      data$total_claims <- apply_deductible_limit(
        data$total_claims
        ,'Limited Layer'
        ,0
        ,(reinsuranceStructureReinstatementLimit + 1) * reinsurance_structure_eel_limit_amount
      )
      data$number_of_reinstatements_used <- (data$total_claims / reinsurance_structure_eel_limit_amount)
      data$number_of_reinstatements_used <- ifelse(
        data$number_of_reinstatements_used>reinsuranceStructureReinstatementLimit
        ,reinsuranceStructureReinstatementLimit
        ,data$number_of_reinstatements_used
      )
      data$number_of_reinstatements_used <- round(data$number_of_reinstatements_used,2)
    }
  }
  #apply reinsurance structure AL
  data$total_claims <- apply_deductible_limit(
    data$total_claims
    ,reinsuranceStructureAL
    ,reinsurance_structure_al_dedctible_amount
    ,reinsurance_structure_al_limit_amount
  )
  data$total_claims <- round(data$total_claims,2)
  return(data)
}

#' A function to run the shiny simulator application
#'
#' @return Opens the shiny simulator application
#' @export
run_shiny_simulator = function(){shinyApp(ui = shiny_simulator_ui, server = shiny_simulator_server)}
