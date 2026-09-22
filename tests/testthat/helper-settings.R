#shared settings for the simulator tests: small runs with a fixed seed keep the suite fast

#settings for a run without reinsurance; named arguments override or (with NULL) remove entries
base_settings <- function(...) {
  settings <- list(
    numOfSimulations = 2000,
    freq_params = c(3),
    sev_params = c(6, 1.5),
    seedSetBinary = TRUE,
    seedValue = 42,
    freqDistr = "Poisson",
    sevDistr = "LogNormal",
    paretoSlice = FALSE,
    sevCapBinary = FALSE,
    reinsuranceStructureEEL = "No Reinsurance Structure",
    reinsuranceStructureAL = "No Reinsurance Structure",
    reinsuranceStructureLimitedReinstatements = FALSE,
    multiprocessing = FALSE
  )
  utils::modifyList(settings, list(...))
}

#settings for a run with an each-and-every-loss layer (two reinstatements) and an aggregate layer
layered_settings <- function(...) {
  settings <- base_settings(
    reinsuranceStructureEEL = "Limited Layer",
    reinsurance_structure_eel_dedctible_amount = 1000,
    reinsurance_structure_eel_limit_amount = 5000,
    reinsuranceStructureLimitedReinstatements = TRUE,
    reinsuranceStructureReinstatementLimit = 2,
    reinsuranceStructureAL = "Limited Layer",
    reinsurance_structure_al_dedctible_amount = 500,
    reinsurance_structure_al_limit_amount = 10000
  )
  utils::modifyList(settings, list(...))
}

run_simulation <- function(...) do.call(simulate_function, base_settings(...))

run_layered_simulation <- function(...) do.call(simulate_function, layered_settings(...))
