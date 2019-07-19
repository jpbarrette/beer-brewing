
#functions here are a complete rip off from biabcalculator.com

# I don't have any thrub (I pour everything in the fermenter), however whole leaf hops has some wort absorption.
#
# https://www.homebrewtalk.com/forum/threads/leaf-hop-absorption-measured.241469/
#
# 138.3 ml/oz
get_whole_hops_volume = function(wholehop_weight) {
  wholehop_weight*138.3/28.3495
}

boiloff_rate = if (exists("boiloff_rate_override")) boiloff_rate_override else 2.75

system_variables <- list(trub_size = 0.0, boiloff_rate = boiloff_rate, grain_absorption = 0.48, kettle_size = 27)

create_metric_recipe <- function(grain_weight, wholehop_weight, grain_temp, batch_size, mash_temp, boil_time, system_variables) {
  # thermodynamic const is 0.2 for metric system
  list(grain_weight = grain_weight, grain_temp = grain_temp, batch_size = batch_size, mash_temp = mash_temp, boil_time = boil_time, trub_size = system_variables$trub_size + get_whole_hops_volume(whole_hops_weight), boiloff_rate = system_variables$boiloff_rate, grain_absorption = system_variables$grain_absorption, thermodynamic_constant = 0.41)
}

get_water_needed = function (recipe) {
  (recipe$grain_weight * recipe$grain_absorption) + (recipe$boiloff_rate * recipe$boil_time / 60) + recipe$batch_size + recipe$trub;
};

get_strike_water_temp = function (recipe) {
  (recipe$thermodynamic_constant / ((get_water_needed(recipe) / recipe$grain_weight) * 4)) * (recipe$mash_temp - recipe$grain_temp) + recipe$mash_temp;
};

get_total_mash_volume = function (recipe) {
  # this is the mash volume plus grains.  Used to see if the batch will all fit in the kettle
  # (grain * .08) + waterNeeded
  (recipe$grain_weight * 0.667) + get_water_needed(recipe)  # grain takes up (in volume, ie space) .08 gallons/pound or .667 L/kg
}

get_pre_boil_wort_volume = function (recipe) {
  # volume after bag is removed from mash - be sure to include squeezed (and find a constant or calculation there)
  total_water <- get_water_needed(recipe)
  total_water - (recipe$grain_weight * recipe$grain_absorption)
}

get_post_boil_wort_volume = function (recipe) {
  # volume in kettle after boil
  # preBoilWortVolume-(boilOff * Boil time in hrs)
  get_pre_boil_wort_volume(recipe) - (recipe$boiloff_rate * (recipe$boil_time / 60))
}

get_fermenter_volume = function (recipe) {
  # volume into fermenter - remove trub value
  get_post_boil_wort_volume(recipe) - recipe$trub
}

get_lovibond_to_ebc = function(lov) {
  ((1.3546 * lov) - 0.76) * 1.97
}

