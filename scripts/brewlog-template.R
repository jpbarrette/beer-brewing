library(xml2)
library(knitr)
library(rvest)
library(dplyr)

source("biab-calculator.R")

# retrieve the xml recipe
beerxml_output <- read_xml(brewlog_file)
data <- XML::xmlParse(brewlog_file)

nodes = XML::getNodeSet(data, "//HOPS/HOP[FORM=\"Leaf\" and USER_HOP_USE != \"Dry Hop\"]")
if (length(nodes) > 0)
{
  whole_hops <- XML::xmlToDataFrame(nodes, stringsAsFactors = FALSE) %>% transform(Amount = as.numeric(AMOUNT))  
  whole_hops_weight <- sum(whole_hops$Amount)
} else
  whole_hops_weight <- 0

batch_size <- xml_double(xml_find_first(beerxml_output, xpath='//BATCH_SIZE'))
grain_weight <- sum(xml_double(xml_find_all(beerxml_output, xpath='//FERMENTABLES/*/AMOUNT')))
grain_temp <- xml_double(xml_find_first(beerxml_output, xpath='//MASH/GRAIN_TEMP'))
mash_temp <- xml_double(xml_find_first(beerxml_output, xpath='//MASH/MASH_STEPS/*/STEP_TEMP'))
boil_time <- xml_double(xml_find_first(beerxml_output, xpath='//BOIL_TIME'))

recipe <- create_metric_recipe(grain_weight, whole_hops_weight, grain_temp, batch_size, mash_temp, boil_time, system_variables)

preboil_wort_volume <- round(get_pre_boil_wort_volume(recipe), 2)
postboil_wort_volume <- round(get_post_boil_wort_volume(recipe), 2)

style <- xml_text(xml_find_first(beerxml_output, xpath='//STYLE/NAME'))
efficiency <- xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/EFFICIENCY'))
recipe_source <- xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/BREWER'))
original_gravity <- sprintf("%.3f", as.numeric(strsplit(xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/EST_OG')), " ")[[1]][[1]]))
final_gravity <- sprintf("%.3f", as.numeric(strsplit(xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/EST_FG')), " ")[[1]][[1]]))

sprintf("%.3f", as.numeric(strsplit(xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/EST_OG')), " ")[[1]][[1]]))

recipe_price_calculator <- function(recipe, hops_weight) {
                                        # yeast ~ 3, cleaning product ~ 2 (200g chemipro + 6ml starsan), shipping ~ 2, malt ~ 3/kg, hop ~ 7/100g

    hopprice = if (exists("hop_price_override")) hop_price_override else 7.0
  7 + 3*recipe$grain_weight + hopprice*hops_weight/100
}


abv <-  xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/EST_ABV'))  
ibu <-  xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/IBU'))  
srm <-  xml_text(xml_find_first(beerxml_output, xpath='//RECIPE/EST_COLOR'))  

# TODO fix this
timetaken <- function(mins)
{
   hrs <- mins %/% 60
   days <- hrs %/% 24
   
   ifelse (days == 1, "1 day", days)
           ifelse(days >= 1, sprintf("%d days", as.integer(days)),
                  sprintf("%.0f mins", mins))
}



data <- XML::xmlParse(brewlog_file)
fermentables <- XML::xmlToDataFrame(nodes = XML::getNodeSet(data, "//FERMENTABLES/*"), stringsAsFactors = FALSE, colClasses=c("character", "integer", "character", "double", "double", "double", "character", "character")) %>% mutate(Name = paste(ORIGIN, " - " , NAME), Amount = AMOUNT, Yield = YIELD, Lovibond = COLOR, "Bill %" = sprintf("%.1f", Amount/sum(Amount) * 100)) %>% select(Name, Amount, Yield, Lovibond, "Bill %") 

hops <- XML::xmlToDataFrame(nodes = XML::getNodeSet(data, "//HOPS/*"), stringsAsFactors = FALSE) %>% select(AMOUNT, ALPHA, NAME, FORM, USE, TIME) %>% transform(AMOUNT = as.numeric(AMOUNT), ALPHA = as.numeric(ALPHA), TIME = as.numeric(TIME))
#hops$TIME <- as.difftime(hops$TIME, units="mins") 
yeasts <- XML::xmlToDataFrame(nodes = XML::getNodeSet(data, "//YEASTS/*"), stringsAsFactors = FALSE) %>% rename(Name = NAME, Attenuation = ATTENUATION) %>% select(Name, Attenuation) %>% transform(Attenuation = paste(Attenuation, "%", sep = ""))

#TODO Add IBU
orig_hops <- hops
hops <- mutate(hops, Amount=sprintf("%.fg", AMOUNT*1000), AA=ALPHA, Variety=NAME, Type=FORM, Use = USE, Time = timetaken(TIME)) %>% 
  select(Amount, Variety, Type, AA, Use, Time)

recipe_cost <- recipe_price_calculator(recipe, sum(orig_hops$AMOUNT*1000))

