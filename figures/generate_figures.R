library("reshape2")
library("stringr")
library("scales")
library("plyr")
library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("rgcam")
library("jgcricolors")
library("rmap")
library( "zoo" )
source( "colors.R" )	# colors used for figures

# SET WORKING DIRECTORY AND FOLDERS ------------------------------------------------------------------

setwd("D:/INFEWS/Trade/metarepo/figures")

PLOT_FOLDER <- "output/"

# Create the specified output directory inside the current working directory
dir.create(PLOT_FOLDER)

# DEFINE GROUPS, LEVELS, LABELS FOR PLOTS -------------------------------------------------
crop_group <- c( "corn",
                 "fibercrop",
                 "misccrop",
                 "oilcrop",
                 "othergrain",
                 "palmfruit",
                 "rice",
                 "roottuber",
                 "sugarcrop",
                 "wheat")

ls_group <- c("beef",
              "dairy",
              "pork",
              "poultry",
              "sheepgoat",
              "othermeat_fish")

crop_staples_group <- c("corn",
                        "othergrain",
                        "rice",
                        "roottuber",
                        "wheat")

crop_nonstaples_group <- c("fibercrop",
                           "misccrop",
                           "oilcrop",
                           "palmfruit",
                           "sugarcrop")

#Land use
crop_land_group <- c("Biomass", "Corn", "FiberCrop", "Fodder", "MiscCrop", "OilCrop", "OtherGrain", "PalmFruit",  "Rice", "RootTuber", "SugarCrop", "Wheat")


#GHG emissions
pri_ene_trans_GHG_group <- c("coal", "unconventional oil", "crude oil", "natural gas", "gas pipeline", "gas processing", "refining")
crop_luc_GHG_group <- c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "MiscCrop", "OilCrop", "OtherGrain", "PalmFruit", "Rice", "RootTuber","SugarCrop", "Wheat", 
                        "biomass", "UnmanagedLand")
neg_biomass_GHG_group <- c("regional biomass", "regional biomassOil", "regional corn for ethanol", "regional sugar for ethanol")
livestock_GHG_group <- c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")
ind_urb_GHG_group <- c("H2 central production", "N fertilizer", "cement", "desalinated water", 
                       "industrial energy use", "industrial feedstocks", "industrial processes", "urban processes", "process heat cement")
trn_GHG_group <- c("H2 forecourt production", "trn_aviation_intl","trn_freight","trn_freight_road",
                   "trn_pass","trn_pass_road","trn_pass_road_LDV","trn_pass_road_LDV_4W","trn_shipping_intl")
elec_GHG_group <- c("backup_electricity", "elec_biomass (IGCC CCS)", "elec_biomass (IGCC)", "elec_biomass (conv CCS)", "elec_biomass (conv)",
                    "elec_coal (IGCC CCS)","elec_coal (IGCC)","elec_coal (conv pul CCS)","elec_coal (conv pul)",
                    "elec_gas (CC CCS)","elec_gas (CC)","elec_gas (steam/CT)",
                    "elec_refined liquids (CC CCS)","elec_refined liquids (CC)","elec_refined liquids (steam/CT)",
                    "electricity","electricity_net_ownuse", "csp_backup")
bld_GHG_group <- c("comm cooling", "comm heating", "comm others", "resid cooling", "resid heating", "resid others", "district heat")


plot_regions <- list("Colombia", "Argentina", "Uruguay", "Brazil", "Central America and Caribbean", "Mexico", "South America_Northern", "South America_Southern")

export_LAC_regions <- list("Argentina", "Brazil", "South America_Southern", "Uruguay")
import_LAC_regions <- list("Central America and Caribbean", "Colombia", "Mexico", "South America_Northern")


# SCENARIOS
# ARM_Reference = REF
# SW_high_CL_Reference = MI
# ARM_Policy_CO2_10pa_2025 = CO2
# SW_high_CL_Policy_CO2_10pa_2025 = MI + CO2
# ARM_Policy_10pa_2025 = CM
# SW_high_CL_Policy_10pa_2025 = MI + CM
# ARM_Policy_diff_MAC_10pa_2025 = CM (diff. MAC)
# SW_high_CL_Policy_diff_MAC_10pa_2025 = MI + CM (diff. MAC)
scenario_levels <- c("ARM_Reference",  "SW_high_CL_Reference",
                     "ARM_Policy_CO2_10pa_2025", "SW_high_CL_Policy_CO2_10pa_2025",
                     "ARM_Policy_10pa_2025","SW_high_CL_Policy_10pa_2025",
                     "ARM_Policy_diff_MAC_10pa_2025", "SW_high_CL_Policy_diff_MAC_10pa_2025")

region_facet_labels <- c("Argentina" = "Argentina",
                         "Brazil" = "Brazil",
                         "South America_Southern" = "S. South America",
                         "Uruguay" = "Uruguay",
                         "Central America and Caribbean" = "Central America and Caribbean",
                         "Colombia" = "Colombia",
                         "Mexico" = "Mexico",
                         "South America_Northern" = "N. South America")

region_short_labels <- c("Argentina" = "ARG",
                         "Brazil" = "BRA",
                         "South America_Southern" = "SAS",
                         "Uruguay" = "URY",
                         "Central America and Caribbean" = "CAC",
                         "Colombia" = "COL",
                         "Mexico" = "MEX",
                         "South America_Northern" = "SAN")

# DEFINE FUNCTIONS ---------------------------------------------------------------
#   Define functions for interpolating between years
all.na_new  <- function(x) return( all( is.na(x) ) )

interpolate_NAs_new <- function(df) {
  
  if( !is.data.frame( df ) ) {
    
    warning( "interpolate_NAs expects a data frame; attempting to convert" )
    
    df <- as.data.frame( df )
    
  }
  
  #     Convert columns that are all NA to numeric
  df <- dplyr::mutate_if( df, function( x ) all.na_new( x ), as.numeric )
  
  if( length( rle( sapply( df, is.numeric ) )$lengths ) > 2) {
    
    warning( "interpolate_NAs found mixed numeric and non-numeric columns;
                  make sure all value columns are numeric")
    
  }
  
  value.cols <- sapply( df, is.numeric )
  df_flipped <- t( df[value.cols] )
  df[ , value.cols] <- t( na.approx( df_flipped, na.rm = F ) )
  
  return(df)
  
}

#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )




# READ IN ADDITIONAL FILES -----------------------------------------------------------

basin_to_country_mapping <- readr::read_csv("./input/basin_to_country_mapping.csv", skip = 7) %>%
  select(GCAM_basin_ID, Basin_long_name, GLU_name)

GCAM_region_names <- readr::read_csv("./input/GCAM_region_names.csv", skip = 6) 

pop_ssp2_reg_basin <- readr::read_csv("./input/pop_ssp2_reg_basin.csv")

gdp_pc_ssp2_reg_basin <- readr::read_csv("./input/gdp_pc_ssp2_reg_basin.csv")

basin_reg_livestock_share <- readr::read_csv("./input/basin_reg_livestock_share.csv")

AgHA_to_CL <- readr::read_csv("./input/L2012.AgHAtoCL_irr_mgmt.csv", skip = 2)

GCAM_YEARS <- as.character(c(1990, seq(2005,2100, by = 5)))

basin_runoff <- readr::read_csv("./input/basin_runoff.csv") %>%
  pivot_longer(cols = GCAM_YEARS, names_to = "year") %>%
  mutate(year = as.numeric(year))

POP_GCAM <- readr::read_csv("./input/POP_GCAM.csv")
GDP_GCAM <- readr::read_csv("./input/GDP_GCAM.csv", skip = 1)
GWP <- readr::read_csv("./input/GWP.csv")


IAMC_YEARS <- as.character(c(seq(2005,2100, by = 5)))
#IAMC Database
IAMC_15C <- readr::read_csv("./input/iamc-1.5c.csv") %>%
  interpolate_NAs_new() %>%
  pivot_longer(cols = IAMC_YEARS, names_to = "year") %>%
  mutate(year = as.numeric(year))

IAMC_2C <- readr::read_csv("./input/iamc-2c.csv") %>%
  interpolate_NAs_new() %>%
  pivot_longer(cols = IAMC_YEARS, names_to = "year") %>%
  mutate(year = as.numeric(year))

# SELECT DATABASE ---------------------------------------------------------

prj <- loadProject("./input/1_GCAM_queries_full.dat")
# RETREIVE QUERIES  --------------------------------------------------------
## Retrieve queries for all scenarios in the dataset,
## formatted as a single table
#AG PROD
biomass_exports <- getQuery(prj, "Crop exports") %>%  filter(grepl("biomass", sector))
biomass_imports_domestic <- getQuery(prj, "Quantity available for crop commodity demand (domestic and imported)") %>%  filter(grepl("biomass", sector))

exports <- getQuery(prj, "traded ag commodity sources")
imports_domestic <- getQuery(prj, "regional ag commodity sources")

traded_ag_commodity_prices <- getQuery(prj, "traded ag commodity prices")
regional_ag_commodity_prices <- getQuery(prj, "regional ag commodity price")
crop_prices <- getQuery(prj, "ag commodity prices")

biomass_commodity_prices <- getQuery(prj, "regional biomass commodity price")
meat_dairy_prices <- getQuery(prj, "meat and dairy prices")

ag_prod_subsector <- getQuery(prj, "ag production by subsector (land use region)")

food_cons <- getQuery(prj, "food consumption by type (specific)")

an_water_C <- getQuery(prj, "outputs by tech") %>% filter(output == "water_td_an_C")
an_water_W <- getQuery(prj, "outputs by tech") %>% filter(output == "water_td_an_W")


land_alloc_basin <- getQuery(prj, "land allocation in a specified land use region")
water_wd_basin <- getQuery(prj, "water withdrawals by water mapping source")

#GHG
LUC_CO2 <- getQuery(prj, "LUC emissions by region")
nonCO2_res_prod <- getQuery(prj, "nonCO2 emissions by resource production")
nonCO2_sector <- getQuery(prj, "nonCO2 emissions by sector")
nonCO2_subsector <- getQuery(prj, "nonCO2 emissions by subsector")

GHG_price <- getQuery(prj, 'CO2 prices')


# LAC/ROW AND LAC-REGION CALCULATIONS ------------------------------------------------------------

  # COMMODITY PRODUCTION AND TRADE -------------------------------------------------------------


#process biomass
region_biomass_exports <- biomass_exports %>%
  select(-region, -input) %>%
  separate(subsector, c("region", NA), sep = " traded") %>%
  mutate(sector = gsub("traded ", "", sector))

region_biomass_imports <- biomass_imports_domestic %>%
  filter(grepl("imported", subsector)) %>%
  mutate(sector = gsub("total ", "", sector)) %>%
  select(-input, -subsector, -technology)

region_biomass_domestic <- biomass_imports_domestic %>%
  filter(grepl("domestic", subsector)) %>%
  mutate(sector = gsub("total ", "", sector)) %>%
  select(-input, -subsector, -technology)

#combine biomass with the rest of crop and livestock
region_exports <- exports %>%
  select(-region, -input) %>%
  separate(subsector, c("region", NA), sep = " traded") %>%
  mutate(sector = gsub("traded ", "", sector)) %>%
  mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
  bind_rows(region_biomass_exports) %>%
  filter(sector %in% c("biomass", crop_group, ls_group))

region_imports <- imports_domestic %>%
  filter(grepl("imported", subsector)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  mutate(sector = gsub("total ", "", sector)) %>%
  mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
  select(-subsector, -input) %>%
  bind_rows(region_biomass_imports) %>%
  filter(sector %in% c("biomass", crop_group, ls_group))

region_domestic <- imports_domestic %>%
  filter(grepl("domestic", subsector)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  mutate(sector = gsub("total", "", sector)) %>%
  mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
  select(-subsector, -input) %>%
  bind_rows(region_biomass_domestic) %>%
  filter(sector %in% c("biomass", crop_group, ls_group))

#prices
region_domestic_biomass_prices <- biomass_commodity_prices %>%
  filter(subsector == "domestic biomass") %>%
  mutate(sector = gsub("total ", "", sector)) %>%
  select(-subsector)

region_imported_biomass_prices <- biomass_commodity_prices %>%
  filter(subsector == "imported biomass") %>%
  mutate(sector = gsub("total ", "", sector)) %>%
  select(-subsector)

#combine biomass prices with the rest of prices
region_domestic_prices <- regional_ag_commodity_prices %>%
  filter(grepl("domestic ", subsector)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
  select(-subsector) %>%
  bind_rows(region_domestic_biomass_prices) %>%
  dplyr::rename(price.domestic = value) %>%
  filter(sector %in% c("biomass", crop_group, ls_group)) %>%
  mutate(price.domestic = price.domestic * 3.8,
         Units = "2020$")

region_import_prices <- regional_ag_commodity_prices %>%
  filter(grepl("imported ", subsector)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
  select(-subsector) %>%
  bind_rows(region_imported_biomass_prices) %>%
  dplyr::rename(price.imports = value) %>%
  filter(sector %in% c("biomass", crop_group, ls_group))%>%
  mutate(price.imports = price.imports * 3.8,
         Units = "2020$")

region_export_prices <- traded_ag_commodity_prices %>%
  filter(sector != "traded unconventional oil") %>%
  select(-region) %>%
  separate(subsector, c("region", NA), sep = " traded") %>%
  mutate(sector = gsub("traded ", "", sector)) %>%
  mutate(sector = gsub("root_tuber", "roottuber", sector)) %>%
  dplyr::rename(price.exports = value)  %>%
  filter(sector %in% c("biomass", crop_group, ls_group)) %>%
  mutate(price.exports = price.exports * 3.8,
         Units = "2020$")


#Combine region's imports, exports, and calculate net trade volume by commodity
region_net_trade_volume <- region_imports %>%
  full_join(region_exports, by = c("region", "Units", "scenario", "sector", "year"), suffix = c(".imports", ".exports")) %>%
  filter(year %in% c(seq(2015, 2050, 5))) %>%
  #Imports are negative, exports are positive
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(value.imports = value.imports * -1,
         value.exports = if_else(is.na(value.exports), 0, value.exports),
         value.net = value.exports + value.imports)

#value of net trade by commodity
region_net_trade_value <- region_net_trade_volume %>%
  left_join(region_import_prices, by = c("scenario", "region", "sector", "year")) %>%
  select(-Units.x, -Units.y) %>%
  left_join(region_export_prices, by = c("scenario", "region", "sector", "year")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(value.imports = value.imports * price.imports * 10^9,
         value.exports = value.exports * price.exports * 10^9) %>%
  mutate(value.net = value.exports + value.imports,
         Units = "2020$")

#total value of net trade
region_total_net_trade_value <- region_net_trade_value %>%
  group_by(scenario, region, year, Units) %>%
  summarise_at( c("value.imports", "value.exports", "value.net"), sum)

#value of domestic commodity
region_domestic_value <- region_domestic %>%
  filter(year %in% c(seq(2015, 2050, 5))) %>%
  left_join(region_domestic_prices, by = c("scenario", "region", "sector", "year")) %>%
  mutate(value.domestic = value * price.domestic * 10^9,
         Units = "2020$") %>%
  select(-Units.x, -Units.y, -value)

#total value of domestic commodities
region_total_domestic_value <- region_domestic_value %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value.domestic = sum(value.domestic))

#value of production by commodity
region_production_value <- region_domestic_value %>%
  full_join(region_net_trade_value, by = c("scenario", "region", "sector", "year", "Units")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(value.production = value.domestic + value.exports)

#total value of production
region_total_production_value <- region_production_value %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value.production = sum(value.production))

#volume of production by commodity
region_production_volume <- region_domestic %>%
  filter(year %in% c(seq(2015, 2050, 5))) %>%
  full_join(region_net_trade_volume, by = c("scenario", "region", "sector", "year", "Units")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  dplyr::rename(value.domestic = value) %>%
  mutate(value.production = value.domestic + value.exports)

#consumer prices
region_consumer <- region_domestic_value %>%
  left_join(region_domestic, by = c("scenario", "region", "sector", "year"),
            suffix = c(".value", ".volume")) %>%
  dplyr::rename(volume.domestic = value) %>%
  left_join(region_net_trade_value,  by = c("scenario", "region", "sector", "year")) %>%
  left_join(region_imports, by = c("scenario", "region", "sector", "year")) %>%
  dplyr::rename(volume.imports = value) %>%
  select(-Units.x, -Units.y) %>%
  mutate(consumer_expend = value.domestic + value.imports*-1) %>%
  mutate(consumer_price = consumer_expend / ((volume.domestic + volume.imports) * 10^9)) %>%
  select(scenario, region, sector, year, consumer_expend, consumer_price)

#producer prices
region_producer_price <- region_domestic_prices %>%
  mutate(price = "price.producer") %>%
  dplyr::rename(value = price.domestic) %>%
  mutate(sector = tolower(sector)) %>%
  select(-Units)

#producer and consumer prices
region_producer_consumer_prices <- region_consumer %>%
  mutate(value = consumer_price,
         price = "price.consumer") %>%
  select(-consumer_expend, -consumer_price) %>%
  bind_rows(region_producer_price) %>%
  mutate(Units = "2020$")

#consumer expenditure
region_consumer_expend <- region_consumer %>%
  select(-consumer_price)

#total consumer expenditure
region_total_consumer_expend <- region_consumer_expend %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(consumer_expend))

#domestic and imported prices
region_domestic_imported_prices <- region_import_prices %>%
  mutate(value = price.imports,
         price = "price.imports") %>%
  select(-price.imports) %>%
  bind_rows(region_producer_price) %>%
  mutate(price = if_else(price == "price.producer", "price.domestic", price),
         Units = "2020$")

 

  # GROUPED PRODUCTION VALUE  ------------------------------------------------
biomass_production_value <- region_production_value %>%
  filter(sector == "biomass") %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.production)) %>%
  mutate(agg_sector = "biomass")

crop_staples_production_value <- region_production_value %>%
  filter(sector %in% crop_staples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.production)) %>%
  mutate(agg_sector = "staple crops")

crop_nonstaples_production_value <- region_production_value %>%
  filter(sector %in% crop_nonstaples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.production)) %>%
  mutate(agg_sector = "non-staple crops")

livestock_production_value <- region_production_value %>%
  filter(sector %in% ls_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.production)) %>%
  mutate(agg_sector = "livestock")

grouped_production_value <- bind_rows(biomass_production_value,
                                      crop_staples_production_value,
                                      crop_nonstaples_production_value,
                                      livestock_production_value)

LAC_grouped_production_value <- grouped_production_value %>%
  filter(region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  group_by(scenario, agg_sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "LAC")

ROW_grouped_production_value <- grouped_production_value %>%
  filter(region %!in% c(export_LAC_regions, import_LAC_regions)) %>%
  group_by(scenario, agg_sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "ROW")

  # GROUPED NET TRADE VALUE -------------------------------------------------
biomass_net_trade_value <- region_production_value %>%
  filter(sector == "biomass") %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.net)) %>%
  mutate(agg_sector = "biomass")

crop_staples_net_trade_value <- region_production_value %>%
  filter(sector %in% crop_staples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.net)) %>%
  mutate(agg_sector = "staple crops")

crop_nonstaples_net_trade_value <- region_production_value %>%
  filter(sector %in% crop_nonstaples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.net)) %>%
  mutate(agg_sector = "non-staple crops")

livestock_net_trade_value <- region_production_value %>%
  filter(sector %in% ls_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.net)) %>%
  mutate(agg_sector = "livestock")

grouped_net_trade_value <- bind_rows(biomass_net_trade_value,
                                     crop_staples_net_trade_value,
                                     crop_nonstaples_net_trade_value,
                                     livestock_net_trade_value)

LAC_grouped_net_trade_value <- grouped_net_trade_value %>%
  filter(region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  group_by(scenario, agg_sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "LAC")

ROW_grouped_net_trade_value  <- grouped_net_trade_value  %>%
  filter(region %!in% c(export_LAC_regions, import_LAC_regions)) %>%
  group_by(scenario, agg_sector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "ROW")
  # GROUPED EXPORT IMPORT VALUE -----------------------------------------------------

crop_staples_export_value <- region_production_value %>%
  filter(sector %in% crop_staples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.exports)) %>%
  mutate(agg_sector = "staple crops",
         trade = "export")

crop_nonstaples_export_value <- region_production_value %>%
  filter(sector %in% crop_nonstaples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.exports)) %>%
  mutate(agg_sector = "non-staple crops",
         trade = "export")

livestock_export_value <- region_production_value %>%
  filter(sector %in% ls_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.exports)) %>%
  mutate(agg_sector = "livestock",
         trade = "export")

biomass_export_value <- region_production_value %>%
  filter(sector == "biomass") %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.exports)) %>%
  mutate(agg_sector = "biomass",
         trade = "export")

crop_staples_import_value <- region_production_value %>%
  filter(sector %in% crop_staples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.imports)) %>%
  mutate(agg_sector = "staple crops",
         trade = "import")

crop_nonstaples_import_value <- region_production_value %>%
  filter(sector %in% crop_nonstaples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.imports)) %>%
  mutate(agg_sector = "non-staple crops",
         trade = "import")

livestock_import_value <- region_production_value %>%
  filter(sector %in% ls_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.imports)) %>%
  mutate(agg_sector = "livestock",
         trade = "import")

biomass_import_value <- region_production_value %>%
  filter(sector == "biomass") %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.imports)) %>%
  mutate(agg_sector = "biomass",
         trade = "import")

grouped_trade_value <- bind_rows(crop_staples_export_value,
                                 crop_nonstaples_export_value,
                                 livestock_export_value,
                                 biomass_export_value,
                                 crop_staples_import_value,
                                 crop_nonstaples_import_value,
                                 livestock_import_value,
                                 biomass_import_value)

grouped_trade_value$agg_sector <- factor(grouped_trade_value$agg_sector,
                                         levels = c("biomass", "staple crops", "non-staple crops", "livestock"))

grouped_trade_value$scenario <- factor(grouped_trade_value$scenario,
                                       levels = scenario_levels)



  # GROUPED DOMESTIC VALUE --------------------------------------------------

crop_staples_domestic_value <- region_production_value %>%
  filter(sector %in% crop_staples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.domestic)) %>%
  mutate(agg_sector = "staple crops")

crop_nonstaples_domestic_value <- region_production_value %>%
  filter(sector %in% crop_nonstaples_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.domestic)) %>%
  mutate(agg_sector = "non-staple crops")

livestock_domestic_value <- region_production_value %>%
  filter(sector %in% ls_group) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.domestic)) %>%
  mutate(agg_sector = "livestock")

biomass_domestic_value <- region_production_value %>%
  filter(sector == "biomass") %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value.domestic)) %>%
  mutate(agg_sector = "biomass")

grouped_domestic_value <- bind_rows(crop_staples_domestic_value,
                                    crop_nonstaples_domestic_value,
                                    livestock_domestic_value,
                                    biomass_domestic_value)

  # TOTAL PRODUCTION AND NET TRADE (% FROM REF) -----------------------------
total_production_value <- grouped_production_value %>%
  filter(region %in% plot_regions) %>%
  group_by(region, scenario, year) %>%
  dplyr::summarise(value = sum(value))

total_net_trade_value <- grouped_net_trade_value %>%
  filter(region %in% plot_regions) %>%
  group_by(region, scenario, year) %>%
  dplyr::summarise(value = sum(value))

grouped_export_value <- bind_rows(crop_staples_export_value,
                                  crop_nonstaples_export_value,
                                  livestock_export_value,
                                  biomass_export_value)

grouped_import_value <- bind_rows(crop_staples_import_value,
                                  crop_nonstaples_import_value,
                                  livestock_import_value,
                                  biomass_import_value)

total_export_value <- grouped_export_value %>%
  filter(region %in% plot_regions) %>%
  group_by(region, scenario, year) %>%
  dplyr::summarise(value = sum(value))

total_import_value <- grouped_import_value %>%
  filter(region %in% plot_regions) %>%
  group_by(region, scenario, year) %>%
  dplyr::summarise(value = sum(value))



# change from 2050 ARM
total_production_value_2050 <- total_production_value %>%
  filter(year == 2050, scenario == "ARM_Reference")

total_net_trade_value_2050 <- total_net_trade_value %>%
  filter(year == 2050, scenario == "ARM_Reference")


total_export_value_2050 <- total_export_value %>%
  filter(year == 2050, scenario == "ARM_Reference")


total_import_value_2050 <- total_import_value %>%
  filter(year == 2050, scenario == "ARM_Reference")

# percent change
pct_production_change <- total_production_value %>%
  filter(year == 2050) %>%
  left_join(total_production_value_2050, by = "region", suffix = c(".scen", ".ARM")) %>%
  mutate(pct = ((value.scen - value.ARM) / value.ARM)*100) %>%
  na.omit() %>%
  mutate(label = "Production revenue")

pct_net_trade_change <- total_net_trade_value %>%
  filter(year == 2050) %>%
  left_join(total_net_trade_value_2050, by = "region", suffix = c(".scen", ".ARM")) %>%
  mutate(pct = ((value.scen - value.ARM) / value.ARM)*100) %>%
  na.omit()

pct_export_change <- total_export_value %>%
  filter(year == 2050) %>%
  left_join(total_export_value_2050, by = "region", suffix = c(".scen", ".ARM")) %>%
  mutate(pct = ((value.scen - value.ARM) / value.ARM)*100) %>%
  na.omit() %>%
  mutate(label = "Export value")

pct_import_change <- total_import_value %>%
  filter(year == 2050) %>%
  left_join(total_import_value_2050, by = "region", suffix = c(".scen", ".ARM")) %>%
  mutate(pct = ((value.scen - value.ARM) / value.ARM)*100) %>%
  na.omit() %>%
  mutate(label = "Import value")


pct_change_plot <- bind_rows(pct_production_change,
                             pct_export_change,
                             pct_import_change) 



  # GROUPED FOOD DEMAND ------------------------------------------------------
grouped_crop_staples <- food_cons %>%
  mutate(subsector = tolower(subsector)) %>%
  filter(subsector %in% crop_staples_group) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "crop staples")

grouped_crop_nonstaples <- food_cons %>%
  mutate(subsector = tolower(subsector)) %>%
  filter(subsector %in% crop_nonstaples_group) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "crop nonstaples")

grouped_ls_nonstaples <- food_cons %>%
  mutate(subsector = tolower(subsector)) %>%
  filter(subsector %in% ls_group) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "livestock")

grouped_food_cons <- bind_rows(grouped_crop_staples, 
                               grouped_crop_nonstaples,
                               grouped_ls_nonstaples)    

global_food_cons_crop_staples <- food_cons %>%
  filter(sector == "FoodDemand_Staples") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "crop staples")

global_food_cons_crop_nonstaples <- food_cons %>%
  mutate(technology = tolower(technology)) %>%
  filter(technology %in% crop_nonstaples_group) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "crop nonstaples")

global_food_cons_livestock <- food_cons %>%
  mutate(technology = tolower(technology)) %>%
  filter(technology %in% ls_group) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "meat")  

grouped_global_food_cons <- bind_rows(global_food_cons_crop_staples,
                                      global_food_cons_crop_nonstaples,
                                      global_food_cons_livestock)

grouped_global_food_cons$scenario <- factor(grouped_global_food_cons$scenario,
                                            levels = scenario_levels)
# GHG EMISSIONS CALCULATIONS ----------------------------------------------
GHG_res_prod <- nonCO2_res_prod %>%
  select(-resource) %>%
  dplyr::rename(sector = subresource)

GHG_sector <- nonCO2_sector

nonCO2 <- bind_rows(GHG_res_prod, GHG_sector)%>%
  filter(ghg != "CO2") %>%
  left_join(GWP, by = "ghg") %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit()

#LUC CO2, converted
CO2_LUC_region_total <- LUC_CO2 %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(value = value * 3.6667,
         sector = "LUC CO2")


# FFI CO2, converted
CO2_FFI_region_total <- bind_rows(GHG_res_prod,
                                  GHG_sector) %>%
  filter(ghg == "CO2") %>%
  left_join(GWP, by = "ghg") %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit() %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "FFI CO2")


# Total CO2, converted
CO2_region_total <- bind_rows(GHG_res_prod,
                              GHG_sector) %>%
  filter(ghg == "CO2") %>%
  left_join(GWP, by = "ghg") %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit() %>%
  bind_rows(LUC_CO2) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Total CO2")

CO2_global_total <- CO2_region_total %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))

#Non-CO2, converted
nonCO2_region_total <- bind_rows(GHG_res_prod,
                                 GHG_sector) %>%
  filter(ghg != "CO2") %>%
  left_join(GWP, by = "ghg") %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit() %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Non-CO2")

#TOTAL GHG, converted
GHG_region_total <- bind_rows(GHG_sector, GHG_res_prod) %>%
  left_join(GWP, by = "ghg") %>%
  mutate(Units = "MTCO2e", value = value * AR5all, SAR = NULL, AR5 = NULL,
         AR4 = NULL, SARall = NULL, AR5all = NULL, AR4all = NULL) %>%
  na.omit() %>%
  bind_rows(LUC_CO2) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#Non-CO2 grouped
nonCO2_FFI <- nonCO2 %>%
  filter(sector %in% c(pri_ene_trans_GHG_group,
                       ind_urb_GHG_group,
                       trn_GHG_group,
                       elec_GHG_group,
                       bld_GHG_group,
                       neg_biomass_GHG_group)) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "FFI Non-CO2")

nonCO2_livestock <- nonCO2 %>%
  filter(sector %in% livestock_GHG_group) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Livestock Non-CO2")

nonCO2_crop <- nonCO2 %>%
  filter(sector %in% crop_luc_GHG_group)  %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Crop Non-CO2") 


nonCO2_grouped <- nonCO2 %>%
  filter(sector %!in% c(pri_ene_trans_GHG_group,
                        ind_urb_GHG_group,
                        trn_GHG_group,
                        elec_GHG_group,
                        bld_GHG_group,
                        neg_biomass_GHG_group,
                        livestock_GHG_group,
                        crop_luc_GHG_group)) %>%
  group_by(scenario, sector, region, year) %>%
  dplyr::summarise(value = sum(value))  %>%
  bind_rows(nonCO2_FFI,
            nonCO2_livestock,
            nonCO2_crop)


grouped_GHG_sector <- bind_rows(CO2_LUC_region_total,
                                CO2_FFI_region_total,
                                nonCO2_grouped)

grouped_GHG_sector$sector <- factor(grouped_GHG_sector$sector, levels = c("FFI CO2", "FFI Non-CO2", "LUC CO2","Crop Non-CO2","Livestock Non-CO2"))

grouped_GHG_sector$region <- factor(grouped_GHG_sector$region, levels = c(export_LAC_regions, import_LAC_regions))

global_grouped_GHG_sector <- grouped_GHG_sector %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value = sum(value))

LAC_grouped_GHG_sector <- grouped_GHG_sector %>%
  filter(region %in% plot_regions) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "LAC")

ROW_grouped_GHG_sector <- grouped_GHG_sector %>%
  filter(region %!in% plot_regions) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "ROW")

LAC_ROW_grouped_GHG_sector <- bind_rows(LAC_grouped_GHG_sector,
                                        ROW_grouped_GHG_sector)

  # CUMULATIVE EMISSIONS ----------------------------------------------------


cum_grouped_GHG_sector <- grouped_GHG_sector %>%
  filter(year >= 2020) %>%
  group_by(scenario, region, sector) %>%
  mutate(value_5yr = value*5) %>%
  mutate(cum_value = cumsum(value_5yr))

cum_global_grouped_GHG_sector <- global_grouped_GHG_sector %>%
  filter(year >= 2020) %>%
  group_by(scenario, sector) %>%
  mutate(value_5yr = value*5) %>%
  mutate(cum_value = cumsum(value_5yr))

cum_IAMC_15C_total_CO2 <- IAMC_15C %>%
  filter(year >= 2020) %>%
  group_by(Model, Scenario) %>%
  mutate(value_5yr = value*5) %>%
  mutate(cum_value = cumsum(value_5yr)) %>%
  mutate(cum_value = if_else(year == 2020, 0, cum_value))

cum_IAMC_2C_total_CO2 <- IAMC_2C %>%
  filter(year >= 2020) %>%
  group_by(Model, Scenario) %>%
  mutate(value_5yr = value*5) %>%
  mutate(cum_value = cumsum(value_5yr)) %>%
  mutate(cum_value = if_else(year == 2020, 0, cum_value))

cum_global_nonCO2 <- cum_global_grouped_GHG_sector %>%
  filter(sector %in% c("FFI Non-CO2", "Crop Non-CO2", "Livestock Non-CO2")) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_value = sum(cum_value)) %>%
  mutate(cum_value = if_else(year == 2020, 0, cum_value))

cum_global_total_GHG <- cum_global_grouped_GHG_sector %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_value = sum(cum_value)) %>%
  mutate(value = if_else(year == 2020, 0, value))

cum_global_total_CO2 <- cum_global_grouped_GHG_sector %>%
  filter(sector %in% c("FFI CO2", "LUC CO2")) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(cum_value = sum(cum_value)) %>%
  mutate(cum_value = if_else(year == 2020, 0, cum_value)) %>%
  mutate(sector = "Total CO2")

  # GHG PRICES --------------------------------------------------------------

nonCO2_price <- GHG_price %>%
  filter(market == "ArgentinanonCO2", scenario == "ARM_Policy_10pa_2025") %>%
  mutate(value = value * (108.6 / 60.818) * (12/44) ,
         Units = "2020$/tCO2e")

# WER LEVEL CALC (FOR MAPS) --------------------------------------------------------
  # WER PRODUCTION VALUE --------------------------------------------------


#crop production by basin
crop_basin_production_value <- ag_prod_subsector %>%
  mutate(sector = tolower(sector)) %>%
  filter(sector %!in% c('pasture', 'forest', 'foddergrass', 'fodderherb')) %>%
  group_by(scenario, region, sector, subsector, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  left_join(region_producer_price, by = c("scenario", "region", "sector", "year"),
            suffix = c(".volume", ".price")) %>%
  mutate(value = value.volume * value.price * 10^9,
         Units = "2020$") %>%
  separate(subsector, c(NA, "basin")) %>%
  left_join(basin_to_country_mapping, by = c("basin" = "GLU_name")) %>%
  select(-value.volume, -value.price, -price, -basin, -GCAM_basin_ID, -Units) %>%
  filter(year >= 2015 & year <= 2050)

#livestock production by basin (estimate)
# calculate share of basin water consumption by animal type for 2015-2050 scenario data
basin_livestock_water <- an_water_W %>%
  left_join(basin_reg_livestock_share, by = c("region", "subsector" = "GLU_name")) %>%
  mutate(value = share*value) %>%
  select(scenario, region, subsector, Basin_long_name, GCAM_commodity, year, value)

#for each animal type, calculate share of region water by basin
# this has only a very negligible impact on data (could just use 2015 shares)
region_livestock_water <- basin_livestock_water %>%
  group_by(scenario, region, GCAM_commodity, year) %>%
  dplyr::summarise(value = sum(value))

basin_livestock_water_share <- basin_livestock_water %>%
  left_join(region_livestock_water, by = c("scenario", "region", "GCAM_commodity", "year"), suffix = c(".basin", ".total")) %>%
  mutate(share = value.basin/value.total) %>%
  select(-value.basin, -value.total)

# calculate share of livestock value by basin in 2015-2050 scenario data

livestock_basin_production_value <-   region_production_value %>%
  select(scenario, region, sector, year, value.production) %>%
  filter(sector %in% c("beef", "dairy", "pork", "poultry", "sheepgoat")) %>%
  left_join(basin_livestock_water_share, by = c("scenario", "region", "year", "sector" = "GCAM_commodity")) %>%
  mutate(value = value.production *share) %>%
  select(scenario, region, sector, Basin_long_name, year, value)

#Crop and livestock production value at the basin scale
crop_livestock_basin_production_value <-  bind_rows(crop_basin_production_value,
                                                    livestock_basin_production_value) %>%
  arrange(scenario, region, Basin_long_name, year)

total_basin_production_value <- crop_livestock_basin_production_value %>%
  group_by(scenario, region, Basin_long_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

# create sub-region, remove basins that don't actually exist in the data
total_basin_production_value_clean <- total_basin_production_value %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern"))


  # REGION CONSUMER EXPENDITURE --------------------------------

#region population
region_pop <- POP_GCAM %>%
  pivot_longer(as.character(GCAM_YEARS), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year))

#get per capita consumer expenditure (total)
region_consumer_expend_pc <- region_pop %>%
  filter(year >= 2015 & year <= 2050) %>%
  left_join(region_total_consumer_expend, by = c("region", "year"), 
            suffix = c(".pop", ".cons_exp")) %>%
  mutate(value.cons_exp_pc = value.cons_exp / (value.pop*10^3)) %>%
  select(-value.pop, -value.cons_exp)

#per capita cons. expend by grouped commodity
biomass_consumer_expend <- region_consumer_expend %>%
  filter(sector == "biomass") %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(consumer_expend)) %>%
  mutate(agg_sector = "biomass")

crop_staples_consumer_expend <- region_consumer_expend %>%
  filter(sector %in% crop_staples_group) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(consumer_expend)) %>%
  mutate(agg_sector = "staple crops")

crop_nonstaples_consumer_expend <- region_consumer_expend %>%
  filter(sector %in% crop_nonstaples_group) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(consumer_expend)) %>%
  mutate(agg_sector = "non-staple crops")

livestock_consumer_expend <- region_consumer_expend %>%
  filter(sector %in% ls_group) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(consumer_expend)) %>%
  mutate(agg_sector = "livestock")

grouped_consumer_expend <- bind_rows(biomass_consumer_expend,
                                     crop_staples_consumer_expend,
                                     crop_nonstaples_consumer_expend,
                                     livestock_consumer_expend)

grouped_consumer_expend_pc <- grouped_consumer_expend %>%
  left_join(region_pop, by = c("region", "year"), 
            suffix = c(".cons_exp", ".pop")) %>%
  mutate(value.cons_exp_pc = value.cons_exp / (value.pop*10^3)) %>%
  select(-value.pop, -value.cons_exp)

  # WER LAND ALLOCATION ---------------------------------------------------

AgHA_to_CL_clean <- AgHA_to_CL %>%
  filter(year <= 2050, region %in% plot_regions) %>%
  separate(AgProductionTechnology, c("landleaf", "GLU_name", "irr_rfd", "lo_hi"), sep = "_") %>%
  left_join(basin_to_country_mapping) %>%
  select(-AgSupplySector, -AgSupplySubsector)

land_alloc_basin_clean <- land_alloc_basin %>%
  filter(year <= 2050) %>%
  separate(landleaf, c("landleaf", "GLU_name", "irr_rfd", "lo_hi"), sep = "_") %>%
  left_join(basin_to_country_mapping) %>%
  left_join(AgHA_to_CL_clean, by = c("region", "landleaf", "GLU_name", "irr_rfd", "lo_hi", "year", "GCAM_basin_ID", "Basin_long_name")) %>%
  # if there is no harvest area, set multiplier to 1 so that there is no change in area calculated
  mutate(harvests.per.year = if_else(is.na(harvests.per.year), 1, harvests.per.year)) %>%
  mutate(value = value * harvests.per.year)

#add up all land use types
total_land_alloc_basin <- land_alloc_basin_clean %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#crop and biomass
bio_alloc_basin <- land_alloc_basin_clean %>%
  filter(landleaf %in% c("biomassGrass", "biomassTree")) %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(landleaf = "Biomass")

fodder_alloc_basin <- land_alloc_basin_clean %>%
  filter(landleaf %in% c("fodderGrass", "fodderHerb")) %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(landleaf = "Fodder")

crop_bio_alloc_basin <- land_alloc_basin_clean %>%
  filter(landleaf %in% c(crop_land_group)) %>%
  bind_rows(bio_alloc_basin) %>%
  bind_rows(fodder_alloc_basin) %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

crop_bio_share <- total_land_alloc_basin %>%
  left_join(crop_bio_alloc_basin, by = c("scenario", "region", "GLU_name", "year", "Basin_long_name"), suffix = c(".total", ".crop")) %>%
  mutate(value.crop = if_else(is.na(value.crop), 0, value.crop)) %>%
  mutate(value = value.crop / value.total)


#pasture
pasture_alloc_basin <- land_alloc_basin_clean %>%
  filter(landleaf %in% c("Pasture", "ProtectedUnmanagedPasture", "UnmanagedPasture")) %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()


pasture_share <- total_land_alloc_basin %>%
  left_join(pasture_alloc_basin, by = c("scenario", "region", "GLU_name", "year", "Basin_long_name"), suffix = c(".total", ".pasture")) %>%
  mutate(value.pasture = if_else(is.na(value.pasture), 0, value.pasture)) %>%
  mutate(value = value.pasture / value.total)

#otherarable
otherarable_alloc_basin <- land_alloc_basin_clean %>%
  filter(landleaf %in% c("OtherArableLand")) %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()


#forest
forest_alloc_basin <- land_alloc_basin_clean %>%
  filter(landleaf %in% c("Forest", "ProtectedUnmanagedForest", "UnmanagedForest")) %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()


forest_share <- total_land_alloc_basin %>%
  left_join(forest_alloc_basin, by = c("scenario", "region", "GLU_name", "year", "Basin_long_name"), suffix = c(".total", ".forest")) %>%
  mutate(value.forest = if_else(is.na(value.forest), 0, value.forest)) %>%
  mutate(value = value.forest / value.total)

# all ag land
ag_land_alloc_basin <- bind_rows(otherarable_alloc_basin,
                                 pasture_alloc_basin,
                                 crop_bio_alloc_basin) %>%
  group_by(scenario, region, GLU_name, year, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

ag_land_share <- total_land_alloc_basin %>%
  left_join(ag_land_alloc_basin, by = c("scenario", "region", "GLU_name", "year", "Basin_long_name"), suffix = c(".total", ".ag")) %>%
  mutate(value.ag = if_else(is.na(value.ag), 0, value.ag)) %>%
  mutate(value = value.ag / value.total)

  # WER WATER WITHDRAWALS -------------------------------------------------
#livestock water withdrawals
ls_water_wd_basin <- an_water_W %>%
  mutate(GLU_name = subsector) %>%
  select(Units, scenario, region, GLU_name, year, value) %>%
  left_join(basin_to_country_mapping) %>%
  ungroup()

# irrigation water withdrawals
irr_water_wd_basin <- water_wd_basin %>%
  filter(grepl("water_td_irr", input)) %>%
  separate(input, c(NA, "GLU_name"), sep = "water_td_irr_") %>%
  separate(GLU_name, c("GLU_name", NA), sep = "_W") %>%
  left_join(basin_to_country_mapping) %>%
  ungroup()

ls_irr_water_wd_basin <- bind_rows(ls_water_wd_basin,
                                   irr_water_wd_basin) %>%
  group_by(Units, scenario, region, GLU_name, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(basin_to_country_mapping) %>%
  ungroup()

  # BASIN WATER RUNOFF AND SCARCITY ------------------------------------------------------


water_runoff_basin <- basin_runoff %>%
  separate(Basin, into = c("GLU_name", NA)) %>%
  left_join(basin_to_country_mapping) %>%
  filter((region %in% plot_regions | (region == "USA" & GLU_name %in% c("California", "UsaColoRS", "RioGrande")))) %>%
  select(-region, -scenario, -subresource, -Units)

#scarcity = demand/supply
# aggregated to the basin (not WER level)
water_scarcity <- ls_irr_water_wd_basin %>%
  filter((region %in% plot_regions | (region == "USA" & GLU_name %in% c("California", "Lower Colorado", "Rio Grande")))) %>%
  group_by(scenario, GLU_name, year, GCAM_basin_ID, Basin_long_name) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(water_runoff_basin, by = c("GLU_name", "year", "GCAM_basin_ID", "Basin_long_name"),
            suffix = c(".demand", ".supply")) %>%
  mutate(scarcity = value.demand / value.supply) 

water_scarcity_2015_2050 <- water_scarcity %>%
  filter(year %in% c(2015, 2050)) %>%
  mutate(subRegion = Basin_long_name, value = scarcity) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  ungroup() %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value) %>%
  mutate(subRegion = gsub("Arkansas_White_Red_Basin", "Arkansas_White_Red", subRegion),
         subRegion = gsub("Brahmani", "Brahamani", subRegion),
         subRegion = gsub("California_River_Basin", "California_River", subRegion),
         subRegion = gsub("Great_Basin", "Great", subRegion),
         subRegion = gsub("Great_Lakes_Basin", "Great_Lakes", subRegion),
         subRegion = gsub("HamuniMashkel", "Hamun_i_Mashkel", subRegion),
         subRegion = gsub("Lower_Colorado_River_Basin", "Lower_Colorado_River", subRegion),
         subRegion = gsub("Lower_Mississippi_River_Basin", "Lower_Mississippi_River", subRegion),
         subRegion = gsub("Mahanadi", "Mahandi", subRegion),
         subRegion = gsub("Mid_Atlantic_Basin", "Mid_Atlantic", subRegion),
         subRegion = gsub("Missouri_River_Basin", "Missouri_River", subRegion),
         subRegion = gsub("New_England_Basin", "New_England", subRegion),
         subRegion = gsub("Ohio_River_Basin", "Ohio_River", subRegion),
         subRegion = gsub("Pacific_Northwest_Basin", "Pacific_Northwest", subRegion),
         subRegion = gsub("Rio_Grande_River_Basin", "Rio_Grande_River", subRegion),
         subRegion = gsub("Sittaung", "Sittang", subRegion),
         subRegion = gsub("South_Atlantic_Gulf_Basin", "South_Atlantic_Gulf", subRegion),
         subRegion = gsub("Tennessee_River_Basin", "Tennessee_River", subRegion),
         subRegion = gsub("Texas_Gulf_Coast_Basin", "Texas_Gulf_Coast", subRegion),
         subRegion = gsub("Upper_Colorado_River_Basin", "Upper_Colorado_River", subRegion),
         subRegion = gsub("Upper_Mississippi_Basin", "Upper_Mississippi", subRegion),
         subRegion = gsub("Yenisei", "Yenisey", subRegion)) %>%
  mutate(subRegion = if_else((subRegion == "Hong_(Red_River)"), "Hong_Red_River", subRegion)) %>%
  mutate(subRegion = if_else((subRegion == "Republic of the Congo"), "Congo", subRegion)) %>%
  filter(scenario %in% c("ARM_Reference.2015","ARM_Reference.2050", "SW_high_CL_Reference.2050", "ARM_Policy_10pa_2025.2050", "SW_high_CL_Policy_10pa_2025.2050"))



# MAIN PAPER FIGURES ------------------------------------------------------

  # FIG 2A ------------------------------------------------------------------

LAC_ROW_grouped_production_value <- bind_rows(LAC_grouped_production_value,
                                              ROW_grouped_production_value)

LAC_ROW_grouped_production_value$agg_sector <- factor(LAC_ROW_grouped_production_value$agg_sector,
                                                      levels = c("biomass", "staple crops", "non-staple crops", "livestock"))

LAC_ROW_grouped_production_value$scenario <- factor(LAC_ROW_grouped_production_value$scenario,
                                                    levels = scenario_levels)

plot <- LAC_ROW_grouped_production_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025")) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 1, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural production revenue (domestic + exports) in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"2A_LAC ROW grouped prod value.pdf"), height = 2.5, width = 5.5)


  # FIG 2B ------------------------------------------------------------------
LAC_ROW_grouped_net_trade_value  <- bind_rows(LAC_grouped_net_trade_value,
                                              ROW_grouped_net_trade_value)

LAC_ROW_grouped_net_trade_value$agg_sector <- factor(LAC_ROW_grouped_net_trade_value$agg_sector,
                                                     levels = c("biomass", "staple crops", "non-staple crops", "livestock"))

LAC_ROW_grouped_net_trade_value$scenario <- factor(LAC_ROW_grouped_net_trade_value$scenario,
                                                   levels = scenario_levels)

LAC_ROW_total_net_trade_value <- LAC_ROW_grouped_net_trade_value %>%
  group_by(scenario, year, Units, region) %>%
  dplyr::summarise(value = sum(value))

LAC_ROW_net_trade_value_plot <- LAC_ROW_grouped_net_trade_value %>%
  left_join(LAC_ROW_total_net_trade_value, by = c("scenario", "year", "region", "Units"), suffix = c(".grouped", ".total"))


plot <- LAC_ROW_net_trade_value_plot %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025")) %>%
  ggplot(aes(x = interaction(scenario, year), y = value.grouped/(10^9), fill = agg_sector)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_errorbar(aes(x = interaction(scenario, year), ymin = value.total/(10^9), ymax = value.total/(10^9)), lty = "32", width = 1, size = 0.4)+
  facet_wrap(.~region, nrow = 1, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural net trade revenue (exports - imports) in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"2B_LAC ROW grouped net trade value.pdf"), height = 2.5, width = 5.5)


  # FIG 2C ------------------------------------------------------------------
LAC_ROW_total_GHG <- LAC_ROW_grouped_GHG_sector %>%
  group_by(scenario, year, region) %>%
  dplyr::summarise(value = sum(value))

LAC_ROW_GHG_sector_plot <- LAC_ROW_grouped_GHG_sector %>%
  left_join(LAC_ROW_total_GHG, by = c("scenario", "year", "region"), suffix = c(".grouped", ".total"))

plot <-  LAC_ROW_GHG_sector_plot  %>%
  filter(year == 2050 | (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025")) %>%
  ggplot(aes(x = interaction(scenario, year), y = value.grouped/1000, fill = sector)) +
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity", size = 1)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_errorbar(aes(x = interaction(scenario, year), ymin = value.total/1000, ymax = value.total/1000), lty = "32", width = 1, size = 0.4)+
  facet_wrap(.~region, nrow = 1, scales = "free")+
  scale_fill_manual(values = pal_GHG)+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank2" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank2" = "",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  labs(title = "GHG emissions by type in 2050",  x = "Scenario", y = "GtCO2e") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"2C_LAC ROW GHG.pdf"), height = 2.5, width = 5.5)
  # FIG 3A ------------------------------------------------------------------

grouped_production_value$agg_sector <- factor(grouped_production_value$agg_sector,
                                              levels = c("biomass", "staple crops", "non-staple crops", "livestock"))

grouped_production_value$scenario <- factor(grouped_production_value$scenario,
                                            levels = scenario_levels)

grouped_production_value$region <- factor(grouped_production_value$region, levels = c(export_LAC_regions, import_LAC_regions))


plot <- grouped_production_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025"), region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 2, scales = "free", labeller = as_labeller(region_facet_labels))+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural production revenue (domestic + exports) in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.spacing.x = unit(2, "lines"))+
  ggsave(paste0(PLOT_FOLDER,"3A_LAC grouped prod value.pdf"), height = 5.5, width = 10.5)



  # FIG 3B ------------------------------------------------------------------

total_net_trade_value <- grouped_net_trade_value %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value))

grouped_net_trade_value_plot <- grouped_net_trade_value %>%
  left_join(total_net_trade_value, by = c("scenario", "region", "year", "Units"), suffix = c(".grouped", ".total"))

grouped_net_trade_value_plot$agg_sector <- factor(grouped_net_trade_value_plot$agg_sector,
                                                  levels = c("biomass", "staple crops", "non-staple crops", "livestock"))

grouped_net_trade_value_plot$scenario <- factor(grouped_net_trade_value_plot$scenario,
                                                levels = scenario_levels)


grouped_net_trade_value_plot$region <- factor(grouped_net_trade_value_plot$region, levels = c(export_LAC_regions, import_LAC_regions))

plot <- grouped_net_trade_value_plot %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025"), region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value.grouped/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(aes(x = interaction(scenario, year), ymin = value.total/(10^9), ymax = value.total/(10^9)), lty = "32", width = 1, size = 0.4)+
  facet_wrap(.~region, nrow = 2, scales = "free", labeller = as_labeller(region_facet_labels))+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural net trade revenue (exports - imports) in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.spacing.x = unit(2, "lines"))+
  ggsave(paste0(PLOT_FOLDER,"3B_LAC grouped net trade value.pdf"), height = 5.5, width = 10.5)


  # FIG 4 -------------------------------------------------------------------
pct_change_plot$region <- factor(pct_change_plot$region, levels = c(export_LAC_regions, import_LAC_regions))

pct_change_plot$label <- factor(pct_change_plot$label, levels = c("Import value", "Export value", "Production revenue"))

plot <- ggplot() +
  geom_point(data = filter(pct_change_plot, scenario.scen %in% c("SW_high_CL_Reference", 
                                                                 "ARM_Policy_10pa_2025",
                                                                 "SW_high_CL_Policy_10pa_2025")),
             aes(x = region, y = pct, color = scenario.scen))+
  geom_hline(yintercept = 0, color = "black", size = 1) +
  facet_wrap(.~label, nrow = 1, scales = "free")+
  scale_x_discrete(labels = region_short_labels ) +
  scale_color_manual(name = "Scenario",
                     values = pal_3,
                     labels = c("SW_high_CL_Reference" = "MI",
                                "ARM_Policy_10pa_2025" = "CM",
                                "SW_high_CL_Policy_10pa_2025" = "MI + CM"))+
  labs(title = "% change from 2050 REF", x = "Region", y = "%") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right",
        panel.spacing.x = unit(2, "lines"))+
  ggsave(paste0(PLOT_FOLDER,"4_pct change.pdf"), height = 3, width = 10)

  # FIG 5A ------------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig 5a, we used the DiffPrcnt maps.

plot_basin_production_value_2050 <- total_basin_production_value_clean %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(param = "param1") %>%
  select(scenario, subRegion, param, value)


plot_map <- rmap::map(plot_basin_production_value_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/5A", sep = ""),
                      title = paste("Basin agricultural production value in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_LAC_production_MI_CM_", "2050", sep = ""),
                      pdfpng = "pdf")

plot_basin_production_value <- total_basin_production_value_clean %>%
  filter(year %in% c(2015, 2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(param = "param1") %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, param, value)

plot_map <- rmap::map(plot_basin_production_value,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/5A", sep = ""),
                      nameAppend = paste("_LAC_production_2015_2050", sep = ""),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"),
                      pdfpng = "pdf")


  # FIG 5B ------------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig 5b, we used the DiffPrcnt maps.

plot_region_consumer_2050 <- region_consumer_expend_pc %>%
  filter(region %in% plot_regions, year == 2050) %>%
  dplyr::rename(value = value.cons_exp_pc,
                subRegion = region) %>%
  mutate(param = "param1") %>%
  select(scenario, subRegion, param, value)

plot_map <- rmap::map(plot_region_consumer_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/5B", sep = ""),
                      title = paste("Region per capita consumer expenditure"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", 
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_LAC_reg_consumer_MI_CM_", "2050", sep = ""),
                      pdfpng = "pdf")


plot_region_consumer <- region_consumer_expend_pc %>%
  filter(region %in% plot_regions, year %in% c(2015, 2050), scenario == "ARM_Reference") %>%
  dplyr::rename(value = value.cons_exp_pc,
                subRegion = region) %>%
  mutate(param = "param1") %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, param, value)



plot_map <- rmap::map(plot_region_consumer,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/5B", sep = ""),
                      title = paste("Region per capita consumer expenditure"),
                      nameAppend = paste("_LAC_reg_consumer_2015_2050", sep = ""),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = "ARM_Reference.2050",
                      pdfpng = "pdf")



  # FIG 6A ------------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig 6a, we used the DiffAbs maps.

crop_bio_share_2050 <- crop_bio_share %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern")) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(crop_bio_share_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/6A", sep = ""),
                      title = paste("Basin crop/biomass land in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", 
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_crop_bio_share_MI_CM_", "2050", sep = ""),
                      pdfpng = 'pdf')

crop_bio_share_2015_2050 <- crop_bio_share %>%
  filter(year %in% c(2015, 2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(crop_bio_share_2015_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/6A", sep = ""),
                      title = paste("Basin crop/biomass land"),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"),
                      nameAppend = paste("_crop_bio_alloc_basin_policy_2015_2050", sep = ""),
                      pdfpng = 'pdf')

  # FIG 6B ------------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig 6b, we used the DiffAbs maps.

pasture_share_2050 <- pasture_share %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern")) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(pasture_share_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/6B", sep = ""),
                      title = paste("Basin pasture land in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", 
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_pasture_share_MI_CM_", "2050", sep = ""),
                      pdfpng= 'pdf')

pasture_share_2015_2050 <- pasture_share %>%
  filter( year %in% c(2015,2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(pasture_share_2015_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/6B", sep = ""),
                      title = paste("Basin pasture land"),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"),
                      nameAppend = paste("_pasture_alloc_basin_policy_2015_2050", sep = ""),
                      pdfpng = 'pdf')

  # FIG 6C ------------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig 6c, we used the DiffAbs maps.

ls_irr_water_wd_basin_2050 <- ls_irr_water_wd_basin %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(ls_irr_water_wd_basin_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/6C", sep = ""),
                      title = paste("Basin livestock + irrigation water withdrawal in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", 
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_water_wd_basin_MI_CM_", "2050", sep = ""),
                      pdfpng = 'pdf')

ls_irr_water_wd_2015_2050 <- ls_irr_water_wd_basin %>%
  filter(year %in% c(2015, 2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(ls_irr_water_wd_2015_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/6C", sep = ""),
                      title = paste("Basin livestock + irrigation water withdrawal"),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"),
                      nameAppend = paste("_water_wd_basin_2015_2050", sep = ""),
                      pdfpng = "pdf")



# SI FIGURES --------------------------------------------------------------
  # FIG S1 ---------------------------------------------------------------

plot <-  ggplot()+
  geom_line(data = filter(cum_global_total_CO2, scenario %in% c("ARM_Policy_10pa_2025"), year <= 2050),
            aes(x = year, y = cum_value/1000), color = "black", size = 1) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  geom_line(data = filter(cum_IAMC_15C_total_CO2, year <= 2050),
            aes(x = year, y = cum_value/1000, group = interaction(Model, Scenario)), color = "darkorchid3", alpha = 0.1, size = 0.5) +
  geom_line(data = filter(cum_IAMC_2C_total_CO2, year <= 2050),
            aes(x = year, y = cum_value/1000, group = interaction(Model, Scenario)), color = "darkturquoise", alpha = 0.1, size = 0.5) +
  labs(title = "Cumulative global CO2 emissions",  x = "", y = "GtCO2e") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  scale_color_discrete(labels = c("ARM_Policy_10pa_2025" = "CM"))+
  scale_y_continuous(limits = c(0,NA))+
  ggsave(paste0(PLOT_FOLDER,"S1_global_cum_CO2_IAMC.png"), height = 5, width = 5.5)

plot <-  ggplot()+
  geom_line(data = filter(CO2_global_total, scenario %in% c("ARM_Policy_10pa_2025"), year >=2015,  year <= 2050),
            aes(x = year, y = value/1000), color = "black", size = 1) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  geom_line(data = filter(IAMC_15C, year >= 2015, year <= 2050),
            aes(x = year, y = value/1000, group = interaction(Model, Scenario)), color = "darkorchid3", alpha = 0.1, size = 0.5) +
  geom_line(data = filter(IAMC_2C, year >= 2015, year <= 2050),
            aes(x = year, y = value/1000, group = interaction(Model, Scenario)), color = "darkturquoise", alpha = 0.1, size = 0.5) +
  labs(title = "Global CO2 emissions",  x = "", y = "GtCO2e") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  scale_color_discrete(labels = c("ARM_Policy_10pa_2025" = "CM"))+
  scale_y_continuous(limits = c(NA,NA))+
  ggsave(paste0(PLOT_FOLDER,"S1_global_CO2_IAMC.png"), height = 5, width = 5.5)





  # FIG S2 ------------------------------------------------------------------

plot <- nonCO2_price %>%
  filter(year >= 2020 & year <= 2050)  %>%
  ggplot(aes(x = year, y = value  , color = "blue")) +
  geom_line(size = 0.6) +
  labs(title = paste0("Non-CO2 price"), x = "Year", y = "2020$/tCO2e") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  ggsave(paste0(PLOT_FOLDER, "S2_GHG price.png"), height = 3, width = 4)


  # FIG S3 ------------------------------------------------------------------


water_scarcity_2015_2050 <- water_scarcity %>%
  filter(year %in% c(2015, 2050)) %>%
  mutate(subRegion = Basin_long_name, value = scarcity) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  ungroup() %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value) %>%
  mutate(subRegion = gsub("Arkansas_White_Red_Basin", "Arkansas_White_Red", subRegion),
         subRegion = gsub("Brahmani", "Brahamani", subRegion),
         subRegion = gsub("California_River_Basin", "California_River", subRegion),
         subRegion = gsub("Great_Basin", "Great", subRegion),
         subRegion = gsub("Great_Lakes_Basin", "Great_Lakes", subRegion),
         subRegion = gsub("HamuniMashkel", "Hamun_i_Mashkel", subRegion),
         subRegion = gsub("Lower_Colorado_River_Basin", "Lower_Colorado_River", subRegion),
         subRegion = gsub("Lower_Mississippi_River_Basin", "Lower_Mississippi_River", subRegion),
         subRegion = gsub("Mahanadi", "Mahandi", subRegion),
         subRegion = gsub("Mid_Atlantic_Basin", "Mid_Atlantic", subRegion),
         subRegion = gsub("Missouri_River_Basin", "Missouri_River", subRegion),
         subRegion = gsub("New_England_Basin", "New_England", subRegion),
         subRegion = gsub("Ohio_River_Basin", "Ohio_River", subRegion),
         subRegion = gsub("Pacific_Northwest_Basin", "Pacific_Northwest", subRegion),
         subRegion = gsub("Rio_Grande_River_Basin", "Rio_Grande_River", subRegion),
         subRegion = gsub("Sittaung", "Sittang", subRegion),
         subRegion = gsub("South_Atlantic_Gulf_Basin", "South_Atlantic_Gulf", subRegion),
         subRegion = gsub("Tennessee_River_Basin", "Tennessee_River", subRegion),
         subRegion = gsub("Texas_Gulf_Coast_Basin", "Texas_Gulf_Coast", subRegion),
         subRegion = gsub("Upper_Colorado_River_Basin", "Upper_Colorado_River", subRegion),
         subRegion = gsub("Upper_Mississippi_Basin", "Upper_Mississippi", subRegion),
         subRegion = gsub("Yenisei", "Yenisey", subRegion)) %>%
  mutate(subRegion = if_else((subRegion == "Hong_(Red_River)"), "Hong_Red_River", subRegion)) %>%
  mutate(subRegion = if_else((subRegion == "Republic of the Congo"), "Congo", subRegion)) %>%
  filter(scenario %in% c("ARM_Reference.2015","ARM_Reference.2050", "SW_high_CL_Reference.2050", "ARM_Policy_10pa_2025.2050", "SW_high_CL_Policy_10pa_2025.2050"))

water_scarcity_2015_2050_plot <- water_scarcity_2015_2050 %>%
  filter(scenario %in% c("ARM_Reference.2015","ARM_Reference.2050", "SW_high_CL_Reference.2050", "ARM_Policy_10pa_2025.2050", "SW_high_CL_Policy_10pa_2025.2050"))

numeric2Cat_param <- list("param")
numeric2Cat_breaks <- list(c(0,0.1,0.2,0.4,Inf))
numeric2Cat_labels <- list(c("None (0 to 0.1)","Low (0.1 to 0.2)","Moderate (0.2 to 0.4)","Severe (>0.4)"))
numeric2Cat_palette <- list(c("None (0 to 0.1)"="#3288BD","Low (0.1 to 0.2)"="#ABDDA4","Moderate (0.2 to 0.4)"="#FDAE61","Severe (>0.4)"="#9E0142"))
numeric2Cat_legendTextSize <- list(c(1))
numeric2Cat_list <-list(numeric2Cat_param=numeric2Cat_param,
                        numeric2Cat_breaks=numeric2Cat_breaks,
                        numeric2Cat_labels=numeric2Cat_labels,
                        numeric2Cat_palette=numeric2Cat_palette,
                        numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)

plot_map <- rmap::map(water_scarcity_2015_2050_plot,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S3", sep = ""),
                      nameAppend = paste("_wd_runoff_scarcity_", "2015_2050", sep = ""),
                      numeric2Cat_list = numeric2Cat_list,
                      pdfpng = 'pdf')

  # FIG S4 ------------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig S4, we used the KMEANS maps.
ag_land_share_2050 <- ag_land_share %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern")) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(ag_land_share_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S4", sep = ""),
                      title = paste("Basin agricultural land in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", 
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_ag_land_share_2015_2050", sep = ""),
                      pdfpng = 'pdf')

  # FIG S5A ------------------------------------------------------------------

plot <- LAC_ROW_grouped_production_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025")) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 1, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_CO2_10pa_2025.2050" = "CO2",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050" = "MI + CO2",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050" = "CM (diff. MAC)",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050" = "MI + CM (diff. MAC)"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_CO2_10pa_2025.2050",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = c(pal_grouped_commodities))+
  labs(title = "Agricultural production revenue (domestic + exports) in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"S5A_LAC ROW grouped prod value_SI.png"), height = 4, width = 12)



  # FIG S5B -----------------------------------------------------------------

plot <- LAC_ROW_grouped_net_trade_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025")) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 1, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_CO2_10pa_2025.2050" = "CO2",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050" = "MI + CO2",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050" = "CM (diff. MAC)",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050" = "MI + CM (diff. MAC)"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_CO2_10pa_2025.2050",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural net trade revenue",  x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"S5B_LAC ROW grouped net trade value_SI.png"), height = 4, width = 12)


  # FIG S6A -----------------------------------------------------------------

plot <- grouped_production_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025"),
         region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 2, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_CO2_10pa_2025.2050" = "CO2",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050" = "MI + CO2",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050" = "CM (diff. MAC)",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050" = "MI + CM (diff. MAC)"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_CO2_10pa_2025.2050",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural production revenue (domestic + exports) in 2050",  x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")+
  ggsave(paste0(PLOT_FOLDER,"S6A_LAC grouped prod value_SI.png"), height = 8, width = 15)

  # FIG S6B -----------------------------------------------------------------
plot <- grouped_net_trade_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025"),
         region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 2, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_CO2_10pa_2025.2050" = "CO2",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050" = "MI + CO2",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050" = "CM (diff. MAC)",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050" = "MI + CM (diff. MAC)"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_CO2_10pa_2025.2050",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural net trade revenue in 2050",  x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")+
  ggsave(paste0(PLOT_FOLDER,"S6B_LAC grouped net trade value_SI.png"), height = 8, width = 15)


  # FIG S7A ------------------------------------------------------------------

region_production_value$scenario <- factor(region_production_value$scenario, levels = scenario_levels)

region_production_value$sector <- factor(region_production_value$sector, levels = c(ls_group, crop_staples_group, crop_nonstaples_group, "biomass"))

region_production_value$region <- factor(region_production_value$region, levels = c(export_LAC_regions, import_LAC_regions))

plot <- region_production_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025"), region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value.production, fill = sector)) +
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity", size = 1)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(.~region, nrow = 2, scales = "free")+
  scale_fill_manual(values = pal_ag)+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_CO2_10pa_2025.2050" = "CO2",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050" = "MI + CO2",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050" = "CM (diff. MAC)",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050" = "MI + CM (diff. MAC)"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_CO2_10pa_2025.2050",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050") ) +
  labs(title = "Agricultural production revenue by commodity",  x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"S7A_production value by commodity_SI.png"), height = 8, width = 15)

  # FIG S7B ------------------------------------

region_net_trade_value$scenario <- factor(region_net_trade_value$scenario, levels = scenario_levels)

region_net_trade_value$sector <- factor(region_net_trade_value$sector, levels = c(ls_group, crop_staples_group, crop_nonstaples_group, "biomass"))

region_net_trade_value$region <- factor(region_net_trade_value$region, levels = c(export_LAC_regions, import_LAC_regions))


plot <- region_net_trade_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025"), region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value.net, fill = sector)) +
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity", size = 1)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(.~region, nrow = 2, scales = "free")+
  scale_fill_manual(values = pal_ag)+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_CO2_10pa_2025.2050" = "CO2",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050" = "MI + CO2",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050" = "CM (diff. MAC)",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050" = "MI + CM (diff. MAC)"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_CO2_10pa_2025.2050",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050") ) +
  labs(title = "Agricultural net trade revenue by commodity",  x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"S7B_net trade value by commodity_SI.png"), height = 8, width = 15)



  # FIG S8 ------------------------------------------------------------------

grouped_global_food_cons %>%
  filter(year >= 2015 & year <= 2050) %>%
  ggplot(aes(x = year, y = value, color = scenario, linetype = scenario)) +
  geom_line(size = 0.6) +
  facet_wrap(.~sector, nrow = 1, scales = "free")+
  scale_x_continuous() +
  scale_linetype_manual(name = "Scenario", values = c("ARM_Reference" = 1,
                                                      "SW_high_CL_Reference" = 2,
                                                      "ARM_Policy_CO2_10pa_2025" = 1 ,
                                                      "SW_high_CL_Policy_CO2_10pa_2025" = 2,
                                                      "ARM_Policy_10pa_2025" = 1,
                                                      "SW_high_CL_Policy_10pa_2025" = 2,
                                                      "ARM_Policy_diff_MAC_10pa_2025" = 1,
                                                      "SW_high_CL_Policy_diff_MAC_10pa_2025" = 2),
                        labels = c("ARM_Reference" = "REF",
                                   "SW_high_CL_Reference" = "MI",
                                   "ARM_Policy_CO2_10pa_2025" = "CO2" ,
                                   "SW_high_CL_Policy_CO2_10pa_2025" = "MI + CO2",
                                   "ARM_Policy_10pa_2025" = "CM",
                                   "SW_high_CL_Policy_10pa_2025" = "MI + CM",
                                   "ARM_Policy_diff_MAC_10pa_2025" = "CM (diff. MAC)",
                                   "SW_high_CL_Policy_diff_MAC_10pa_2025" = "MI + CM (diff. MAC)"))+
  scale_color_manual(name = "Scenario", values = c("ARM_Reference" = "#E69F00",
                                                   "SW_high_CL_Reference" = "#E69F00",
                                                   "ARM_Policy_CO2_10pa_2025" = "#56B4E9" ,
                                                   "SW_high_CL_Policy_CO2_10pa_2025" = "#56B4E9",
                                                   "ARM_Policy_10pa_2025" = "#009E73",
                                                   "SW_high_CL_Policy_10pa_2025" = "#009E73",
                                                   "ARM_Policy_diff_MAC_10pa_2025" = "#0072B2",
                                                   "SW_high_CL_Policy_diff_MAC_10pa_2025" = "#0072B2"),
                     labels = c("ARM_Reference" = "REF",
                                "SW_high_CL_Reference" = "MI",
                                "ARM_Policy_CO2_10pa_2025" = "CO2" ,
                                "SW_high_CL_Policy_CO2_10pa_2025" = "MI + CO2",
                                "ARM_Policy_10pa_2025" = "CM",
                                "SW_high_CL_Policy_10pa_2025" = "MI + CM",
                                "ARM_Policy_diff_MAC_10pa_2025" = "CM (diff. MAC)",
                                "SW_high_CL_Policy_diff_MAC_10pa_2025" = "MI + CM (diff. MAC)"))+
  labs(title = paste0( " Global food consumption by category"), x = "Year", y = "Pcal") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave(paste0(PLOT_FOLDER, "S8_food cons global.png"), height = 4.5, width = 15)



  # FIG S9A -----------------------------------------------------------------
plot <- livestock_production_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025"), agg_sector == "livestock") %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 4, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Livestock production revenue (domestic + exports) in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"S9A_Global grouped prod value_ls.png"), height = 8.5, width = 15)



  # FIG S9B -----------------------------------------------------------------
plot <- livestock_net_trade_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025"), agg_sector == "livestock") %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 4, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Livestock net trade revenue (exports - imports) in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"S9B_Global grouped net trade value_ls_ref_GHG.png"), height = 8.5, width = 15)


  # FIG S9C -----------------------------------------------------------------

plot <- region_net_trade_volume %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025"), sector == "beef") %>%
  ggplot(aes(x = interaction(scenario, year), y = value.exports, fill = sector)) +
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity", size = 1)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  facet_wrap(.~region, nrow = 5, scales = "free")+
  scale_fill_manual(values = pal_ag)+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  labs(title = "Gross beef exports by region in 2050",  x = "Scenario", y = "Mt") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")+
  ggsave(paste0(PLOT_FOLDER,"S9C_Global beef exports.png"), height = 10, width = 15)



  # FIG S10 -----------------------------------------------------------------
grouped_trade_value$region <- factor(grouped_trade_value$region, levels = c(export_LAC_regions, import_LAC_regions))

plot <- grouped_trade_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_CO2_10pa_2025",
                         "SW_high_CL_Policy_CO2_10pa_2025",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025",
                         "ARM_Policy_diff_MAC_10pa_2025",
                         "SW_high_CL_Policy_diff_MAC_10pa_2025"), region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 2, scales = "free")+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_CO2_10pa_2025.2050" = "CO2",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050" = "MI + CO2",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050" = "CM (diff. MAC)",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050" = "MI + CM (diff. MAC)"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_CO2_10pa_2025.2050",
                              "SW_high_CL_Policy_CO2_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050",
                              "blank2" = "",
                              "ARM_Policy_diff_MAC_10pa_2025.2050",
                              "SW_high_CL_Policy_diff_MAC_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Agricultural gross trade revenue (exports and imports)",  x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")+
  ggsave(paste0(PLOT_FOLDER,"S10_LAC gross trade value_SI.png"), height = 8, width = 15)


  # FIG S11A ----------------------------------------------------------------
grouped_import_value <- grouped_trade_value %>%
  filter(trade == "import") %>%
  mutate(value = value * -1) %>%
  select(-trade)

grouped_import_value$agg_sector <- factor(grouped_import_value$agg_sector,
                                          levels = c("biomass", "staple crops", "non-staple crops", "livestock"))


grouped_import_value$region <- factor(grouped_import_value$region, levels = c(export_LAC_regions, import_LAC_regions))

plot <- grouped_import_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025"), region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 2, scales = "free", labeller = as_labeller(region_facet_labels))+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Imported agricultural expenditure in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        panel.spacing.x = unit(2, "lines"))+
  ggsave(paste0(PLOT_FOLDER,"S11A_LAC grouped import value.png"), height = 5.5, width = 10.5)


  # FIG S11B -----------------------------------------------------------------

grouped_domestic_value$agg_sector <- factor(grouped_domestic_value$agg_sector,
                                            levels = c("biomass", "staple crops", "non-staple crops", "livestock"))

plot <- grouped_domestic_value %>%
  filter(year == 2050 || (scenario == "ARM_Reference" & year == 2015),
         scenario %in% c("ARM_Reference",
                         "SW_high_CL_Reference",
                         "ARM_Policy_10pa_2025",
                         "SW_high_CL_Policy_10pa_2025"), region %in% c(export_LAC_regions, import_LAC_regions)) %>%
  ggplot(aes(x = interaction(scenario, year), y = value/(10^9), fill = agg_sector)) +
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1)+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  facet_wrap(.~region, nrow = 2, scales = "free", labeller = as_labeller(region_facet_labels))+
  scale_x_discrete(labels = c("ARM_Reference.2015" = "2015",
                              "blank1" = "",
                              "ARM_Reference.2050" = "REF",
                              "SW_high_CL_Reference.2050" = "MI",
                              "blank2" = "",
                              "ARM_Policy_10pa_2025.2050" = "CM",
                              "SW_high_CL_Policy_10pa_2025.2050" = "MI + CM"),
                   limits = c("ARM_Reference.2015",
                              "blank1",
                              "ARM_Reference.2050",
                              "SW_high_CL_Reference.2050",
                              "blank2",
                              "ARM_Policy_10pa_2025.2050",
                              "SW_high_CL_Policy_10pa_2025.2050") ) +
  scale_fill_manual(name = "Commodity",
                    values = pal_grouped_commodities)+
  labs(title = "Domestic agricultural value in 2050", x = "Scenario", y = "billion 2020$") +
  theme_bw() +
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        panel.spacing.x = unit(2, "lines"))+
  ggsave(paste0(PLOT_FOLDER,"S11B_LAC grouped domestic value.png"), height = 5.5, width = 10.5)

  # FIG S12 -----------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig S12, we used the KMEANS map.
plot_basin_gdp_pc <- gdp_pc_ssp2_reg_basin %>%
  filter(year %in% c(2015,2050)) %>%
  mutate(subRegion = paste(basin_nm, "_X_", region, sep = "")) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  select(subRegion, year, value) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern",
                           "Salinas_Grandes_X_South America_Southern"))

plot_map <- rmap::map(plot_basin_gdp_pc,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S12", sep = ""),
                      title = paste("Per capita GDP"),
                      nameAppend = paste("_LAC_basin_gdp_pc_2015_2050", sep = ""))

  # FIG S13 -----------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig S13, we used the DiffAbs maps.


forest_share_2050 <- forest_share %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern")) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(forest_share_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S13", sep = ""),
                      title = paste("Basin forest land in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", 
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_forest_share_MI_CM_", "2050", sep = ""),
                      pdfpng = 'pdf')

forest_share_2015_2050 <- forest_share %>%
  filter(year %in% c(2015,2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(forest_share_2015_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S13", sep = ""),
                      title = paste("Basin forest land"),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"),
                      nameAppend = paste("_forest_alloc_basin_2015_2050_", sep = ""),
                      pdfpng = 'pdf')



  # FIG S14A -----------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig S14A, we used the DiffAbs maps.

ls_water_wd_basin_2050 <- ls_water_wd_basin %>%
  left_join(basin_to_country_mapping) %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern")) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(ls_water_wd_basin_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S14A", sep = ""),
                      title = paste("Basin livestock water withdrawal in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", "ARM_Policy_CO2_10pa_2025",
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_ls_water_wd_basin_MI_CM_", "2050", sep = ""),
                      pdfpng = 'pdf')

ls_water_wd_2015_2050 <- ls_water_wd_basin %>%
  filter(year %in% c(2015, 2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(ls_water_wd_2015_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S14A", sep = ""),
                      title = paste("Basin livestock  water withdrawal"),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"),
                      nameAppend = paste("_ls_water_wd_basin_2015_2050", sep = ""),
                      pdfpng = "pdf")


  # FIG S14B -----------------------------------------------------------------
# Note: for maps, rmap generates several maps. For fig S14B, we used the DiffAbs maps.

irr_water_wd_basin_2050 <- irr_water_wd_basin %>%
  left_join(basin_to_country_mapping) %>%
  filter(year == 2050, region %in% plot_regions) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  filter(subRegion %!in% c("Orinoco_X_Brazil",
                           "Magdalena_X_South America_Northern",
                           "Orinoco_X_South America_Southern",
                           "North_Chile_Pacific_Coast_X_Argentina",
                           "South_America_Colorado_X_South America_Southern",
                           "Negro_X_South America_Southern")) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(irr_water_wd_basin_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S14B", sep = ""),
                      title = paste("Basin irrigation water withdrawal in 2050"),
                      scenRef = "ARM_Reference",
                      scenDiff = c("SW_high_CL_Reference", "ARM_Policy_CO2_10pa_2025",
                                   "ARM_Policy_10pa_2025", "SW_high_CL_Policy_10pa_2025"),
                      nameAppend = paste("_irr_water_wd_basin_MI_CM_", "2050", sep = ""),
                      pdfpng = 'pdf')

irr_water_wd_2015_2050 <- irr_water_wd_basin %>%
  filter(year %in% c(2015, 2050), region %in% plot_regions, scenario == "ARM_Reference") %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  mutate(subRegion = gsub("Basin_", "", subRegion)) %>%
  mutate(scenario = paste0(scenario,".",year)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(irr_water_wd_2015_2050,
                      folder = paste(getwd(), "/", PLOT_FOLDER, "/S14B", sep = ""),
                      title = paste("Basin  irrigation water withdrawal"),
                      scenRef = "ARM_Reference.2015",
                      scenDiff = c("ARM_Reference.2050"),
                      nameAppend = paste("_irr_water_wd_basin_2015_2050", sep = ""),
                      pdfpng = "pdf")

