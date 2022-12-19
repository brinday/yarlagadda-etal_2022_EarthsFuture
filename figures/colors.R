pal_grouped_commodities <- c("biomass" = "#648370" ,
                             "staple crops" = "gold",
                             "non-staple crops" = "goldenrod",
                             "livestock" =  "#a36a69")

pal_16 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966")

pal_3 <- c("SW_high_CL_Reference"  = "#619cff",
           "ARM_Policy_10pa_2025" = "#f8766d",
           "SW_high_CL_Policy_10pa_2025" = "darkorchid")

pal_GHG <- c("FFI CO2" = "#999999",
             "FFI Non-CO2" = "#0072B2",
             "LUC CO2" = "#009E73",
             "Crop Non-CO2" = "goldenrod",
             "Livestock Non-CO2" =  "#a36a69")

pal_ag <- c('beef' = "#999999",
            'dairy' = "#E69F00",
            'pork' = "#56B4E9",
            'poultry' = "#009E73",
            'sheepgoat' = "#F0E442", 
            'corn' = "#0072B2",
            'othergrain' = "#D55E00", 
            'rice' = "#CC79A7",
            'roottuber' = "#333333",
            'wheat' = "#FFCC00",
            'fibercrop' = "#CC6600",
            'misccrop' = "#006600",
            'oilcrop' = "#3333CC",
            'palmfruit' = "#CC0033",
            'sugarcrop' = "#0099CC",
            'biomass' = "#999966")
              
pal_metis <- c(`a Coal` = "gray40",  `c coal` = "gray40", `3 coal` = "gray40", coal = "gray40", `coal (conv pul)` = "gray40",
               Coal = "gray40",
               `b Coal w/CCS` = "gray20", `c coal CCS` = "gray20", `Coal CCS` = "gray20",
               `coal (IGCC CCS)` = "#c0237c",
               `coal (IGCC)` = "gray60",
               `coal (conv pul CCS)` = "#e0237c",
               `c Gas` = "darkslategray1", `b natural gas` = "darkslategray1", `2 gas` = "darkslategray1", Gas = "darkslategray1",
               gas = "darkslategray1", `gas (CC)` = "darkslategray1",
               `d Gas w/CCS` = "deepskyblue1", `b natural gas CCS` = "deepskyblue1",
               `gas (CC CCS)` = "deepskyblue1", `Gas CCS` = "deepskyblue1",
               `gas (CT)` = "darkslategray4",
               `gas (steam)` = "darkslategray",
               `e Oil` = "firebrick4", `a oil` = "firebrick4", oil = "firebrick4", Oil = "firebrick4", liquids="firebrick4",
               `refined liquids` = "firebrick4", `1 liquids` = "firebrick4",
               `refined liquids (CC)` = "firebrick4",
               `refined liquids (CT)` = "firebrick1",
               `refined liquids (steam)` = "firebrick3",
               `refined liquids (CC CCS)` = "#f7988f", `f Oil w/CCS` = "#f7988f", `a oil CCS` = "#f7988f", `Oil w/CCS` = "#f7988f",
               `Oil CCS` = "#f7988f",
               `fossil fuel liquid`="firebrick4",
               `International Aviation liquids` = "indianred",`International Aviation Oil` = "indianred", `liquids av`="indianred",
               `liquids intl av`="indianred", `fossil fuel liquids intl av`="indianred", `oil intl av`="indianred",
               `liquids intl shp`="lightcoral", `fossil fuel liquids intl shp`="lightcoral",`oil intl shp`="lightcoral",
               `International Ship liquids` = "lightcoral", `International Ship Oil` = "lightcoral",`liquids shp`="lightcoral",
               `g Biomass` = "darkolivegreen2", `g Bioenergy` = "darkolivegreen2", `d biomass` = "darkolivegreen2",  biomass = "darkolivegreen2",
               Biomass = "darkolivegreen2", `Biomass CCS` = "darkolivegreen2",
               bioenergy = "darkolivegreen2", `d bioenergy` = "darkolivegreen2", Bioenergy = "darkolivegreen2", `h Bioenergy w/CCS` = '#7efd97',
               `j traditional bioenergy` = "chartreuse3",`traditional bioenergy` = "chartreuse3",`Traditional Bioenergy` = "chartreuse3",
               `biomass (IGCC CCS)` = "#00a31d", `4 biomass` = "#00a31d",
               `biomass (IGCC)` = "#00c31d",
               `biomass (conv CCS)` = "#00e31d",
               `biomass (conv)` = "#00f31d",
               biofuel = "darkolivegreen1", Biofuel = "darkolivegreen1", `biomass liquids`="darkolivegreen1",
               `coal to liquids`='gray40', `gas to liquids`='darkslategray1', `oil refining`='firebrick4',
               `j traditional biomass` = "#11d081", `traditional biomass` = "#11d081",
               `h Biomass w/CCS` = "#88c892", `d biomass CCS` = "#88c892",
               `i Nuclear` = "#ef8e27", `e nuclear` = "#ef8e27", nuclear = "#ef8e27", Gen_II_LWR = "#ef8e27",
               Nuclear = "#ef8e27",
               Gen_III = "#af8e27",
               `j Geothermal` = "darkmagenta", `i geothermal` = "darkmagenta", geothermal = "darkmagenta", Geothermal = "darkmagenta",
               `k Hydro` = "#3d86f9", `f hydro` = "#3d86f9", hydro = "#3d86f9", Hydro = "#3d86f9",
               `l Wind` = "#fdd67b", `g wind` = "#fdd67b",  wind = "#fdd67b", Wind = "#fdd67b",
               `m Solar` = "#fdfa28", `h solar` = "#fdfa28", solar = "#fdfa28", Solar = "#fdfa28",
               CSP = "#cdd67b",
               PV = "#fdd67b",
               `n CHP` = "#507fab",
               #---------------
               # Other
               #--------------
               `heat`="darkslategray",
               `6 hydrogen` = "orange", `hydrogen` = "orange",`Hydrogen Production and Refining` = "black", `Refining` = "black",
               `Refining and Hydrogen Production` = "black", `CO2 Refining and Hydrogen Production` = "black",
               `o Battery` = "#92a75d",
               `energy reduction` = "grey",
               `Total` = "black", `total` = "black",
               `Other` = "grey70", other = "grey70",
               `energy` = "grey50", `Energy` = "grey50",
               `Fossil` = "gray20", `fossil` = "gray20", `FOSSIL` = "gray20",
               `fossil fuel`="gray20",
               #---------------
               # Sectors
               #--------------
               `transport intl. aviation` = "cadetblue3",`Transport Intl Av`="cadetblue3",`trans intl av`="cadetblue3", `transport intl av`="cadetblue3",
               `International Aviation` = "cadetblue3",
               `International Ship` = "cadetblue4",
               `transport intl. shipping` = "cadetblue4",`Transport Intl Shp`="cadetblue4",`trans intl shp`="cadetblue4", `transport intl shp`="cadetblue4",
               `building` = "#facda4", buildings = "#facda4", elect_td_bld = "#facda4", Building = "#facda4",
               Buildings = "#facda4", `CO2 Buildings` = "#facda4", `comm non-building` = "#ff230e",
               `CDD`="cornflowerblue",
               `Cooling`= "cornflowerblue", `cooling`= "cornflowerblue", `COOLING`= "cornflowerblue",
               `HDD`="coral2",
               `Heating`= "coral2", `heating`= "coral2", `HEATING`= "coral2",
               `Commercial Cooling` = '#342DFC', `Commercial Heating` = '#E11F26', `Commercial Others`='#BD8A25',
               `Residential Cooling` = '#6865C1', `Residential Heating` = "#D8686C", `Residential Others`='#D4C592',
               `Residential CoolingHeating` = '#6865C1', `Commercial CoolingHeating` = '#342DFC',
               `industry` = "#cef4d1", elect_td_ind = "#cef4d1",`Industry` = "#cef4d1", `CO2 Industry`="#cef4d1",
               `transportation` = "#d0f6f7", `Transportation` = "#d0f6f7", `transport` = "#d0f6f7", `Transport` = "#d0f6f7",
               `CO2 Transport` = "#d0f6f7",
               elect_td_trn = "#d0f6f7", `trn_pass_road_bus` = "purple",
               Agriculture = "forestgreen", agriculture = "forestgreen", Ag = "forestgreen", AG = "forestgreen", Agri = "forestgreen",
               ag = "forestgreen", crops='forestgreen', Crops='forestgreen',
               `Electricity` = "lavender", electricity="lavender", `5 electricity` = "lavender",
               municipal = "dodgerblue", mining = "grey75",
               livestock='goldenrod2', Livestock= 'goldenrod2', `CO2 Livestock` = 'goldenrod2',
               #---------------
               # Transportation
               #--------------
               LDV = "#B34545", Bus = "#CADC28", Rail = "#58A989", Plane = "#697080", MotorBike = "#5E85D2",
               Liquids = 'firebrick4', Electric = 'lavender', Truck='#792f2f', Ship='#2f4e79',
               
               #---------------
               # Emissions
               #--------------
               `LUC Emission`="grey30", LUC="grey30", LUCemiss="grey30",
               `LUC Absorption`="darkolivegreen4",
               delivered_gas="darkslategray1", refined_liquids_enduse="#d01c2a", H2_enduse = "#507fab",
               delivered_coal = "black", wholesale_gas="darkslategray1", delivered_biomass="darkolivegreen2",
               refined_liquids_industrial="#d01c2a",
               sewage_landfills = 'brown', Waste='#BC7508', `CO2 Waste` = '#BC7508', `CO2 Electricity`='lavender',
               urban="indianred2",Urban="indianred2",
               tundra="antiquewhite1",
               shrubs="lightslateblue",Shrubs="lightslateblue",SHRUBS="lightslateblue",
               `rock and desert`="black",
               pasture="goldenrod1",otherarable="darkorange4",grass="darkolivegreen1",forest="darkgreen",
               naturalOther="grey75",`CO2 Transport Intl Av`="cadetblue3",`CO2 Transport Intl Shp`="cadetblue4",
               CH4 = "firebrick4", CO2 = "black", HFCs = "cadetblue3", N2O = "forestgreen",
               #---------------
               # Agriculture
               #--------------
               Forest = "darkgreen" , NonFoodDemand_Forest = "darkolivegreen1",
               biomass_grass="#00931d", biomass_tree="#00931d", biomass = "#00931d",
               Corn = "gold3", corn = "gold3",
               FiberCrop = "gold4", fibercrop = "gold4",
               MiscCrop = "darkorange4", misccrop = "darkorange4",
               OilCrop = "#999999", oilcrop = "#999999",
               SoySunflower = "gray20",Soy = "gray20",soy = "gray20", SOY = "gray20",
               OtherGrain  = "indianred2", othergrain = "indianred2",
               PalmFruit = "firebrick3" , palmfruit = "firebrick3",
               Rice = "steelblue2", rice = "steelblue2",
               RootTuber  = "mediumpurple", Root_Tuber  = "mediumpurple", roottuber = "mediumpurple",
               SugarCrop = "yellow2",sugarcrop = "yellow2",
               Wheat  = "burlywood", wheat = "burlywood",
               shrubland="lightslateblue",Shrubland="lightslateblue",SHRUBLAND="lightslateblue",
               FodderHerb = "darkseagreen4", FodderGrass = "mediumseagreen",
               Fodder = "darkseagreen4",
               Pasture = "goldenrod1",
               UnmanagedLand = "black",
               IRR = "dodgerblue3", irr = "dodgerblue3", Irr = "dodgerblue3",
               irrigation = "dodgerblue3", Irrigation = "dodgerblue3", IRRIGATION = "dodgerblue3",
               RFD = "gold1", rfd = "gold1", Rfd = "gold1",
               RAINFED = "gold1", rainfed = "gold1", Rainfed = "gold1",
               
               #---------------
               # Livestock
               #--------------
               beef = "#E69F00",
               dairy =  "#FFCC00",
               pork = "#CC79A7",
               poultry = "#3333CC",
               sheepgoat = "#0072B2"
            

)


pal_land_alloc <- c("biomass" = "#009E73",
                    "crops" = "gold3",
                    #"forest (managed)" = "gray70",
                    #"forest (unmanaged)" = "black",
                    "forest" = "black",
                    "grass" = "dodgerblue3",
                    "otherarable" = "darkorange4",
                    "pasture (grazed)" = "indianred3",
                    "pasture (other)" = "#f7988f",
                    "shrubs" = "mediumpurple"
                    )

pal_water_wd <- c("livestock" ='goldenrod2',
                  "municipal water" = "dodgerblue",
                  "industry" = "#cef4d1",
                  "crops" = "forestgreen",
                  "electricity" = "lavender",
                  "biomass" = "mediumseagreen",
                  "primary energy" = "indianred2")

