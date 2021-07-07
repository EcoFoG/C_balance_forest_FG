#### Theme for all the graph (to be source at begining of graph function)
#########################################################################


#### Set theme
#######################################

MyTheme <- theme_bw() + 
  theme(panel.grid.major = element_blank(), # hide major grid lines
        panel.grid.minor = element_blank()) # hide minor grid lines)
      


### Set my colors for fluxes
myColFlux <- c("Net overall" = "black", 
               "Net historical"="hotpink1",
               "Net natural forest (sim.)"="purple", 
               "Net plantation (sim.)" = "goldenrod",
               "Sawmill" = "dodgerblue", 
               "Biomass plant"= "red2", 
               "Logistic activities" = "slategray3", 
               "Decay on site"= "saddlebrown",
               "Recovery in natural forest" = "darkgreen", 
               "Regeneration in natural forest" = "palegreen3",
               "Growth in plantation" = "olivedrab3")

### Set my colors for quantity of Timber or electricity produces per year
myColQt <- c("Total" = "black", 
             "Historical" ="hotpink1",
             "Natural forest"="purple", 
             "Plantation" = "goldenrod")



### Set my colors for fluxes for publication
myColFluxPub <- c("Net overall" = "black", 
               "Net historical"="hotpink1",
               "Net natural forest (sim.)"="purple", 
               "Net plantation (sim.)" = "goldenrod",
               "Sawmill" = "dodgerblue", 
               "Biomass plant"= "red2", 
               "Logistic activities" = "slategray3", 
               "Decay on site"= "saddlebrown",
               "Recovery in natural forest" = "darkgreen", 
               "Regrowth in natural forest" = "palegreen3",
               "Growth in plantation" = "olivedrab3")
  