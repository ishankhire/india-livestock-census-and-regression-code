# Set up
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working directory
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable
library(stringr) #For converting to title case
library(betareg) #For beta regression
library(gt) #For generating tables

# Load data
df_census <- read.csv("census_updated.csv")
df_crops <- read.csv("crops.csv")
df_gdp_poultry <- read.csv("gdp_poultry_updated.csv")

# Tidy up the data
df_census$state <- str_to_title(df_census$state) #Convert census states to title case
df_census$population <- as.numeric(gsub(",","",df_census$population)) #Convert to numeric
df_census$area <- as.numeric(gsub(",","",df_census$area)) #Convert to numeric
df_gdp_poultry <- df_gdp_poultry[-nrow(df_gdp_poultry),] #Remove the final row (all India)
names(df_gdp_poultry)[1] <- "state"

# Rename some state names to remain consistent across data sets
df_gdp_poultry[which(df_gdp_poultry$state=="A&N Islands"),"state"] <- "Andaman & Nicobar Islands"
df_gdp_poultry[which(df_gdp_poultry$state=="D&N Haveli"),"state"] <- "Dadra & Nagar Haveli"
df_gdp_poultry[which(df_gdp_poultry$state=="Jammu &  Kashmir"),"state"] <- "Jammu & Kashmir"
df_census[which(df_census$state=="Nct Of Delhi"),"state"] <- "Delhi"
df_crops[which(df_crops$state=="Chattisgarh"),"state"] <- "Chhattisgarh"
df_crops[which(df_crops$state=="Orissa"),"state"] <- "Odisha"

# Calculate urbanisation (expressed as a %)
df_census_wide <- reshape(df_census,idvar="state",timevar="type",direction="wide")
df_census_wide$urbanisation <- 100*df_census_wide$population.Urban/df_census_wide$population.Total

# Calculate population density
df_census_wide$population_density <- df_census_wide$population.Total/df_census_wide$area.Total

# Match variables by state
df_states <- merge(df_census_wide,df_gdp_poultry,by="state",all=T)
df_states <- merge(df_states,df_crops,by="state",all=T)

# Set all "NA" crop production to 0 (which is indeed what NA crop production means)
df_states[which(is.na(df_states$maize_1000_tonnes)),"maize_1000_tonnes"] <- 0
df_states[which(is.na(df_states$soy_1000_tonnes)),"soy_1000_tonnes"] <- 0

# Check for correlations between predictors
cor(df_states[,c("percapita_USD_PPP","population_density","urbanisation",
                 "maize_1000_tonnes","soy_1000_tonnes")],
    use="pairwise.complete.obs")

# Generate linear model
lm1 <- lm(proportion_fowl_commercial~
     log(percapita_USD_PPP) + 
     population_density + 
     urbanisation + 
     maize_1000_tonnes + 
     soy_1000_tonnes,
     weights=total_poultry,
   data=df_states)
summary(lm1)
lm2 <- betareg(proportion_fowl_commercial~
            log(percapita_USD_PPP) + 
            population_density + 
            urbanisation + 
            maize_1000_tonnes + 
            soy_1000_tonnes,
          weights=total_poultry,
          data=df_states)
summary(lm2)

# Generate predictions of the model by state
df_states$proportion_commercial_modelpredicted <- predict(lm2,newdata=df_states)

# Calculate residuals by state
df_states$proportion_commercial_residual <- df_states$proportion_fowl_commercial-
  df_states$proportion_commercial_modelpredicted

# Create publication-ready table of state residuals
df_states_publication <- df_states[order(df_states$proportion_commercial_residual),
                                   c("state",
                                     "proportion_fowl_commercial",
                                     "proportion_commercial_modelpredicted",
                                     "proportion_commercial_residual")]
df_states_publication <- df_states_publication[complete.cases(df_states_publication),]
df_states_publication$proportion_fowl_commercial <- round(100*df_states_publication$proportion_fowl_commercial,1)
df_states_publication$proportion_commercial_modelpredicted <- round(100*df_states_publication$proportion_commercial_modelpredicted,1)
df_states_publication$proportion_commercial_residual <- round(100*df_states_publication$proportion_commercial_residual,1)
names(df_states_publication) <- c("State","Fowl - Proportion Commercial (%)",
                                  "Model Prediction (%)","Residual (%)")

df_states_publication_swanky <- gt(df_states_publication)
df_states_publication_swanky <- tab_style(df_states_publication_swanky, style=cell_text(weight = "bold"),
                                          locations = list(cells_body(rows=c(1:7)),cells_column_labels()))
df_states_publication_swanky <- cols_align(df_states_publication_swanky, align = "center")
df_states_publication_swanky <- tab_caption(df_states_publication_swanky,
                          caption="Table 3: Residuals by state")
df_states_publication_swanky <- tab_footnote(df_states_publication_swanky,
                           footnote="Bold font denotes a state explored further in this publication")
gtsave(df_states_publication_swanky, filename = "df_states_publication_swanky.html")

# Create publication-ready table of beta regression
lm2_summary <- summary(lm2)
lm2_caption <- paste0("Pseudo R-squared = ",round(lm2_summary$pseudo.r.squared,3),";",
       "log likelihood = ",round(lm2_summary$loglik/10^8,3),"x10^8 on 7 df")
lm2_summary_coef <- as.data.frame(lm2_summary$coefficients$mean)
lm2_summary_coef$Parameter <- rownames(lm2_summary_coef)
lm2_summary_coef <- lm2_summary_coef[,c(5,1,2,3,4)]
lm2_summary_coef <- format(lm2_summary_coef,digits=3)
lm2_summary_coef$`Pr(>|z|)` <- "< 0.001"
lm2_summary_coef$Estimate <- gsub("e","x10^",lm2_summary_coef$Estimate)
lm2_summary_coef$`Std. Error` <- gsub("e","x10^",lm2_summary_coef$`Std. Error`)
lm2_swanky <- gt(lm2_summary_coef)
lm2_swanky <- tab_style(lm2_swanky, style=cell_text(weight = "bold"),
                        locations = list(cells_column_labels()))
lm2_swanky <- tab_footnote(lm2_swanky,
                          footnote=lm2_caption)
lm2_swanky <- tab_caption(lm2_swanky,
                          caption="Table 2: Beta regression model")
gtsave(lm2_swanky, filename = "lm2_swanky.html")


max(df_states[!is.na(df_states$percapita_USD_PPP),"population_density"])


