

X2020_US_County_Level_Presidential_Results$county_fips = as.character(X2020_US_County_Level_Presidential_Results$county_fips)



OMW_data = OMW_data %>% rename(
  OWMvalue = value
)
OMW_data = select(OMW_data, geo_value, time_value, OWMvalue)

IE_data = IE_data %>% rename(
  IEvalue = value
)
IE_data = select(IE_data, geo_value, time_value, IEvalue)

CC1_data = CC1_data %>% rename(
  CC1value = value
)
CC1_data = select(CC1_data, geo_value, time_value, CC1value)

VA_data = VA_data %>% rename(
  VAvalue = value
)
VA_data = select(VA_data, geo_value, time_value, VAvalue)

D_data = D_data %>% rename(
  Dvalue = value
)
D_data = select(D_data, geo_value, time_value, Dvalue)


Politics_data <- left_join(MW_data, X2020_US_County_Level_Presidential_Results, by = c("geo_value" = "county_fips"))
Politics_data <- left_join(OMW_data, Politics_data, by = c("geo_value" = "geo_value", "time_value" = "time_value"))
Politics_data <- left_join(IE_data, Politics_data, by = c("geo_value" = "geo_value", "time_value" = "time_value"))
Politics_data <- left_join(CC1_data, Politics_data, by = c("geo_value" = "geo_value", "time_value" = "time_value"))
Politics_data <- left_join(VA_data, Politics_data, by = c("geo_value" = "geo_value", "time_value" = "time_value"))
Politics_data <- left_join(D_data, Politics_data, by = c("geo_value" = "geo_value", "time_value" = "time_value"))


Politics_data = na.omit(Politics_data)

skimr::skim(Politics_data)

Politics_data

pd = select(Politics_data, geo_value, value, OWMvalue, IEvalue, VAvalue, CC1value, Dvalue, county_name, votes_gop, votes_dem, total_votes)
head(pd)


pd$county_name = as.factor(pd$county_name)
by_geo_value = pd %>% group_by(county_name)

x = by_geo_value %>% summarise(
  "Avg_mask_wearers" = median(value),
  "Avg_other_mask_wearers" = median(OWMvalue),
  "Avg_attended_large_indoor_event" = median(IEvalue),
  "Avg_vaccience_acceptance" = median(VAvalue),
  "Avg_number_of_confirmed_cases" = median(CC1value),
  "Avg_number_of_COVID_deaths" = median(Dvalue)
  #"Is Blue" = votes_dem > votes_gop
)


pd = left_join(pd, x, by = c("county_name" = "county_name"))

 

pd$AboveAvg = ifelse(pd$value > pd$Avg_mask_wearers, 1, 0)

pd$Politics = ifelse(pd$votes_dem > pd$votes_gop, "Dem", "Gop")

county_df = map_data("county")


test = pd %>% group_by(Politics) %>% summarise("Percent of Mask Wearers" = median(Avg_mask_wearers),
                                        "Percent of Others Who Wear Masks" = median(Avg_other_mask_wearers),
                                        "Percent of Others Who Attended a Large Indoor Event" = median(Avg_attended_large_indoor_event),
                                        "Percent of Other who Would Get or Have Gotten the Vaccine" = median(Avg_vaccience_acceptance),
                                        "Average COVID Cases per 100,000" = median(Avg_number_of_confirmed_cases),
                                        "Average COVID Deaths per 100,000" = median(Avg_number_of_COVID_deaths)
                                        )

readr::write_csv(test, file = "politicsmed.csv")
library(readxl)
library(readr)
politics_clean <- read_csv("politics_clean.csv")

politics_c <- read_excel("poltiics_c.xlsx")
politics_d <- read_excel("politics_d.xlsx")

ggplot(politics_clean, aes(x = category, y = value, fill = politics)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_manual(values =c("blue", "red"))

ggplot(politics_c, aes(x = category, y = value, fill = politics)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_manual(values =c("blue", "red"))

ggplot(politics_d, aes(x = category, y = value, fill = politics)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_manual(values =c("blue", "red"))




