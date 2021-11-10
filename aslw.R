#sarah.power@dfo-mpo.gc.ca

##libraries
install.packages("librarian")
librarian::shelf(dplyr, purrr, tidyr, reshape2, openxlsx, ggplot2, scales)

##input
df1 <- read.xlsx(xlsxFile = "data/SOG_aslw_data1.xlsx", sheet = 1)
df2 <- read.xlsx(xlsxFile = "data/SOG_aslw_data2.xlsx", sheet = 1)
df3 <- read.xlsx(xlsxFile = "data/SOG_aslw_data3.xlsx", sheet = 1)

df <- rbind(df1, df2, df3) %>%
  mutate(Year = as.factor(substr(Season,1,4)),
         Gear = as.factor(Gear),
         Age = as.factor(Year_of_Life),
         Sex = as.factor(Sex),
         Sources = as.factor(Source)) %>%
  select(-FishNum, -Year_of_Life) %>% #no need to know individual fish number
  mutate(Sources=recode(Sources, 
                         `6`="Fish & Bait",
                         `5`="Test Fishery",
                         `0`="Roe",
                         `1`="Special Use")) #%>%
  #filter(Source == "Test Fishery") #take out test fishery
  
unique(df$Sources)
# Source codes are: 0 = roe, 5 = test, 6 = F&B, 1 - special use. Gear codes are 19 = gillnet, 29 = seine, 1 = other
# Plot distribution of ages by gear and source, by year

len <- ggplot(df, aes(x=Year, y=Length, fill = Sources)) + 
  #scale_fill_viridis_c() +
  geom_boxplot() + 
  labs(y = "Length (mm)")+
  theme(legend.position = "top")
  #facet_wrap(~Sex)

weight <- ggplot(df, aes(x=Year, y=Weight, fill = Sources)) + 
  #scale_fill_viridis_c() +
  geom_boxplot() + 
  labs(y = "Weight (g)")+
  theme(legend.position = "top")
#facet_wrap(~Sex)

ageDistGearPlot <- ggplot(data = df , mapping = aes(x=Age, fill = Source)) +
  scale_fill_viridis_b() +
  geom_bar(position = "dodge") + 
  labs(y = "Number of fish", fill = "Sample type") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Year, nrow = 4, scales = "free_y") +
  #myTheme + 
  theme(legend.position = "top")
ggsave(ageDistGearPlot, path = "figures")

