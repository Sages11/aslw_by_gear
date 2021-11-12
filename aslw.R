#sarah.power@dfo-mpo.gc.ca

##libraries
install.packages("librarian")
librarian::shelf(dplyr, purrr, tidyr, reshape2, openxlsx, ggplot2, scales)

##function
`%!in%` <- Negate(`%in%`)

##Settings for ggsave function
mywidth <- 12
myheight <- 7

##input
df1 <- read.xlsx(xlsxFile = "data/SOG_aslw_data1.xlsx", sheet = 1)
df2 <- read.xlsx(xlsxFile = "data/SOG_aslw_data2.xlsx", sheet = 1)
df3 <- read.xlsx(xlsxFile = "data/SOG_aslw_data3.xlsx", sheet = 1)

df <- rbind(df1, df2, df3)

df$Sources <- paste0(df$Source, df$Gear)
df <- df%>% # Source codes are: 0 = roe, 5 = test, 6 = F&B, 1 - special use. 
            #Gear codes are 19 = gillnet, 29 = seine, 1 = other
  mutate(Sources = as.factor(recode(Sources,
                                    `6Seine`="Food & Bait",
                                    `5Seine`="Test Fishery",
                                    `0Gillnet`="Gillnet",
                                    `0Seine`="Seine",
                                    `1Seine`="Special Use area 13-14")))
unique(df$Sources)
#re-order factor levels for region
df$Sources <- factor(df$Sources, levels=c('Food & Bait', 
                                      'Gillnet', 
                                      'Seine',
                                      'Test Fishery', 
                                      'Special Use area 13-14'))
#df <- df[order(levels(df$Sources)),]
unique(df$Sources)
df <- df %>%
  mutate(Year = as.factor(paste0(substr(Season,1,3), substr(Season, 5,5))),
         #Gear = as.factor(Gear),
         Age = as.factor(Year_of_Life),
         Sex = as.factor(Sex),
         Area = as.factor(substr(Section, 1,2))) %>%
  select(-FishNum, -Year_of_Life) #no need to know individual fish number
  
#These are the values we want to remove from df
df4 <- df %>% 
  filter(Sources == "Special Use area 13-14") %>% 
  filter(Area %!in% c("13", "14"))

df <- anti_join(df, df4) %>% 
  filter(as.numeric(Age)< 9)

df5 <- df %>%  # This should have nothing in it if the anti_jion worked.
  filter(Sources == "Special Use area 13-14") %>% 
  filter(Area %!in% c("13", "14"))

ratiodf <- df %>%
  #filter(Sex != "U") %>%
  group_by(Year, Sources, Sex) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "Sex",
              values_from = "n") %>%
  mutate( n = F + M,
    Fratio = F/n,
    myaxis = paste0(Sources, "\n", "n=", n))

unique(df$Sex)  
##Plots

ggplot(df, aes(x=Year, y=Length, fill = Sources)) + 
  #scale_fill_viridis_c() +
  geom_boxplot(position = position_dodge2(width = 0.9, preserve = "single")) + 
  labs(y = "Length (mm)", x = NULL)+
  theme(legend.position = "top",
        text = element_text(size = 20),
        legend.title = element_blank())  
  #facet_wrap(~Sex, col =1)
ggsave("figures/length.png", width = mywidth, height = myheight)

ggplot(df, aes(x=Year, y=Weight, fill = Sources)) + 
  #scale_fill_viridis_c() +
  geom_boxplot(position = position_dodge2(width = 0.9, preserve = "single")) + 
  labs(y = "Weight (g)")+
  theme(legend.position = "top",
        text = element_text(size = 20),
        legend.title = element_blank())  
ggsave("figures/weight.png", width = mywidth, height = myheight)

ggplot(df, aes(x=Sex, y=Weight, fill = Sources)) + 
  #scale_fill_viridis_c() +
  geom_boxplot(position = position_dodge2(width = 0.9, preserve = "single")) + 
  #facet_wrap(~Age)+
  labs(y = "Weight (g)")+
  theme(legend.position = "top",
        text = element_text(size = 20),
        legend.title = element_blank())  
ggsave("figures/sex.png", width = mywidth, height = myheight)

ggplot(data = df , mapping = aes(x=Age, fill = Sources)) +
  #scale_fill_viridis_b() +
  geom_bar(position = "dodge") + 
  labs(y = "Number of fish", fill = "Sources") +
  #scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Year, nrow = 4, scales = "free_y", dir = "h") +
  theme(legend.position = "top",
        text = element_text(size = 18),
        legend.title = element_blank())  
ggsave("figures/age.png", width = mywidth, height = myheight)

ggplot(data = df , mapping = aes(x=Length, fill = Sources)) +
  #scale_fill_viridis_b() +
  geom_bar(position = "dodge") + 
  labs(y = "Number of fish", fill = "Source") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Age, ncol = 3, scales = "free_y", dir ="v") +
  theme(legend.position = "top",
        text = element_text(size = 18),
        legend.title = element_blank())  
ggsave("figures/lengthagegear.png", width = mywidth, height = myheight)

ggplot(data = df , mapping = aes(x=Length)) +
  #scale_fill_viridis_b() +
  geom_bar(position = "dodge") + 
  labs(y = "Number of fish", fill = "Source") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Age, ncol = 3, scales = "free_y", dir ="v") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        legend.title = element_blank())  
ggsave("figures/lengthage.png", width = mywidth, height = myheight)

ggplot(data = df , mapping = aes(x=Sex, fill = Sources)) +
  #scale_fill_viridis_b() +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single")) + 
  labs(y = "Number of fish", fill = "Source") +
  #scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Age, ncol = 3, scales = "free_y", dir ="v") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        legend.title = element_blank())  
ggsave("figures/Sexage.png", width = mywidth, height = myheight)

ggplot(data = df , mapping = aes(x=Sex, fill = Sources)) +
  #scale_fill_viridis_b() +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single")) + 
  labs(y = "Number of fish", fill = "Source") +
  #scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ Year, ncol = 3, scales = "free_y", dir ="v") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        legend.title = element_blank())  
ggsave("figures/Sexyear.png", width = mywidth, height = myheight)

ggplot(data = df , mapping = aes(x=Sex, fill = Sources)) +
  #scale_fill_viridis_b() +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single")) + 
  labs(y = "Number of fish", fill = "Source") +
  #scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma) +
  #facet_wrap(~ Age, ncol = 3, scales = "free_y", dir ="v") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        legend.title = element_blank())  

ggsave("figures/Sexgear.png", width = mywidth, height = myheight)

ggplot(data = ratiodf , mapping = aes(x=Sources, y=F/(F+M), fill = Sources)) +
  geom_bar(stat = "identity") + 
  labs(y = "F/(F+M)", x = NULL) +
  geom_hline(yintercept = 0.5, alpha = 0.7, linetype =2) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+
  facet_wrap(~ Year, ncol = 3, scales = "free_y", dir ="h") +
  theme(legend.position = "top",
        text = element_text(size = 16),
        axis.text.x=element_blank (),
        legend.title = element_blank())  

ggsave("figures/Sexgearratio.png", width = mywidth, height = myheight)
