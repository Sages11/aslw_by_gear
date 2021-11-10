#sarah.power@dfo-mpo.gc.ca

##libraries
install.packages("librarian")
librarian::shelf(dplyr, purrr, tidyr, reshape2)

##input
df1 <- read.csv("data/SOG_aslw_data1.csv")
df2 <- read.csv("data/SOG_aslw_data2.csv")
df3 <- read.csv("data/SOG_aslw_data3.csv")

df <- rbind(df1, df2, df3)