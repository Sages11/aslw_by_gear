#sarah.power@dfo-mpo.gc.ca

##libraries
install.packages("librarian")
librarian::shelf(dplyr, purrr, tidyr, reshape2, openxlsx)

##input
df1 <- read.xlsx(xlsxFile = "data/SOG_aslw_data1.xlsx", sheet = 1)
df2 <- read.xlsx(xlsxFile = "data/SOG_aslw_data2.xlsx", sheet = 1)
df3 <- read.xlsx(xlsxFile = "data/SOG_aslw_data3.xlsx", sheet = 1)
#df1 <- read.xlsx("data/SOG_aslw_data1.xlsx")
#df2 <- read.csv("data/SOG_aslw_data2.csv")
#df3 <- read.csv("data/SOG_aslw_data3.csv")

df <- rbind(df1, df2, df3)
