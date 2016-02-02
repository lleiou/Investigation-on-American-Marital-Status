

library("survey")
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")
library("choroplethrMaps")

#getting the data frame from household data
colsToKeep<-c("ST","FINCP","HINCP","FES","HHT","SERIALNO")

husa <- fread("ss13husa.csv", select=colsToKeep)
husb <- fread("ss13husb.csv", select=colsToKeep)

hus <- rbind(husa,husb) #This is a list
hus <- as.data.frame(hus) #Transform a list to a data frame

#getting the data frame from population data
colsToKeep<-c("ST","SCHL","WAGP","AGEP","INDP","SERIALNO","SEX", "MSP","PINCP")

pusa <- fread("ss13pusa.csv", select=colsToKeep)
pusb <- fread("ss13pusb.csv", select=colsToKeep)

pus <- rbind(pusa,pusb)
pus <- as.data.frame(pus)

hp <- left_join(hus,pus, by.x="SERIALNO")


hp<-hp %>%
  mutate(INDP2 = NA)

hp$INDP2[hp$INDP>=0170 & hp$INDP<=0290]<-"AGR"
hp$INDP2[hp$INDP>=0370 & hp$INDP<=0490]<-"EXT"
hp$INDP2[hp$INDP>=0570 & hp$INDP<=0690]<-"UTL"
hp$INDP2[hp$INDP==0770]<-"CON"
hp$INDP2[hp$INDP>=1070 & hp$INDP<=3990]<-"MFG"
hp$INDP2[hp$INDP>=4070 & hp$INDP<=4590]<-"WHL"
hp$INDP2[hp$INDP>=4670 & hp$INDP<=5790]<-"RET"
hp$INDP2[hp$INDP>=6070 & hp$INDP<=6390]<-"TRN"
hp$INDP2[hp$INDP>=6470 & hp$INDP<=6780]<-"INF"
hp$INDP2[hp$INDP>=6870 & hp$INDP<=7190]<-"FIN"
hp$INDP2[hp$INDP>=7270 & hp$INDP<=7790]<-"PRF"
hp$INDP2[hp$INDP>=7860 & hp$INDP<=7890]<-"EDU"
hp$INDP2[hp$INDP>=7970 & hp$INDP<=8290]<-"MED"
hp$INDP2[hp$INDP>=8370 & hp$INDP<=8470]<-"SCA"
hp$INDP2[hp$INDP>=8560 & hp$INDP<=8690]<-"ENT"
hp$INDP2[hp$INDP>=8770 & hp$INDP<=9290]<-"SRV"
hp$INDP2[hp$INDP>=9370 & hp$INDP<=9590]<-"ADM"
hp$INDP2[hp$INDP>=9670 & hp$INDP<=9870]<-"MIL"
hp$INDP2[hp$INDP==9920]<-"UNEMPLOYED AND LAST WORKED 5 YEARS AGO OR EARLIER OR NEVER WORKED"



#for convenience, we can save the subset for future use.
#next time when we want to use the data, we can simply load the data we saved last time.
save(hp, file="hp.Rdata")
load("hp.RData")



# subset the data according to the variables we're focusing on 
#caluculate the sum of married people and single people
hp <-tbl_df(hp)

#remove NA in MSP column
#and calculate the sum of the remaining individual
hp <- hp %>%
  na.omit(MSP)


#Convert state number to state abbreviation

ST.anno <- read.csv("statenames.csv", header=T)
colnames(ST.anno)[1] <- "ST"
hp <- left_join(hp, ST.anno, by = "ST")

#calculate the sum of married people and unmarried people
#Then calculate the married/unmarried rate across the country

#mutate adds another columi to the data frame

#calculate the married rate and single rate in every state
StatePop<-hp %>%
  group_by(ST)%>%
  summarize(count=n())

StateHp<-hp%>%
  filter(MSP==1)%>%
  group_by(ST)%>%
  summarize(count=n())

#combine the two data set together in order to let them devide within
#the correct group

StateHp <- left_join(StateHp,ST.anno , by="ST")

StateHpRate<-StateHp %>%
  mutate ("StateHpRate" = StateHp$count/StatePop$count)


StateHpRate<-StateHpRate %>%
  select("region"=name,"value"=StateHpRate)


#Change all the name of states into lower letter or the 
#function "state_choropleth" would report a error
StateHpRate$region <- StateHpRate$region %>% tolower()

#plot the rate of the mariage in every state
state_choropleth(StateHpRate,
                 title = "The Rate of Mariage in Every State", 
                 num_colors = 5,
                 legend     = "Household",
)

