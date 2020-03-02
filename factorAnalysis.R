# Import libraries ####
library(ggplot2)
library(dplyr)

## REQUIRED PACKAGES

#install.packages("PerformanceAnalytics")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("lavaan")

library("PerformanceAnalytics")
library("FactoMineR")
library("factoextra")
library("lavaan")


# Import data (output from Jupyter Notebook) and fuse ####

mydata <- read.csv('outputs/data/for_r/mydata_with_indices.csv')

# reverse youth unemployment
mydata$Youth.Unemployment <- 1-mydata$Youth.Unemployment
# drop protected sea as not all countries apply
mydata <- subset(mydata, select=-c(Protected.Sea,oneoverco2,oneoverpl,oneoverps))

# Add 2013 WB classifications to mydata and clean #######

wb_groups <- read.csv('data/new/wb_income_groups_2013.csv', stringsAsFactors = FALSE, na.strings=c(""))
#wb_groups <- na.omit(wb_groups)

### add numerical classifications
income_groups <- c("L","LM","UM","H")
income_group_numbers <- c(1,2,3,4)

for (i in seq(1,4))
{wb_groups$income_group_number[wb_groups$Income.group == income_groups[i]] <- income_group_numbers[i]}

### fix names in wb dataset to match old mydata
wb_groups$Economy[wb_groups$Code == "MKD"] <- "Macedonia, FYR"  # remove North from Macedonia
wb_groups$Economy[wb_groups$Code == "SWZ"] <- "Swaziland" # change Eswatini is Swaziland
wb_groups$Economy[wb_groups$Code == "PRK"] <- "Korea, Dem. Rep."  #	Korea, Dem. People's Rep change
wb_groups$Economy[wb_groups$Code == "FRO"] <- "Faeroe Islands"  # Faroe to Faeroe
wb_groups$Economy[wb_groups$Code == "STP"] <- "Sao Tome and Principe"  # sao tome - accents
wb_groups$Economy[wb_groups$Code == "CUW"] <- "Curacao"  # curacao - accents
wb_groups$Economy[wb_groups$Code == "CIV"] <- "Cote d'Ivoire"  # Cote d'Ivoire - accents

### both cabo and cape verde were in mydata, and cabo had better data. Remove Cape Verde.
#subset(mydata, country=="Cabo Verde")
#subset(mydata, country=="Cape Verde")
mydata <- subset(mydata, country!="Cape Verde")

### add WB groups to mydata

# check that all countries in mydata are also in wb_groups
setdiff(mydata$country,na.omit(wb_groups)$Economy)
# only Nauru doesn't have a 2013 wb classification
# we'll use it's later (2016+) assignment, UM
wb_groups$Income.group[wb_groups$Economy == "Nauru"] <- "UM"

wb_countries <- wb_groups$Economy
wb_group_numbers <- wb_groups$income_group_number
wb_group_names <- wb_groups$Income.group

mydata$wb_group <- NA
for (i in seq(1,length(wb_countries)))
{mydata$wb_group[mydata$country == as.character(wb_countries[i])] <- wb_group_names[i]}


### save
write.csv(mydata, file="outputs/data/mydata_with_wb_2013.csv")
summary(mydata)

wb_group_sizes <- wb_groups %>% group_by(Income.group) %>% summarise(counts = n())
### create cleaned mydata
is.na(mydata) <- sapply(mydata, is.infinite) # make infs into NaNs
mydata_clean <- na.omit(mydata) # omit rows with any missing values


# Explore correlation matrices #########

#chart.Correlation(mydata[4:18], histogram=TRUE, pch=19)

# transform to normal distribution
mydata_trans <- mydata
mydata_trans$CO2.emissions <- log(mydata$CO2.emissions)
mydata_trans$Air.Pollution <- log(mydata$Air.Pollution)
mydata_trans$Protected.Land <- exp(mydata$Protected.Land)
mydata_trans$Women.Parliament <- (mydata$Women.Parliament)**2
mydata_trans$Education <- sqrt(mydata$Education)
mydata_trans$Child.Mortality <- log(mydata$Child.Mortality)
mydata_trans$Water <- log(mydata$Water)
mydata_trans$Youth.Unemployment <- exp(mydata$Youth.Unemployment)
mydata_trans$Hunger <- log(mydata$Hunger)
mydata_trans$Violence <- log(mydata$Violence)
mydata_trans$Sanitation <- log(mydata$Sanitation)
mydata_trans$Poverty <- log(mydata$Poverty)
mydata_trans$Alternative.Energy <- exp(mydata$Alternative.Energy)

# clean mydata_trans
is.na(mydata_trans) <- sapply(mydata_trans, is.infinite)
mydata_trans_clean <- na.omit(mydata_trans)
#chart.Correlation(mydata_trans_clean[4:18], histogram=TRUE, pch=19)


# Explore data availablity ########

### number of countries in cleaned data
mydata %>% summarise(n_distinct(country))
mydata_clean %>% summarise(n_distinct(country))
#summary(mydata_clean)

# countries and years appearing in cleaned data
country_occurances <- mydata_clean %>% group_by(country) %>% summarise(counts = n())
country_occurances$wb_group <- NA
for (i in seq(1,length(wb_countries)))
{country_occurances$wb_group[country_occurances$country == as.character(wb_countries[i])] <- wb_group_numbers[i]}
ggplot(country_occurances,aes(x=reorder(country,wb_group), y=counts,fill=wb_group))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 270))+
  labs(x="Country", y="Number of years with full data", fill="WB\nIncome\nClass")


# PCA - Reproduce full paper ########

data <- mydata_clean[4:18] # 18 not 19, because we removed Protected.Sea
# nice plot
result <- PCA(data,graph=FALSE)
fviz_screeplot(result,linecolor="red") + xlab("Principal Component")
fviz_pca_var(result, col.var="contrib")+
  scale_color_gradient2(low="cyan3", mid="mediumpurple",
                        high="deeppink1",midpoint=5)+theme_bw()+theme(panel.border = element_blank())


# PCA - Transformed, normally distributed data ####

data2 <- mydata_trans_clean[4:18]
# nice plot
result2 <- PCA(data2, graph=FALSE)
fviz_screeplot(result2) + xlab("Principal Component")
fviz_pca_var(result2, col.var="contrib")+
  scale_color_gradient2(low="cyan3", mid="mediumpurple",
                        high="deeppink1",midpoint=5)+theme_bw()+theme(panel.border = element_blank())


# PCA - WB income groups ######
# WB income levels: 1 Low, 2 Lower Middle, 3 Upper Middle, 4 High

#### Add goal numbers to variables

names(mydata_clean) <- c("country.year","country","year","G13.CO2.emissions","G12.Air.Polution","G15.Protected.Land","G4.Education","G5.Women.Parliament",
                         "G3.Child.Mortality","G6.Water","G10.GINI","G8.Youth.Unemployment","G2.Hunger","G16.Violence","G9.Internet","G11.Sanitation",
                         "G1.Poverty","G7.Alternative.Energy","wb_group")

### per income group:

names <- c("Low Income","Lower Middle Income","Upper Middle Income","High Income")
for (i in c(1,2,3,4))
  {
    name <- names[i]
    income_group <- income_groups[i]
    data = subset(mydata_clean, subset= wb_group %in% c(income_group), select=-c(wb_group))
    # get number of rows
    n_rows <- length(data$country)
    n_countries <- length(unique(data$country))
    n_countries_thisGroup <- wb_group_sizes$counts[wb_group_sizes$Income.group == income_group][1]
    
    # countries and years appearing in subsetted data
    wb_country_occurances <- data %>% group_by(country) %>% summarise(counts = n())
    ggplot(wb_country_occurances,aes(x=country,y=counts))+
      geom_bar(stat="identity", fill='steelblue')+
      geom_text(aes(label=counts), vjust=1.6, color="white", size=3.5)+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 310))+
      ggtitle(name)
    ggsave(paste("./outputs/figs_factor/barplot_",income_group,".png",sep=""))
    
    # perform PCA transform
    result <- PCA(data[4:18],graph=FALSE)

    # scree plot
    fviz_screeplot(result)+
      xlab("Principal Component")+
      ggtitle(paste(name," Scree Plot\nBased on ",n_countries," countries [of ",n_countries_thisGroup," in this group] over ",n_rows," rows.",sep="")) 
    ggsave(paste("./outputs/figs_factor/Scree_",income_group,".png",sep=""))
    
    # 2D plot
    fviz_pca_var(result, col.var="contrib", repel=TRUE)+
      scale_color_gradient2(low="cyan3", mid="mediumpurple", high="deeppink1",midpoint=5)+
      theme_bw()+theme(panel.border = element_blank())+
      ggtitle(paste(name,"\nBased on ",n_countries," countries [of ",n_countries_thisGroup," in this group] over ",n_rows," rows.",sep=""))
    ggsave(paste("./outputs/figs_factor/PCA_",income_group,".png",sep=""))
  }


### for L and LM combined:

name <- "Low and Lower Middle" #names[i]
income_group <- "L_LM" #income_groups[i]
data = subset(mydata_clean, subset= wb_group %in% c(income_groups[1],income_groups[2]), select=-c(wb_group))
# get number of rows
n_rows <- length(data$country)
n_countries <- length(unique(data$country))
n_countries_thisGroup <- (wb_group_sizes$counts[wb_group_sizes$Income.group == income_groups[1]][1] + wb_group_sizes$counts[wb_group_sizes$Income.group == income_groups[2]][1])
 
# countries and years appearing in subsetted data
wb_country_occurances <- data %>% group_by(country) %>% summarise(counts = n())
ggplot(wb_country_occurances,aes(x=country,y=counts))+
  geom_bar(stat="identity", fill='steelblue')+
  geom_text(aes(label=counts), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 310))+
  ggtitle(name)
ggsave(paste("./outputs/figs_factor/barplot_",income_group,".png",sep=""))

# perform PCA transform
result <- PCA(data[4:18],graph=FALSE)

# scree plot
fviz_screeplot(result)+
  xlab("Principal Component")+
  ggtitle(paste(name," Scree Plot\nBased on ",n_countries," countries [of ",n_countries_thisGroup," in these groups] over ",n_rows," rows.",sep="")) 
ggsave(paste("./outputs/figs_factor/Scree_",income_group,".png",sep=""))

# 2D plot
fviz_pca_var(result, col.var="contrib", repel=TRUE)+
  scale_color_gradient2(low="cyan3", mid="mediumpurple", high="deeppink1",midpoint=5)+
  theme_bw()+theme(panel.border = element_blank())+
  ggtitle(paste(name,"\nBased on ",n_countries," countries [of ",n_countries_thisGroup," in these groups] over ",n_rows," rows.",sep=""))
ggsave(paste("./outputs/figs_factor/PCA_",income_group,".png",sep=""))


# PCA per country: Bangladesh #
mydata_bang <- mydata[ which(mydata$country=='Bangladesh' & mydata$year>1989), ]
mydata_bang_clean <- na.omit(mydata_bang)[4:18]

result <- PCA(mydata_bang_clean,graph=FALSE)

# scree plot
fviz_screeplot(result)+
  xlab("Principal Component")+
  ggtitle("Scree Plot - Bangladesh") 
ggsave("./outputs/figs_factor/Scree_bangladesh.png")

# 2D plot
fviz_pca_var(result, col.var="contrib", repel=TRUE)+
  scale_color_gradient2(low="cyan3", mid="mediumpurple", high="deeppink1",midpoint=5)+
  theme_bw()+theme(panel.border = element_blank())+
  ggtitle("Bangladesh")
ggsave("./outputs/figs_factor/PCA_bangladesh.png")



# New Vito UN data (four countries) PCA ####

newdata <- na.omit(read.csv('./outputs/data/vito_reduced_common.csv'))

# Subset to chosen columns per goal
# X for R dataframe.
chosen_targets <- c('country','year',
                    'X1.4.1.SP_ACS_BSRVH2O.ALLAREA.BOTHSEX.ALLAGE',
                    'X2.1.1.SN_ITK_DEFC.ALLAREA.BOTHSEX.ALLAGE', # same
                    'X3.2.1.SH_DYN_IMRT.ALLAREA.BOTHSEX.YOUNG', # same
                    # 4 - Equal Good Education, no good data
                    'X5.5.1.SG_GEN_PARL.ALLAREA.FEMALE.ALLAGE', # same
                    'X6.2.1.SH_SAN_DEFECT.ALLAREA.BOTHSEX.ALLAGE',
                    'X7.1.1.EG_ELC_ACCS.ALLAREA.BOTHSEX.ALLAGE',
                    'X8.1.1.NY_GDP_PCAP.ALLAREA.BOTHSEX.ALLAGE',
                    'X9.b.1.NV_IND_TECH.ALLAREA.BOTHSEX.ALLAGE',
                    #'X10.b.1.DC_TRF_TOTL.ALLAREA.BOTHSEX.ALLAGE',
                    # 11 - no good data
                    # 12 - no good data
                    'CO2pc', # 13 - ADD IN CO2PC AS INDICATOR
                    # 14 - no good data
                    'X15.5.1.ER_RSK_LSTI.ALLAREA.BOTHSEX.ALLAGE',
                    # 16
                    'X17.8.1.IT_USE_ii99.ALLAREA.BOTHSEX.ALLAGE')

chosen <- subset(newdata, select=chosen_targets)

names(chosen) <- c('country','year',
                   'G1.Basic.Water',
                   'G2.Hunger',
                   'G3.Child.Mortality',
                   'G5.Women.Parliament',
                   'G6.Public.Defacation',
                   'G7.Electricy.Access',
                   'G8.GDP.pc.Growth',
                   'G9.High.Tech.Industry',
                   'G13.proxy.CO2.pc',
                   #'10.Dev.Assistance',
                   'G15.RedList',
                   'G17.Internet')

names(chosen)


# transform - code everythin between 0 and 1 (except GDPpc - log this), and higher is better:
chosen_trans <- chosen
chosen_trans$G1.Basic.Water <- chosen$G1.Basic.Water/100
chosen_trans$G2.Hunger <- 1 - (chosen$G2.Hunger/100)
chosen_trans$G3.Child.Mortality <- 1 - (chosen$G3.Child.Mortality/1000)
chosen_trans$G5.Women.Parliament <- chosen$G5.Women.Parliament/100
chosen_trans$G6.Public.Defacation <- 1 - (chosen$G6.Public.Defacation/100)
chosen_trans$G7.Electricy.Access <- chosen$G7.Electricy.Access/100
chosen_trans$G8.GDP.pc.Growth <- log(chosen$G8.GDP.pc.Growth)
chosen_trans$G9.High.Tech.Industry <- chosen$G9.High.Tech.Industry/100

minus_co2_temp <- -chosen$G13.proxy.CO2.pc
chosen_trans$G13.proxy.CO2.pc <- minus_co2_temp # take minus to invert

chosen_trans$G15.RedList #untouched, higher is better and already between 0 and 1
chosen_trans$G17.Internet <- chosen$G17.Internet/100
# clean any NAs that were created
chosen_trans <- na.omit(chosen_trans)

chart.Correlation(chosen_trans[-c(1)])

# PCA and plots

for (chosen_country in c('Bangladesh','Tanzania','Ethiopia','Lao PDR'))
  {
    forPCA <- subset(chosen_trans, subset= (country==chosen_country), select= -c(country,year))
    #chart.Correlation(forPCA, histogram=TRUE, pch=19)
    result <- PCA(forPCA,graph=FALSE)
    
    # scree plot
    fviz_screeplot(result)+
      xlab("Principal Component")+
      ggtitle(paste("Scree Plot - ",chosen_country," - New Data",sep="")) 
    ggsave(paste("./outputs/figs_factor/Scree_new_",chosen_country,".png",sep=""))
    
    # 2D plot
    fviz_pca_var(result, col.var="contrib", repel=TRUE)+
      scale_color_gradient2(low="cyan3", mid="mediumpurple", high="deeppink1",midpoint=5)+
      theme_bw()+theme(panel.border = element_blank())+
      ggtitle(paste(chosen_country,'- New Data'))
    ggsave(paste("./outputs/figs_factor/PCA_new_",chosen_country,".png",sep=""))
  }