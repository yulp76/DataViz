library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rJava)
library(xlsxjars)
library(xlsx)

setwd("D:/OneDrive/Chicago/0 - Academic/2017 Fall/CAPP 30239 - Data Visualization/Repo/DataViz/Static-1")


# Graph 1
country <- read.xlsx("Country Code.xls", 1) %>% 
  mutate(Code = as.character(CODE), Country = as.character(Country))
NODA <- read_csv("NODA.csv") %>%
  filter(TIME == 2015) %>% 
  select(LOCATION,Value) %>%
  rename(Million_USD = Value) %>% 
  mutate(LOCATION = as.character(LOCATION))
PCT_GNI <- read_csv("NODA_PC_GNI.csv") %>% 
  filter(TIME == 2015) %>% 
  select(LOCATION,Value) %>%
  rename(Pct_GNI = Value) %>% 
  mutate(LOCATION = as.character(LOCATION))

df <- NODA %>% 
  left_join(PCT_GNI, by = "LOCATION") %>% 
  left_join(country, by = c("LOCATION" = "Code")) %>% 
  na.omit() %>% 
  mutate(GNI = Million_USD/(Pct_GNI/100)/1000)

df$threshold <- cut(df$Pct_GNI, breaks = c(0, 0.7, 100),
               labels = c("<=0.7", ">0.7"))

ggplot(data=df, aes(x=GNI, y=Million_USD)) +
  geom_point(aes(color=threshold, size=Pct_GNI), alpha=0.35)+
  geom_text(size=2.5, hjust=0.5, vjust=0.1, angle=20, aes(label=Country))+
  geom_line(stat="smooth", method="loess", color="maroon", alpha=0.8)+
  scale_x_log10()+
  scale_y_log10()+
  scale_size_continuous(range = c(10, 20))+
  theme(legend.position = "bottom",
        axis.text=element_text(color="maroon"),
        axis.title=element_text(size=10,face="bold"),
        title=element_text(size=12, face="bold"))+
  labs(x="GNI in Brillion USD", 
       y="ODA in Million USD",
       title="Graph 1. What is the Relationship between Foreign Aid and National Income",
       subtitle="Net Official Development Assistance (ODA) and Gross National Income (GNI) for Donor Countries in 2015\n*Regression Line Model: Local Polynomial Regression",
       color="Compared to 0.7% ODA/GNI Target",
       size="ODA/GNI in %")

# Graph 2
income <- read_csv("ODA by Income.csv") %>% 
  gather(Recipient, Percentage, -Country)

ggplot(data=income, aes(x=Country, y=Percentage, fill=factor(Recipient, levels=c("Least Developed",
                                                                         "Other Low Income",
                                                                         "Lower Middle Income",
                                                                         "Upper Middle Income"))))+
  geom_bar(stat = "identity")+
  labs(title="Graph 2. Do the Poorest Countries Receive the Most Assistance?",
       subtitle="Decompostion of Aid by Recipient Country Income Level\nfor Development Assistance Committee Countries in 2014-2015",
       fill="Recipient Countries by Income Level") +
  theme(legend.position = "bottom",
        axis.text=element_text(color="maroon"),
        axis.text.x=element_text(angle = 90, hjust = 1),
        axis.title=element_text(size=10,face="bold"),
        title=element_text(size=12, face="bold"))

# Graph 3
sector <- read_csv("us_foreign_aid_sector.csv") %>% 
  filter(fiscal_year == 2015, transaction_type_name == "Disbursements") %>%
  mutate(amount = current_amount/1000000) %>% 
  select(sector_category_name, sector_name, amount)

ggplot(data=sector, aes(x=amount, y=sector_name))+
  geom_segment(aes(xend=0, yend=sector_name))+
  scale_x_log10()+
  geom_point()+
  facet_grid(sector_category_name ~ ., scales="free_y")+
  labs(x="Amount of Disbursements in Million USD", 
       y="Sector",
       title="Graph 3. How is US Foreign Aid Spent?",
       subtitle="Breakdown by Sector of US Foreign Aid Disbursements in 2015")+
  theme(axis.text=element_text(color="maroon"),
        axis.title=element_text(size=10,face="bold"),
        title=element_text(size=12, face="bold"),
        strip.text.y=element_text(size=6, color="white"),
        strip.background=element_rect(fill="maroon"),
        panel.grid.minor.x=element_blank(),
        axis.ticks=element_blank())
