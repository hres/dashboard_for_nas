library(shiny)
library(shinydashboard)
library(RISmed)  #pubmed query package
library(dplyr)
library(magrittr)
library(plotly)
library(openfda)
library(pool)
library(data.table)
library(lubridate)
library(stringr)
library(DT)


hcopen <- dbPool(drv=RPostgreSQL::PostgreSQL(),
                 host     = "shiny.hc.local",
                 dbname   = "hcopen",
                 user     = "hcreader",
                 password = "canada1")

data_date   <- "20160630"

table_name_cv_drug_product_ingredients <- paste0("cv_drug_product_ingredients_", data_date)
table_name_cv_reports<- paste0("cv_reports_", data_date)
table_name_cv_report_drug<- paste0("cv_report_drug_indication_joined_", data_date)
table_name_cv_reactions<- paste0("cv_reactions_meddra_", data_date)

cv_reports<- tbl(hcopen, table_name_cv_reports)%>%filter(DATINTRECEIVED_CLEAN >="2015-01-01")%>%select(REPORT_ID,DATINTRECEIVED_CLEAN)
cv_report_drug<- tbl(hcopen, table_name_cv_report_drug)%>%select(REPORT_ID,DRUGNAME,DRUG_PRODUCT_ID)
cv_drug_product_ingredients <- tbl(hcopen, table_name_cv_drug_product_ingredients)%>%select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME)
cv_reactions<- tbl(hcopen, table_name_cv_reactions)%>%select(REPORT_ID,PT_NAME_ENG)


dpd_new<-read.csv('NAS2016.csv',header=T,sep=",",stringsAsFactors = F,encoding='UTF-8')%>%distinct()

drugchoice<-dpd_new%>%distinct(brand_name)%>%`$`("brand_name")%>%sort()
ingchoice<-dpd_new%>%distinct(ingredient)%>%`$`("ingredient")%>%sort()

#advisory issued by Health Canada from 2016-March
HCadv<-fread('HCAdvisory.csv',sep=',',header=T,stringsAsFactors = F)
USadv<-fread('USadv.csv',sep=',',header=T,stringsAsFactors = F)

#notice of compliance table:
NOCtab<-fread('NOC_mapping.csv',header=T,sep=',',stringsAsFactors = F)
        # select(Ingredient=NOC_PI_MEDIC_INGR_ENG_NAME,
        #        BrandName=NOC_BR_BRANDNAME,
        #        Date=NOC_DATE,
        #        Manufacturer=NOC_MANUFACTURER_NAME,
        #        Submission_Type=NOC_ENG_SUBMISSION_TYPE,
        #        Reason_supplement=NOC_ENG_REASON_SUPPLEMENT,
        #        DIN=NOC_DP_DIN)

#format NOC date
# NOCtab$Date<-gsub("^9","199",NOCtab$Date)
# NOCtab$Date[grep("^[^199]",NOCtab$Date)]<-paste0("20",NOCtab$Date[grep("^[^199]",NOCtab$Date)])
# NOCtab$Date<-ymd(NOCtab$Date)

label_change<-fread('label_changed.csv',header=T,sep=",",stringsAsFactors = F)
safety_review<-fread("safety_review.csv",header=T,sep=",",stringsAsFactors = F)
RDS_tb<-fread("RDS_tb.csv",header=T,sep=",",stringsAsFactors = F)
SBD_tb<-fread("SBD_tb.csv",header=T,sep=",",stringsAsFactors = F)