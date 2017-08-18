# figure out newly approved substance within the last 12 months:

# hcopen <- dbPool(drv =RPostgreSQL::PostgreSQL(),
#                  host     = "shiny.hc.local",
#                  dbname   = "hcopen",
#                  user     = "hcreader",
#                  password = "canada1")
# 
# date<-"20170403"
# tablename<-function(x){
#   paste0(x,'_',date)
# }
# dpd_status<-tbl(hcopen,tablename('dpd_status'))%>%as.data.table()
# 
# #configure data:

dpd_status_all$HISTORY_DATE<-as.Date(dpd_status_all$HISTORY_DATE,format='%d-%b-%Y')

# #From Notice of compliance table: get new drug submission
# NAS<-NOCtab%>%filter(Submission_Type=="New Drug Submission (NDS)")%>%
#   mutate(Date=ymd(Date))%>%
#   filter(Date>='2016-01-01',Date<'2017-08-01')%>%
#   distinct(DIN)
#NAS%<>%mutate(DIN=trimws(DIN))
#dpd_drug_all$DRUG_IDENTIFICATION_NUMBER<-trimws(dpd_drug_all$DRUG_IDENTIFICATION_NUMBER)

dpd_new<-dpd_drug_all%>%filter(CLASS!="Veterinary")%>%
           `[`(c(2,5,6,10))%>%
           left_join(dpd_ingred_all[,c(2,4,6,7)],by="DRUG_CODE")%>%
           left_join(dpd_status_all[,c(2,3,4,5)],by="DRUG_CODE")

#filter out active ingredients that has been approved in another drug before:

dpd_new<-dpd_new%>%group_by(INGREDIENT,STATUS)%>%slice(which.min(HISTORY_DATE))
dpd_new%<>%filter(HISTORY_DATE>='2017-01-01')
dpd_new%<>%filter(STATUS!='DORMANT')

#link table to company names
dpd_new%<>% left_join(dpd_comp_all[,c(2,5,15)])

#list of NAS 2016 from online report:
NAS_2016<-c("Lancora","Lixiana","Praluent","Uptravi","Zontivity",
            "Blexten","Rupatadine","Taltz","Dotarem","MDK-Nitisinone",
            "Nitisinone Tablets","Orfadin","Ravicti","Epclusa","Sunvepra",
            "Xtoro","Zepatier","BAT","Bridion","Brivlera", "Zinbryta","Adynovate",
            "Afstlya","Alecensaro","Cotellic","Darzalex","Empliciti","Ibrance","Idelvion",
            "Kyprolis","Lynparza","Ninlaro","Praxbind","Tagrisso","Venclexta","Cinqair","Orkambi")

NAS_2016<-toupper(NAS_2016)

dpd_new%<>%filter(BRAND_NAME%in%NAS_2016)

colnames(dpd_new)<-tolower(colnames(dpd_new))

dpd_new%<>%left_join(newly_marketed[,c(1,20,21)])
                  

####NAS in 2017
#RDS csv file download from DHPR
RDS<-fread('rdsResult (1).csv',header=T,sep=',',stringsAsFactors = F)

RDS%<>%filter(grepl("^New Drug Submission",`Type of submission / Type of application`))

RDS$`Decision date`<-ymd(RDS$`Decision date`)

RDS%<>%filter(`Decision date`>='2017-01-01')
RDS%<>%select(`Drug/device name`)

dpd_new2017<-dpd_drug_all%>%filter(CLASS!="Veterinary")%>%
  `[`(c(2,5,6,10))%>%
   semi_join(RDS,by=c('BRAND_NAME'='Drug/device name'))%>%
  left_join(dpd_ingred_all[,c(2,4,6,7)],by="DRUG_CODE")%>%
  left_join(dpd_status_all[,c(2,3,4,5)],by="DRUG_CODE")


dpd_new2017$INGREDIENT<-gsub("\\(.*\\)","",dpd_new2017$INGREDIENT)

dpd_new2017<-dpd_new2017%>%group_by(INGREDIENT,STATUS)%>%slice(which.min(HISTORY_DATE))
dpd_new2017%<>%filter(HISTORY_DATE>='2017-01-01')
dpd_new2017%<>%filter(STATUS!='DORMANT')
dpd_new2017%<>% left_join(dpd_comp_all[,c(2,5,15)])  

write.csv(dpd_new2017,'dpd_new2017.csv',row.names = F)

#################################################
#Add two ingredients infliximab and clozapine
# dpd_add<-dpd_ing[,c(1,2,3,5,6)]%>%filter(ingredient=="INFLIXIMAB"|ingredient=="CLOZAPINE")%>%
#          left_join(dpd_drug_all[,c(1,3,4,5,7,9,10,11)],by="drug_code")%>%
#          left_join(dpd_company[,c(1,4,14,15)],by="drug_code")%>%
#          left_join(dpd_status)%>%
#          filter(status=="MARKETED")
#          
# dpd_add<-dpd_add%>%group_by(ingredient)%>%mutate(first_marketed=min(history_date))
# indication_add<-data.frame(ingredient=c("INFLIXIMAB","CLOZAPINE"),Indication=c("Crohn's disease|Rheumatoid Arthritis","Schizophrenia"))
# atc_add<-data.frame(ingredient=c("INFLIXIMAB","CLOZAPINE"),ATC=c("ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS(L)","NERVOUS SYSTEM(N)"))
# 
# dpd_add%<>%left_join(atc_add)%<>%left_join(indication_add)
# dpd_add%<>%select(colnames(dpd_new))
# dpd_add$drug_code<-as.integer(dpd_add$drug_code)
# dpd_add$drug_identification_number<-as.integer(dpd_add$drug_identification_number)
# dpd_add$number_of_ais<-as.integer(dpd_add$number_of_ais)
# dpd_add$ai_group_no<-as.integer(dpd_add$ai_group_no)
# dpd_add$active_ingredient_code<-as.integer(dpd_add$active_ingredient_code)
# dpd_add$strength<-as.integer(dpd_add$strength)
# dpd_add$history_date<-as.character(dpd_add$history_date)
# dpd_add$first_marketed<-as.character(dpd_add$first_marketed)
# dpd_new<-bind_rows(dpd_add,dpd_new)
#####################################


write.csv(dpd_new,"NAS2016.csv",row.names = F)



#map ing to ATC code for categories:
dpd_new<-dpd_new%>%
         mutate(ingredient2=tolower(ingredient))%>%
         left_join(drugbank[,c(1,2)],by=c("ingredient2"="ACTIVE_INGREDIENT_NAME"))%>%
         select(-ingredient2)

dpd_new<-left_join(dpd_new,dpd_drug_all[,c(1,4)])