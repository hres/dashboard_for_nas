library(rvest)


#scraping us FDA safety alerts for drug medical products:

allurl<-c('http://wayback.archive-it.org/7993/20170110235424/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm479348.htm',
          'http://wayback.archive-it.org/7993/20170111132847/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm428326.htm',
          'http://wayback.archive-it.org/7993/20170111132857/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm380008.htm',
          'http://wayback.archive-it.org/7993/20170111132858/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm333878.htm',
          'http://wayback.archive-it.org/7993/20170111132914/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm285497.htm',
          'http://wayback.archive-it.org/7993/20170111132925/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm238512.htm',
          'http://wayback.archive-it.org/7993/20170111132935/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm196258.htm',
          'http://wayback.archive-it.org/7993/20170111132947/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm091428.htm',
          'http://wayback.archive-it.org/7993/20170111133047/http://www.fda.gov/Safety/MedWatch/SafetyInformation/SafetyAlertsforHumanMedicalProducts/ucm152982.htm')
         

table<-list()
link<-list()
table2<-list()
    
for(i in seq_along(allurl)){
  table[[i]]<-allurl[i]%>%
    read_html()%>%
    html_node(xpath='//*[@id="customTable"][1]')%>%
    html_table()
  
  
  link[[i]]<-allurl[i]%>%
    read_html()%>%
    html_nodes(xpath='//*[@id="customTable"][1]/tbody/tr/td/a')%>%
    html_attr('href')%>%
    `[`(1:nrow(table[[i]]))
  
  table2[[i]]<-data.frame(table[[i]],Link=paste0('http://wayback.archive-it.org',link[[i]]))
}
      

#consolidate all data frames:
USadv<-do.call("bind_rows",table2)

USadv$Date.Issued.Updated<-mdy(USadv$Date.Issued.Updated)

write.csv(USadv,"USadv.csv",row.names = F)
