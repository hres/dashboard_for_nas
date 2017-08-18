library(jsonlite)
library(stringr)


#scraping regulatory decision summary from DHPR:
#parse json file and extract table&links:

RDS_tb<-fromJSON('rdsResult.json',simplifyDataFrame = T)

#filter out withdrawn application, filter out medical device application
RDS_tb<-RDS_tb%>%filter(decision=="Approved")%>%
                 filter(is_md="FALSE")%>%
                 select(-application_number,-application_type,-din_list,-is_md,-summary_title,-din)
                 
#clean up din column:
# RDS_tb$din<-str_replace_all(RDS_tb$din,'[^\\d{8}|\\|\\d{8}]',"")


RDS_tb$link_id<-sapply(RDS_tb$link_id,function(x)paste0('https://hpr-rps.hres.ca/reg-content/regulatory-decision-summary-detail.php?lang=en&linkID=',x)) 

#Not able to scrape DHPR website:
# url<-'https://hpr-rps.hres.ca/reg-content/regulatory-decision-summary-detail.php?lang=en&linkID=RDS00024'
# 
# purpose<-read_html(url)%>%
#          html_node(xpath='//*[@id="Purpose"]/p[1]')%>%
#          html_text()

#scraping Summary basis of decision from DHPR:
#parse json file and extract table&links:
SBD_tb<-fromJSON('sbdResult.json',simplifyDataFrame = T)

#convert list to vector for din_list:
SBD_tb$din_list<-sapply(SBD_tb$din_list,function(x)paste(x,collapse='|'))
SBD_tb$din_list<-str_extract_all(SBD_tb$din_list,'\\d{8}')
SBD_tb$din_list<-sapply(SBD_tb$din_list,function(x)paste(x,collapse='|'))

SBD_tb$brand_name<-gsub("<sup>.*</sup>","",SBD_tb$brand_name)

#filter out medical devices and select columns
SBD_tb%<>%filter(is_md=="FALSE")%<>%select(brand_name,med_ingredient,date_issued,manufacturer,link_id)
SBD_tb$link_id<-sapply(SBD_tb$link_id,function(x)paste0('https://hpr-rps.hres.ca/reg-content/summary-basis-decision-detailTwo.php?lang=en&linkID=',x))

write.csv(RDS_tb,'RDS_tb.csv',row.names = F)
write.csv(SBD_tb,'SBD_tb.csv',row.names = F)