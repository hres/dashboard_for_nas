library(rvest)

#scraping safety reviews conducted by health canada:

url<-'http://www.hc-sc.gc.ca/dhp-mps/medeff/reviews-examens/ssr-rei-eng.php'





link<-url%>%
      read_html()%>%
      html_nodes(xpath='/html/body/div/div/div[6]/div[2]/div/ul/li/a')%>%
      html_attr('href')

title<-url%>%
  read_html()%>%
  html_nodes(xpath='/html/body/div/div/div[6]/div[2]/div/ul/li')%>%
  html_text()




safety_review<-data_frame(title,link)%>%slice(1:146)
safety_review%<>%mutate(title=gsub("Summary Safety Review -","",title))%<>%
               mutate(Date=str_extract(title,"\\d{4}-\\d{2}-\\d{2}"))

safety_review%<>%mutate(title=gsub("\\[\\d{4}-\\d{2}-\\d{2}\\]","",title))%<>%
               select(Title=title,Date,Link=link)

safety_review[grep("^/dhp-mps",safety_review$Link),'Link']<-sapply(safety_review[grep("^/dhp-mps",safety_review$Link),'Link'],
                                                                   function(x)paste0('www.hc-sc.gc.ca',x))

write.csv(safety_review,"safety_review.csv",row.names = F)