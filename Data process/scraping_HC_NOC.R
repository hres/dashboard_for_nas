library(rvest)



url<-'https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/notice-compliance/database/data-extract.html'

title<-url%>%
      read_html()%>%
      html_nodes('strong')%>%
      html_text()

title<-gsub("\\:.*","",title)

link<-url%>%
      read_html()%>%
      html_nodes('a')%>%
      html_attr('href')%>%
      str_subset("\\.txt")

if (!(file.exists("~/NOC"))) dir.create("~/NOC", recursive = TRUE)
for(i in seq_along(title[1:7]))
  filename[i]=paste0("~/NOC/",title[i],".txt")
  download.file(link, filename,mode="wb")
  

ncfiles <- list.files("~/NOC", pattern = ".*txt")

nctables <- list()

for (i in ncfiles) {
  varname <- i %>% 
    str_extract(regex(".*(?=\\.txt$)"))
  
  ncfilepath <- paste0("~/NOC/", i)
  nctables[[varname]] <- read.delim(ncfilepath,
                                     header = FALSE,
                                     quote="\"",
                                     sep = ",",
                                     stringsAsFactors = FALSE,
                                     fileEncoding="latin1")
  
  print(paste0(varname, " read!"))
}

#extract variable names for all tables:
nameurl<-'https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/notice-compliance/database/read-file.html'

ncreadme<-html_session(nameurl)
ncvar<-html_table(ncreadme)

nctablename<-ncreadme%>%
  html_nodes("caption")%>%
  html_text()

var <- list()
for (i in 1:length(nctablename)) var[[nctablename[i]]] <- ncvar[[i]]$`Name`

var<-var[order(names(var))]
#assign variable names to text files:
for (i in 1:7) { names(nctables[[i]]) <- var[[i]]}



#Mapping NOC files:
NOC_product_ingredient<-nctables[[5]]
NOC_main<-nctables[[3]]
NOC_DIN_product<-nctables[[2]]
NOC_brand<-nctables[[1]]

NOC_drug<-left_join(NOC_product_ingredient[,c(1,3,5,6,7)],NOC_brand[,c(1,3)],by="NOC_NUMBER")%>%
          left_join(NOC_DIN_product[,c(1,4)])%>%distinct()

NOC_main%<>%select(-contains("_FR_"))

NOC_final_table<-NOC_drug%>%left_join(NOC_main)
NOC_final_table[NOC_final_table==""]<-NA

#clean up NOC tab for Report App:
NOCtab<-fread('NOC_mapping.csv',header=T,sep=',',stringsAsFactors = F)%>%
  select(Ingredient=NOC_PI_MEDIC_INGR_ENG_NAME,
         BrandName=NOC_BR_BRANDNAME,
         Date=NOC_DATE,
         Manufacturer=NOC_MANUFACTURER_NAME,
         Submission_Type=NOC_ENG_SUBMISSION_TYPE,
         Reason_supplement=NOC_ENG_REASON_SUPPLEMENT,
         DIN=NOC_DP_DIN)

#format NOC date
NOCtab$Date<-gsub("^9","199",NOCtab$Date)
NOCtab$Date[grep("^[^199]",NOCtab$Date)]<-paste0("20",NOCtab$Date[grep("^[^199]",NOCtab$Date)])
NOCtab$Date<-ymd(NOCtab$Date)

write.csv(NOCtab,"NOC_mapping.csv",row.names=F)

#
write.csv(NOC_final_table,"NOC_mapping.csv",row.names = F)
  