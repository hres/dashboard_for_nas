shinyServer(function(input, output) {

drugSearch<-reactiveValues() 
faers_query <- reactiveValues()

observe({
  if(input$name_type=='Brand Name'){
    name<-input$brand
  }else{
    name<-input$ing
  }
  
  
drugSearch$name<-name
})

#build CV search query:

CV_query<-reactive({
  
  withProgress(message="Querying Canada Vigilance Database",value=0,{
  
  cv_report_drug_filtered <- cv_report_drug
  if (input$name_type == "Brand Name" & drugSearch$name!='') {
  cv_report_drug_filtered %<>% filter(DRUGNAME == drugSearch$name)
  }else if(input$name_type == "Active Ingredient" & drugSearch$name!="") {
      related_drugs <- cv_drug_product_ingredients %>% filter(ACTIVE_INGREDIENT_NAME == tolower(drugSearch$name))
      cv_report_drug_filtered %<>% semi_join(related_drugs, by = "DRUG_PRODUCT_ID")
    }
  incProgress(0.4)
  
  selected_ids <-  cv_reports%>%
    semi_join(cv_report_drug_filtered)%>%as.data.frame()  
  
  incProgress(0.9)
   
  return(selected_ids)
  })
})


output$cv<-renderPlotly({
  id<-CV_query()
  
   validate(
     need(nrow(id)>0,"No record in Canada Vigilance Database from the search")
   )
  
  id<-id%>%mutate(Month = floor_date(ymd(DATINTRECEIVED_CLEAN), "month")) %>%
          group_by(Month)%>%
          summarise(n=n_distinct(REPORT_ID))%>%
          mutate(Month=format(as.Date(Month),"%Y-%m"))
      
  
  mytitle<-paste("<b>Health Canada:</b>Adverse Event Report counts for",drugname(),"Total:",prettyNum(sum(id$n)))
                 
  p<-plot_ly(id,x=~Month,y=~n,type='scatter',mode="lines")%>%
    layout(title=mytitle,xaxis=list(title="",autotick=T,tickangle=-25),yaxis=list(title=""))
})
 

output$cv_event<-DT::renderDataTable({
  id<-CV_query()
  
  validate(
    need(nrow(id)>0,"No record in Canada Vigilance Database from the search")
  )
  
  id<-cv_reactions%>%semi_join(id,copy=T)%>%
      as.data.frame()%>%
      group_by(PT_NAME_ENG)%>%
      tally(sort=T)
  
  DT::datatable(id,
  options=list(pageLength=10)
  )

  
})

#build FDA search plot:
openfda_query <-reactive({
  
withProgress(message="Querying OpenFDA",value=0,{
  
openfda_query<-fda_query("/drug/event.json")

if(input$time_selection=="Since 2016-01-01"){
query_str <- paste0("[","2016-01-01", "+TO+",Sys.Date(), "]")
}else{
query_str<- paste0("[","1989-06-30", "+TO+",Sys.Date(), "]")
}

incProgress(0.1)
openfda_query %<>% fda_filter("receiptdate", query_str)

incProgress(0.5)

if(drugSearch$name!="") {
  query_str <- paste0('"', gsub(" ", "+",drugSearch$name), '"')
  query_str_combine <- sprintf('(%s)', paste0(query_str, collapse = '+'))
  
  if (input$name_type == "Brand Name") {
    openfda_query %<>% fda_filter("patient.drug.openfda.brand_name.exact", query_str_combine)
  } else {
    openfda_query %<>% fda_filter("patient.drug.openfda.substance_name.exact", query_str_combine)
  }
}

incProgress(0.8)

result <- openfda_query %>% fda_search() %>% fda_limit(1) %>% fda_exec()
validate(
  need(!is.null(result),"There is no reports for selected drug within the time period chosen")
)
return(openfda_query)

 })
})


drugname<-reactive({
  s<-drugSearch$name
  
  if(s=='') {
    s <- 'All'
  }
  
  return(s)
})

output$fda<-renderPlotly({
  
  
  total_results <- openfda_query() %>%
    fda_count("receiptdate") %>%
    fda_limit(1000) %>%
    fda_exec() %>%
    mutate(month = floor_date(ymd(time), "month")) %>%
    mutate(month=format(as.Date(month),"%Y-%m"))%>%
    count(month, wt = count)

  
  
  totalreport<-sum(total_results$n)
  mytitle<-paste("<b>FDA:</b>Adverse Event Report counts for",drugname(),"Total:",prettyNum(totalreport))
  p<-plot_ly(total_results,x=~month,y=~n,type='scatter',mode='lines')%>%
    layout(title=mytitle,xaxis=list(title="",autotick=T,tickangle=-25),yaxis=list(title=""))
})

output$fda_event<-DT::renderDataTable(
  eventcount<-openfda_query()%>%
              fda_count("patient.reaction.reactionmeddrapt.exact")%>%
              fda_limit(1000)%>%
              fda_exec()%>%
              rename(Adverse_event_preferred_term=term),
  
  options=list(pageLength=10)
  
)

tableselection<-function(x,y,z){
  
  table<-x
  
  if(drugSearch$name!=""){
    if(input$name_type=="Active Ingredient"){
      line<-str_detect(table[[y]], regex(drugSearch$name, ignore_case = TRUE))
      table%<>%filter(line)
    }else{
      line<-str_detect(table[[z]], regex(drugSearch$name, ignore_case = TRUE))
      table%<>%filter(line)
    }
  }
  
  return(table)
}



output$drugtable<-DT::renderDataTable(
  tableselection(dpd_new,"ingredient","brand_name")%>%select(DIN=drug_identification_number,Product_Name=brand_name,Ingredient=ingredient,
                   strength,strength_unit,Date=history_date,Status=status,Indication=Indication,ATC=ATC),
  options=list(pageLength=25,scrollX=T)
)



output$company<- renderPlotly({
  
  company_count<-dpd_new%>%distinct(ingredient,company_name)%>%group_by(company_name)%>%tally()
  
  p<-plot_ly(company_count,x=~n,y=~reorder(company_name,n),type="bar",orientation='h')%>%
     layout(margin=list(l=350),
            title="Count of Manufacturers",
            xaxis=list(
              title="",
              showticklabels=TRUE,
              tickfont=list(family='Arial,sans-serif',size=14)
            ),
            yaxis=list(title=''))
})


output$status<-renderPlotly({
  
  count<-dpd_new%>%distinct(ingredient,status)%>%count(status)
  status_count<-bind_rows(data.frame(status="Total Product",n=n_distinct(dpd_new$drug_identification_number)),count)
  
  p<-plot_ly(status_count,x=~status,y=~n,type="bar")%>%
    layout(
           title="Count of NAS status",
           xaxis=list(
             title="",
             showticklabels=TRUE
           ),
           yaxis=list(title='')
           )
  
})

output$ATC<-renderPlotly({
  atc_count<-dpd_new%>%group_by(ATC)%>%tally()%>%mutate(percent=round(100*n/sum(n),1))
  
  p<-plot_ly(atc_count,labels=~ATC,values=~percent,type="pie")%>%
     layout(title="Active substance by ATC categories")
})

#build pubmed tab:
pubmeddata<-reactive({

withProgress(message="Querying Pubmed...",value=0.1,{
  

validate(
  need(drugSearch$name!="","Please select a drugname to search in Pubmed")
)

  if(drugSearch$name!=""){
  search_query<-EUtilsSummary(drugSearch$name,type='esearch',retmax=500,mindate=2006,maxdate=2016)
  
  incProgress(0.3)
  records<-EUtilsGet(search_query)}
  
  incProgress(0.8)
  pubmed_data<-data.frame('Title'=ArticleTitle(records),'Journal'=MedlineTA(records),
                          'Country'=Country(records),'Year_published'=YearPubmed(records))
  
  return(pubmed_data)
  })
})

output$pubtime<-renderPlotly({
  timecount<-pubmeddata()%>%group_by(Year_published)%>%tally()
  
  mytitle<-paste("Number of Pubmed articles containing",drugSearch$name,"since 2006")
  p<-plot_ly(timecount,x=~Year_published,y=~n,type='bar')%>%
    layout(title=mytitle,
           xaxis=list(title="Year Published",autotick=F),
           yaxis=list(title=""))
})


output$pubcountry<-renderPlotly({
  countrycount<-pubmeddata()%>%group_by(Country)%>%tally()
  
  p<-plot_ly(countrycount,x=~n,y=~reorder(Country,n),type='bar',orientation='h')%>%
    layout(margin=list(l=150),
           title="Count of Countries for Pubmed articles",
           xaxis=list(
             title="",
             showticklabels=TRUE,
             tickfont=list(family='Arial,sans-serif',size=14)
           ),
           yaxis=list(title=''))
})

output$pubjournal<-renderPlotly({
  
  journalcount<-pubmeddata()%>%group_by(Journal)%>%tally()
  
  p<-plot_ly(journalcount,x=~n,y=~reorder(Journal,n),type="bar",orientation='h')%>%
    layout(margin=list(l=250),
           title="Count of Journals for Pubmed articles",
           xaxis=list(
             title="",
             showticklabels=TRUE,
             tickfont=list(family='Arial,sans-serif',size=14)
           ),
           yaxis=list(title=''))
})


output$pubtable<-DT::renderDataTable(
  pubmeddata(),
  options=list(pageLength=10,
               scrollX=T)
)



#build tab for advisory:
tableselection2<-function(x){
  table<-x
  
  if(drugSearch$name!=""){
    table%<>%filter(str_detect(table[[1]], regex(drugSearch$name, ignore_case = TRUE)))
  }
  
  return(table)
}



output$HCtablename<-renderText({
  
  s <- drugSearch$name
  if(s=='') {
    s <- 'All'
  }
  out <- paste( '<h4><b>Advisory issued by Health Canada for:<i>', s, '</i></b><br><br>',
                "Number of advisories:",prettyNum(nrow(tableselection2(HCadv))))
  return(out)
})

output$HCadvise<-DT::renderDataTable({
  
  table<-tableselection2(HCadv)
  validate(
    need(nrow(table)>0,"There is no advisory issued by Health Canada for selected product")
  )
  
  table<-table%>%mutate(link = paste("<a href=",link, "target=_blank>", link, "</a>"))%>%
            select(Title=title,Type=Type_of_communication,Date=date,Content=content,Link=link)
  DT::datatable(table,escape=F,
  options=list(pageLength=5,scrollX=T))
})



output$FDAadvise<-DT::renderDataTable({
  
  table<-tableselection2(USadv)
  validate(
    need(nrow(table)>0,"There is no safety alert issued by FDA for selected product since 2009")
  )
  
  table<-table%>%mutate(Link = paste("<a href=",Link, "target=_blank>", Link, "</a>"))%>%
    select(Content=Product.Name,Date=Date.Issued.Updated,Link)
  DT::datatable(table,escape=F,
                options=list(pageLength=5))
})

output$FDAtablename<-renderText({
  
  s <- drugSearch$name
  if(s=='') {
    s <- 'All'
  }
  out <- paste( '<h4><b>Safety alert issued by FDA for:<i>', s, '</i></b><br><br>',
                "Number of advisories:",prettyNum(nrow(tableselection2(USadv))))
  return(out)
})
######tab for NOC:


output$NOCtitle<-renderText({
  s<-drugname()
  
  out<-paste0("<h4><b>Health Canada:</b>Notice of Compliance for","<i>",s,"</i></h4>",
             "<b>Data Source:</b>",
             "<a href='https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/notice-compliance/database/data-extract.html'",
             "target='_blank'> Notice of Compliance Data Extract</a><br>",
             "<b>Last Updated:</b> August 18,2017")
  return(out)
    
})

output$NOCtable<-DT::renderDataTable({
  
  table<-tableselection(NOCtab,"Ingredient","BrandName")%>%arrange(desc(Date))
  validate(
    need(is.data.frame(table)&nrow(table)>0,'There is no Notice of Compliance available for selected Drug')
  )
  
  DT::datatable(table,options=list(pageLength=10,ScrollX=T,autoWidth=T))
}
)


#tab for labeling change:
output$labelchangetitle<-renderText({
  s<-drugname()
  
  out<-paste0('<h4><b>Health Canada:</b>Product Monograph Brand Safety updates for ',s,'since June 2016</h4>',
             '<b>Data Source:</b>',
             "<a href='https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database/label-safety-assessment-update/product-monograph-brand-safety-updates.html'",
             "target='_blank'> Product Monograph Brand Safety updates</a><br>")
             
  return(out)
})

output$labelchangetable<-DT::renderDataTable({
  table<-tableselection(label_change,"ingredient","brandname")%>%rename(Updates=`Reason for submission`,`Brand Name`=brandname)
  validate(
    need(nrow(table)>0,"There is no Product Monograph Brand safety updates for selected product since June 2016")
  )
  
  
  DT::datatable(table,options=list(pageLength=10,ScrollX=T))

})


output$safereviewtitle<-renderText({
  s<-drugname()
  
  out<-paste('<h4><b>Health Canada:</b>Safety Review completed for',s,'since 2014')
  return(out)
})




output$safereviewtable<-DT::renderDataTable({
  
  table<-tableselection2(safety_review)
  
  validate(
    need(nrow(table)>0,"There is no Safety Review conducted for selected product since 2014")
  )
  
  table<-table%>%mutate(Link = paste("<a href=",Link, "target=_blank>", Link, "</a>"))

  DT::datatable(table,escape=F,
                options=list(pageLength=10,scrollX=T))
  
})


output$rdstitle<-renderText({
  s<-drugname()
  
  out<-paste('<h4><b>Health Canada:</b>Regulatory Desicion Summary for',s)
  return(out)
})

output$rdstable<-DT::renderDataTable({
  
  table<-tableselection(RDS_tb,"medical_ingredient","drugname")
  
  validate(
    need(nrow(table)>0,"There is no Regulatory Decision Summary for selected product since 2014")
  )
  
  table<-table%>%mutate(link_id = paste("<a href=",link_id, "target=_blank>", link_id, "</a>"))%>%
                 select(`Drug Name`=drug_name,`Medicinal ingredient`=medical_ingredient,Manufacturer=manufacturer,
                        Outcome=decision,Date=date_decision,`Type of Submission`=type_submission,Link=link_id)
  
  DT::datatable(table,escape=F,
                options=list(pageLength=10,scrollX=T))
})

output$sbdtitle<-renderText({
  s<-drugname()
  
  out<-paste('<h4><b>Health Canada:</b>Summary Basis of Decision for',s)
  return(out)
})

output$sbdtable<-DT::renderDataTable({
  
  table<-tableselection(SBD_tb,"med_ingredient","brandname")
    
  
  validate(
    need(nrow(table)>0,"There is no Summary Basis of Decision for selected product since 2014")
  )
  
  table<-table%>%mutate(link_id = paste("<a href=",link_id, "target=_blank>", link_id, "</a>"))%>%
                 select(`Drug Name`=brand_name,`Medicinal ingredient`=med_ingredient,Manufacturer=manufacturer,
                         Date=date_issued,Link=link_id)
  
  DT::datatable(table,escape=F,
                options=list(pageLength=10,scrollX=T))
})

})
