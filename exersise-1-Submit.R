library(dplyr)
library(tidyr)
refine_original<-read.csv("refine_original.csv", header=TRUE, as.is = TRUE)
refine_original
##Chapter 3 Exercise 1 question #1
vec_philips<- c("phillipS","phillps", "phllips","philips","phillips",
                "Phillips","phillips","fillips","phlips")
vec_akzo<- c("akzo",	"Akzo",	"AKZO","akz0","ak zo","akzo","akzo")
vec_van_houten<- c("Van Houten","van Houten","van houten","van houten","Van Houten")
vec_unilever<- c("unilever",	"Unilever",	"unilever",	"unilver")

refine_original$ï..company[refine_original$ï..company %in% vec_philips] <- "philips"
refine_original$ï..company[refine_original$ï..company %in% vec_akzo]<- "akzo"
refine_original$ï..company[refine_original$ï..company%in%vec_van_houten]<-"van houten"
refine_original$ï..company[refine_original$ï..company%in%vec_unilever]<-"unilever"
##Chapter 3 Exercise 1 question #2
refine_original<-separate(refine_original,Product.code...number, c("Product_code","Product_number"), 
                          sep="-", remove = TRUE)
##Chapter 3 Exercise 1 question #3
refine_original <- refine_original %>% mutate(Product_category 
                                              = ifelse(grepl("p", Product_code), 
                                                                        "smartphone", ifelse(grepl("v", Product_code), "TV", 
                                                                                             ifelse(grepl("x", Product_code),"Laptop", 
                                                                                                    ifelse(grepl("q", Product_code),"Tablet", " ")))))
##Chapter 3 Exercise 1 question #4
refine_original<- unite(refine_original,full_address, c(address,city, country), sep = "," )
##Chapter 3 Exercise 1 question #5 PART a
refine_original<-refine_original %>% mutate(company_philips=
                 ifelse(grepl("philips",ï..company),1,0))%>% mutate (company_akzo=
                 ifelse(grepl("akzo",ï..company),1,0))%>% mutate (company_van_houten =
                 ifelse(grepl("van houten",ï..company),1,0))%>% mutate (company_unilever=
                 ifelse(grepl("unilever",ï..company),1,0))
##Chapter 3 Exercise 1 question #5 PART B  product_smartphone, product_tv, product_laptop and product_tablet.
refine_original<-refine_original %>% mutate(product_smartphone=ifelse(grepl("p",Product_code),1,0))
refine_original<-refine_original%>% mutate (product_tv=ifelse(grepl("v",Product_code),1,0))
refine_original<-refine_original%>% mutate (product_laptop =ifelse(grepl("x",Product_code),1,0))
refine_original<-refine_original%>% mutate (product_tablet= ifelse(grepl("q",Product_code),1,0))
refine_clean <-refine_original
write.csv(refine_clean,file="refine_clean.csv")