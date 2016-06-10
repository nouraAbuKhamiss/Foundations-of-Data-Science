#Load data into R
df<-read.csv("refine_original.csv")
#Fix the misspwlling in the company column
df$company<- gsub("\\s","",df$company)
philips <- grep("\\Bips$\\b", df$company, TRUE)
df$company[philips]<- "philips"
akzo <- grep("\\bakz.\\b",df$company,TRUE)
df$company[akzo]<-"akzo"
van<-grep("van\\B", df$company,TRUE)
df$company[van]<-"van houten"
unil <- grep("uni\\B", df$company, TRUE)
df$company[unil]<-"unilever"
library(tidyr)
#Seperate product code and number
df<-separate(df, Product.code...number,c("product_code","product_number"),sep="-")
#Add product category column
df$product_category<-""
#Populate product category
p<-grep("p", df$product_code,TRUE)
df$product_category[p]<-"Smartphone"
v<-grep("v", df$product_code,TRUE)
df$product_category[v]<-"TV"
x<-grep("x", df$product_code,TRUE)
df$product_category[x]<-"Laptop"
q<-grep("q", df$product_code,TRUE)
df$product_category[q]<-"Tablet"
df<-df[c(1,2,8,3,4,5,6,7)]
#Add full address
df<-unite(df, full_address,address, city, country, sep=" , ")
#Create binary variables
df$company_philips<-ifelse(df$company=="philips",1,0)
df$company_akzo<-ifelse(df$company=="akzo",1,0)
df$company_vanhouten<-ifelse(df$company=="van houten",1,0)
df$company_unilever<-ifelse(df$company=="unilever",1,0)
df$product_smartphone<-ifelse(df$product_category=="Smartphone",1,0)
df$product_tv<-ifelse(df$product_category=="TV",1,0)
df$product_laptop<-ifelse(df$product_category=="Laptop",1,0)
df$product_tablet<-ifelse(df$product_category=="Tablet",1,0)
#Write file
write.csv(df,"refine_clean.csv")
