library(readr)
library(dplyr)
library(ggplot2) 
library(choroplethr)

fs104_school<- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
fs104_country<- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
fs105_school<- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
fs105_country<- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
fs106_school<- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
fs106_country<- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
map_en<- read_csv("CountriesComparisionTable.csv")
taiwan_std<-read_csv("Student_RPT_07.csv",locale = locale(encoding = "BIG5"))
world_tstd<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")



#1.請問哪些國家來台灣唸書的學生最多呢？
#請取出前十名的國家與總人數，由大到小排序(5分)。
fs104_country<-mutate(fs104_country,totalstd=`學位生-正式修讀學位外國生`+`學位生-僑生(含港澳)`+
                        `學位生-正式修讀學位陸生`+`非學位生-外國交換生`+`非學位生-外國短期研習及個人選讀`+
                        `非學位生-大專附設華語文中心學生`+`非學位生-大陸研修生`+`非學位生-海青班`+境外專班)
fs105_country<-mutate(fs105_country,totalstd=學位生_正式修讀學位外國生+`學位生_僑生(含港澳)`+
                        學位生_正式修讀學位陸生+非學位生_外國交換生+非學位生_外國短期研習及個人選讀+
                        非學位生_大專附設華語文中心學生+非學位生_大陸研修生+非學位生_海青班+境外專班)
fs106_country<-mutate(fs106_country,totalstd=學位生_正式修讀學位外國生+`學位生_僑生(含港澳)`+
                        學位生_正式修讀學位陸生+非學位生_外國交換生+非學位生_外國短期研習及個人選讀+
                        非學位生_大專附設華語文中心學生+非學位生_大陸研修生+非學位生_海青班+境外專班)

allcountrystd<-merge(fs104_country,fs105_country,by="國別",all=T)%>%merge(fs106_country,by="國別",all=T)
allcountrystd$totalstd.x[is.na(allcountrystd$totalstd.x)]<-0
allcountrystd<-mutate(allcountrystd,all_std=totalstd.x+totalstd.y+totalstd)
select_10country<-arrange(allcountrystd,desc(all_std))%>%select("國別",all_std)
head(select_10country,10)
#又哪間大學的境外生最多呢？
#請取出前十名的大學與總人數，由大到小排序(5分)。加總103年以後的資料回答此題
fs104_school$`非學位生-大陸研修生`<-gsub("…",replacement=0,fs104_school$`非學位生-大陸研修生`)
fs104_school$`非學位生-大陸研修生`<-as.numeric(fs104_school$`非學位生-大陸研修生`)


fs104_school<-mutate(fs104_school,totalstd=`學位生-正式修讀學位外國生`+`學位生-僑生(含港澳)`+
                       `學位生-正式修讀學位陸生`+`非學位生-外國交換生`+`非學位生-外國短期研習及個人選讀`+
                       `非學位生-大專附設華語文中心學生`+`非學位生-大陸研修生`+`非學位生-海青班`+境外專班)
fs105_school<-mutate(fs105_school,totalstd=學位生_正式修讀學位外國生+`學位生_僑生(含港澳)`+
                       學位生_正式修讀學位陸生+非學位生_外國交換生+非學位生_外國短期研習及個人選讀+
                       非學位生_大專附設華語文中心學生+非學位生_大陸研修生+非學位生_海青班+境外專班)
fs106_school<-mutate(fs106_school,totalstd=學位生_正式修讀學位外國生+`學位生_僑生(含港澳)`+
                       學位生_正式修讀學位陸生+非學位生_外國交換生+非學位生_外國短期研習及個人選讀+
                       非學位生_大專附設華語文中心學生+非學位生_大陸研修生+非學位生_海青班+境外專班)

allschoolstd<-merge(fs104_school,fs105_school,by="學校名稱",all=T)%>%merge(fs106_school,by="學校名稱",all=T)
allschoolstd$totalstd.x[is.na(allschoolstd$totalstd.x)]<-0
allschoolstd<-mutate(allschoolstd,all_std=totalstd.x+totalstd.y+totalstd)
select_10school<-arrange(allschoolstd,desc(all_std))%>%select("學校名稱",all_std)
head(select_10school,10)

#2.承1，請用bar chart呈現各個國家(全部)來台灣唸書的學生人數(10分)。
ggplot()+geom_bar(data=allcountrystd,aes(x=國別,y=all_std),stat = "identity")+coord_flip()
#3.承1，請用面量圖呈現各個國家來台灣唸書的學生人數，人數越多顏色越深(10分)。
draw_country<-select(allcountrystd,Taiwan="國別",all_std)
draw_country<-merge(draw_country,map_en,by="Taiwan")%>%select(c("English","all_std"))
draw_country<-draw_country[complete.cases(draw_country$all_std),]
colnames(draw_country)<- c("region", "value")
draw_country$region<-tolower(draw_country$region)
draw_country<-group_by(draw_country,region)%>%summarise(value=sum(value))
draw_country$region<- gsub("and", "", draw_country$region, fixed=TRUE)
country_choropleth(draw_country,num_colors=1)

#4.台灣大專院校的學生最喜歡去哪些國家進修交流呢？請取出前十名的國家與總人數，由大到小排序(5分)。
taiwan_std$`對方學校(機構)國別(地區)`->taiwan_std$school_country

most_twstdgo<-group_by(taiwan_std,school_country)%>%select(school_country,"小計")%>%
  summarize(count_std=sum(小計))%>%arrange(desc(count_std))
head(most_twstdgo,10)
#又哪間大學的出國交流學生數最多呢？請取出前十名的大學與總人數，由大到小排序(5分)。加總103年以後的資料回答此題。
most_schoolstdgo<-group_by(taiwan_std,學校名稱)%>%select("學校名稱","小計")%>%
  summarize(count_std=sum(小計))%>%arrange(desc(count_std))
head(most_schoolstdgo,10)


#5.承4，請用bar chart呈現台灣大專院校(全部)的學生去各國家進修交流人數(10分)。
ggplot()+geom_bar(data=taiwan_std,aes(x=school_country,y="小計"),stat = "identity")+coord_flip()

#6.承4，請用面量圖呈現台灣大專院校的學生去各國家進修交流人數，人數越多顏色越深(10分)
draw_moststdgo<-select(taiwan_std,Taiwan="school_country","小計")
draw_moststdgo<-merge(draw_moststdgo,map_en,by="Taiwan")%>%select(c("English","小計"))
colnames(draw_moststdgo)<- c("region", "value")
draw_moststdgo$region<-tolower(draw_moststdgo$region)
draw_moststdgo<-group_by(draw_moststdgo,region)%>%summarise(value=sum(value))
draw_moststdgo$region<- gsub("and", "", draw_moststdgo$region, fixed=TRUE)
country_choropleth(draw_moststdgo,num_colors=1)

#7.台灣學生最喜歡去哪些國家留學呢？請取出前十名的國家與總人數，由大到小排序(5分)。
world_tstd<-select(world_tstd,"國別","總人數")
most_like_country<-arrange(world_tstd,desc(總人數))
head(most_like_country,10)

#8.承7，請用面量圖呈現台灣學生去各國家留學人數，人數越多顏色越深(10分)。
draw_stdgoworld<-select(world_tstd,Taiwan="國別","總人數")
draw_stdgoworld<-merge(draw_stdgoworld,map_en,by="Taiwan")%>%select(c("English","總人數"))
colnames(draw_stdgoworld)<- c("region", "value")
draw_stdgoworld$region<-tolower(draw_stdgoworld$region)
draw_stdgoworld<-group_by(draw_stdgoworld,region)%>%summarise(value=sum(value))
draw_stdgoworld$region<- gsub("and", "", draw_stdgoworld$region, fixed=TRUE)
country_choropleth(draw_stdgoworld)

#9.請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？
#想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。
compare_std<-merge(select_10country,most_like_country,"國別")
colnames(compare_std)<-c("國別","來台讀書人數","出國留學人數")

ggplot(data=compare_std) +geom_bar(aes(x=factor(1),y=來台讀書人數,fill=國別),stat = "identity") +coord_polar("y", start=0)
ggplot(data=compare_std) +geom_bar(aes(x=factor(1),y=出國留學人數,fill=國別),stat = "identity") +coord_polar("y", start=0)

