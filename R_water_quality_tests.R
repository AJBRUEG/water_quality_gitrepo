#upload data from xlsx
in1 <- read.table("clipboard",sep="\t",header=TRUE,stringsAsFactors=FALSE,fill=TRUE)
limits <- read.table("clipboard",sep="\t",header=TRUE,stringsAsFactors=FALSE,fill=TRUE)
library(dplyr)
library(tidyr)
library(ggplot2)

library(dplyr)
library(agricolae)
tempdf1 <- in1
head(tempdf1 %>% dplyr::filter(Material=="Cyanuric Acid"))
unique(tempdf1$Condition)
unique(tempdf1$Measurement)

#graph
ggplot(tempdf1)+
  geom_boxplot(aes(x=Material,y=Measurement,fill=Condition))+
  geom_hline(data=limits,aes(yintercept=Limit..Desired.Unit),color="red")+
  facet_wrap(.~Material,scales="free")

#create summary
#create a summary of the findings
tempdf2 <- tempdf1 %>%
  dplyr::group_by(Material,Condition) %>%
  dplyr::summarize(median=median(Measurement))
head(tempdf2)
#combine the limits with tempdf2
tempdf2 <- left_join(x=tempdf2,y=limits,by="Material")
head(tempdf2)
tempdf2 <- tempdf2 %>%
  dplyr::select(!c(Desired.Unit,Limit,Unit,Molecular.Weight)) %>%
  dplyr::rename("Limit"="Limit..Desired.Unit")
head(tempdf2)
#calculate the % of limit that there is, and pivot wide
tempdf2 <- tempdf2 %>%
  dplyr::mutate(percent_to_limit=(median-Limit)/Limit)
head(tempdf2)
#pivot wide
tempdf2 <- tempdf2 %>%
  tidyr::pivot_wider(id_cols=Material,names_from=Condition,values_from=c("median","percent_to_limit"))
head(data.frame(tempdf2))
#write table
write.table(tempdf2,sep="|",quote=FALSE,row.names=FALSE)


#-----STAT TESTS-----

# 
# tempdf2 <- tempdf1 %>%
#   tidyr::pivot_wider(id_cols=c(Material,Rep),names_from=Condition,values_from=Measurement)
# head(tempdf2)

#t test

# tempdf1 %>%
#   dplyr::group_by(Material)
#   dplyr::summarize(pval=t.test(Filtered ~ Unfiltered,data=.,var.equal=TRUE)$p.value)
# library(rstatix)

# tempdf1 %>%
#   dplyr::select(Material,Condition,Measurement) %>%
#   dplyr::group_by(Material,Condition) %>%
#   dplyr::summarize(Measurement2=list(Measurement)) %>%
#   tidyr::pivot_wider(id_cols=Material,names_from=Condition,values_from=Measurement2) %>%
#   dplyr::group_by(Material) %>%
#   dplyr::mutate(ttest=t.test(unlist(Filtered) ~ unlist(Unfiltered))$p.value)

# tempdf1 %>%
#   dplyr::group_by(Material) %>%
#   summarize(ttest=list(t.test(Measurement~Condition,var.equal=TRUE)$p.value)
            
            
            
            
# #try wilcoxan test
# 
# # tempdf2 %>%
# #   dplyr::group_by(Material) %>%
# #   do(w=wilcox.test(tempdf2$Filtered~tempdf2$Unfiltered,paired=FALSE))
# #   summarize(wilcox=w$p.value)
# 
# tempdf2 %>%
#   dplyr::group_by(Material) %>%
#   dplyr::summarize(w=wilcox.test(Filtered~Unfiltered,paired=FALSE)$p.value)
# 
# tempdf3 <- split(tempdf2,f=tempdf2$Material)
# tempdf3 <- lapply(tempdf3,function(x){
#   #input df
#   df <- x
#   #run wilcoxan test, store p-value
#   pvalue <- wilcox.test(x=df$Filtered,y=df$Unfilitered,paired=FALSE)$p.value
#   #export pvalue
#   df$wilcox_pvalue <- pvalue
#   print(head(df))
# })
# tempdf4 <- combine(tempdf3)
# tempdf4 %>%
#   dplyr::group_by(Material) %>%
#   dplyr::summarize(filtered=median(Filtered),
#                    unfiltered=median(Unfiltered),
#                    wilcox_pvalue=median(wilcox_pvalue))
# 
# tempdf1 %>%
#   reframe(pvalue = wilcox.test(Measurement ~ Condition, 
#                                paired = FALSE)$p.value, .by = Material)

