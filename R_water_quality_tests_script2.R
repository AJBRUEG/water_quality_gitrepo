#analyze water quality data

#-----set up environment-----

library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)

#-----GET DATA-----


##GET RGB FILES OF DATA
#list all rgb files of data
rgblist <- list.files(path="C:/Users/drewj/OneDrive/R_based_work/water_quality/rgb_values/",pattern="day.*txt")
#read in all rgblist
in_rgbfiles <- lapply(rgblist,function(x){
  #print filename
  print(x)
  #read in file
  tempdf <- read.table(file=paste0("C:/Users/drewj/OneDrive/R_based_work/water_quality/rgb_values/",x),header=FALSE,stringsAsFactors = FALSE,sep="\t",fill=TRUE)
  #add filename to column and rownames as a new column
  tempdf <- tempdf %>%
    dplyr::mutate(file=x) %>%
    tibble::rownames_to_column("id")
})
#remove rgblist
rm(rgblist)

##GET RGB CHARTS

#list all chart files
chartlist <- list.files(path="C:/Users/drewj/OneDrive/R_based_work/water_quality/rgb_values/",pattern="chart.*")
#read in all chart files
in_charts <- lapply(chartlist,function(x){
  #print filename
  print(x)
  #read in file
  tempdf <- read.table(file=paste0("C:/Users/drewj/OneDrive/R_based_work/water_quality/rgb_values/",x),header=FALSE,stringsAsFactors = FALSE,sep="\t",fill=TRUE)
  #add filename to column and rownames as a new column
  tempdf <- tempdf %>%
    dplyr::mutate(file=x) %>%
    tibble::rownames_to_column("id")
})
#remove chartlist
rm(chartlist)

##GET LAYOUT

#input table layout data
in_layout_chart <- read.csv("C:/Users/drewj/OneDrive/R_based_work/water_quality/Layouts/testing_chart_key.csv",header=TRUE,stringsAsFactors = FALSE)
head(in_layout_chart)
in_layout_tests <- read.csv("C:/Users/drewj/OneDrive/R_based_work/water_quality/Layouts/testing_strip_layout.csv",header=TRUE,stringsAsFactors = FALSE)
head(in_layout_tests)

##GET LIMITS

#input limits
in_limits <- read.csv("C:/Users/drewj/OneDrive/R_based_work/water_quality/water_limits.csv",header=TRUE,stringsAsFactors = FALSE)

#-----CONVERT HEX TO RGB, and add metrics-----

#use the function "col2rgb" to convert the hex value to rgb
#start each hex value with "#" (you need this symbol first in order for col2rgb to recognize it)
#you'll need to specify the vector position as 1,2,or 3 in order to get the red, green,  or blue values
#Ex: col2rgb("#C59101")[1]

#incorporate an id into the layouts
in_layout_chart <- tibble::rownames_to_column(in_layout_chart,"id")
in_layout_tests <- tibble::rownames_to_column(in_layout_tests,"id")

#charts, incorporate layout
a2_charts <- lapply(in_charts,function(x){
  #print head
  print(head(x,n=1))
  #left_join x and in_layout_chart by id
  tempdf <- left_join(x=x,y=in_layout_chart,by="id")
  #return tempdf
  return(tempdf)
})

#tests, incorporate layout
a2_tests <- lapply(in_rgbfiles,function(x){
  #print head
  print(head(x,n=1))
  #left_join x and in_layout_chart by id
  tempdf <- left_join(x=x,y=in_layout_tests,by="id")
  #return tempdf
  return(tempdf)
})

#convert lists to big dataframe
a3_charts <- bind_rows(a2_charts)
a3_tests <- bind_rows(a2_tests)
rm(a2_charts,a2_tests)

#get the RGB values for the values in a3_charts & then summarize
a3_charts <- a3_charts %>%
  #add # to values
  dplyr::mutate(V1=paste0("#",V1)) %>%
  #get red, green, and blue values
  dplyr::rowwise() %>%
  dplyr::mutate(red=col2rgb(V1)[1],
                green=col2rgb(V1)[2],
                blue=col2rgb(V1)[3]) %>%
  dplyr::group_by(Material,Unit,Value) %>%
  dplyr::summarize(red=median(red),
                   green=median(green),
                   blue=median(blue))
head(a3_charts)

#get the RGB values for the values in a3_tests
a3_tests <- a3_tests %>%
  #add # to values
  dplyr::mutate(V1=paste0("#",V1)) %>%
  #get red, green, and blue values
  dplyr::rowwise() %>%
  dplyr::mutate(red=col2rgb(V1)[1],
                green=col2rgb(V1)[2],
                blue=col2rgb(V1)[3]) %>%
  #split the file into "source","day", and "strip"
  dplyr::mutate(source=gsub("_s.*","",file)) %>%
  dplyr::mutate(source=gsub(".*_","",source)) %>%
  dplyr::mutate(day=gsub("_.*","",file)) %>%
  dplyr::mutate(day=as.numeric(gsub("day","",day))) %>%
  dplyr::mutate(strip=gsub(".*_","",file)) %>%
  dplyr::mutate(strip=gsub("strip","",strip)) %>%
  dplyr::mutate(strip=as.numeric(gsub(".txt","",strip))) %>%
  dplyr::mutate(rep=paste(day,strip,sep="_")) %>%
  #reorder
  dplyr::select(source,rep,Material,Unit,red,green,blue)
head(a3_tests)

#-----APPLY POINT-TO-POINT ANALYSIS-----

#For each reading in "a3_tests", reference it with the chart
#the chart will have red, green, and blue values
#the sample in "a3_tests" will also have red, green, and blue values
#Use point-to-point analysis to calculate the value of the sample for red, green, and blue
#calculate median of these 3 values to determine what the final value is

a3_tests$value <- apply(a3_tests,1,function(x){
  #get the source, material, red, green, and blue values
  source <- as.character(x["source"])
  material <- as.character(x["Material"])
  test_red <- as.numeric(as.character(x["red"]))
  test_green <- as.numeric(as.character(x["green"]))
  test_blue <- as.numeric(as.character(x["blue"]))
  #print the source and material you are testing
  print(paste(source,material,sep="_"))
  #get a temporary df from the chart for the material you are working on
  tmpdf <- a3_charts %>% filter(Material==material) %>% ungroup() %>% select(!Unit)
  #calculate the slope and intercept for red
  ##y=Value,x=red
  ##y=mx+b
  red_redmax <- ifelse(test_red>max(tmpdf$red),NA,min(tmpdf$red[tmpdf$red>test_red]))
  red_redmin <- ifelse(test_red<min(tmpdf$red),NA,max(tmpdf$red[tmpdf$red<test_red]))
  red_valmax <- tmpdf$Value[tmpdf$red==red_redmax]
  red_valmin <- tmpdf$Value[tmpdf$red==red_redmin]
  red_slope <-ifelse(is.na(red_redmax)|is.na(red_redmin),NA,(red_valmax-red_valmin)/(red_redmax-red_redmin))
  red_int <- ifelse(is.na(red_redmax)|is.na(red_redmin),NA,red_valmax-(red_slope*red_redmax))
  #calculate the slope and intercept for green
  ##y=Value,x=green
  ##y=mx+b
  green_greenmax <- ifelse(test_green>max(tmpdf$green),NA,min(tmpdf$green[tmpdf$green>test_green]))
  green_greenmin <- ifelse(test_green<min(tmpdf$green),NA,max(tmpdf$green[tmpdf$green<test_green]))
  green_valmax <- tmpdf$Value[tmpdf$green==green_greenmax]
  green_valmin <- tmpdf$Value[tmpdf$green==green_greenmin]
  green_slope <-ifelse(is.na(green_greenmax)|is.na(green_greenmin),NA,(green_valmax-green_valmin)/(green_greenmax-green_greenmin))
  green_int <- ifelse(is.na(green_greenmax)|is.na(green_greenmin),NA,green_valmax-(green_slope*green_greenmax))
  #calculate the slope and intercept for blue
  ##y=Value,x=blue
  ##y=mx+b
  blue_bluemax <- ifelse(test_blue>max(tmpdf$blue),NA,min(tmpdf$blue[tmpdf$blue>test_blue]))
  blue_bluemin <- ifelse(test_blue<min(tmpdf$blue),NA,max(tmpdf$blue[tmpdf$blue<test_blue]))
  blue_valmax <- tmpdf$Value[tmpdf$blue==blue_bluemax]
  blue_valmin <- tmpdf$Value[tmpdf$blue==blue_bluemin]
  blue_slope <-ifelse(is.na(blue_bluemax)|is.na(blue_bluemin),NA,(blue_valmax-blue_valmin)/(blue_bluemax-blue_bluemin))
  blue_int <- ifelse(is.na(blue_bluemax)|is.na(blue_bluemin),NA,blue_valmax-(blue_slope*blue_bluemax))
  
  #calculate the Value based on each of the 3 colors
  red_val <- (red_slope*test_red)+red_int
  green_val <- (green_slope*test_green)+green_int
  blue_val <- (blue_slope*test_blue)+blue_int
  
  #determine the average of the 3 values
  val <- mean(c(red_val,green_val,blue_val),na.rm=TRUE)
  
  #specify "out" if val=NaN (when there is no data available)
  #assumes that, if there is no data available, the value is the minimum
  #DOUBLE CHECK THIS, SEE IF CORRECT
  out <- ifelse(is.nan(val),min(tmpdf$Value),val)
  
  #return out
  return(out)
})

#graph
ggplot(a3_tests)+
  geom_boxplot(aes(x=source,y=value,fill=source),outlier.shape=NA)+
  geom_jitter(aes(x=source,y=value),width=0.1,size=0.3)+
  #specify limits
  geom_hline(data=in_limits,aes(yintercept=Limit..Desired.Unit),color="red")+
  facet_wrap(.~Material,
             scales="free")+
  #rotate x-axis text
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  #labels
  labs(y="PPM")+
  #hide legend
  theme(legend.position="none")

#summarize
summ1 <- a3_tests %>%
  group_by(Material,source) %>%
  summarize(amount=median(value)) %>%
  pivot_wider(id_cols=Material,names_from=source,values_from=amount) %>%
  left_join(y=in_limits %>% select(Material,Limit..Desired.Unit),by="Material") %>%
  rename("Limit"="Limit..Desired.Unit")

write.table(summ1,sep="|",quote=FALSE,row.names=FALSE)
  

#FINISH