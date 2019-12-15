setwd("D:/BSE DATASCIENCE PROJECT TRACK/final project folder")
original_data = read.csv("OnlineNewsPopularity.csv")

"""preprocessing the data for machine learning"""
target = c("target")
original_data[,target] <- NA
for (i in 1:nrow(original_data)){
  if(original_data$shares[i] >= 1400){
    original_data$target[i] = 1
  }else{
    original_data$target[i] = 0
  }
}
typeof(original_data$target)
original_data$target = as.integer(original_data$target)
cleaned_data = original_data
cleaned_data = subset(original_data, select = -c(url, timedelta, is_weekend, shares))

"""dealing with outliers"""
toomanywords = which(cleaned_data$n_tokens_content > 6500)
toomanycontent = which(cleaned_data$n_unique_tokens > 650)
toomanylinks = which(cleaned_data$num_hrefs > 200)
toomanyself = which(cleaned_data$num_self_hrefs > 80)
toomanyimgs = which(cleaned_data$num_imgs > 120)
toomanyvids = which(cleaned_data$num_videos > 75)
toomanykw_max_min = which(cleaned_data$kw_max_min > 25000)
toomanykw_min_min = which(cleaned_data$kw_min_min > 300)
toomanykw_max_avg = which(cleaned_data$kw_max_avg > 200000)
cleaned_data = cleaned_data[-toomanywords,]
cleaned_data = cleaned_data[-toomanycontent,]
cleaned_data = cleaned_data[-toomanylinks,]
cleaned_data = cleaned_data[-toomanyself,]
cleaned_data = cleaned_data[-toomanyimgs,]
cleaned_data = cleaned_data[-toomanyvids,]
cleaned_data = cleaned_data[-toomanykw_max_min,]
cleaned_data = cleaned_data[-toomanykw_min_min,]
cleaned_data = cleaned_data[-toomanykw_max_avg,]
#a total of 122 rows were deleted

"""exporting cleaned data for model building"""
write.csv(cleaned_data, file = "edited_data.csv")


"""preprocessing the data for visualisation"""
original_data = read.csv("OnlineNewsPopularity.csv")
target = c("target")
original_data[,target] <- NA
for (i in 1:nrow(original_data)){
  if(original_data$shares[i] >= 1400){
    original_data$target[i] = 1
  }else{
    original_data$target[i] = 0
  }
}
typeof(original_data$target)
original_data$target = as.integer(original_data$target)
cleaned_data = original_data
cleaned_data = subset(original_data, select = -c(url, timedelta, is_weekend))

"""creating column for combined weekdays and channel"""
comb_data = cleaned_data
com = c("combined_weekday")
comb_data[,com] <- NA
for(i in 1:nrow(comb_data)){
  if(comb_data$weekday_is_monday[i]=="1"){
    comb_data$combined_weekday[i]="Monday"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$weekday_is_tuesday[i]=="1"){
    comb_data$combined_weekday[i]="Tuesday"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$weekday_is_wednesday[i]=="1"){
    comb_data$combined_weekday[i]="Wednesday"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$weekday_is_thursday[i]=="1"){
    comb_data$combined_weekday[i]="Thursday"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$weekday_is_friday[i]=="1"){
    comb_data$combined_weekday[i]="Friday"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$weekday_is_saturday[i]=="1"){
    comb_data$combined_weekday[i]="Saturday"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$weekday_is_sunday[i]=="1"){
    comb_data$combined_weekday[i]="Sunday"
  }
}
unique(comb_data$combined_weekday)
sum(is.na(comb_data$combined_weekday))
#combining all data channels in one column
comchan = c("combined_channel")
comb_data[,comchan]<-NA
for(i in 1:nrow(comb_data)){
  if(comb_data$data_channel_is_lifestyle[i]=="1"){
    comb_data$combined_channel[i]="Lifestyle"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$data_channel_is_entertainment[i]=="1"){
    comb_data$combined_channel[i]="Entertainment"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$data_channel_is_bus[i]=="1"){
    comb_data$combined_channel[i]="Business"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$data_channel_is_socmed[i]=="1"){
    comb_data$combined_channel[i]="SocialMedia"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$data_channel_is_tech[i]=="1"){
    comb_data$combined_channel[i]="Tech"
  }
}
for(i in 1:nrow(comb_data)){
  if(comb_data$data_channel_is_world[i]=="1"){
    comb_data$combined_channel[i]="World"
  }
}
unique(comb_data$combined_channel)
sum(is.na(comb_data$combined_channel))
#the 6134 NA are to be replaced with not_mentioned
comb_data$combined_channel[is.na(comb_data$combined_channel)] = "NotMentioned"

vis_final_data = comb_data
write.csv(vis_final_data, "final_data_for_visualisation.csv")
