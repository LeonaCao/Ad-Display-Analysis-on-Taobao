# read in the data
# 1) raw_sample
raw_sample <- read.csv("~/raw_sample.csv")
# 2) ad_feature
ad_feature <- read.csv("~/ad_feature.csv")
# 3) user_profile
user_profile <- read.csv("~/user_profile.csv")

# view the data
# 1) raw_sample
View(raw_sample)
# 2) ad_feature
View(ad_feature)
# 3) user_profile
View(user_profile)

# merge the data
# 1) first merge raw_sample and ad_feature on "adgroup_id"
df <- merge(raw_sample, ad_feature, by="adgroup_id")
# 2) then merge it with user_profile on "userid"
# a. rename the "user" column in df to be consistent with user_profile
names(df)[names(df) == "user"] <- "userid"
# b. merge the data
df <- merge(df, user_profile, by="userid")

# view the merged data
View(df)

# convert bigint to datetime for the time_stamp column
df$time_stamp <- as.POSIXct(as.numeric(as.character(df$time_stamp)), origin="1970-01-01")

# remove the nonclk column because of redundancy
df <- subset(df, select = -nonclk)

# recode NULL values in the brand column to NA
df$brand[df$brand == "NULL"] <- NA

# remove missing values
df <- na.omit(df)

# check the updated dimesion of df
dim(df)

# check the data type of df
str(df)

# recode "2" in the final_gender_code column to "0" for consistency (0 for females)
df$final_gender_code[df$final_gender_code == 2] <- 0

# convert data types for certain columns
df$userid <- as.character(df$userid)
df$adgroup_id <- as.character(df$adgroup_id)
df$clk <- factor(df$clk)
df$cate_id <- as.character(df$cate_id)
df$campaign_id <- as.character(df$campaign_id)
df$customer <- as.character(df$customer)
df$cms_segid <- as.character(df$cms_segid)
df$cms_group_id <- as.character(df$cms_group_id)
df$final_gender_code <- factor(df$final_gender_code)
df$age_level <- factor(df$age_level, order = TRUE) # ordinal 
df$pvalue_level <- factor(df$pvalue_level, order = TRUE) # ordinal 
df$shopping_level <- factor(df$shopping_level, order = TRUE) # ordinal 
df$occupation <- factor(df$occupation)
df$new_user_class_level <- factor(df$new_user_class_level, order = TRUE, levels = c('4', '3', '2', '1')) # ordinal by descending 

# check the data type of df again
str(df)

# convert the data type of the time_stamp column
df$time_stamp <- as.POSIXct(df$time_stamp, format="%Y-%m-%d %H:%M:%S")

# split date and time from the time_stamp column
df$date <- as.Date(df$time_stamp)
df$day <- weekdays(as.Date(df$date))
df$time <- format(df$time_stamp, "%H:%M:%S")

# recode the day of a week to ordinal factor
df$day_code <- ifelse(df$day=="Sunday",0,ifelse
                         (df$day=="Monday",1,ifelse
                           (df$day=="Tuesday",2,ifelse
                             (df$day=="Wednesday",3,ifelse
                               (df$day=="Thursday",4,ifelse
                                 (df$day=="Friday",5,6))))))
df$day_code <- factor(df$day_code, order = TRUE)

# convert data type of the time column from character to time
df$time <- as.POSIXct(df$time, format="%H:%M:%S")

# recode the time to period
x=as.POSIXct(strptime(c("000000","050000","120000","180000","220000","235959"),
                      "%H%M%S"))
labs=c("night","morning","afternoon","evening","night")
df$time_period <- labs[findInterval(df$time,x)]

# recode the time_period column to ordinal factor
df$period_code <- ifelse(df$time_period=="morning",0,ifelse
                         (df$time_period=="afternoon",1,ifelse
                           (df$time_period=="evening",2,3)))
df$period_code <- factor(df$period_code, order = TRUE)

# recode the pid to dummy variable because there are just two classes
df$pid <- ifelse(df$pid=="430548_1007",0,1)
df$pid <- factor(df$pid)

# check the updated data type of df
str(df)

# install a package for SQL query
install.packages('sqldf')
library(sqldf)

# aggregate the data:
df1 = sqldf("select
	campaign_id
,	final_gender_code
,	age_level
,	pvalue_level
,	shopping_level
,	occupation
,	new_user_class_level
,	pid
,	day
,	day_code
,	period_code
,	sum(price) 'campaign_cost'
,	count(clk) 'impressions'
,	sum(clk) 'clicks'
,	sum(price)*1000/count(clk) 'CPM'
,	sum(clk)/count(clk) 'CTR'
from
	df
group by
	campaign_id
,	final_gender_code
,	age_level
,	pvalue_level
,	shopping_level
,	occupation
,	new_user_class_level
,	pid
,	day
,	day_code
,	period_code")

# regress the CTR on variables and interactions we are interested in
ml <- lm( CTR ~ CPM
          + as.factor(final_gender_code)
          + as.factor(pvalue_level)
          + as.factor(shopping_level)
          + as.factor(occupation)
          + as.factor(new_user_class_level)
          + as.factor(pid)
          + as.factor(day_code)
          + as.factor(period_code)
          + as.factor(final_gender_code):as.factor(pvalue_level)
          + as.factor(final_gender_code):as.factor(shopping_level)
          + as.factor(final_gender_code):as.factor(new_user_class_level)
          + as.factor(final_gender_code):as.factor(occupation)
          + as.factor(final_gender_code):as.factor(period_code)
          , data = df1
)

# view the output
summary(ml)

# install the package for hypothesis testing
install.packages('car')
library(car)

# compare effect of consumption grade of mid and high on males
linearHypothesis(ml,"as.factor(final_gender_code)1:as.factor(pvalue_level).L = as.factor(final_gender_code)1:as.factor(pvalue_level).Q")

# compare effect of consumption grade of mid and high on females
linearHypothesis(ml,"as.factor(pvalue_level).L = as.factor(pvalue_level).Q")

# compare effect of shopping level of moderate and deep on males
linearHypothesis(ml,"as.factor(final_gender_code)1:as.factor(shopping_level).L = as.factor(final_gender_code)1:as.factor(shopping_level).Q")

# compare effect of shopping level of moderate and deep on females
linearHypothesis(ml,"as.factor(shopping_level).L = as.factor(shopping_level).Q")

# compare effect of afternoon and evening on males
linearHypothesis(ml,"as.factor(final_gender_code)1:as.factor(period_code)1 = as.factor(final_gender_code)1:as.factor(period_code)2")

# compare effect of afternoon and evening on females
linearHypothesis(ml,"as.factor(period_code)1 = as.factor(period_code)2")

# compare afternoon effect between genders
linearHypothesis(ml,"as.factor(period_code)1 = as.factor(final_gender_code)1:as.factor(period_code)1")

# compare evening effect between genders
linearHypothesis(ml,"as.factor(period_code)2 = as.factor(final_gender_code)1:as.factor(period_code)2")

