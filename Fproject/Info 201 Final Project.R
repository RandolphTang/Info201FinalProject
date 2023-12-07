#Info 201 Team BD: Randolph Jenkins, Vridhi Manchanda, Yaqi Wang
#Final Project: Data Wrangling 

library(stringr)
library(dplyr)

#Data Joining

region_df <- read.csv("salaries-by-region.csv")

college_type_df <- read.csv("salaries-by-college-type.csv")


df <- merge(x = college_type_df, y = region_df, 
            by = "School.Name", all.x = TRUE)

#Data Cleaning and Augmentation
#removing the duplicate columns in the joined data set that were in both initial data sets, to avoid duplicate columns 

df$Starting.Median.Salary.y <- NULL
df$Mid.Career.Median.Salary.y <- NULL
df$Mid.Career.10th.Percentile.Salary.y <- NULL
df$Mid.Career.25th.Percentile.Salary.y <- NULL
df$Mid.Career.75th.Percentile.Salary.y <- NULL
df$Mid.Career.90th.Percentile.Salary.y <- NULL



#adding a Categorical Column 
#called ivy.type to categorize schools as ivy (1) or non-ivy (0)

ivy_type_list <- c()

for (school in df$School.Type){
  
  if (school == "Ivy League"){
    
    ivy_type_list <- c(ivy_type_list, 1)
    
  }
  else{
    
    ivy_type_list <- c(ivy_type_list, 0)
    
  }
  
}

df$Ivy.Type <- ivy_type_list

#adding Numerical Column 
#called Start.Mid.Salary.Diff which is the difference between
#the starting and mid career salaries to get an idea of how the salaries change 
#from starting to mid career to base conclusions on how much the increase difffers


df$Starting.Median.Salary.x <- as.numeric(gsub("[$,]", "", df$Starting.Median.Salary.x))

df$Mid.Career.Median.Salary.x <- as.numeric(gsub("[$,]", "", df$Mid.Career.Median.Salary.x))

starting_salary <- c(df$Starting.Median.Salary.x)

mid_career_salary <- c(df$Mid.Career.Median.Salary.x)

salary_diff_list <- mid_career_salary - starting_salary 

df$Start.Mid.Salary.Diff <- salary_diff_list

#adding Summarization Data Frame 
#that calculates the average of the salary difference 
#between start and mid career salary based on school type
#showing ivy league schools have the highest average increase in salary from start to mid career

group_df <- group_by(df, School.Type)

summarize_df <- summarize(group_df, mean(Start.Mid.Salary.Diff))





