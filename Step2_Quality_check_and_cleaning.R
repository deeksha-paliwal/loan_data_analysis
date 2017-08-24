
###### Quality Check for Respondent_Name_TS #######

# getting the unique values for respondent names present in the data
Respondent_Names <- unique(as.character(merged_data$Respondent_Name_TS))
length(Respondent_Names)
head(Respondent_Names)

#So there are 1602 unique kinds of respondent names present in the data
#just checking if the first national bank is documented as FNB somewhere in the data?
fnb <- Respondent_Names[which(substr(Respondent_Names, 1,3) == 'FNB')]
fnb

#issue#1 :So this above exploration opens up a new issue here, that there are some names follwed by period(or punctuation)
# but these names aren't different

#issue 1: Some names don't contain the full name of the bank and are just left in the middle for example-
Respondent_Names[991] #missing the last words, i.e. credit union
Respondent_Names[994] #missing last caharacter 'n' in the name

# The respondent names correspond to the unique Respondent_ID
length(unique(merged_data$Respondent_ID))

#So, the unique number of IDs are 1384
# actual Issue here is that there are names in the data that are repeated with some other modified name!

#Let's explore this issue by creating key_value pair for Respondent_ID and Respondent_Name
key_value <- unique(merged_data[,c('Respondent_ID','Respondent_Name_TS')])

key_value$Respondent_ID <- as.character(key_value$Respondent_ID) #converting the IDs to character

rep <- count(key_value, Respondent_ID) #group_by Respondent_ID and count the number of rows for each ID

rep <- rep[which(rep$n > 1),] #filter out the ID cases where repition exist

key_value_repeat <- key_value[which(key_value$Respondent_ID %in% rep$Respondent_ID),]

#one possible technique might be to get rid of punctuation in the names
resp <- gsub( "[^[:alnum:] ]", "", Respondent_Names )
resp <- unique(resp)
length(resp) #So this technique reduced the number of different names from 1602 to 1536 which is a big improvement


#but for other repition cases the best technique might be to create a unique Respondent_ID and Respondent_Name key-value pair table
#and whenever a new data is inputted in the system, match the name by joining the loan_application data to this key_value table to match the respondent name.
#My assumption here is that the Respondent_ID aren't updated every year and are constant


######### Loan_Amount ##########

#getting the descriptive statistics for the Loan_Amount
summary(merged_data$Loan_Amount_000)
var(merged_data$Loan_Amount_000)
sd(merged_data$Loan_Amount_000)

#To view the initial distribution
qplot(data = merged_data, x = Loan_Amount_000) + ylab("Number of Applications")


#To view relationship to Applicant Income
plot(merged_data$Loan_Amount_000, merged_data$Applicant_Income_000, ylim = range(0,4000))

#To view histogram with limits upto 30000k
hist(merged_data$Loan_Amount_000, breaks=20000, xlim = range(1,30000))

#Loan Amount Distribution by State
ggplot(merged_data) + geom_boxplot(aes(State, Loan_Amount_000,colour= factor(As_of_Year))) +
  ggtitle('Loan Amount Distribution by State')

#To view no. of instances where nonconventional loan was issued to people wuth high income range
nrow(merged_data[which(merged_data$Applicant_Income_000 > 1000 & merged_data$Conventional_Status != 'Conventional'),])

#To view no. of instances where high amount unconventional loan was issued
nrow(merged_data[which(merged_data$Loan_Amount_000 > 1000 & merged_data$Conventional_Status != 'Conventional'),])

#Distribution in range 1k to 2000k
hist(merged_data$Loan_Amount_000, breaks=20000, xlim = range(1,2000))


# Technique for cleaning the Loan_Amount_000
# Create a function cleaning_LoanAmount() for Quality Assssment of Loan_Amount_000
cleaning_LoanAmount <- function(dat) {
  
  dat$Loan_Amount_000 = as.numeric(as.character(dat$Loan_Amount_000))
  
  dat$Loan_Amount_000[which(dat$Loan_Amount_000 >999)] = (dat$Loan_Amount_000)/1000
  
  return(dat)
}

# Call cleaning_LoanAmount(dat) function for Quality assessment of Loan_Amount_000
clean_data <- cleaning_LoanAmount(merged_data)


#### County_Name and County_Code ####

#These values are important for demographic segementation of market

 ## Data completeness ##

nrow(merged_data[which(merged_data$County_Name == ""),])
#First issue is that there are 837 rows in the data with missing County_Name

as.character(unique(merged_data$County_Code))
nrow(merged_data[which(merged_data$County_Code == "NA "),])
# Missing data causing loss of information: 837 rows


 ## Data integrity ##

length(unique(merged_data$County_Name))
length(unique(merged_data$County_Code))
#Differing lengths of key_value pairs

key_value_c <- unique(merged_data[,c('County_Code','State','County_Name')])

key_value_c$County_Code <- as.character(key_value_c$County_Code) #converting the Codes to character

rep_c <- count(key_value_c, County_Code) #group_by County_Code and count the number of rows for each Code

rep_c <- rep_c[which(rep_c$n > 1),] #filter out the Code cases where repition exist

key_value_c_repeat <- key_value_c[which(key_value_c$County_Code %in% rep_c$County_Code),]

#Issue here is that the county code is corresponding to a given state and is repeated within data for different states and hence creates confusion
# Better way would be to assign a unique code to every county present in the data


#### Applicant_Income_000 ####
#This is an important information w.r.t. Loan Apllication becuase the decision of loan origination or rejection highly depends on this value

 ## Data Completeness ##

nrow(merged_data[is.na(merged_data$Applicant_Income_000), ])
# Very high number of missing data: 117853 rows

  ## Data-entry error ##

# As observed above in the Loan_Amount section, the data scaling seems to be error prone
# might have had some rows where the data is entered as it is instead of scaling it on a scale of thousands
summary(merged_data$Applicant_Income_000)
hist(merged_data$Applicant_Income_000, breaks = 500, xlim = range(143,9999))

#now since maximum is at 9999, let's check what's happening here
nrow(merged_data[which(merged_data$Applicant_Income_000 == 9999), ])
hist(merged_data$Applicant_Income_000, breaks = 500, xlim = range(1000,9999), ylim = range(0,1000))

#Applicant opting for loan with Income greater than 1 million is unlikely
# So let's clean this variable similar to Loan_Amount

cleaning_ApplicantIncome <- function(dat) {
  
  dat$Applicant_Income_000 = as.numeric(as.character(dat$Applicant_Income_000))
  
  dat$Applicant_Income_000[which(dat$Applicant_Income_000 >999)] = (dat$Applicant_Income_000[which(dat$Applicant_Income_000 >999)])/1000
  
  return(dat)
}

