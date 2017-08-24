
###### init ######

#creating the initialization function
init <- function(){

institution <- read.csv("2012_to_2014_institutions_data.csv") #reading the instituition data from disk
loan_app <- read.csv("2012_to_2014_loans_data.csv") # reading the loan application data
nbuckets <- 4 # No. of buckets needed for loan amount, it can be modified for other values if needed

#merging the 2 tables by agency_code, respondent_ID and As_of_Year; and keeping only the Respondent_Name_TS
new_data <- merge(x = loan_app, y = institution[ , c("Agency_Code", "As_of_Year", "Respondent_ID","Respondent_Name_TS")], 
                     all.x=T, by = c("Agency_Code", "As_of_Year", "Respondent_ID"))


#removing the data not needed anymore for better memory utilization
rm(institution)
rm(loan_app)

#Converting the amount and income values to integer
new_data$Applicant_Income_000 <- as.integer(as.character(new_data$Applicant_Income_000))
new_data$Loan_Amount_000 <- as.integer(as.character(new_data$Loan_Amount_000))

#Now since we need to create buckets for loan amount, it'd be better to clean it before binning
#Logic for this will be explained in the data quality section of code. These functions will clean the loan_Amount and the applicant_income variables

cleaning_LoanAmount <- function(dat) {
  
  dat$Loan_Amount_000[which(dat$Loan_Amount_000 >999)] = round((dat$Loan_Amount_000[which(dat$Loan_Amount_000 >999)])/1000)
  
  return(dat)
}

cleaning_ApplicantIncome <- function(dat) {
  
  dat$Applicant_Income_000 = as.numeric(as.character(dat$Applicant_Income_000))
  
  dat$Applicant_Income_000[which(dat$Applicant_Income_000 >999)] = (dat$Applicant_Income_000[which(dat$Applicant_Income_000 >999)])/1000
  
  return(dat)
}

#getting the cleaned data using the above functions
clean_data <- cleaning_LoanAmount(new_data)
clean_data <- cleaning_ApplicantIncome(clean_data)

#Now creating a function to get non-NA values for loan amount
#Note: though this data set doesn't have any NA value for Loan_Amount_000, 
# but it's better to make the function robust for future values
ntile_notna <- function(a, n){
  
  out <- rep(NA, length(a))
  a_na <- is.na(a)
  out[!a_na] <- ntile(a[!a_na],n)
  return(out)
  }

#equal frequency discretization for loan_amount buckets
clean_data <- clean_data %>% mutate(Loan_buckets = ntile_notna(clean_data$Loan_Amount_000, nbuckets))

#creating new variable for product segments based on categories of loan
clean_data$product_segment <- as.factor(case_when(
  clean_data$Conventional_Conforming_Flag == 'Y' ~ 'Conventional_Conforming',
  clean_data$Conventional_Conforming_Flag == 'N' & clean_data$Conventional_Status == 'Conventional' ~ 'Conventional_Jumbo',
  clean_data$Conventional_Conforming_Flag == 'N' & clean_data$Conforming_Status == 'Conforming' ~ 'NonConventional_Conforming',
  clean_data$Conventional_Conforming_Flag == 'N' ~ 'NonConventional_NonConforming'
))

return(clean_data) #return the final data object

}


merged_data <- init()


######## hmd_to_json function ########

# here the states argument will take the state abbreviations as mentioned in original loandata
# and there are 3 product segments considered: conventional_conforming accept either of the values 'conventional_jumbo' or 'nonconventional_conforming' or 'conventional_conforming' or 'nonconventional_nonconfirming'

to_json <- function(data, states, conventional_conforming){
  
  #this if block is for case when conventional_conforming is missing
  if(missing(conventional_conforming)){
    #return original data when states are missing as well
    if(missing(states)){
      out <- data
    }
    # when states are not missing, check if the states has valid values matching the values data
    else if (all(states %in% unique(data$State)) == TRUE){
      out <- data[which(data$State %in% states), ] #filter data by the given states
    }
    #error handling for invalid state values
    else{
      return(print(paste('please enter valid state abbreviation from:', paste(as.character(unique(data$State)), sep = " ", collapse = ","), sep = " ")))
    }
  }
  # else if block for case when conventional_conforming is not missing but state is missing
  else if(missing(states)){
    
    #in the following if else if blocks, filtering for given product segement
    
    if(tolower(conventional_conforming) == 'conventional_conforming'){   # tolower funciton used to avoid case-sensitivity of input
      out <- data[which(data$product_segment == 'Conventional_Conforming'), ]
    }
    else if(tolower(conventional_conforming) == 'conventional_jumbo'){  # tolower funciton used to avoid case-sensitivity of input
      out <- data[which(data$product_segment == 'Conventional_Jumbo'), ]
    }
    else if(tolower(conventional_conforming) == 'nonconventional_conforming'){  # tolower funciton used to avoid case-sensitivity of input
      out <- data[which(data$product_segment == 'NonConventional_Conforming'), ]
    }
    else if(tolower(conventional_conforming) == 'nonconventional_nonconforming'){   # tolower funciton used to avoid case-sensitivity of input
      out <- data[which(data$product_segment == 'NonConventional_NonConforming'), ]
    }
    #final else for error handling in case when input doesn't match any product segment
    else return(print("please enter either of these conventional_conforming values: 'conventional_jumbo' or 'nonconventional_conforming' or 'conventional_conforming' or 'nonconventional_nonconfirming'"))
  }
  
  
  # final else block for case when neither of the states or conventional_conforming is missing 
  else{
    # checking for valid state input
    if (all(states %in% unique(data$State)) == TRUE){
      
      # if state input valid, filter by the state and the product segment
      if(tolower(conventional_conforming) == 'conventional_conforming'){   # tolower funciton used to avoid case-sensitivity of input
        out <- data[which(data$product_segment == 'Conventional_Conforming'), ]
      }
      else if(tolower(conventional_conforming) == 'conventional_jumbo'){
        out <- data[which(data$product_segment == 'Conventional_Jumbo'), ]
      }
      else if(tolower(conventional_conforming) == 'nonconventional_conforming'){
        out <- data[which(data$product_segment == 'NonConventional_Conforming'), ]
      }
      else if(tolower(conventional_conforming) == 'nonconventional_nonconforming'){
        out <- data[which(data$product_segment == 'NonConventional_NonConforming'), ]
      }
      # for error handling in case of invalid product segment
      else return(print("please enter either of these conventional_conforming values: 'conventional_jumbo' or 'nonconventional_conforming' or 'conventional_conforming' or 'nonconventional_nonconfirming'"))
    }
    # for error handling in case of invalid state input
    else{
      return(print(paste('please enter valid state abbreviation from:', paste(as.character(unique(data$State)), sep = " ", collapse = ","), sep = " ")))
    }
  }

  #finally converting the filtered data to json and writing it on the disk at path mentioned in path_write
  # some part of the following code is taken from https://blog.exploratory.io/saving-the-data-to-json-file-1dedb8d31a37#.pbh9kstf0
  out %>% 
    toJSON() %>%
    write_lines(paste(path_write,paste(paste(states,collapse = "_"),conventional_conforming,"data.JSON", sep = "_"), sep="/"))
  #in above line, naming the file written on disk with the state mentioned and product segment

}

