# visualizations

prod_segment <- ggplot(merged_data, aes(x = State, fill = product_segment)) +
  geom_bar(stat = 'count', position = 'dodge') + facet_grid(~As_of_Year) +
  labs(y = "Market_size", title = 'Product segment market-size by state and year')

ggplotly(prod_segment) 

#Assuming we are only interested in Conventional/Conforming markets, filtering out the loans that are Non-conventional&Non-Conforming
merged_data_filtered <- merged_data[which(merged_data$product_segment != 'NonConventional_NonConforming'),]

#plotting the market share in terms of no. of loan applications for each states
fin <- ggplot(merged_data_filtered, aes(x = State,group=factor(As_of_Year))) +
  geom_bar(data = merged_data_filtered,aes(x = as.factor(State), y = ..prop.., fill = factor(..x..)),stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5 ) +
  labs(y = "Market_share", fill="State", title = 'Market-size by state and year') +
  facet_grid(~As_of_Year) + scale_y_continuous(labels = scales::percent)

fin

# plotting the graph to observe the change in market size year over year
p <- ggplot() + 
  geom_bar(aes(fill = factor(As_of_Year), x = State), data = merged_data_filtered, position = 'dodge') +
  labs(x = 'State', y = 'Size', fill='Year', title = 'Market Size change year over years')

ggplotly(p)  


#function to calculate change in market-size for the state in a given year

market_change <- function(dat, state, year1, year2){
  
  if (state %in% c('DC','DE','MD','VA','WV')){
    if(year1 %in% c(2012,2013,2014) & year2 %in% c(2012,2013,2014)){
      a <- nrow(dat[which(dat$As_of_Year == year1 & dat$State == state),])
      b <- nrow(dat[which(dat$As_of_Year == year2 & dat$State == state),])
      change <- round(((b-a)/a * 100), digits=2)
      return(paste(change,'%'))
    }
    else{
      return(print("please enter year in range[2012,2014]"))
    }
  }
  else{
    return(print("please enter state abrreviation from: 'DC','DE','MD','VA','WV'"))
  }
}

market_size_change <- as.data.frame(rep(0,5))
market_size_change$state <- c('DC','DE','MD','VA','WV')

for(i in 1:5){
  market_size_change$y_2012_2013[i] <- market_change(merged_data_filtered, market_size_change$state[i], 2012,2013)
  market_size_change$y_2013_2014[i] <- market_change(merged_data_filtered, market_size_change$state[i], 2013,2014)
}

market_size_change <- market_size_change[,2:4]

#plotting the market-volume i.e. the total loan_amounts for a given year in a given state
volume <- ggplot(merged_data_filtered) +
  geom_bar(data = merged_data_filtered,aes(x = as.factor(State), weight = Loan_Amount_000, group = factor(As_of_Year),fill = factor(..x..)))+
  facet_grid(~As_of_Year)+ scale_y_continuous(labels = scales::dollar) +
  labs(y = "Total_market_volume", fill="State", title = 'Market-volume by state and year', x = 'State')
  
volume

#The concern that arises from the above graph is that the size of the states vary and affect the totla_market_volume
# since the census tracts roughly depend upon population size with an optimum 4000, the count of census tracts in a given state might be an important metric
# so a better metric for market-volume might be to find the average volume per census tract in a state for a given year

avg_volume_per_ct <- function(dat){
  dat$vol_per_ct <- NULL
  for ( i in 1:nrow(dat)){
      tot_vol <- sum(merged_data[which(merged_data$State == dat$state[i] & merged_data$As_of_Year == dat$year[i]),]$Loan_Amount_000)
      count_ct <- length(unique(merged_data[which(merged_data$State == dat$state[i] & merged_data$As_of_Year == dat$year[i]),]$Census_Tract_Number))
      dat$vol_per_ct[i] <- tot_vol/count_ct
  }
  return(dat)
}

state <- rep(unique(merged_data_filtered$State), length(unique(merged_data_filtered$As_of_Year)))
year <- c(rep(2012,5),rep(2013,5),rep(2014,5))
data_avg_vol <- data.frame(state,year)

data_avg_vol <- avg_volume_per_ct(data_avg_vol)

volume_per_ct <- ggplot(data_avg_vol) +
  geom_bar(data = data_avg_vol,aes(x = as.factor(state), weight = vol_per_ct, group = factor(year),fill = factor(..x..)))+
  facet_grid(~year)+ scale_y_continuous(labels = scales::dollar) +
  labs(y = "Avg_market_volume", fill="State", title = 'Avg Market-volume per census tract by state and year', x = 'State')

volume_per_ct

#cleaning Applicant_Income column
merged_data_filtered$Applicant_Income_000[is.na(merged_data_filtered$Applicant_Income_000)] <- 0
merged_data_filtered$Applicant_Income_000[which(merged_data_filtered$Applicant_Income_000==0)] <- mean(merged_data_filtered$Applicant_Income_000)

mean_income_state <- 	aggregate(merged_data_filtered$Applicant_Income_000,by=list(State=merged_data_filtered$State),mean,na.rm=TRUE)
colnames(mean_income_state) <- c('State','mean_income')


attach(mean_income_state)
symbols(State, mean_income, mean_income*100, inches=0.60, fg="white", bg="lightblue",
        main="Bubble Plot with point size proportional to mean_income",
        ylab="Mean_Income",
        xlab="State")
text(State, mean_income, paste(State, paste0(round(mean_income, 2),'k'), sep=','), cex=1)
detach(mean_income_state)

#Conforming limit visualization

ggplot(merged_data_filtered[which(merged_data_filtered$Conforming_Limit_000 != 'NA'),]) + 
  geom_bar(data = merged_data_filtered[which(merged_data_filtered$Conforming_Limit_000 != 'NA'),], aes(x=factor(Conforming_Limit_000), fill = factor(As_of_Year)), position = 'dodge')+
  labs(x = 'Conforming limit in thousands', y='Number of applications', fill='Year', title = "Market-size by conforming limit") +
  coord_flip()


