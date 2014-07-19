
## constants
data_path <- "/Users/cray/Downloads/Incubator/Q3.csv"
outputQ3_3 <- "/Users/cray/Downloads/Incubator/output/Q3_3.txt"
outputQ3_4 <- "/Users/cray/Downloads/Incubator/output/Q3_4.txt"
cities <- c('A','B','C','D','E')
cities_len <- length(cities)
MAX_IMPOSSIBLE_NUM <- 99999

## define a useful printf()
printf <- function(...) cat(sprintf(...))

## read & sort, the data has not been cleaned
printf("Read file: %s ...", data_path)
data <- read.csv(data_path, header=TRUE, na.strings=-9999)
printf("OK!\n")

## delete all rows with -9999
#cleaned_data <- na.omit(data)

## sort by year, month, day, hour
sorted_data <- cleaned_data[with( cleaned_data, order(year, month, day, hour) ), ]

## sliding windows to calculate the distances every two hours
# add a 'city' column
sorted_data["city"] <- NA
# first 5 tags
sorted_data$city[c(1:cities_len)]<- cities
# take first 1,000 rows for short test
#sorted_data <- sorted_data[1:1000,]
attach(sorted_data)

## start to calculate the distances
printf("Calculating the distance...\n")
first_row <- 1
first_row_len <- cities_len
second_row <- 1 + first_row_len
total_rows <- dim(sorted_data)[1]
while (second_row < total_rows){
   if ( (second_row %% 1000) < cities_len ) {
       printf("%d...", second_row)  #show progress
   }
   ymdh <- c("year","month","day","hour")
   first_ymdh <- sorted_data[second_row, ymdh]
   potential_subset <- subset( sorted_data[second_row:(second_row+cities_len-1), ], (day==first_ymdh$day)&&(hour==first_ymdh$hour) )
   potential_subset_len <- dim(potential_subset)[1]
   end_row <- second_row + potential_subset_len -1
   
   # get two subset matrix
   tpww <- c("temp","pressure","wind_dir","wind_speed")
   first_matrix <- sorted_data[first_row:(first_row+first_row_len-1), tpww]
   next_matrix  <- potential_subset[1:potential_subset_len, tpww]
   both_matrix  <- rbind(first_matrix, next_matrix)
   dist_matrix  <- as.matrix( dist(both_matrix, diag=1) )
   
   # tag the city name one-by-one, for preventing duplications
   num_to_tag = potential_subset_len
   for (i in 1:num_to_tag) {
     this_row <- dist_matrix[ first_row_len+i, c(1:first_row_len) ]
     min_col <- which.min( this_row )
     sorted_data[second_row+i-1, "city"] <- cities[min_col]
     # make this column largest, for preventing duplications
     dist_matrix[, min_col] <- MAX_IMPOSSIBLE_NUM
   }

   first_row <- second_row
   first_row_len <- potential_subset_len
   second_row <- end_row +1
}

## Predicting...
printf("Start to predicting...\n")
library(forecast)
t_diff <- difftime("2012-03-03", "2011-12-31")
results_20120101 <- matrix(,ncol=0, nrow=24)  #only save final 24 hours of temperature
results_20120303 <- matrix(,ncol=0, nrow=24)  #only save final 24 hours of temperature
for (this_city in cities) {
  temp_matrix[this_city] <- subset(sorted_data, city==this_city, temp)
  
  ## make time series
  #ts_temp <- ts(temp_matrix[this_city], start=c(2000, 1), end=c(2011, 12)) 
  #fit = ets(ts_temp)
  
  # Fit an  model
  #fit <- auto.arima(temp_matrix[this_city])
  fit <- arima(x=temp_matrix[this_city], order=c(3,0,1), seasonal=c(0,1,2))
  print(Box.test(fit$residuals,lag=1))
  
  
  # Get point forecasts (12 of them)
  # 24 hours of temperature at Jan 1st, 2012
  forecasts <- predict(fit, n.ahead=24)
  results_20120101 <-  cbind(results_20120101, as.matrix(as.data.frame(forecasts)$pre) )
  
  # 24 hours of temperature at March 3rd, 2012
  num_ahead = 24*as.integer(t_diff)
  forecasts <- predict( fit, n.ahead=num_ahead )
  results_20120303 <-  cbind(results_20120303, as.matrix(as.data.frame(forecasts)$pre)[(num_ahead-23):num_ahead] )

}

## save to files
printf("Starting to save files:\n%s\n%s\n", outputQ3_3, outputQ3_4)
ok_20120101 <- round( results_20120101 ,0)
ok_20120303 <- round( results_20120303 ,0)
write.table(ok_20120101, outputQ3_3, sep=" ", row.names = TRUE, col.names = FALSE)
write.table(ok_20120303, outputQ3_4, sep=" ", row.names = TRUE, col.names = FALSE)

printf("\nDone!\n")









