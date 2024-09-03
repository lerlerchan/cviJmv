
# This file is a generated template, your changes will not be overwritten

CVIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "CVIClass",
    inherit = CVIBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          data <- self$data
          
          # Define the function
          # convert 0,1,2 to 0
          # convert 3, 4 to 1
          transform_cells <- function(df) {
            # Apply the transformation to the entire dataframe
            df <- as.data.frame(lapply(df, function(x) {
              ifelse(x %in% c(0, 1, 2), 0, 
                     ifelse(x %in% c(3, 4), 1, x))
            }))
            
            return(df)
          }
          
          # Transform the dataframe
          transformed_df <- transform_cells(data)
          
         # self$results$text$setContent(transformed_df)
          
          # Function to calculate proportion relevance
          proportion_relevance <- function(df) {
            # Calculate the number of 1s in each column
            total_ones <- rowSums(df == 1, na.rm = TRUE)
            
            # Calculate the proportion of 1s in each column
            proportions <- total_ones / ncol(df)
            
            return(proportions)
          }
          # proportion Relevance
          prpRev <- proportion_relevance(transformed_df)
          
          # Function to average the proportion relevance 
          average_proportion_relevance <- function(proportions) {
            # Sum up all proportions
            total_sum <- sum(proportions)
            
            # Calculate the average by dividing by the number of columns
            average <- total_sum / length(proportions)
            
            return(average)
          }
          
          # Calculate the average proportion relevance
          average_prpRev <- average_proportion_relevance(prpRev)
          
          # Function calculate expert in agreement
          sum_expert_agreement <- function(df) {
            # Apply the colSums function to the dataframe
            col_sums <- colSums(df, na.rm = TRUE)
            
            return(col_sums)
          }
          
          # Calculate the sum of values 
          expAgrSum <- sum_expert_agreement(transformed_df)
          
         # self$results$text$setContent(expAgrSum)
          
          # Function calculate the I-CVI function
          ICVI <- function(df) {
            # Calculate the sum of values in each col
            col_sums <- colSums(df, na.rm = TRUE)
            
            # Calculate the number of rows
            num_rows <- nrow(df)
            
            # Calculate I-CVI by dividing row sums by the number of rows
            I_CVI_values <- col_sums / num_rows
            
            return(I_CVI_values)
          }
          
          
          # Calculate the I-CVI values
          I_CVI_values <- ICVI(transformed_df)
          
          # Define the scvi_ave function
          scvi_ave <- function(col_sums) {
            # Calculate the total of expert agreement (sum of all column sums)
            total_agreement <- sum(col_sums)
            
            # Calculate the number of columns
            num_cols <- length(col_sums)
            
            # Calculate S-CVI/Ave by dividing the total agreement by the number of columns
            scvi_ave_value <- total_agreement / num_cols
            
            return(scvi_ave_value)
          }
          
          # Use the sum_expert_agreement result as input
          scviAve <- scvi_ave(expAgrSum)
          
          # Define the UA function
          UA <- function(df) {
            # Calculate the sum of each column
            col_sums <- colSums(df, na.rm = TRUE)
            
            # Calculate the total sum of all columns
            total_sum <- sum(col_sums)
            
            # Initialize a vector to store the results
            UA_values <- numeric(length(col_sums))
            
            # Loop through each column sum and categorize
            for (i in seq_along(col_sums)) {
              if (col_sums[i] / total_sum == 1) {
                UA_values[i] <- 1
              } else {
                UA_values[i] <- 0
              }
            }
            
            # Return the UA categorization
            return(UA_values)
          }
          
          
          # Calculate the UA values
          UA_values <- UA(transformed_df)
          
          # Print the UA values
          print(UA_values)
          
          scviUA <- sum(UA_values) /ncol(transformed_df)
          
          print(scviUA)
          

          
          
          
        })
)
