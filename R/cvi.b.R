
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
          
          #self$results$text$setContent(transformed_df)
          
          # Function to calculate proportion relevance
          proportion_relevance <- function(df) {
            # Calculate the number of 1s in each column
            total_ones <- rowSums(df == 1, na.rm = TRUE)
            
            # Calculate the proportion of 1s in each column
            proportions <- total_ones / ncol(df)
            
            return(proportions)
          }
          # proportion Relevance
          prpRev_df <- proportion_relevance(transformed_df)
          #self$results$text$setContent(prpRev_df)
          
          # Function to average the proportion relevance 
          average_proportion_relevance <- function(proportions) {
            # Sum up all proportions
            total_sum <- sum(proportions)
            
            # Calculate the average by dividing by the number of columns
            average <- total_sum / length(proportions)
            
            return(average)
          }
          
          # Calculate the average proportion relevance
          average_prpRev <- average_proportion_relevance(prpRev_df)
          
          # Function calculate expert in agreement
          sum_expert_agreement <- function(df) {
            # Apply the colSums function to the dataframe
            col_sums <- colSums(df, na.rm = TRUE)
            
            return(col_sums)
          }
          
          # Calculate the sum of expert agreement 
          expAgrSum_df <- sum_expert_agreement(transformed_df)
        
          #self$results$text$setContent(expAgrSum_df)
          
        
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
          I_CVI_values_df <- ICVI(transformed_df)
          #self$results$text$setContent(I_CVI_values_df)
          
          # Function to calculate the scvi_ave function
          scvi_ave <- function(df) {
            # Calculate the total of expert agreement (sum of all column sums)
            total_agreement <- sum(df)
            
            # Calculate the number of columns
            num_cols <- length(df)
            
            # Calculate S-CVI/Ave by dividing the total agreement by the number of columns
            scvi_ave_value <- total_agreement / num_cols
            
            return(scvi_ave_value)
          }
          
          # Use the sum_expert_agreement result as input
          scviAve <- scvi_ave(I_CVI_values_df)
          #self$results$text$setContent(scviAve)
          scviAvePR <-scvi_ave(prpRev_df)
          
          # Caclaute the UA function
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
          
          #calcualte svi/ua
          calculate_scvi_ua <- function(df) {
            # Calculate the sum of items with universal agreement (all 1s)
            items_with_ua <- sum(apply(df, 2, function(col) all(col == 1)))
            
            # Calculate total number of items
            total_items <- ncol(df)
            
            # Calculate S-CVI/UA
            scvi_ua <- items_with_ua / total_items
            
            return(scvi_ua)
          }
          
          # Calculate the UA values
          UA_values_df <- UA(transformed_df)
          scviUA <- calculate_scvi_ua(transformed_df)
          
          # Function to calculate Content Validity Ratio (CVR) 
          calculate_cvr <- function(df) {
            
            # Count the total number of expert (n)
            n <- nrow(df)
            
            # Count the number of expert indicating "essential" (assuming a rating of 3 or 4 is considered essential)
            ne <- sum(df[[1]] %in% c(3, 4))
            
            # Calculate CVR
            cvr <- (ne - (n / 2)) / (n / 2)
            
            return(cvr)
          }
          #calculate CVR
          cvr <- calculate_cvr(transformed_df)
          
          ########################
          #POPULATE TABLE SECTion#
          ########################
          
          
          #display the s-cvi average table on the result area
          table2 <- self$results$cviTable
          #row1 s-cvi/ave
          table2$setRow(rowNo=1, value=list(
            var = "S-CVI/Ave (based in I-CVI)",
            varSCVI = format(round(scviAve, digits=3), nsmall = 3)
          ))
          table2$setRow(rowNo=2, value=list(
            var="S-CVI/Ave (based on proportion relevance)",
            varSCVI = format(round(scviAvePR, digits=3), nsmall = 3)
          ))
          #row3 s-cvi/ua
          table2$setRow(rowNo=3, value=list(
            var="S-CVI/UA",
            varSCVI = format(round(scviUA, digits=3), nsmall = 3)
          ))
          table2$setRow(rowNo=4, value=list(
            var="Propotion Relevance",
            varSCVI = format(round(average_prpRev, digits=3), nsmall = 3)
          ))
          table2$setRow(rowNo=5, value=list(
            var="CVR",
            varSCVI = format(round(cvr, digits=3), nsmall = 3)
          ))

          # Initialize an empty dataframe
          exp_df <- data.frame(Value = character(), stringsAsFactors = FALSE)
          # Define the number of rows
          num_rows <- nrow(transformed_df)
          # Use a for loop to populate the dataframe
          for (i in 1:num_rows) {
            exp_df <- rbind(exp_df, data.frame(Value = paste0("Expert", i)))
          }
          #combine dataframe  
          combined_df <- cbind(exp_df, prpRev_df)
          
          table3 <- self$results$propTable
          # Populate the table
          for (rowNo in 1:nrow(combined_df)) {
            table3$addRow(rowNo, list(
              Expert = combined_df$Value[rowNo],
              Value = combined_df$prpRev_df[rowNo]
            ))
          }
          
          deps_df <- data.frame(Value = character(), stringsAsFactors = FALSE)
          
          for (item in self$options$dep){
            deps_df <- rbind(deps_df, data.frame(Value = paste(item)))
          }
          
          combined_score_df <- cbind(deps_df, expAgrSum_df, I_CVI_values_df, UA_values_df)
          
          table1 <- self$results$scoreTable2
          
          for(rowNo in 1:nrow(combined_score_df)){
            table1$addRow(rowNo, list(
              Item = combined_score_df$Value[rowNo],
              eA = combined_score_df$expAgrSum_df[rowNo],
              iCvi = combined_score_df$I_CVI_values_df[rowNo],
              uA = combined_score_df$UA_values_df[rowNo]
            ))
          }
          

          
      })
)
