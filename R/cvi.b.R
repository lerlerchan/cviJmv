
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
          
          
          # Calculate the UA values
          UA_values_df <- UA(transformed_df)
          scviUA <- sum(UA_values_df) /ncol(transformed_df)
          #self$results$text$setContent(UA_values_df)
          
          #display frequency
          table1 <- self$results$scoreTable
          for (name1 in self$options$dep){
            table1$addColumn(name1, title=name1)
          }
      
          for (colNo in seq_along(expAgrSum_df)) {
            # Extract the column as a list
            values <- as.list(expAgrSum_df[colNo])
            table1$setRow(rowNo = 1, values)
          }
          table1$setRow(rowNo = 1, list(var = paste("Experts in Agreement")))
          
          for (colNo in seq_along(I_CVI_values_df)){
            values <- as.list(I_CVI_values_df[colNo])
            table1$setRow(rowNo = 2, values)
          }
          table1$setRow(rowNo = 2, list(var = paste("I-CVI")))
          
          # Define the number of rows and columns
          num_rows <- 1
          num_cols <- ncol(transformed_df)
          # Create a DataFrame filled with 0.000
          df <- data.frame(matrix(rep(0.000, num_rows * num_cols), nrow = num_rows, ncol = num_cols))
          
          # Set column names (optional)
          colnames(df) <- paste0("Column", 1:num_cols)
          
  
          if (is.null(UA_values_df)) {
            #self$results$text$setContent("NULL")
            for (colNo in seq_along(UA_values_df)) {
              values <- as.list(UA_values_df[colNo])
              table1$setRow(rowNo = 3, values)
            }
          }else{
            #self$results$text$setContent("NOT NULL")
            # Display 0.00 if UA_values_df is NULL
            for (colNo in seq_along(df)) {
              values <- as.list(df[colNo])
              table1$setRow(rowNo = 3, values)
              }
          }
          
          
          table1$setRow(rowNo = 3, list(var = paste("Universal Agreement (UA)")))
          
          
          # Assuming prpRev_df is your data frame
          num_rows <- nrow(prpRev_df)

          
         # self$results$text$setContent(num_rows)
          
         # for (colNon in seq_len(nrow(UA_values_df))) {
        #    row <- as.list(UA_values_df[colNon, drop=TRUE])
        #    row$var <- 'Universal Agreement (UA)'
        #    table1$setRow(rowNo=colNon, values=row)
        #  }
    
          
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
          
          #output proportion relevance of experts in table
          #table3 <- self$results$propTable
         # for (colNom in seq_along(prpRev_df)) {
        #    if (colNom > table3$rowCount)
        #      break()
        #    values <- as.list(prpRev_df[colNom])
        #    table3$setRow(rowNo=colNom, values)
        #    table3$setRow(rowNo=colNom, list(var=paste("Expert ", colNom)))
        # }

          self$results$text$setContent(prpRev_df)
          
          # Initialize an empty dataframe
          exp_df <- data.frame(Value = character(), stringsAsFactors = FALSE)
          # Define the number of rows
          num_rows <- nrow(transformed_df)
          # Use a for loop to populate the dataframe
          for (i in 1:num_rows) {
            exp_df <- rbind(exp_df, data.frame(Value = paste0("Expert", i)))
          }
          
          table3 <- self$results$propTable
          # Iterate over the column of names from exp_df, not the entire dataframe
          for (expert in exp_df$Value) {
            # expert is now a string, so you can safely pass it to addColumn()
            table3$addColumn(expert, title = as.character(expert))
          }
          
          for (colNo in seq_along(prpRev_df)) {
            # Extract the column as a list
            values <- as.list(prpRev_df[colNo])
            table3$setRow(rowNo = 1, values)
          }
          table3$setRow(rowNo = 1, list(var = paste("Proporation relevance Score")))
          
      })
)
