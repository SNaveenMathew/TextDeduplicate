
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
#*********************************STEP 3 - Create custom function to get all similarity metrics******************************************************
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************

get_ratios <- function(data, allow_remove = T, colname1 = "Cleaned_Address",colname2 = "X1") {
  df1 <- sapply(1:nrow(data), function(u) {
    string1 <- data[, colname1][u]
    string2 <- data[, colname2][u]
    part_rat <- matcher$Partial_ratio(string1 = string1, string2 = string2)
    
    part_token_set_ratio <-matcher$Partial_token_set_ratio(string1 = string1, string2 = string2)
    
    part_token_sort_ratio <-matcher$Partial_token_sort_ratio(string1 = string1, string2 = string2)
    
    qrat <- matcher$QRATIO(string1 = string1, string2 = string2)
    
    rat <- matcher$Ratio(string1 = string1, string2 = string2)
    
    token_set_rat <-matcher$Token_set_ratio(string1 = string1, string2 = string2)
    
    token_sort_rat <-matcher$Token_sort_ratio(string1 = string1, string2 = string2)
    
    uqrat <- matcher$UQRATIO(string1 = string1, string2 = string2)
    
    wrat <- matcher$UWRATIO(string1 = string1, string2 = string2)
    
    uwrat <- matcher$UWRATIO(string1 = string1, string2 = string2)
    
    vec <-c(part_rat, part_token_set_ratio, part_token_sort_ratio, qrat, rat, token_set_rat, token_sort_rat, uqrat, wrat, uwrat)
    
    return(vec)
  })
  df1 <- data.frame(t(df1))
  sds <- sapply(df1, sd)
  colnames(df1) <-c("part_rat", "part_token_set_ratio", "part_token_sort_ratio", "qrat", "rat", "token_set_rat", "token_sort_rat", "uqrat", "wrat", "uwrat")
  if (allow_remove) {
    sd_0 <- colnames(df1)[sds == 0]
    df1 <- df1[sds != 0]
    cors <- cor(df1)
    removes <- c()
    for (i in 1:(ncol(df1) - 1)) {
      for (j in (i + 1):ncol(df1)) {
        if (cors[i, j] == 1 | cors[i, j] == -1) {
          removes <- c(removes, colnames(df1)[i])
        }
      }
    }
    removes <- unique(c(sd_0, removes))
    df1 <- df1[, !(colnames(df1) %in% removes)]
  }
  data <- cbind(data, df1)
  return(data)
}
