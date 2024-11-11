load_data <- function(filepath) {
    data <- readxl::read_excel(filepath, 
        col_types = c("date", "text", "text", 
        "text", "text", "text", "text", "numeric", 
        "text", "text"))
    
    # Ensure the 'Date' column is in date format
    data$Date <- as.Date(data$Date)
    
    # Create Month, Quarter, and Year columns
    data <- data %>%
        mutate(
            Quarter = paste("Q", quarter(Date), sep = ""),
            Year = year(Date)
        )
    return(data)
}


