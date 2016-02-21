client_agg <- function(data) {
  sub_data <- data %>% filter(tolower(order_status) == 'completed' & as.integer(Sys.Date() - due_date) < 30)
  sub_data <- sub_data[,c('client', 'order_price')] %>%
    group_by(client) %>%
    summarise("Montly Total" = sum(order_price), 
              "Daily Average" = sum(order_price) / 30)
  colnames(sub_data)[1] <- "Client"
  return(sub_data)
}