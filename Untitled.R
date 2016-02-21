
  sub_data <- order_data %>% 
    filter(tolower(order_status) == 'completed' & as.integer(Sys.Date() - due_date) < 30)
  
  sub_data <- sub_data[,c('client', 'order_price')] %>% group_by(client) %>% summarise("Client",
                                                                                       "Montly Total" = sum(order_price),
                                                                                       "Daily Average" = sum(order_price) / 30)
  