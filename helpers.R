# Rewrite with closures.
financial_summ <- function(data) {
  total_orders <- length(data$order_num)
  total_payment <- format(round(sum(data$order_price), digits = 2), big.mark = ",", nsmall = 2)
  return(data.frame("Orders" = total_orders, "Payment"= total_payment))
}

client_agg <- function(data) {
  sub_data <- data[,c('client', 'order_price')] %>%
    group_by(client) %>%
    summarise("Montly Total" = format(sum(order_price), big.mark = ",", nsmall = 2), 
              "Daily Average" = format(round(sum(order_price) / 30, digits = 2), big.mark = ",", nsmall = 2),
              "Est. Yearly Total" = format(sum(order_price) * 12, big.mark = ",", nsmall = 2),
              "Number of Orders" = n())
  colnames(sub_data)[1] <- "Client"
  sub_data <- arrange(sub_data, desc(`Est. Yearly Total`))
  return(sub_data)
}

financial_summ_30 <- function(data) {
  data <- data %>% filter(tolower(order_status) == 'completed' & as.integer(Sys.Date() - due_date) < 30)
  total_orders <- length(data$order_num)
  total_payment <- format(round(sum(data$order_price), digits = 2), big.mark = ",", nsmall = 2)
  return(data.frame("Orders" = total_orders, "Payment"= total_payment))
}

client_agg_30 <- function(data) {
  sub_data <- data %>% filter(tolower(order_status) == 'completed' & as.integer(Sys.Date() - due_date) < 30)
  sub_data <- sub_data[,c('client', 'order_price')] %>%
    group_by(client) %>%
    summarise("Montly Total" = format(sum(order_price), big.mark = ",", nsmall = 2), 
              "Daily Average" = format(round(sum(order_price) / 30, digits = 2), big.mark = ",", nsmall = 2),
              "Est. Yearly Total" = format(sum(order_price) * 12, big.mark = ",", nsmall = 2),
              "Number of Orders" = n())
  colnames(sub_data)[1] <- "Client"
  sub_data <- arrange(sub_data, desc(`Est. Yearly Total`))
  return(sub_data)
}