library(readr)

read_orders <- function() {
  orders <- read_csv("~/Dropbox/orders/orders.csv", 
                     col_types = cols(order_num = col_integer(),
                                      client = col_character(),
                                      order_placed_date = col_date(format = "%Y-%m-%d"),
                                      due_date = col_date(format = "%Y-%m-%d"), 
                                      order_price = col_double(),
                                      product = col_character(),
                                      order_quantity = col_character(),
                                      order_note = col_character(),
                                      order_status = col_character()))
  return(orders)
}

read_clients <- function() {
  clients <- read_csv("~/Dropbox/clients/clients.csv",
                      col_types = cols(client_name = col_character(),
                                       client_note = col_character(),
                                       client_lng = col_double(),
                                       client_lat = col_double()))
  return(clients)
}

read_products <- function() {
  products <- read_csv("~/Dropbox/products/products.csv",
                       col_types = cols(product_name = col_character(),
                                        five_by_five_amt = col_character(),
                                        half_tray_amt = col_character(),
                                        full_tray_amt = col_character(),
                                        days_to_grow = col_character(),
                                        product_note = col_character()))
  return(products)
}


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