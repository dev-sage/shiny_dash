library(readr)
library(lubridate)

##############
# Custom CSS # 
##############
form_css <- '.mand_red { color: red; }'

apply_css <- function(label) {
  tagList(label, span('*', class = 'mand_red'))
  
}

###################
# Reading in Data # 
###################
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
                                      order_interval = col_character(),
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

read_grow <- function() {
  grow <- read_csv("~/Dropbox/grow/grow.csv",
                       col_types = cols(order_num = col_character(),
                                        client = col_character(),
                                        delivery_date = col_date(format = "%Y-%m-%d"),
                                        product = col_character(),
                                        order_interval = col_character(),
                                        quantity = col_character(),
                                        note = col_character(),
                                        start_date = col_date(format = "%Y-%m-%d")))
  return(grow)
}

grow_data <- data.frame(order_num = "00000101100211",
                        client = "Gobbles",
                        delivery_date = '2015-03-02',
                        product = 'Sugar',
                        order_interval = 'Once',
                        quantity = '1 5x5',
                        note = 'cool_stuff',
                        start_date = '2015-02-02')

##########################
# Financial Summary Tabs # 
##########################
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

#################
# Grow Schedule # 
#################

# Generate grow dataset
add_to_grow <- function(order, products) {
  order_data <- data.frame(order_num = order$order_num,
                           client = order$client,
                           delivery_date = due_date,
                           product = product,
                           order_interval = order_interval,
                           quantity = order_quantity,
                           note = order_note)

  order_data$start_date <- order_data$delivery_date - products$days_to_grow[products$product_name == order_data$product]
  
  if(order_data$order_interval == '1 Week') { orders_for_year <- det_yearly_orders(order_data, 7) }
  else if(order_data$order_interval == '2 Weeks') { orders_for_year <- det_yearly_orders(order_data, 14) }
  else if(order_data$order_interval == '3 Weeks') { orders_for_year <- det_yearly_orders(order_data, 21) }
  else if(order_data$order_interval == 'Once Only') { orders_for_year <- order_data }
  
  print(orders_for_year)
  # return(orders_for_year)
}

det_yearly_orders <- function(order_data, interval) {
  year_of_orders <- order_data
  for(i in 2:(round(365/interval))) {
    next_order <- order_data
    next_order$delivery_data <- order_data$delivery_date + interval
    next_order$start_date <- order_data$start_date + interval
    year_of_orders <- rbind(year_of_orders, order_data)
    order_data <- next_order
    next_order <- NULL
  }
  return(year_of_orders)
}






