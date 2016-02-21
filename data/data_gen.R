clients <- c("The Green Onion", "Chuck's Chuck Wagon", "Piglet's Spicy Pork",
             "Old Peet's Good Eets", "Exotiq")

order_price <- round(runif(500, 25, 500), 2)

order_status <- ifelse(sample(c(0, 1), 500, replace = TRUE) == 1, "Completed", "In Progress")

order_date <- sample(seq(as.Date('2016-01-01'), as.Date('2016-01-20'), by = "day"), 500,
                     replace = TRUE)

due_date <- sample(seq(as.Date('2016-02-01'), as.Date('2016-02-25'), by = "day"), 500,
                   replace = TRUE)

client <- sample(clients, 500, replace = TRUE)

order_num <- 1:500

order_data <- data.frame(order_num, client, order_date, due_date, order_status, order_price)

client_lat <- c(39.68, 39.66, 39.76, 39.37, 40.03)

client_lon <- c(-104.90, -105.01, -105.10, -104.97, -105.13)

coords <- data.frame(clients, client_lat, client_lon)

order_data_rev <- order_data  %>% left_join(coords, by = c("client" = "clients"))

order_data_rev$client <- as.character(order_data$client)

saveRDS(order_data_rev, file = "~/Google_Drive/Andy_Dash/shiny_dash/data/order_data.rds")
