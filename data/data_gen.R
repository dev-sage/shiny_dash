clients <- c("The Green Onion", "Chuck's Chuck Wagon", "Piglet's Spicy Pork",
             "Old Peet's Good Eets", "Exotiq")

order_price <- round(runif(500, 25, 500), 2)

order_status <- ifelse(sample(c(0, 1), 500, replace = TRUE) == 1, "Completed", "In Progress")

order_date <- sample(seq(as.Date('2015-03-10'), as.Date('2015-03-20'), by = "day"), 500,
                     replace = TRUE)

due_date <- sample(seq(as.Date('2015-04-15'), as.Date('2015-6-15'), by = "day"), 500,
                   replace = TRUE)

client <- sample(clients, 500, replace = TRUE)

order_num <- 1:500

order_data <- data.frame(order_num, client, order_date, due_date, order_status, order_price)

saveRDS(order_data, file = "~/Google_Drive/Andy_Dash/data/order_data.RDS")
head(order_data)
