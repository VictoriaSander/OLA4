# Læs logfiler fra din mappe
log_files <- list.files("/Users/victoriasanderramsing/Documents/1. semester eksamen /OLA4/udpakkede_filer", 
                        pattern = "access.log", 
                        full.names = TRUE)

# Læs hver logfil og kombinér dataene
log_data_list <- lapply(log_files, function(file) {
  read.table(file, header = FALSE, sep = " ", stringsAsFactors = FALSE)
})

# Kombinér logfilerne til én dataframe
log_data <- do.call(rbind, log_data_list)

# Angiv kolonnenavne
colnames(log_data) <- c("IP", "User", "User2", "Date_Time", "Timezone", "Request", "Status_Code", "Data_Size", "Referrer", "User_Agent")

# Omdan datoformatet
log_data$Date <- as.Date(sub("\\[([0-9]{2}/[a-zA-Z]{3}/[0-9]{4}):.*", "\\1", log_data$Date_Time), format = "%d/%b/%Y")

# Tæl antal forespørgsler per dag per IP
ip_per_day <- table(log_data$IP, log_data$Date)
ip_per_day_df <- as.data.frame(ip_per_day)
colnames(ip_per_day_df) <- c("IP", "Date", "Count")

# Pakker til analyse og visualisering
install.packages("dplyr")
library(dplyr)
library(ggplot2)

# Sammenfat IP-aktivitet
ip_activity <- log_data %>%
  group_by(IP) %>%
  summarise(requests = n())

# Top 10 mest aktive IP'er
top_ips_overall <- ip_activity %>%
  arrange(desc(requests)) %>%
  head(10)

# Top 3 mest aktive IP'er
top_ips_top3 <- ip_activity %>%
  arrange(desc(requests)) %>%
  head(3)

# Aktivitet per dag for de 10 mest aktive IP'er
top_ips_per_day <- ip_per_day_df %>%
  filter(IP %in% top_ips_overall$IP)

# Aktivitet per dag for de 3 mest aktive IP'er
top_ips_top3_per_day <- ip_per_day_df %>%
  filter(IP %in% top_ips_top3$IP)

# Visualisering af de 10 mest aktive IP'er
ggplot(top_ips_overall, aes(x = reorder(IP, -requests), y = requests)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "De 10 mest aktive IP-adresser", x = "IP-adresse", y = "Antal requests") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(top_ips_overall$requests), by = 500))

# Visualisering af de 3 mest aktive IP'er
ggplot(top_ips_top3, aes(x = reorder(IP, -requests), y = requests)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 3 mest aktive IP-adresser", x = "IP-adresse", y = "Antal requests") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(top_ips_top3$requests), by = 500))

# Visualisering af daglig aktivitet for de 10 mest aktive IP'er
ggplot(top_ips_per_day, aes(x = Date, y = Count, color = IP, group = IP)) +
  geom_line() +
  geom_point() +
  labs(title = "De 10 mest aktive IP-adresser pr. døgn", x = "Dato", y = "Antal requests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Whois-opslag for den mest aktive IP
most_active_ip <- ip_per_day_df[which.max(ip_per_day_df$Count), "IP"]
whois_info <- system(paste("whois", most_active_ip), intern = TRUE)
cat("Whois-info for den mest aktive IP:\n", whois_info)

# Analyser 404-fejl
log_404 <- subset(log_data, Status_Code == 404)
request_404_counts <- table(log_404$Request)
request_404_df <- as.data.frame(request_404_counts)
colnames(request_404_df) <- c("Request_Type", "Count")

# Find mistænkelige forespørgsler med statuskode 200
log_200 <- subset(log_data, Status_Code == 200)
suspicious_requests <- subset(log_200, grepl("GET", Request))
suspicious_requests_counts <- table(suspicious_requests$Request)
suspicious_requests_df <- as.data.frame(suspicious_requests_counts)
colnames(suspicious_requests_df) <- c("Request_Type", "Count")

# Visualisering af mistænkelige forespørgsler med forbedret layout
# Forkort lange forespørgselstyper
suspicious_requests_df$Request_Type <- substr(suspicious_requests_df$Request_Type, 1, 50)  # Begrænser længden til 50 tegn
ggplot(head(suspicious_requests_df, 10), aes(x = reorder(Request_Type, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Mistænkelige forespørgsler", x = "Forespørgselstype", y = "Antal") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
