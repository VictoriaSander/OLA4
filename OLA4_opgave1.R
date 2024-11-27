###################################################
################ OPGAVE 1 - WEBSCRAPE #############
###################################################
# ---------------------------------------------------------------------------------------------------------------------------
### OPGAVE 1.1 - HENT DATA FRA BILBASEN.DK ###

# Importér nødvendige biblioteker
library(httr)        # Til HTTP-anmodninger
library(rvest)       # Til webscraping
library(tidyverse)   # Til datahåndtering, manipulation og visualisering
library(stringr)     # Til avanceret tekstmanipulation
library(readr)       # Til at læse/skrive CSV-filer

# Definér URL til VW ID4 biler på Bilbasen
vwlink <- "https://www.bilbasen.dk/brugt/bil/vw/id2314?fuel=3&includeengroscvr=true&includeleasing=false"

# Definér HTTP-headers for at undgå at blive blokeret
headers <- add_headers(
  `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0 Safari/537.36",
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
  `referer` = "https://www.bilbasen.dk",
  `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7"
)

# Send en HTTP-anmodning til Bilbasen for at finde antal sider
response <- GET(vwlink, headers)
if (status_code(response) != 200) {
  stop("Fejl ved hentning af data. Statuskode:", status_code(response))
}

# Find antal sider baseret på pagination
page <- read_html(content(response, as = "text", encoding = "UTF-8"))
last_page <- page %>%
  html_element('span[data-e2e="pagination-total"]') %>% 
  html_text(trim = TRUE) %>% 
  as.numeric()
cat("Antal sider fundet:", last_page, "\n")

# Initialiser en tom dataframe
vwdf <- tibble(
  Pris = character(),
  Model = character(),
  Detaljer = character(),
  Beskrivelse = character(),
  Lokation = character(),
  ForhandlerID = character(),
  Forhandler_navn = character(),
  Forhandler_adresse = character(),
  Forhandler_CVR = character(),
  Link_til_bil = character()
)

# Loop gennem hver side på Bilbasen
for (i in 1:last_page) {
  cat("Henter data fra side:", i, "\n")
  loopurl <- paste0(vwlink, "&page=", i)
  Sys.sleep(runif(1, min = 5, max = 10))  # Tilføj pause for at undgå blokering
  
  response <- GET(loopurl, headers)
  page <- read_html(content(response, as = "text", encoding = "UTF-8"))
  carlist <- page %>% html_elements("article.Listing_listing__XwaYe")
  
  # Loop gennem hver bil og hent data
  for (car in carlist) {
    tryCatch({
      Model <- car %>% html_element(".Listing_makeModel__7yqgs") %>% html_text(trim = TRUE)
      Pris <- car %>% html_element(".Listing_price__6B3kE") %>% html_text(trim = TRUE)
      Detaljer <- car %>% html_elements(".ListingDetails_listItem___omDg") %>%
        html_text(trim = TRUE) %>% paste(collapse = ", ")
      Beskrivelse <- car %>% html_element(".Listing_description__sCNNM") %>% html_text(trim = TRUE)
      Lokation <- car %>% html_element(".Listing_location__nKGQz") %>% html_text(trim = TRUE)
      Link_til_bil <- car %>% html_element("a") %>% html_attr("href")
      
      # Sørg for komplet URL til bilens detaljeside
      car_detail_url <- ifelse(startsWith(Link_til_bil, "/"), paste0("https://www.bilbasen.dk", Link_til_bil), Link_til_bil)
      
      # Hent detaljeret data fra bilens side
      Sys.sleep(runif(1, min = 1, max = 3))
      car_detail_response <- GET(car_detail_url, headers)
      car_detail_page <- read_html(content(car_detail_response, as = "text", encoding = "UTF-8"))
      
      ForhandlerID <- car_detail_page %>%
        html_element('a[data-e2e="sellers-other-ads-cta"]') %>% 
        html_attr("href") %>% str_extract("id\\d+")
      Forhandler_navn <- car_detail_page %>% 
        html_element(".bas-MuiTypography-root.bas-MuiVipSectionComponent-sectionHeader.bas-MuiTypography-h3") %>% 
        html_text(trim = TRUE)
      Forhandler_adresse <- car_detail_page %>% 
        html_element(".bas-MuiSellerInfoComponent-addressWrapper") %>% 
        html_text(trim = TRUE)
      Forhandler_CVR <- car_detail_page %>% 
        html_element(".bas-MuiSellerInfoComponent-cvr") %>% 
        html_text(trim = TRUE)
      
      # Tilføj data til dataframe
      vwdf <- vwdf %>%
        add_row(
          Pris = Pris,
          Model = Model,
          Detaljer = Detaljer,
          Beskrivelse = Beskrivelse,
          Lokation = Lokation,
          ForhandlerID = ForhandlerID,
          Forhandler_navn = Forhandler_navn,
          Forhandler_adresse = Forhandler_adresse,
          Forhandler_CVR = Forhandler_CVR,
          Link_til_bil = car_detail_url
        )
    }, error = function(e) {
      message("Fejl i dataekstraktion: ", e)
    })
  }
}

# Gem data som CSV-fil
write_csv(vwdf, paste0("VWid4_DATA_RÅFIL__BILBASEN", Sys.Date(), ".csv"))
message("Data gemt.")
# ---------------------------------------------------------------------------------------------------------------------------
########## OPGAVE 1.2 - RENSE DATA ###############
# Funktion til at fjerne emojis og rense tekst
rens_tekst <- function(tekst) {
  tekst %>%
    str_replace_all("[\U0001F600-\U0001F64F]", "") %>%
    str_replace_all("[\U0001F300-\U0001F5FF]", "") %>%
    str_replace_all("[\U0001F680-\U0001F6FF]", "") %>%
    str_replace_all("\n", ". ") %>%
    str_replace_all("[^\\w\\s.,]", "") %>%
    str_replace_all("\\s{2,}", " ") %>%
    str_trim()
}

# Anvend funktionen til at rense 'Beskrivelse' kolonnen
vwdf <- vwdf %>%
  mutate(
    Beskrivelse = map_chr(Beskrivelse, rens_tekst),
    car_id = str_extract(Link_til_bil, "\\d+$")
  )

# Gem renset data
write_csv(vwdf, paste0("VWid4_RENSET_DATA_BILBASEN", Sys.Date(), ".csv"))
message("Renset data gemt.")

# ---------------------------------------------------------------------------------------------------------------------------
######### OPGAVE 1.3 - HENTE NYE DATA - SIMULERT ############
# Denne opgave simulerer ændringer i datasættet ved at:
# 1. Fjerne 5 biler for at simulere salg.
# 2. Tilføje 2 nye biler.
# 3. Ændre priser for 3 tilfældige biler.

### Trin 1: Tilføj scrapedate til det oprindelige datasæt ###
# Definer den oprindelige scrapedate
oprindelig_scrapedate <- as.Date("2024-11-22")

# Tilføj scrapedate som en ny kolonne i datasættet
vwdf <- vwdf %>%
  mutate(scrapedate = oprindelig_scrapedate)

write_csv(vwdf, paste0("VWid4_RENSET_DATA_BILBASEN_DONE", Sys.Date(), ".csv"))


# Kopiér data og opdater scrapedate for simulering
ny_scrapedate <- oprindelig_scrapedate + 1  # Simuler en ny scrapedate (én dag senere)
simuleret_vwdf <- vwdf %>%
  mutate(scrapedate = ny_scrapedate)

### Trin 2: Fjern 5 biler for at simulere salg ###
# Sæt seed for at sikre, at de samme biler vælges ved hver kørsel
set.seed(123)

# Vælg tilfældigt 5 biler, der skal fjernes
solgte_biler <- sample(nrow(simuleret_vwdf), 5)

# Fjern de valgte rækker fra datasættet
simuleret_vwdf <- simuleret_vwdf[-solgte_biler, ]

# Udskriv hvilke rækker der blev fjernet
print("De følgende biler blev fjernet (solgte biler):")
fjernede_biler <- vwdf[solgte_biler, ]  # Gem de fjernede biler for dokumentation
print(fjernede_biler)

### Trin 3: Tilføj 2 nye biler ###
# Opret en tabel med data for de nye biler
nye_biler <- tibble(
  Pris = c("400,000", "420,000"),                # Pris i DKK
  Model = c("VW ID4 GTX", "VW ID4 Pure Performance"),  # Modeller
  Detaljer = c("2023, 10,000 km, Automatisk", "2022, 15,000 km, Automatisk"),  # Bilens detaljer
  Beskrivelse = c("Nyeste model, høj ydelse.", "Perfekt stand, lavt forbrug."),  # Beskrivelse
  Lokation = c("København", "Aarhus"),          # Lokation for salget
  ForhandlerID = c("ID3000", "ID3001"),         # Forhandlerens ID
  Forhandler_navn = c("Forhandler A", "Forhandler B"),  # Forhandlerens navn
  Forhandler_adresse = c("Adresse A", "Adresse B"),     # Forhandlerens adresse
  Forhandler_CVR = c("CVR3000", "CVR3001"),     # CVR-nummer
  Link_til_bil = c("https://www.bilbasen.dk/bil1", "https://www.bilbasen.dk/bil2"),  # Links
  scrapedate = ny_scrapedate                    # Scrapedate for de nye biler
)

# Tilføj de nye biler til datasættet
simuleret_vwdf <- bind_rows(simuleret_vwdf, nye_biler)

# Udskriv de tilføjede biler
print("De to nye biler blev tilføjet:")
print(nye_biler)

### Trin 4: Ændre priser for 3 tilfældige biler ###
# Sæt seed for at sikre, at de samme biler vælges ved hver kørsel
set.seed(456)

# Vælg tilfældigt 3 biler, hvis priser skal ændres
prisændringer <- sample(nrow(simuleret_vwdf), 3)

# Gem de oprindelige priser for de valgte biler
opdaterede_biler <- simuleret_vwdf %>%
  filter(row_number() %in% prisændringer) %>%
  mutate(Oprindelig_pris = Pris)

# Ændr priserne tilfældigt med ±10%
simuleret_vwdf <- simuleret_vwdf %>%
  mutate(
    Pris = ifelse(
      row_number() %in% prisændringer,               # Hvis bilens række er blandt de valgte
      round(as.numeric(str_remove_all(Pris, "\\D")) * runif(1, 0.9, 1.1)),  # Ændr pris med ±10%
      Pris                                           # Ellers behold den oprindelige pris
    )
  )

# Opdater de nye priser i opdaterede_biler
opdaterede_biler <- opdaterede_biler %>%
  mutate(
    Ny_pris = simuleret_vwdf %>%
      filter(row_number() %in% prisændringer) %>%
      pull(Pris)
  ) %>%
  select(Model, Oprindelig_pris, Ny_pris)  # Behold kun relevante kolonner

# Udskriv de biler med opdaterede priser
print("Biler med opdaterede priser:")
print(opdaterede_biler)

### Trin 5: Gem det simulerede datasæt ###
# Gem det simulerede datasæt som en CSV-fil
write_csv(simuleret_vwdf, paste0("vw_id4_DATA_SIMULERET_", Sys.Date(), ".csv"))
message("Det simulerede datasæt er gemt som CSV.")

# ---------------------------------------------------------------------------------------------------------------------------
#### OPGAVE 1.4 - HENTE OG RENSE TYSK DATA ####
### Trin 1: Definer URL og HTTP-headers ###
# URL til tyske VW ID4 biler
vwlink_de <- "https://www.12gebrauchtwagen.de/auto/vw/id-4"

# Opsæt HTTP-headers for at undgå blokering
headers_de <- add_headers(
  `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0 Safari/537.36",
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
  `referer` = "https://www.12gebrauchtwagen.de",
  `accept-language` = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7"
)


### Trin 2: Hent antal sider ###
# Send en HTTP GET-anmodning for at hente første side
response_de <- GET(vwlink_de, headers_de)
if (status_code(response_de) != 200) {
  stop("Fejl ved hentning af data. Statuskode:", status_code(response_de))
}

# Find antal sider baseret på pagination
rawcontent_de <- content(response_de, as = "text", encoding = "UTF-8")
page_de <- read_html(rawcontent_de)
last_page_de <- page_de %>%
  html_elements("a.pagination-link") %>%  # Find pagination links
  html_text(trim = TRUE) %>%
  as.numeric() %>%
  max(na.rm = TRUE)

cat("Antal sider fundet:", last_page_de, "\n")


### Trin 3: Initialiser datastruktur ###
# Opret en tom dataframe til at gemme data
vwdf_de <- tibble(
  Model = character(),       # Bilens model
  Pris = character(),        # Bilens pris (i EUR som tekst)
  Kørte_kilometer = character(), # kørte kilometer 
  Link_til_bil = character() # Link til bilens side
)

### Trin 4: Loop gennem sider og scrap data ###
for (i in 1:last_page_de) {
  cat("Henter data fra side:", i, "\n")
  
  # Generer URL til den aktuelle side
  loopurl_de <- paste0(vwlink_de, "?page=", i)
  Sys.sleep(runif(1, min = 5, max = 10))  # Undgå blokering
  
  # Hent HTML-indhold fra siden
  response_de <- GET(loopurl_de, headers_de)
  rawcontent_de <- content(response_de, as = "text", encoding = "UTF-8")
  
  # Tjek om siden er tom
  if (nchar(rawcontent_de) == 0) {
    message("Side ", i, " er tom. Springes over.")
    next
  }
  
  # Læs HTML-strukturen fra siden
  page_de <- read_html(rawcontent_de)
  carlist_de <- page_de %>% html_elements(".columns.pt-4")  # Find bil-elementer
  
  # Loop gennem hver bil
  for (car in carlist_de) {
    tryCatch({
      # Hent data for model
      model <- car %>% 
        html_element(".h4.truncate .font-bold") %>% 
        html_text(trim = TRUE)
      
      # Hent pris
      pris <- car %>% 
        html_element(".column.small-6.medium-8") %>% 
        html_text(trim = TRUE)
      
      # Hent kørte kilometer
      km_css_selector <- ".column.medium-4.text-md.mt-half.mileage"  # CSS-vælger fra inspektør
      Kørte_kilometer <- car %>% 
        html_element(km_css_selector) %>% 
        html_text(trim = TRUE)
      
      # Hent link til bil
      link <- car %>% 
        html_element("a") %>% 
        html_attr("href")
      
      # Sørg for fuld URL
      if (!is.na(link) && startsWith(link, "/")) {
        link <- paste0("https://www.12gebrauchtwagen.de", link)
      }
      
      # Tilføj data til dataframe
      vwdf_de <- vwdf_de %>%
        add_row(
          Model = model,
          Pris = pris,
          Kørte_kilometer = Kørte_kilometer,
          Link_til_bil = link
        )
      
    }, error = function(e) {
      # Fejlhåndtering for individuelle elementer
      message("Fejl ved bil på side ", i, ": ", e)
    })
  }
}

write_csv(vwdf_de, paste0("vwid4_RÅFIL_12GEBRWAGEN", Sys.Date(), ".csv"))
read_csv("vwid4_RÅFIL_12GEBRWAGEN2024-11-25.csv")

### Trin 5: Rens data ###
vwdf_de <- vwdf_de %>%
  mutate(
    Pris = str_extract(Pris, "\\d+([.,]\\d+)?"),  # Uddrag kun tal fra pris
    Pris = str_replace_all(Pris, ",", ""),        # Fjern kommaer som tusind-separator
    Pris = as.numeric(Pris),                     # Konverter til numerisk
    Model = rens_tekst(Model),                   # Brug rens_tekst-funktionen på modellen
    Link_til_bil = rens_tekst(Link_til_bil),     # Brug rens_tekst på links, hvis nødvendigt
    Kørte_kilometer = str_extract(Kørte_kilometer, "\\d+(?:\\.\\d+)?") %>% # Uddrag kun tal fra Kørte_kilometer
      str_replace_all("\\.", "") %>%                      # Fjern eventuelle punktummer
      as.numeric()                                       # Konverter til numerisk
  ) %>%
  drop_na()  # Fjern rækker med manglende værdier

### Trin 1: Tilføj scrapedate til det oprindelige datasæt ###
# Definer den oprindelige scrapedate
oprindelig_scrapedate <- as.Date("2024-11-22")

# Tilføj scrapedate som en ny kolonne i datasættet
vwdf_de <- vwdf_de %>%
  mutate(scrapedate = oprindelig_scrapedate)

# Kopiér data og opdater scrapedate for simulering
ny_scrapedate <- oprindelig_scrapedate + 1  # Simuler en ny scrapedate (én dag senere)
simuleret_vwdf_de <- vwdf_de %>%
  mutate(scrapedate = ny_scrapedate)

### Trin 6: Gem renset data ###
# Gem datasættet som CSV-fil
write_csv(vwdf_de, paste0("vwid4_RÅFIL_12GEBRWAGEN_DONE", Sys.Date(), ".csv"))


### TRIN 7 - HVOR SKAL DER KØBES BIL 
# omregn EUR til dkk inklussiv registreringsafgift 
valutakurs <- 7.45  # 1 EUR = 7.45 DKK.    
registreringsafgift <- 0.25  # 25% afgift

# Tilføj kolonner for DKK-priser og priser med registreringsafgift
vwdf_de <- vwdf_de %>%
  mutate(
    Pris = as.numeric(Pris) * 1000,  # Hvis priser er angivet i tusinde
    Pris_DKK = Pris * valutakurs,  # Omregn pris til DKK
    Pris_med_afgift = Pris_DKK * (1 + registreringsafgift)  # Beregn pris inkl. moms/afgift
  )


# opret en kategori for kilometerstand (slid)
# Ekstraher kilometerstand fra 'Detaljer' og konverter til numerisk
extract_kilometers <- function(details) {
  str_extract(details, "\\d{1,3}(?:\\.\\d{3})*") %>%  # Find mønsteret for tusinder
    str_remove_all("\\.") %>%  # Fjern punktummer fra tusind-separator
    as.numeric() %>%           # Konverter til numerisk
    `*`(1000)                  # Skaler til tusinde (fx 6 bliver til 6000)
}

# Anvend funktionen og tilføj kilometerkategorier
vwdf <- vwdf %>%
  mutate(
    Kilometer = extract_kilometers(Detaljer),  # Ekstraher og skaler kilometerstand
    Kilometer_kategori = cut(
      Kilometer,
      breaks = c(0, 10000, 50000, 100000, Inf),  # Definer kategorier: 0-10k, 10-50k, 50-100k, >100k
      labels = c("0-10k", "10-50k", "50-100k", ">100k"),
      include.lowest = TRUE  # Inkluder laveste grænse
    )
  )

# fjerner først "kr" og "." fra pris i vwdf
vwdf <- vwdf %>%
  mutate(
    Pris = str_remove_all(Pris, "kr"),         # Fjern 'kr'
    Pris = str_remove_all(Pris, "\\.")       # Fjern alle punktummer
  )
vwdf <- vwdf %>%
  mutate(Pris = as.numeric(Pris))  # Konverter tekst til numerisk
str(vwdf$Pris)  # Skal nu vise `num`
head(vwdf$Pris)  # Viser de første rækker

# Kombiner det danske og tyske datasæt
kombineret_vw <- bind_rows(
  vwdf %>%
    select(Model, Pris_DKK = Pris, Kilometer_kategori) %>%
    mutate(
      Land = "Danmark",
      Kilometer_kategori = as.character(Kilometer_kategori)  # Konverter til karakter
    ),
  
  vwdf_de %>%
    select(Model, Pris_DKK = Pris_med_afgift, Kilometer_kategori = Kørte_kilometer) %>%
    mutate(
      Land = "Tyskland",
      Kilometer_kategori = cut(  # Konverter Kørte_kilometer til kategorier som i vwdf
        Kilometer_kategori,
        breaks = c(0, 10000, 50000, 100000, Inf),
        labels = c("0-10k", "10-50k", "50-100k", ">100k"),
        include.lowest = TRUE
      ) %>% as.character()  # Konverter til karakter
    )
)

write_csv(kombineret_vw, paste0("vwid4_KOMBINERET", Sys.Date(), ".csv"))

# Visualisere priser baseret på land og kilometerkategori
library(ggplot2)

# Aggregér data for gennemsnitlig pris pr. kilometerkategori og land
aggregated_data <- kombineret_vw %>%
  group_by(Kilometer_kategori, Land) %>%
  summarise(
    Gennemsnit_Pris = mean(Pris_DKK, na.rm = TRUE)
  ) %>%
  ungroup()

# Omform 'Kilometer_kategori' til en ordnet faktor
aggregated_data <- aggregated_data %>%
  mutate(
    Kilometer_kategori = factor(
      Kilometer_kategori,
      levels = c("0-10k", "10-50k", "50-100k", ">100k"),  # Definer rækkefølgen
      ordered = TRUE
    )
  )

# Kontrollér, at kategorien '>100k' er til stede og ordnet korrekt
print(aggregated_data)

# Opdateret søjlediagram for gennemsnitlige priser
ggplot(aggregated_data, aes(x = Kilometer_kategori, y = Gennemsnit_Pris, fill = Land)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +  # Søjler forskudt
  scale_y_continuous(labels = scales::comma) +  # Y-akse med tusind-separator
  scale_fill_manual(values = c("Danmark" = "blue", "Tyskland" = "#ff7f0e")) +  # Farver
  labs(
    title = "Sammenligning af bilpriser på VW ID4 i Danmark og Tyskland",
    subtitle = "Priserne for Tyskland inkluderer importafgifter",
    x = "Kilometerkategori",
    y = "Gennemsnitlig priser i DKK",
    fill = "Land"
  ) +
  theme_minimal(base_size = 14) +  # Tilpas tema
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size = 12),  # Forstør tekst på x-aksen
    axis.text.y = element_text(size = 12),  # Forstør tekst på y-aksen
    legend.text = element_text(size = 12)
  )


# -------------------------------------------------------------------------------------------------------------
## VIGTIGE CSV FILER
#vwdf få fil, VILBASEN 
vwdf <- read_csv("VWid4_DATA_RÅFIL__BILBASEN2024-11-25.csv")
#VWDF - DEN DANSKE WEBSCRAPPING, RENSET, 
vwdf <- read_csv("VWid4_RENSET_DATA_BILBASEN2024-11-25.csv")
# DANSK INKLUSSIV SCRAPEDATE 
vwdf<- read_csv("VWid4_RENSET_DATA_BILBASEN_DONE2024-11-25.csv")

#vwdf_de den rå fil 
vwdf_de<- read_csv("vwid4_RÅFIL_12GEBRWAGEN2024-11-25.csv")
#VWDF - DEN TYSKE WEBSCRAPPING, RENSET, INKLUSSIV SCRAPEDATE
vwdf_de <- read_csv("vwid4_RÅFIL_12GEBRWAGEN_DONE2024-11-25.csv")
# KOMBINERET TYSK OG DANSK - TIL PLOT
kombineret_vw <- read_csv("vwid4_KOMBINERET2024-11-25.csv")