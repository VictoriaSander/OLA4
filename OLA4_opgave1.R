### BILBASEN - VIDEO WULF #####
library(httr) # for at snakke med hjemmesiden 
library(rvest)
library(dplyr)
library(stringr)

# headers
carheaders <- c(firsbb)

#first page 
startlink <- "https://www.bilbasen.dk/brugt/bil/vw/id2314?fuel=3&includeengroscvr=true&includeleasing=false"
rawres <- GET(url=startlink)
rawres$status_code 
rawcontent <- httr::content(rawres,as="text")




#transform text til html-nodes
page <- read_html(rawcontent)

# exstrakere car-element from stratpage (skla komme en car-list)
carlist <- page %>% html_elements("article")


# får en bil ad gangen 
pricetag <- ".Listing_price__6B3kE"
proptag <- ".Listing_properties___ptW"
modeltag <- "[class^='Listing_makeModel']"
detailstag <- "[class^='Listing_details']" #hiver alle detaljer ud, der begynder med "listing details"
detailslistitem <- "[class^='ListingDetails_list']"
descriptiontag <- "[class^='Listing_description']"
lokationtag <- "[class^='Listing_location']"

# dataframe til opsamling 
cn=c("price", "detaljer1", "makemodel", "props", "beskrivelse", "carlink", "carid", "scrapedate")
colldf <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 9))
colnames(colldf)=cn


for (car in carlist) {
  tryCatch({
    car <- carlist[[1]]
    price <- car %>% html_element(pricetag) %>% html_text()
    #props <- car %>% html_elements(proptag) %>% html_text()
    makemodel <- car %>% html_elements(modeltag) %>% html_text()
    #detaljer1 <- car %>% html_elements(detailstag) %>% html_text()
    detaljer1 <- car %>% html_elements(detailslistitem) %>% html_text() %>% paste0(collapse = "_")
    beskrivelse <- car %>% html_elements(descriptiontag) %>% html_text()
    lokation <- car %>% html_elements(lokationtag) %>% html_text()
    carlink <- car %>% html_element("a") %>% html_attr("href") %>% str_extract("[0-9]{7}")
    carid <- carlink %>% str_extract("[0-9]{7}")
    tmdf <- data.frame(price, detaljer1, makemodel, beskrivelse, lokation, carlink, carid, Sys.time())
    colldf <- rbind(colldf,tmdf)
  },
  error = function(cond){
    print(makemodel)
  }
  )
}

## hvorfor kommer der ikke error 

##WebScraping Bilbasen##
library(httr)
library(rvest)
library(dplyr)
library(httr2)
#######################
#Vi skulle vælge et bilmærke at arbejde med, og vi har valgt Porsche til vores opgave##
#Porsche Link

Porschefirstpage<-"https://www.bilbasen.dk/brugt/bil/porsche?fuel=3&includeengroscvr=true&includeleasing=false"
rawresporsche<-GET(url=Porschefirstpage)
porschecontent<- httr::content(rawresporsche, as="text")
porschepage<-read_html(porschecontent)


#extract porsche elements from page 
porschelist <- porschepage %>% html_elements("article")
#Vis
View(porschelist)
#Sections Porsche#
#sectiontag <- #.srp_results__2UEV_"

#Tag liste for porsche - find informationer om valgt porsche 
pricetag <- ".Listing_price_6B3kE"
ptage = "[class*='Listing_price']"
mtag = "[class*='Listing_makemodel']"
propertytag = "[class*='Listing_properties_ptWv']"
detailstag <- "[class*='Listing_details_bkAK3']"  
modeltag <- "[class*='Listing_makemodel_7yqgs']"
descriptiontag <- "[class*='Listing_description_sCNNm']"    
locationtag <- "[class*='Lisitng_location_nKGQz']"

#Lav et loop med tags
for (car in carlist) {
  price <- car %>% html_element(pricetag) %>% html_text()
  makemodel <- car %>% html_element(mtag) %>% html_text()
  properties <- car %>% html_element(propertytag) %>% html_text()
  details <- car %>% html_element(detailstag) %>% html_text()
  model <- car %>% html_element(modeltag) %>% html_text()
  description <- car %>% html_element(descriptiontag) %>% html_text()
  location <- car %>% html_element(locationtag) %>% html_text()
}

#Gem tags som en liste
porsche_info <- list(
  price = price,
  makemodel = makemodel,
  properties = properties,
  details = details,
  model = model,
  description = description,
  location = location
)


# Tilføj data som en ny række i data framen
porsche_data <- rbind(porsche_data, data.frame(
  price = price,
  makemodel = makemodel,
  properties = properties,
  details = details,
  model = model,
  description = description,
  location = location,
  stringsAsFactors = FALSE
))
}

# Vis data framen med alle Porsche-bilerne
print(porsche_data)




## hvis man er banned fra hjemmesiden, skal der oprettes en header 
# man kan sende: Konsturere egen request med httr 
# For at lave en request header til R i httr i - spørg chat 
library(rvest)
library(httr)
url="https://www.bilbasen.dk/brugt/bil/porsche?fuel=3&includeengroscvr=true&includeleasing=false"
response <- GET(
  url = "https://www.bilbasen.dk/brugt/bil/porsche?includeengroscvr=true&includeleasing=false&pagesize=30",
  add_headers(
    `alt-svc` = 'h3=":443"; ma=86400',
    `cache-control` = "no-store, must-revalidate, no-cache, max-age=0, private",
    `content-encoding` = "gzip",
    `content-type` = "text/html; charset=utf-8",
    date = "Wed, 13 Nov 2024 09:02:59 GMT",
    server = "Microsoft-IIS/10.0",
    vary = "Accept-Encoding",
    via = "1.1 0e07d676d3fd3e76057c8adaa3291b8a.cloudfront.net (CloudFront)",
    `x-amz-cf-id` = "Kz3tfs4nTQzPsucKqUtWrMapUPmOwcG4-Nj_002Y3fOEV8mC2mKgGQ==",
    `x-amz-cf-pop` = "CPH50-P1",
    `x-cache` = "Miss from cloudfront",
    `x-frame-options` = "SAMEORIGIN",
    `x-powered-by` = "ASP.NET",
    `:authority` = "www.bilbasen.dk",
    `:method` = "GET",
    `:path` = "/brugt/bil/porsche?includeengroscvr=true&includeleasing=false&pagesize=30",
    `:scheme` = "https",
    accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    `accept-encoding` = "gzip, deflate, br, zstd",
    `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
    cookie = "_sp_su=false; _cmp_analytics=1; _cmp_personalisation=1; _cmp_marketing=1; _cmp_advertising=1; bbtracker=id=604f20b3-72c3-4b40-8c65-ff9a1702db55; bbsession=id=f18b9eff-212f-4dee-8d69-0156c026267c; consentUUID=46b073a2-72bd-449a-b770-eb23a6c7c9b1_35_37; consentDate=2024-11-11T13:44:45.451Z; GdprConsent={\"version\":\"IAB TCF 2.0\",\"consentString\":\"CQH7NoAQH7NoAAGABCENBOFgAP_gAEPAAAZQJtNV7C_1LWlBoX53SZMgUYxX4fhirkQxBBZJk8QFwLOCMBQXkiEwNA3gNiICGBAAOFBBIQFkGICQBQCgKggRrjDEaECUgCJCJ4AEgAMQUwdICFJJGgFCOQAYxvg4chBQ2B6twMrsBMxwi4BGmWY5BoKwWBAVA58pDPv0bBKakxAO9bw0OgnwZF6BE0AAAAAAAAAAAAAAAAAAAAAAAQQFgGgAKABAADIAGgATAA-AERAJoArIBggDfgHLAQFATCQFwAEAALAAqABwADwALwAiAB-AEIAI4AYEAygDLAHcAP2AkQCSgFRAMUAo8BeYDVwG5gP-AhaFABAAKAgKEAGAAPACOATQAnYB1QD-gLEAW4Av8BkIDUwH7igAIB1RgAEA6o6A0AAsACoAHAAXgBEADEAH4AYAAygBogD9AItAR0BJQCogGKAPsAmQBR4C3QF5gMsAauA-4B_YD_gIWjwAQACgICjgCoACAAHgAXACOAFAAPgAjgByADuAIQATsA6oB_QFiALcAX-AyEBqYDcwHLAP3IQCQAFgBiAEcAMAAdwBUQDFAP7AhaRAAgQFIABwAEAAPAHIAdUBYgDUwHLEoBYACAAFgAcACIAGIARwAwACogGKAUeAvMmABAgKSAEgAXACOAO4A6oC3AF_gMsAcsA_cpAZAAWABUADgARAApABiAD8AMAAZQA0QB-gEWAI6ASUAqIBigD2gH2AXmAywB4oD-wH_AQtAhyVABAAKAgKUAGgAXACOAI4AcgA7gCRAF1AOqAqQBbgDUwG5gP3LQAwBgADuAPsWABgDLAamA5Y.YAAAAAAAAAAA\"};",
    priority = "u=0, i",
    referer = "https://www.bilbasen.dk/",
    `sec-ch-ua` = '"Chromium";v="130", "Google Chrome";v="130", "Not?A_Brand";v="99"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"macOS"',
    `sec-fetch-dest` = "document",
    `sec-fetch-mode` = "navigate",
    `sec-fetch-site` = "same-origin",
    `sec-fetch-user` = "?1",
    `upgrade-insecure-requests` = "1",
    `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  )
)

porschecontent <- content(response, "text")
porschepage <- read_html(porschecontent)

response$status_code



## PRIS, MAKE, MODEL, DOORS, AGE, KM, DISTANCE ekstraher fra oprindeligt datasæt 
# Load the necessary libraries

##### FÅ KODE FRA WULLF!!!! 


### HVAD ER Rwhois::whois_query("161.35.185.225)