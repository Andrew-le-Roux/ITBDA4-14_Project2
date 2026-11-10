library(rvest); library(robotstxt); library(dplyr); library(stringr)

if (rstudioapi::isAvailable()) setwd(dirname(rstudioapi::getSourceEditorContext()$path))  
options(scipen = 999)

my_agent    <- "Student ETL project (R rvest, university assignment)"
pause_sec   <- 2                                  
scrape_date <- Sys.Date()

sites <- c(woict = "https://whatsonincapetown.com",
           ctt   = "https://www.capetown.travel")

woict_categories <- c("live-events-cape-town", "performance", "markets-in-cape-town",
                      "food-and-drink-events", "kids-in-cape-town", "sports-events", "art-and-design",
                      "social-events-cape-town", "film-tech-gaming", "activities-experiences")
max_per_category <- 12                           



allowed <- paths_allowed(c(paste0(sites["woict"], "/event/"),
                           paste0(sites["ctt"], "/whats-on-in-cape-town-this-summer-2026/")))
if (!all(allowed)) stop("robots.txt does not allow one of the sites.")

get_page <- function(url) {
  Sys.sleep(pause_sec)
  tryCatch(session(url, httr::user_agent(my_agent)), error = function(e) NULL)
}
txt <- function(node) str_squish(html_text2(node))

month_pattern <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*"
price_pattern <- "(?i:from\\s)?R\\s?\\d[\\d ,]*\\d|(?i:from\\s)?R\\s?\\d|(?i:\\bfree\\b|gratis|no charge)"

find_price <- function(text) {
  sapply(text, function(t) {
    s   <- unlist(str_split(t, "(?<=[.!?])\\s+|\n"))
    s   <- s[str_detect(s, regex("ticket|entry|entrance|admission|cost|price|per person|free", ignore_case = TRUE))]
    hit <- na.omit(str_extract(s, price_pattern))
    if (length(hit) == 0) NA_character_ else str_squish(hit[1])
  }, USE.NAMES = FALSE)
}

woict_hits <- bind_rows(lapply(woict_categories, function(cat) {
  pg <- get_page(paste0(sites["woict"], "/", cat, "/"))
  if (is.null(pg)) return(NULL)
  links <- unique(html_attr(html_elements(pg, "a[href*='/event/']"), "href"))
  links <- head(str_subset(links, "/event/[^/]+/$"), max_per_category)
  data.frame(site_category = rep(cat, length(links)), url = links)
}))

scrape_woict <- function(url) {
  pg <- get_page(url)
  if (is.null(pg)) return(NULL)
  meta  <- txt(html_elements(pg, ".event-meta p"))                      
  field <- function(label) {
    v <- na.omit(str_match(meta, paste0("^", label, ":\\s*(.*)$"))[, 2])
    if (length(v) == 0) NA_character_ else v[1]
  }
  data.frame(source = "What's On in Cape Town", url = url,
             event_name  = txt(html_element(pg, "h1")),
             date_raw    = field("Date"),
             time_raw    = field("Time"),
             price_raw   = field("Price"),
             venue_raw   = field("Venue address"),
             description = paste(html_text2(html_elements(pg, ".entry-content p")), collapse = "\n"))
}
woict <- woict_hits %>%
  left_join(bind_rows(lapply(unique(woict_hits$url), scrape_woict)), by = "url")

sitemap      <- httr::content(get_page(paste0(sites["ctt"], "/post-sitemap.xml"))$response,
                              as = "text", encoding = "UTF-8")                      
ctt_articles <- str_subset(str_extract_all(sitemap, "(?<=<loc>)[^<]+")[[1]], "whats-on-in-cape-town-.*2026")

scrape_ctt <- function(url) {
  pg <- get_page(url)
  if (is.null(pg)) return(NULL)
  paras <- html_elements(pg, ".tts_content_wrapper_1 p")
  ptxt  <- html_text2(paras)
  info  <- str_detect(ptxt, "\\p{So}") & str_detect(ptxt, month_pattern) & str_length(ptxt) < 200
  if (!any(info)) return(NULL)
  parts <- str_split_fixed(str_remove(ptxt[info], "^[\\s\\p{So}\\x{FE0F}]+"), "\\s*[\\p{So}\\x{FE0F}]+\\s*", 2)
  data.frame(source = "Cape Town Tourism", url = url,
             event_name  = sapply(paras[info], function(p)
               txt(html_element(p, xpath = "preceding-sibling::*[self::h2 or self::h3][1]"))),
             date_raw    = parts[, 1],                                      
             venue_raw   = parts[, 2],                                      
             description = sapply(paras[info], function(p)
               txt(html_element(p, xpath = "following-sibling::p[string-length(normalize-space(.)) > 40][1]"))),
             published   = html_attr(html_element(pg, "meta[property='article:published_time']"), "content"))
}
ctt <- bind_rows(lapply(ctt_articles, scrape_ctt)) %>%
  mutate(price_raw = find_price(description))

raw_df <- bind_rows(woict, ctt) %>%
  mutate(record_id = row_number(), .before = 1)

str(raw_df)
print(table(raw_df$source))
print(colSums(is.na(raw_df)))                                    
write.csv(raw_df, "events_raw.csv", row.names = FALSE, fileEncoding = "UTF-8")


month_num <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6,
               jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)

parse_dates <- function(x, ref) {
  if (is.na(x)) return(c(NA, NA))
  m <- str_match_all(x, regex(paste0("(\\d{1,2})(?:st|nd|rd|th)?\\s+", month_pattern,
                                     "\\.?(?:,?\\s+(\\d{4}))?"), ignore_case = TRUE))[[1]]
  if (nrow(m) == 0) return(c(NA, NA))
  day <- as.integer(m[, 2]); mon <- month_num[str_to_lower(substr(m[, 3], 1, 3))]
  yr  <- as.integer(m[, 4]); no_year <- is.na(yr)
  for (i in rev(seq_along(yr))) if (is.na(yr[i]) && i < length(yr)) yr[i] <- yr[i + 1]  
  yr[is.na(yr)] <- as.integer(format(ref, "%Y"))                                       
  d <- as.Date(sprintf("%d-%02d-%02d", yr, mon, day))
  d[no_year & d < ref - 60] <- d[no_year & d < ref - 60] + 365                        
  lead  <- as.integer(str_match(x, "^\\D*?(\\d{1,2})\\s*(?:to|-|\\u2013|&|and|,)\\s*\\d")[, 2])
  start <- if (!is.na(lead) && lead < day[1]) d[1] - (day[1] - lead) else d[1]
  c(start, max(d))
}

parse_time <- function(x) {
  m  <- str_match(x, regex("(\\d{1,2})(?:[:.h](\\d{2}))?\\s*(am|pm)?", ignore_case = TRUE))
  h  <- as.integer(m[, 2]); mi <- coalesce(as.integer(m[, 3]), 0L); ap <- str_to_lower(m[, 4])
  h  <- ifelse(!is.na(ap) & ap == "pm" & h < 12, h + 12L, ifelse(!is.na(ap) & ap == "am" & h == 12, 0L, h))
  ifelse(is.na(h) | h > 23 | mi > 59, NA, sprintf("%02d:%02d", h, mi))
}

parse_price <- function(x) {
  sapply(str_extract_all(x, "R\\s?\\d[\\d ,]*"), function(a) {
    v <- suppressWarnings(as.numeric(str_remove_all(a, "[R\\s,]")))
    v <- v[!is.na(v)]
    if (length(v) == 0) NA else min(v)                      
  })
}

category_rules <- c(
  "Markets"           = "market|bazaar|boeremark|craft fair",
  "Kids & Family"     = "kids|children|family|toddler|holiday programme",
  "Theatre & Comedy"  = "theatre|musical|comedy|stand-up|ballet|opera|magic|illusion|cabaret|drag|play\\b",
  "Music & Nightlife" = "concert|live music|dj\\b|band|jazz|party|club|album|gig|orchestra|choir|r&b|festival",
  "Food & Drink"      = "food|wine|beer|gin\\b|tasting|brunch|restaurant|dinner|lunch|breakfast|eatery|bistro|taproom|chef|menu",
  "Art & Culture"     = "art\\b|arts\\b|exhibition|gallery|museum|heritage|history|film|cinema|book",
  "Sport & Outdoors"  = "run\\b|marathon|race|cycl|hike|trail|rugby|cricket|soccer|surf|yoga|fitness|golf|swim|stormers",
  "Expos & Talks"     = "expo|summit|conference|talk|workshop|convention|show\\b")
classify <- function(text) {
  sapply(text, function(t) {
    hit <- names(category_rules)[str_detect(coalesce(t, ""), regex(paste0("\\b(", category_rules, ")"), ignore_case = TRUE))]
    if (length(hit) == 0) NA_character_ else hit[1]
  }, USE.NAMES = FALSE)
}
site_category_map <- c("live-events-cape-town" = "Music & Nightlife", "performance" = "Theatre & Comedy",
                       "markets-in-cape-town" = "Markets", "food-and-drink-events" = "Food & Drink",
                       "kids-in-cape-town" = "Kids & Family", "sports-events" = "Sport & Outdoors",
                       "art-and-design" = "Art & Culture", "film-tech-gaming" = "Art & Culture")

places <- read.csv(text = "place,pattern,latitude,longitude
V&A Waterfront,V&A|Waterfront|Zeitz MOCAA|Silo District|Robben Island Museum,-33.9036,18.4209
Cape Town CBD,City Centre|CBD|City Hall|Bree St|Long St|Loop St|Greenmarket|Company.?s Garden|Parade St,-33.9221,18.4231
Foreshore,Foreshore|CTICC|Convention Centre|Artscape,-33.9180,18.4300
District Six,District Six|Homecoming Centre|Harrington St,-33.9310,18.4290
Bo-Kaap,Bo-Kaap|Bo Kaap,-33.9200,18.4150
Newlands,Newlands|Kirstenbosch,-33.9800,18.4500
Milnerton,Milnerton|Montague Gardens,-33.8700,18.4950
Gardens,Gardens|Tamboerskloof|Oranjezicht|Vredehoek|Kloof St,-33.9330,18.4100
Green Point,Green Point|DHL Stadium|Cape Town Stadium,-33.9070,18.4060
Sea Point,Sea Point,-33.9150,18.3870
Camps Bay,Camps Bay|Clifton|Bantry Bay,-33.9510,18.3780
Hout Bay,Hout Bay,-34.0420,18.3520
Woodstock,Woodstock,-33.9280,18.4470
Salt River,Salt River,-33.9290,18.4630
Observatory,Observatory|Obz,-33.9380,18.4720
Rondebosch,Rondebosch|Baxter|UCT,-33.9630,18.4760
Claremont,Claremont|Kenilworth,-33.9840,18.4650
Wynberg,Wynberg,-34.0040,18.4660
Constantia,Constantia|Tokai,-34.0280,18.4220
Ottery,Ottery|Youngsfield,-34.0000,18.5160
Athlone,Athlone,-33.9620,18.5050
Langa,Langa,-33.9450,18.5300
Gugulethu,Gugulethu|Guguletu,-33.9800,18.5700
Khayelitsha,Khayelitsha,-34.0400,18.6700
Mitchells Plain,Mitchells Plain,-34.0500,18.6200
Muizenberg,Muizenberg|St James,-34.1080,18.4700
Kalk Bay,Kalk Bay,-34.1280,18.4490
Fish Hoek,Fish Hoek,-34.1360,18.4320
Simon's Town,Simon.?s ?Town|Boulders,-34.1920,18.4330
Noordhoek,Noordhoek|Kommetjie,-34.1000,18.3800
Century City,Century City|Canal Walk,-33.8920,18.5110
Table View,Table View|Blouberg|Bloubergstrand,-33.8240,18.4900
Goodwood,Goodwood|GrandWest|Grand West,-33.9120,18.5500
Bellville,Bellville|Tygerberg|Velodrome,-33.9000,18.6300
Durbanville,Durbanville,-33.8320,18.6470
Kuils River,Kuils River|Brackenfell,-33.9200,18.6900
Somerset West,Somerset West|Lourensford|Helderberg,-34.0840,18.8460
Strand,Strand(?!\\s+(St|Street))|Gordon.?s Bay,-34.1140,18.8270
Stellenbosch,Stellenbosch,-33.9321,18.8602
Franschhoek,Franschhoek|Boschendal|Pniel,-33.9133,19.1169
Paarl,Paarl,-33.7342,18.9621
Wellington,Wellington,-33.6390,19.0110
Philadelphia,Philadelphia|Durbanville Hills,-33.6670,18.5800
Darling,Darling(?!\\s+(and|St|Street))|Darling Brew,-33.3800,18.3800
Langebaan,Langebaan|West Coast,-33.0920,18.0280
Elgin,Elgin|Grabouw,-34.1500,19.0150
Hermanus,Hermanus|Overberg,-34.4187,19.2345
Cape Town (general),Cape Town,-33.9249,18.4241")

match_place <- function(text) {
  sapply(text, function(t) {
    if (is.na(t)) return(NA_character_)
    hit <- places$place[str_detect(t, regex(places$pattern, ignore_case = TRUE))]
    if (length(hit) == 0) NA_character_ else hit[1]
  }, USE.NAMES = FALSE)
}

clean_description <- function(x) {
  sapply(str_split(coalesce(x, ""), "\n"), function(lines) {
    lines <- str_squish(str_remove(lines, "Image:.*$"))
    lines <- lines[!str_detect(lines, regex("^(Last updated|View this post on Instagram|Book now)", ignore_case = TRUE))]
    paste(lines[str_count(lines, "\\S+") >= 6], collapse = " ")
  })
}
ascii <- function(x) str_squish(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = ""))

dates <- lapply(seq_len(nrow(raw_df)), function(i)
  parse_dates(raw_df$date_raw[i], coalesce(as.Date(substr(raw_df$published[i], 1, 10)), scrape_date)))

clean_df <- raw_df %>%
  mutate(
    start_date  = as.Date(sapply(dates, `[`, 1), origin = "1970-01-01"),
    end_date    = as.Date(sapply(dates, `[`, 2), origin = "1970-01-01"),
    recurring   = str_detect(coalesce(date_raw, ""), regex("every|daily|weekly|weekends?\\b|monthly|ongoing", ignore_case = TRUE)),
    start_date  = if_else(is.na(start_date) & recurring, scrape_date, start_date),   
    event_type  = case_when(recurring ~ "Recurring",
                            as.numeric(end_date - start_date) > 31 ~ "Ongoing",
                            TRUE ~ "Once-off"),
    start_time  = parse_time(time_raw),
    price_raw   = coalesce(price_raw, find_price(description)),
    price_rand  = parse_price(price_raw),
    is_free     = is.na(price_rand) & str_detect(coalesce(price_raw, ""), regex("free|gratis|no charge", ignore_case = TRUE)),
    price_rand  = ifelse(is_free, 0, ifelse(between(price_rand, 10, 5000), price_rand, NA)),   # spurious prices
    category    = coalesce(unname(site_category_map[site_category]), classify(event_name), classify(description), "Other"),
    place       = coalesce(match_place(venue_raw), match_place(event_name), match_place(description)),
    event_name  = ascii(event_name),
    venue       = ascii(venue_raw),
    description = ascii(clean_description(description)),
    summary     = str_extract(description, "^.*?[.!?](?=\\s|$)")
  ) %>%
  filter(!is.na(event_name), !is.na(start_date)) %>%                               
  mutate(name_key = str_sub(str_remove_all(str_to_lower(event_name), "[^a-z0-9]"), 1, 30)) %>%
  distinct(name_key, start_date, .keep_all = TRUE)                                 

clean_df <- clean_df %>%
  mutate(price_imputed    = is.na(price_rand),
         location_imputed = is.na(place),
         place            = coalesce(place, "Cape Town (general)")) %>%            
  group_by(category) %>%
  mutate(price_rand = coalesce(price_rand, median(price_rand[price_rand > 0], na.rm = TRUE))) %>%  
  ungroup() %>%
  mutate(price_rand = round(coalesce(price_rand, median(price_rand[price_rand > 0], na.rm = TRUE)))) %>%
  left_join(select(places, place, latitude, longitude), by = "place") %>%
  arrange(start_date) %>%
  mutate(event_id = row_number()) %>%
  select(event_id, source, event_name, category, event_type, start_date, end_date, start_time,
         price_rand, is_free, price_imputed, venue, place, latitude, longitude, location_imputed,
         summary, url)

cat("Raw rows:", nrow(raw_df), "| Clean rows:", nrow(clean_df), "\n")
print(count(clean_df, source, category))
print(as.data.frame(head(select(clean_df, event_name, category, start_date, price_rand, place), 20)))

write.csv(clean_df, "events_clean.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")
write.csv(select(places, place, latitude, longitude), "places.csv", row.names = FALSE)