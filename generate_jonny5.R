# generate_jonny5.R
# Patches jonny5.html with current weekly values
library(tidyverse)
library(lubridate)
library(scales)
library(fredr)
source("config.R")
fredr_set_key(FRED_API_KEY)

# Load cached data
fuel_weekly       <- read_csv("data/cache/fuel_weekly.csv",       show_col_types=FALSE) %>% mutate(date=as.Date(date))
fuel_scenarios    <- read_csv("data/cache/fuel_scenarios.csv",    show_col_types=FALSE)
gppi_scores       <- read_csv("data/cache/gppi_scores.csv",       show_col_types=FALSE) %>% mutate(date=as.Date(date))
category_outlooks <- read_csv("data/cache/category_outlooks.csv", show_col_types=FALSE)

# Key values
latest_fuel <- fuel_weekly %>% slice_max(date, n=1)
latest_gppi <- gppi_scores %>% filter(!is.na(score_transportation)) %>% slice_max(date, n=1)
ds <- fuel_scenarios %>% filter(target=="diesel_retail") %>%
  group_by(scenario) %>% summarise(avg=mean(point_forecast,na.rm=TRUE),.groups="drop")
base_d  <- ds %>% filter(scenario=="Base") %>% pull(avg)
dis_d   <- ds %>% filter(scenario=="Supply Disruption") %>% pull(avg)
cons_d  <- ds %>% filter(scenario=="Conservative") %>% pull(avg)
curr_d  <- as.numeric(latest_fuel$diesel_retail)
curr_wti <- as.numeric(latest_fuel$wti_crude)
curr_g  <- as.numeric(latest_fuel$gasoline_retail)
gppi_s  <- as.numeric(latest_gppi$gppi_composite)
gppi_pct <- min(max(gppi_s*3,-5),15)
wti_yoy <- fuel_weekly %>%
  mutate(wti_yoy=(wti_crude-lag(wti_crude,52))/lag(wti_crude,52)*100) %>%
  slice_max(date,n=1) %>% pull(wti_yoy) %>% as.numeric()

gs_ds <- fuel_scenarios %>% filter(target=="gasoline_retail") %>%
  group_by(scenario) %>% summarise(avg=mean(point_forecast,na.rm=TRUE),.groups="drop")
base_g <- gs_ds %>% filter(scenario=="Base") %>% pull(avg)
dis_g  <- gs_ds %>% filter(scenario=="Supply Disruption") %>% pull(avg)
cons_g <- gs_ds %>% filter(scenario=="Conservative") %>% pull(avg)

# PADD regional diesel
padd_series <- c(
  "East Coast"="GASDESECW","Midwest"="GASDESMWW",
  "Gulf Coast"="GASDESGCW","Rocky Mountain"="GASDESRMW","West Coast"="GASDESWCW")
padd_data <- map_dfr(names(padd_series), function(r) {
  tryCatch({
    fredr(padd_series[r], observation_start=as.Date("2025-06-01"), frequency="w") %>%
      select(date,value) %>% mutate(region=r, date=as.Date(date))
  }, error=function(e) NULL)
})
padd_summary <- padd_data %>%
  group_by(region) %>% arrange(date) %>%
  mutate(prior_week=lag(value,1)) %>% slice_max(date,n=1) %>% ungroup() %>%
  mutate(vs_national=round(value-curr_d,3),
         vs_last_week=round(value-prior_week,3),
         vs_cheapest=round(value-min(value,na.rm=TRUE),3)) %>%
  arrange(value)
padd_js <- padd_summary %>% mutate(label=region, price=round(value,3))
padd_prices_js <- paste0("{",paste(sprintf('"%s":%.3f',padd_js$label,padd_js$price),collapse=","),"}")
cheapest    <- padd_summary %>% slice_min(value,n=1)
most_exp    <- padd_summary %>% slice_max(value,n=1)
spread      <- round(most_exp$value - cheapest$value, 2)
weekly_diff <- round((10*2500/7.5)*spread, 0)

# Watch text
watch1 <- sprintf("WTI crude at $%.2f/bbl — %s year over year. %s", curr_wti,
  ifelse(!is.na(wti_yoy),sprintf("%+.0f%%",wti_yoy),"data pending"),
  ifelse(!is.na(wti_yoy)&&wti_yoy>20,"Supply disruption scenario is not theoretical at current levels.",
  ifelse(!is.na(wti_yoy)&&wti_yoy>0,"Prices elevated vs last year — monitor for further movement.",
  "Prices below last year — favorable conditions.")))
watch2 <- sprintf("GPPI at %.2f — %s pressure. %s showing highest upstream cost signal at %.1f%% YoY.",
  gppi_s, ifelse(gppi_s>1.5,"high",ifelse(gppi_s>0.5,"moderate",ifelse(gppi_s>-0.5,"neutral","easing"))),
  ifelse(nrow(category_outlooks%>%filter(!is.na(yoy_change)))>0,
    (category_outlooks%>%filter(!is.na(yoy_change))%>%slice_max(yoy_change,n=1))$category[1],"Food categories"),
  ifelse(nrow(category_outlooks%>%filter(!is.na(yoy_change)))>0,
    (category_outlooks%>%filter(!is.na(yoy_change))%>%slice_max(yoy_change,n=1))$yoy_change[1],0))
watch3 <- sprintf("Diesel forecast range: $%.3f (conservative) to $%.3f (supply disruption) over 13 weeks vs current $%.3f. %s",
  cons_d,dis_d,curr_d,
  ifelse(dis_d-curr_d>0.50,"Disruption tail risk is significant — review surcharges and bid rates.",
  ifelse(cons_d<curr_d,"Base scenario shows modest relief — conditions may improve.",
  "Prices expected to hold near current levels.")))

# Category table
cat_rows <- paste(sapply(1:nrow(category_outlooks), function(i) {
  yoy <- category_outlooks$yoy_change[i]
  signal <- ifelse(is.na(yoy),"—",ifelse(yoy>5,"High",ifelse(yoy>2,"Moderate",ifelse(yoy>0,"Low",ifelse(yoy>-2,"Stable","Easing")))))
  action <- ifelse(is.na(yoy),"No data",ifelse(yoy>5,"Lock in now",ifelse(yoy>2,"Review soon",ifelse(yoy>0,"Monitor",ifelse(yoy>-2,"Hold","Wait")))))
  yoy_d  <- ifelse(is.na(yoy),"N/A",paste0(round(yoy,1),"%"))
  sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    category_outlooks$category[i],yoy_d,signal,action)
}),collapse="
")

# PADD table
padd_rows <- paste(sapply(1:nrow(padd_js), function(i) {
  r <- padd_js[i,]
  fleet_diff <- round((10*2500/7.5)*r$vs_cheapest,0)
  wow_color <- ifelse(r$vs_last_week>0,"#F44336",ifelse(r$vs_last_week<0,"#4CAF50","#666"))
  nat_color <- ifelse(r$vs_national>0.10,"#F44336",ifelse(r$vs_national< -0.10,"#4CAF50","#666"))
  sprintf('<tr><td><strong>%s</strong></td><td>$%.3f/gal</td><td style="color:%s;">%+.3f</td><td style="color:%s;">%+.3f</td><td>%s</td></tr>',
    r$label,r$price,wow_color,r$vs_last_week,nat_color,r$vs_national,
    ifelse(r$vs_cheapest==0,"Cheapest ✅",paste0("+$",format(fleet_diff,big.mark=","),"/wk vs Gulf Coast")))
}),collapse="
")

# Chart data
fuel_history <- fuel_weekly %>% filter(date>=max(date)-weeks(52),!is.na(diesel_retail)) %>% arrange(date)
hist_labels  <- paste0('["',paste(format(fuel_history$date,"%Y-%m-%d"),collapse='","'  ),'"]')
hist_values  <- paste0("[",paste(round(fuel_history$diesel_retail,3),collapse=","),"]")
fuel_fc      <- fuel_scenarios %>% filter(target=="diesel_retail") %>% arrange(scenario,forecast_date)
fc_labels_js <- paste0('["',paste(format(as.Date(fuel_fc%>%filter(scenario=="Base")%>%pull(forecast_date)),"%Y-%m-%d"),collapse='","'  ),'"]')
fc_base <- paste0("[",paste(ifelse(is.na(fuel_fc%>%filter(scenario=="Base")%>%pull(point_forecast)),"null",round(fuel_fc%>%filter(scenario=="Base")%>%pull(point_forecast),3)),collapse=","),"]")
fc_cons <- paste0("[",paste(ifelse(is.na(fuel_fc%>%filter(scenario=="Conservative")%>%pull(point_forecast)),"null",round(fuel_fc%>%filter(scenario=="Conservative")%>%pull(point_forecast),3)),collapse=","),"]")
fc_dis  <- paste0("[",paste(ifelse(is.na(fuel_fc%>%filter(scenario=="Supply Disruption")%>%pull(point_forecast)),"null",round(fuel_fc%>%filter(scenario=="Supply Disruption")%>%pull(point_forecast),3)),collapse=","),"]")
gppi_hist   <- gppi_scores %>% filter(!is.na(gppi_composite),date>=max(date[!is.na(gppi_composite)])-months(12)) %>% arrange(date)
gppi_labels <- paste0('["',paste(format(gppi_hist$date,"%Y-%m-%d"),collapse='","'  ),'"]')
gppi_values <- paste0("[",paste(ifelse(is.na(gppi_hist$gppi_composite),"null",round(gppi_hist$gppi_composite,3)),collapse=","),"]")

# Load Chart.js
chartjs_content <- paste(readLines("chartjs.min.js"),collapse="
")

# Generate HTML
cat("  Building jonny5.html...\n")
source("generate_jonny5_html.R")
cat("✓ jonny5.html regenerated\n")
