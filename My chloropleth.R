setwd("C:/Users/jacarter2017/Desktop/Maps module/learn-chapter-5-master")
library(ggplot2)
library(sf)
library(censusapi)
library(leaflet)
library(dplyr)
#use st_join to run what you've gotta
public_housing_location<-"Public_Housing_Buildings.shp"
public_housing<-st_read(public_housing_location)
public_housing_mn<-public_housing %>% 
  filter(STATE2KX=="27") %>% 
  rename("county"=CURCNTY)

library(tidycensus)
Sys.getenv("CENSUS_KEY")
census_api_key("b90c6af10bf4ee9b571c1693742935f92f071bbf")

mn_income<-get_acs(geography="county",year=2016,variables="B19013_001E",
                   state="MN",geometry = T)
#let's extract county number from geoid
mn_income$county<-substring(mn_income$GEOID,3)
#let's harmonize the crs
public_housing_mn<-public_housing_mn %>% 
  st_transform(crs=st_crs(mn_income))

public_housing_county_merge<-st_join(public_housing_mn,mn_income,left=T)

units<-public_housing_county_merge %>% 
  group_by(county.x) %>% 
  summarize(units=sum(ACC_UNITS))

public_housing_county_units<-st_join(mn_income,units,left=T)
#Mn population
mn_pop<-getCensus(name="acs5",
                  vintage=2015,
                  key="b90c6af10bf4ee9b571c1693742935f92f071bbf",
                  vars=c("NAME","B01003_001E"),
                  region="county:*")

mn_pop<-mn_pop[which(mn_pop$state=="27"),]
poverty_ratiovars<-c("NAME","C17002_002E","C17002_003E","C17002_004E","C17002_005E","C17002_006E",
                     "C17002_007E","C17002_008E")
mn_poor<-getCensus(name="acs5",
                  vintage=2015,
                  key="b90c6af10bf4ee9b571c1693742935f92f071bbf",
                  vars=poverty_ratiovars,
                  region="county:*")
colnames(mn_poor)<-c("state","county","NAME","Under 50%","50% to 99%","100% to 124%",
                     "125% to 149%","150% to 184%","185% to 199%","200% and above")
mn_poor<-mn_poor %>% 
  mutate(`Below poverty line`=`Under 50%`+`50% to 99%`)
mn_poor<-mn_poor %>% 
  filter(state=="27")

public_housing_county_units<-left_join(public_housing_county_units,mn_poor,by="county")

public_housing_county_units<-public_housing_county_units %>%
  select(GEOID,NAME.x,`Below poverty line`,county,units,state,geometry) %>% 
  mutate(per_unit=ifelse(!is.na(units),`Below poverty line`/units,0))

st_crs(public_housing_county_units)
print(popup_mn)
pal_mn<-colorNumeric("Reds",domain=public_housing_county_units$per_unit)
popup_mn<-paste0("<strong>",public_housing_county_units$NAME.x,
                 "</strong><br />Total number of people below the poverty line: ",
                 public_housing_county_units$`Below poverty line`,
                 "<br />Number of people below the poverty line per unit: ",
                 as.character(round(public_housing_county_units$per_unit,0)))
public_housing_county_units<-st_transform(public_housing_county_units,crs="+proj=longlat +datum=WGS84")
#minnesota latitude/longitude
#46.7296° N, 94.6859° W
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-94.6859,46.7296, zoom = 6) %>% 
  addPolygons(data=public_housing_county_units,
              fillColor = ~pal_mn(public_housing_county_units$per_unit),
              fillOpacity = .9,
              weight=.2,
              smoothFactor=.2,
              popup=~popup_mn) %>% 
  addLegend(pal=pal_mn,
            values=public_housing_county_units$per_unit,
            position="bottomright",
            title="People per public housing unit")

ggplot(public_housing_county_buildings)+
  geom_sf()+
  theme_void()+
  theme(panel.grid.major = element_line("transparent"))+
  labs("Minnesota Counties")
