library(tidyverse) #ezekben a csomagokban lévő függvényeket használjuk
library(ggthemes)

#read:beolvassuk a Klímaadatbázist, és az etnográfiai atlaszt, innen:
#https://d-place.org/parameters
#read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/data.csv')
ea_dat=read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/data.csv')
#read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/variables.csv')
ea_var=read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/variables.csv')
#read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/codes.csv')
ea_code=read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/codes.csv')
#read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/societies.csv')
ea_soc=read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/EA/societies.csv')
read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/ecoClimate/data.csv')
climate_code=read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/ecoClimate/data.csv')
#read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/ecoClimate/variables.csv')
climate_var=read_csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/refs/heads/master/datasets/ecoClimate/variables.csv')

#Rename: az adatbázisban át kell neveznünk egyes oszlopokat
#mert az adatbázisokban más azonosítója van ugyan annak a kultúrának
ea_var_2=ea_var |>
    rename(var_id=id)

ea_soc_2=ea_soc |>
    rename(soc_id=id)

climate_var_2=climate_var |>
      rename(climate_id=id,climate_title=title)

climate_code_2=climate_code|>
     rename(climate_id=var_id, climate_code=code)

#select: kiválasztjuk a szükséges adatsorokat

ea_dat_2=ea_dat|>
   select(soc_id,var_id,code)

ea_var_3=ea_var_2|>
   select(var_id,title,category)

ea_soc_3=ea_soc_2|>
   select(soc_id,pref_name_for_society)

climate_var_3=climate_var_2|>
      select(climate_id,climate_title)

climate_code_3=climate_code_2|>
      select(soc_id,climate_id,climate_code)

#Join: összekapcsoljuk a két adatbázisból az adatokat, amiket fel szeretnénk
#használni
ea_dat_3 = ea_dat_2|>
   left_join(ea_soc_3)|>
   left_join(ea_var_3)|>
   left_join(ea_code)|>
   left_join(climate_code_3)|>
   left_join(climate_var_3)

#Filter: kiszűrjük a számunkra releváns adatokat, ez most az éves
#átlaghőmérséklet és a populáció mérete

ea_dat_4=ea_dat_3|>
   filter(
  var_id == 'EA202',
  climate_id =='AnnualMeanTemperature'
 )
#szűrjük a hőmérsékleti állandóságra is
ea_dat_5=ea_dat_3|>
   filter(
  var_id =='EA202',
  climate_id =='TemperatureConstancy'
)

#csinálunk egy filterezést a vallásosságról és a politikai komplexitásról is

ea_dat_5=ea_dat_3|>
   filter(
  var_id== 'EA113',
  climate_id=='AnnualMeanTemperature'
)
#Plots: ábrákat szerkesztünk az adatok vizualizására
#Az ábráink igazolni hivatottak, hogy a hipotézisnek megfelelően a 
#melegebb éghajlatú területeken a populáció nagyobb

my_x = ea_dat_4 |> 
  pull(title) |> 
  unique()

my_y = ea_dat_4 |> 
  pull(climate_title) |> 
  unique()

ea_dat_4 |>
  filter(!is.na(climate_id)) |> 
  mutate(log10_code = log10(code)) |> 
  ggplot(aes(y = log10_code, x = climate_id, label = pref_name_for_society)) +
  geom_label() +
  theme_few() +
  geom_smooth(alpha = .1) +
  ylab('Population size (log10)') +
  xlab(my_y)

my_x = ea_dat_4 |> 
  pull(title) |> 
  unique()

my_y = ea_dat_5 |> 
  pull(climate_title) |> 
  unique()

ea_dat_5 |>
  filter(!is.na(code)) |> 
  mutate(log10_code = log10(code)) |> 
  ggplot(aes(y = log10_code, x = climate_code, label= category)) +
  geom_label() +
  theme_few() +
  geom_smooth(alpha = .1) +
  ylab('category') +
  xlab(my_y)

boxplot.default(climate_title,var_id)

