# installazione pacchetti
install.packages("readxl")
library("readxl")
library("tidyr")
library("ggplot2")
library("dplyr")
library("plotly")
install.packages("ggpubr")
library("ggpubr") 

# CREAZIONE DATASET: 1997-2019
#####
## Totale famiglie allacciate alla rete (migliaia):
famiglie = read_excel( "Dataset.xlsx", sheet = 1, skip = 5 ) %>%
  filter( Regioni == "Italia" ) %>%
  pivot_longer( !Regioni, names_to = "Anno", values_to = "Migliaia" ) %>%
  filter( Anno >= 1997, Anno <= 2019, !is.na(Migliaia) ) %>%
  mutate( Milioni = Migliaia/1000 ) %>%
  select( Anno, Milioni )

## Popolazione residente media annua:			
popolazione = read_excel( "Dataset.xlsx", sheet = 2, skip = 5 ) %>%
  filter( Regioni == "Italia" ) %>%
  pivot_longer( !Regioni, names_to = "Anno", values_to = "Abitanti" ) %>%
  mutate( Milioni = Abitanti/1000000 ) %>%
  filter( Anno >= 1997, Anno <= 2019 ) %>%
  select( Regioni, Anno, Milioni )

## Emissioni di anidride solforosa complessive e da processi energetici: [Mt]
anidridesolforosa = read_excel( "Dataset.xlsx", sheet = 4, skip = 5, n_max = 40 ) %>%
  filter( Anno >= 1997, Anno <= 2019 ) %>%
  pivot_longer( !Anno, names_to = "Emissioni", values_to = "Mt" )

## Emissioni di ossidi di azoto complessive e da processi energetici: [Mt]
ossidoazoto = read_excel( "Dataset.xlsx", sheet = 4, skip = 50 ) %>%
  filter( Anno >= 1997, Anno <= 2019 ) %>%
  pivot_longer( !Anno, names_to = "Emissioni", values_to = "Mt" )

## Emissioni di gas serra complessive e da processi energetici: [MtCO2 equivalente]
# CO2, CH4, NO2, HFC, PFC, SF6, NF3
gaserra = read_excel( "Dataset.xlsx", sheet = 5, skip = 4 ) %>%
  filter( Anno >= 1997, Anno <= 2019 )
colnames( gaserra )[11] = "di cui da processi energetici" 
gaserra = pivot_longer( gaserra, !Anno, names_to = "Emissioni", values_to = "MtCO2 equivalente" )

# Prezzi di acquisto dell'energia elettrica in Italia: [€/kWh]
prezzoenergia = read_excel( "Dataset.xlsx", sheet = 6, skip = 4, range = "A6:E37" )
prezzoenergia = prezzoenergia[-c(1,2,3),] %>%
  separate( col = 1, sep = "-", c("Anno", "Semestre") ) %>%
  select( !Semestre )
prezzoenergia = prezzoenergia[,-2] %>%
  separate( col = 2, sep = "-", c("utenza domestica al netto delle imposte", 
                                  "utenza domestica al lordo delle imposte") )
prezzoenergia = prezzoenergia[,-4] %>%
  separate( col = 4, sep = "-", c("utenza industriale al netto delle imposte", 
                                  "utenza industriale al lordo delle imposte") ) %>%
  pivot_longer( !Anno, names_to = "Prezzo", values_to = "Costo" ) %>%
  filter( Costo != "NA" ) %>% 
  mutate( '€/kWh' = as.numeric(Costo) ) %>%
  select( !Costo )

# Prezzi di acquisto del gas  naturale in Italia:
prezzogas = read_excel( "Dataset.xlsx", sheet = 6, skip = 4, range = "A6:I37" )
prezzogas = prezzogas[,-c(2:5)]
prezzogas = prezzogas[-c(1,2,3),] %>%
  separate( col = 1, sep = "-", c("Anno", "Semestre") ) %>%
  select( !Semestre )
prezzogas = prezzogas[,-2] %>%
  separate( col = 2, sep = "-", c("Prezzo utenza domestica al netto delle imposte", 
                                  "Prezzo utenza domestica al lordo delle imposte") )
prezzogas = prezzogas[,-4] %>%
  separate( col = 4, sep = "-", c("Prezzo utenza industriale al netto delle imposte", 
                                  "Prezzo utenza industriale al lordo delle imposte") ) %>%
  pivot_longer( !Anno, names_to = "Prezzo", values_to = "Costo" ) %>%
  filter( Costo != "NA" ) %>% 
  mutate( '€/GJ' = as.numeric(Costo) ) %>%
  select( !Costo )

# Prezzi dei prodotti energetici in Italia:
prezzoprodotto = read_excel( "Dataset.xlsx", sheet = 7, skip = 5 ) %>%
  unite( col = 'Prodotto', c('Prodotti', 'Unità di misura'), sep = " ", remove = TRUE, na.rm = TRUE ) %>%
  pivot_longer( !Prodotto, names_to = "Anno", values_to = "Costo" ) %>%
  filter( Anno >= 1995, Anno <= 2020 ) %>% 
  mutate( Prezzo = as.numeric(Costo) ) %>%
  select( !Costo )

# Dipendenza energetica italiana in %:
dipenergetica = read_excel( "Dataset.xlsx", sheet = 8, skip = 5 ) %>%
  filter( Anno >= 1997, Anno <= 2019 ) %>%
  pivot_longer( !Anno, names_to = "Fonte", values_to = "Percentuale" )  

## Produzione lorda di energia elettrica per fonti:
prodlorda = read_excel( "Dataset.xlsx", sheet = 9, skip = 14, n_max = 7 ) %>%
  pivot_longer( !Fonte, names_to = "Anno", values_to = "GWh" ) %>%
  mutate( TWh = GWh/1000 ) %>% 
  select( Fonte, Anno, TWh ) %>%
  filter( Anno >= 1997, Anno <= 2019 )

## Consumi totali di energia:
consumoenergia = read_excel( "Dataset.xlsx", sheet = 10, skip = 57 ) %>%
  mutate( 'Consumi finali [Mtep]' = `Consumi finali [ktep]` / 1000 ) %>% 
  select( !'Consumi finali [ktep]' ) %>%
  mutate( 'TWh' = `Consumi finali [Mtep]` * 13 ) %>% 
  select( !'Consumi finali [Mtep]' ) %>%
  filter( Anno >= 1997, Anno <= 2019 )

## Quota di energia da fonti rinnovabili rispetto ai consumi finali: 
quotarinnovabile = read_excel( "Dataset.xlsx", sheet = 11, skip = 4 )

## Consumi specifici medi di combustibile della produzione lorda di energia elettrica da fonti fossili:
# Per produzione lorda si intende la somma delle quantità di energia elettrica prodotte, misurate ai morsetti dei generatori elettrici.
consumofossilelordo = read_excel( "Dataset.xlsx", sheet = 12, skip = 6, n_max = 8 )
consumofossilelordo = pivot_longer( consumofossilelordo, !Combustibili, names_to = "Anno", values_to = "MJ/kWh" ) %>%
  filter( Anno >= 1997, Anno <= 2019 )

## Consumi specifici medi di combustibile della produzione netta di energia elettrica da fonti fossili:
# Per produzione netta si intende la somma delle quantità di energia elettrica prodotte, misurate in uscita dagli impianti di produzione.
consumofossilenetto = read_excel( "Dataset.xlsx", sheet = 12, skip = 21 )
consumofossilenetto = pivot_longer( consumofossilenetto, !Combustibili, names_to = "Anno", values_to = "Prezzo" ) %>%
  filter( Anno >= 1997, Anno <= 2019 ) %>%
  mutate( "MJ/kWh" = as.numeric(Prezzo) ) %>% 
  select( !Prezzo ) %>% filter( !is.na(`MJ/kWh`) )
#####

## Totale famiglie allacciate alla rete (migliaia):
# View(famiglie)

## Popolazione residente media annua:			
# View(popolazione)

## Emissioni di anidride solforosa complessive e da processi energetici: [Mt]
# View(anidridesolforosa)

## Emissioni di ossidi di azoto complessive e da processi energetici: [Mt]
# View(ossidoazoto)

## Emissioni di gas serra complessive e da processi energetici: [MtCO2 equivalente]
# View(gaserra)
 
## Prezzi di acquisto dell'energia elettrica in Italia:
# View(prezzoenergia)

## Prezzi di acquisto del gas  naturale in Italia:
# View(prezzogas)

## Prezzi dei prodotti energetici in Italia:
# View(prezzoprodotto)

## Dipendenza energetica italiana in %:
# View(dipenergetica)
 
## Produzione lorda di energia elettrica per fonti:
# View(prodlorda)
 
## Consumi totali di energia:
# View(consumoenergia)
 
## Quota di energia da fonti rinnovabili rispetto ai consumi finali: 
# View(quotarinnovabile)
 
## Consumi specifici medi di combustibile della produzione lorda di energia elettrica da fonti fossili:
# Per produzione lorda si intende la somma delle quantità di energia elettrica prodotte, misurate ai morsetti dei generatori elettrici.
# View(consumofossilelordo)
 
## Consumi specifici medi di combustibile della produzione netta di energia elettrica da fonti fossili:
# Per produzione netta si intende la somma delle quantità di energia elettrica prodotte, misurate in uscita dagli impianti di produzione.
# View(consumofossilenetto)

#####
# GRAFICI DATASET:
##### 

# grafico relativo al totale famiglie allacciate alla rete (milioni):
gfamiglie = ggplot( data = famiglie, mapping = aes( x = Anno, y = Milioni ) ) +
  geom_point( na.rm = TRUE ) + geom_line( group = 1 ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ) ) +
  labs( title = "FAMIGLIE ALLACCIATE ALLA RETE ELETTRICA" )

# grafico relativo al totale popolazione residente media annua:			
gpopolazione = ggplot( data = popolazione, mapping = aes( x = Anno, y = Milioni ) ) +
  geom_point( na.rm = TRUE ) + geom_line( group = 1 ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ) ) +
  labs( title = "POPOLAZIONE RESIDENTE MEDIA" )

# grafico relativo alle emissioni di anidride solforosa complessive: [Mt]
ganisolfo = ggplot( data = anidridesolforosa, aes( x = Anno, y = Mt, fill = Emissioni ) ) +
  geom_col( color = "black", position = position_dodge() ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill ="lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "EMISSIONI DI ANIDRIDE SOLOFOROSA" )

# grafico relativo alle emissioni di ossidi di azoto complessive: [Mt]
gossazo = ggplot( data = ossidoazoto, aes( x = Anno, y = Mt, fill = Emissioni ) ) +
  geom_col( color = "black", position = position_dodge() ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "EMISSIONI DI OSSIDO DI AZOTO" )

# grafico relativo alle emissioni di gas serra complessive: [MtCO2 equivalente]
ggasserra = ggplot( data = gaserra %>% filter( Emissioni == 'Emissioni di gas-serra' | Emissioni == 'di cui da processi energetici' ), 
                    aes( x = Anno, y = `MtCO2 equivalente`, fill = Emissioni ) ) +
  geom_col( color = "black", position = position_dodge() ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "EMISSIONI DI GAS SERRA" )

# grafico relativo al prezzo di acquisto dell'energia elettrica in Italia:
gpreele = ggplot( data = prezzoenergia, aes( x = Anno, y = `€/kWh`, group = Prezzo, colour = Prezzo ) ) + 
  scale_y_continuous( breaks = c(0,0.10,0.15,0.20,0.245) ) +
  #geom_point( na.rm = TRUE ) +
  geom_smooth( se = FALSE ) + 
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "PREZZO DI ACQUISTO DELL'ENERGIA ELETTRICA" )

# grafico relativo al prezzo di acquisto del gas  naturale in Italia:
gpregas = ggplot( data = prezzogas, aes( x = Anno, y = `€/GJ`, group = Prezzo, colour = Prezzo ) ) + 
  #geom_point( na.rm = TRUE ) +
  geom_smooth( se = FALSE ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "PREZZO DI ACQUISTO DEL GAS NATURALE" )

# grafico relativo al prezzo dei prodotti energetici in Italia:
gprepro = ggplot( data = prezzoprodotto, aes( x = Anno, y = Prezzo, group = Prodotto, colour = Prodotto ) ) + 
  #geom_point( na.rm = TRUE ) +
  geom_smooth( se = FALSE ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill="lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "PREZZO DI ACQUISTO DEI PRODOTTI ENERGETICI" )

# grafico relativo alla dipendenza energetica italiana in %:
gdipene = ggplot( data = dipenergetica, aes( x = Anno, y = Percentuale, colour = Fonte ) ) +
  geom_point() +
  geom_smooth( se = FALSE )  +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "DIPENDENZA ENERGETICA IN ITALIA" )

# grafico istogramma relativo alla produzione lorda di energia per fonte
gprofon = ggplot( data = prodlorda %>% filter( Fonte != "TOTALE" ), aes( x = Anno, y = TWh, fill = Fonte ) ) +
  geom_col( color = "black", position = position_dodge() ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "PRODUZIONE LORDA DI ENERGIA PER FONTE" )

# grafico istogramma relativo alla produzione lorda di energia totale
gprotot = ggplot( data = prodlorda %>% filter( Fonte != "TOTALE" ), aes( x = Anno, y = TWh, fill = Fonte ) ) +
  geom_col( color = "black" ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "PRODUZIONE LORDA DI ENERGIA TOTALE" )

# grafico consumi totali di energia:
gconene = ggplot( data = consumoenergia, mapping = aes( x = Anno, y = `TWh` ) ) +
  # geom_point() + 
  geom_smooth( se = FALSE, colour = "black" ) +
  labs( title = "CONSUMI TOTALI DI ENERGIA" )

# grafico istogramma relativo ai consumi specifici medi di combustibile della produzione netta 
# di energia elettrica da fonti fossili:
gconspefos = ggplot( consumofossilenetto %>% 
                       filter( Combustibili != "TOTALE" ), aes( x = Anno, y = `MJ/kWh`, group = Combustibili, colour = Combustibili ) ) +
  #geom_point( na.rm = TRUE ) +
  geom_smooth( se = FALSE ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "CONSUMI SPECIFICI DI COMBUSTIBILE PER LA PRODUZIONE DI ENERGIA ELETTRICA DA FONTI FOSSILI" )

# grafico istogramma relativo ai consumi totali medi di combustibile della produzione netta 
# di energia elettrica da fonti fossili:
gcontotfos = ggplot( consumofossilenetto %>% 
                       filter( Combustibili == "TOTALE" ), aes( x = Anno, y = `MJ/kWh`, group = Combustibili, colour = Combustibili ) ) +
  #geom_point( na.rm = TRUE ) +
  geom_smooth( se = FALSE ) +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill = "lightgrey",
                                           size = 0.5, linetype = "solid", 
                                           colour = "black") ) +
  labs( title = "CONSUMI TOTALI DI COMBUSTIBILE PER LA PRODUZIONE DI ENERGIA ELETTRICA DA FONTI FOSSILI" )

#####
# PRESENTAZIONE:
#####

# relazione famiglie, popolazione e consumo totale di energia:
ggarrange( gpopolazione, gfamiglie, gconene, ncol = 2, nrow = 2 ) 

# relazione produzione e consumo:
ggarrange( gprofon, gprotot, gconene, ncol = 2, nrow = 2 )
 
# relazione inquinamento: 
ggarrange( ganisolfo, gossazo, ggasserra, ncol = 2, nrow = 2 ) 

# relazione prezzi:
ggarrange( gpreele, gpregas, gprepro, ncol = 2, nrow = 2 ) 

# grafico relativo alla produzione lorda di energia totale
gprototline = ggplot( data = prodlorda %>% filter( Fonte == "TOTALE" ), aes( x = Anno, y = TWh, group=Fonte ) ) +
  geom_smooth( se = FALSE, colour="black" ) + 
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
        legend.background = element_rect( fill = "lightgrey", 
                                          size = 0.5, linetype = "solid", 
                                          colour = "black") ) +
  labs( title = "PRODUZIONE LORDA DI ENERGIA TOTALE" )

# relazione produzione totale, consumo e dipendenza energetica:
ggarrange( gprototline, gconene, gdipene, ncol = 2, nrow = 2 ) 

# grafico interrativo relativo alla dipendenza energetica:
ggplotly( ggplot( data = dipenergetica, aes( x = Anno, y = Percentuale, colour = Fonte ) ) +
  geom_point() +
  geom_smooth(se=FALSE)  +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill="lightgrey",
                                          size = 0.5, linetype = "solid", 
                                          colour = "black") ) +
  labs( title = "DIPENDENZA ENERGETICA IN ITALIA" ))

# relazione fra consumi combustibili, prezzo acquisto dei prodotti energetici,
# emissione di gas serra, produzione lorda di energia totale, dipendenza energetica:
ggasserraline = ggplot( data = gaserra %>% filter( Emissioni == 'Emissioni di gas-serra'), 
                        aes( x = Anno, y = `MtCO2 equivalente`, fill = Emissioni ) ) +
  #geom_point() +
  geom_smooth( se = FALSE )  +
  theme( axis.text.x = element_text( angle = 45, size = 5 ), 
         legend.background = element_rect( fill ="lightgrey",
                                          size = 0.5, linetype = "solid", 
                                          colour = "black") ) +
  labs( title = "EMISSIONI DI GAS SERRA" )

#forse 2x3
ggarrange( gconspefos, gcontotfos, gprepro, ggasserraline, gprofon, gdipene, ncol = 3, nrow = 2 )

# relazione prezzi, consumi totali di combustibile, 
# dipendenza energetica, relazione consumo-produzione :
relconpro = merge( prodlorda %>% filter( Fonte == "TOTALE" ),
                   consumoenergia, by = "Anno" ) %>%
  select( !Fonte )
colnames( relconpro ) = c("Anno", "Produzione", "Consumo")
relconpro = pivot_longer( relconpro, !Anno, names_to = "Destinazione", values_to = "TWh" )

grelconpro = ggplot( relconpro, aes( x = Anno, y = TWh, group = Destinazione, colour = Destinazione ) ) +
    #geom_point() +
    geom_smooth( se = FALSE )  +
    theme( axis.text.x = element_text( angle = 45, size = 5 ), 
           legend.background = element_rect( fill = "lightgrey",
                                            size = 0.5, linetype = "solid", 
                                            colour = "black") ) +
    labs( title = "RELAZIONE PRODUZIONE-CONSUMO DI ENERGIA" )
  
ggarrange( gpreele, gpregas, gprepro, gdipene, gcontotfos, grelconpro, ncol = 3, nrow = 2 ) 

#####