library(haven)
library(dplyr)
library(xlsx)

dir <- "G:\\My Drive\\Proyecto desarrollo economico San Antonio\\"
est_orig <- read_sav(paste0(dir,"Datos_Todo_190731 (2)\\","OFICIAL Estudio San Antonio.sav"), encoding = "UTF-8")
est_orig$id <- rownames(est_orig)

load("G:/My Drive/Proyecto desarrollo economico San Antonio/EncuestaRecodificadas10_08.RData")
estudio$id <- rownames(estudio)

varident_f <- c("id","q0003_0001","q0003_0002","q0003_0003","q0003_0004","q0003_0005","q0003_0006",
              "q0003_0007","q0003_0008","q0003_0009","q0003_0010","q0002","q0004","q0005","q0006")

varident_i <- c("id","q0002","q0004","q0005","q0006",
              "q0105_0001", "q0105_0002", "q0105_0003", "q0105_0004", "q0105_0005", "q0105_0006", 
              "q0105_0007", "q0105_0008", "q0105_0009", "q0105_0010", "q0106_0001", "q0106_0002", 
              "q0106_0003", "q0106_0004", "q0106_0005", "q0106_0006")

orig_idf <- est_orig[varident_f]

complete_est <- right_join(orig_idf,estudio,by = c("id","q0002","q0004","q0005","q0006"))
orig_idi <- est_orig[varident_i]

complete_est <- left_join(complete_est,orig_idi,by = c("id","q0002","q0004","q0005","q0006"))

complete_est$q0106_0001 <- as.character(complete_est$q0106_0001)	
complete_est$q0106_0002 <- as.character(complete_est$q0106_0002)

file <- paste0(dir,"Datos por rubro1910.csv")
write.csv(complete_est, file, row.names = FALSE, na = "")

rub1 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 1)
write.csv(rub1, paste0(dir,"A-Agriculturaganadcazaysilv.csv"), row.names = FALSE, na = "")

rub2 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 2)
write.csv(rub2, paste0(dir,"B-Pesca.csv"), row.names = FALSE, na = "")

rub3 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 3)
write.csv(rub3, paste0(dir,"C-Explotación de minas y canteras.csv"), row.names = FALSE, na = "")

rub4 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 4)
write.csv(rub4, paste0(dir,"D-Industrias manufactureras no metálicas.csv"), row.names = FALSE, na = "")

rub5 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 5)
write.csv(rub5, paste0(dir,"E-Industrias manufactureras metálicas.csv"), row.names = FALSE, na = "")

rub6 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 6)
write.csv(rub6, paste0(dir,"F-Suministro de electricidad, gas y agua.csv"), row.names = FALSE, na = "")

rub7 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 7)
write.csv(rub7, paste0(dir,"G-Construcción.csv"), row.names = FALSE, na = "")

rub8 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 8)
write.csv(rub8, paste0(dir,"H-Comercio al por mayor y menor, rep. veh.automotoresenseres domésticos.csv"), row.names = FALSE, na = "")

rub9 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 9)
write.csv(rub9, paste0(dir,"I-Hoteles y restaurantes.csv"), row.names = FALSE, na = "")

rub10 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 10)
write.csv(rub10, paste0(dir,"J-Transporte, almacenamiento y comunicaciones.csv"), row.names = FALSE, na = "")

rub11 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 11)
write.csv(rub11, paste0(dir,"K-Intermediación financiera.csv"), row.names = FALSE, na = "")

rub12 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 12)
write.csv(rub12, paste0(dir,"L-Actividades inmobiliarias, empresariales y de alquiler.csv"), row.names = FALSE, na = "")

rub13 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 13)
write.csv(rub13, paste0(dir,"M-Adm. pública y defensa, planes de seg. social afiliación obligatoria.csv"), row.names = FALSE, na = "")

rub14 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 14)
write.csv(rub14, paste0(dir,"N-Enseñanza.csv"), row.names = FALSE, na = "")

rub15 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 15)
write.csv(rub15, paste0(dir,"O-Servicios sociales y de salud.csv"), row.names = FALSE, na = "")

rub16 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 16)
write.csv(rub16, paste0(dir,"P-Otras actividades de servicios comunitarias, sociales y personales.csv"), row.names = FALSE, na = "")

rub17 <- complete_est %>% 
  filter(as.numeric(q0006_r) == 17)
write.csv(rub17, paste0(dir,"R-Organizaciones y órganos extraterritoriales.csv"), row.names = FALSE, na = "")
