library(haven)
library(readxl)
library(dplyr)

#dir <- "C:/Users/pamel/Documents/Synthesis/"
dir <- "G:/Shared drives/Proyectos en ejecucion/2019/Proyecto desarrollo economico San Antonio/"

estudio1 <- read_sav(paste0(dir,"Datos/OFICIAL�Estudio San Antonio0911.sav"), encoding = "UTF-8")
estudio <- estudio1
items <- read_excel(paste0(dir,"Datos/rename_vars.xlsx"),sheet = "VAR")

source(paste0(dir,"Syntax/RecodeVars.R"))

source(paste0(dir,"Syntax/function_tables.R"))

#############Emprendedores###################################

vars_emprend <- items %>% 
  filter(encuesta == 0 | encuesta == 2) %>%
  dplyr::select(origvar,clase) %>% na.omit()

tablas("empren", vars_emprend, datos = estudio)

#############Formales###################################
vars_formales <- items %>% 
  filter((encuesta == 0 | encuesta == 1)) %>% 
  dplyr::select(origvar,clase) %>% na.omit()

tablas("formal", vars_formales, datos = estudio)

tablas("formal", vars_formales, rubro = "B - Pesca", datos = estudio)

tablas("formal", vars_formales, rubro = "D - Industrias manufactureras no metÃ¡licas")

tablas("formal", vars_formales, rubro = "E - Industrias manufactureras metÃ¡licas")

tablas("formal", vars_formales, rubro = "H - Comercio al por mayor y menor, rep. veh.automotores/enseres domÃ©sticos")

tablas("formal", vars_formales, rubro = "I - Hoteles y restaurantes")

tablas("formal", vars_formales, rubro = "J - Transporte, almacenamiento y comunicaciones")

tablas("formal", vars_formales, rubro = "P - Otras actividades de servicios comunitarias, sociales y personales")

###################################################
###################################################

# recodificada <- estudio %>%
#   mutate(FormalInfo = q0002,
#          RubroArea = as.numeric(q0006_r),
#          RubroSubArea = q0006,
#          TipoEmpresa = q0007,
#          ClasifTamE = as.numeric(ClasifTamE),
#          AgnosLugar = q0012,
#          CantTrabajadores = q0026,
#          Recicla = q0054,
#          AguaPotable = q0071_0001,
#          Alcantarillado = q0071_0002,
#          Electricidad = q0071_0003,
#          Internet = q0071_0004,
#          #factafectanempresa = as.numeric(factafectanempresa),
#          #principalresiduo = as.numeric(principalresiduo),
#          Empexporto = q0034,
#          ventas = q0033_rec,
#          ventadirecta = ifelse(q0002 == 1, ifelse(is.na(q0039_0001), 0, ifelse(q0039_0001 == 1, 1, 9)), NA),
#          retiroresiduos = ifelse(q0002 == 1, ifelse(is.na(q0055_0001), 0, ifelse(q0055_0001 == 1, 1, 9)), NA)) %>%
#   select(respondent_id, FormalInfo, RubroArea, RubroSubArea,TipoEmpresa, ClasifTamE,AgnosLugar, CantTrabajadores, Recicla, AguaPotable, Alcantarillado, Electricidad,
#          Internet, factafectanempresa, principalresiduo, Empexporto, ventas, ventadirecta, retiroresiduos) 
# 
# recodificada %>% summary()



###################################################
###################################################

items<- read_excel(paste0(dir,"Datos/rename_vars.xlsx"),sheet = "AGRO")
estudio <- read_sav(paste0(dir,"Ultimo envio/Agropecuario/nueva_agropecu.sav"), encoding = "UTF-8")
#############Agropecuaria#################################

vars_agropec <- items %>% 
  filter(encuesta == 3) %>%
  dplyr::select(origvar,clase) %>% na.omit()
source(paste0(dir,"Syntax/RecodeVars_agro.R"))
tablas("agro", vars_agropec)
