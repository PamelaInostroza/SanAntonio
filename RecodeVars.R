library(stringr)
library(stringi)
library(rlang)
library(SnowballC)
library(tidytext)
library(sjlabelled)
library(tm)
library(tidyr)

for_items <- items %>% filter(encuesta %in% c(0,1) & type != " ") %>% select(origvar) 
inf_items <- items %>% filter(encuesta %in% c(0,2) & type != " ") %>% select(origvar) 

estudio$pmiss <- ifelse(estudio$q0002 == 1,rowSums(is.na(estudio[,for_items$origvar]))/length(for_items$origvar),
         ifelse(estudio$q0002 == 2,rowSums(is.na(estudio[,inf_items$origvar]))/length(inf_items$origvar),0))

estudio <- estudio[estudio$pmiss < 0.66 & !is.na(estudio$q0002), ]

estudio <- estudio %>% mutate(q0078 = factor(q0078, levels = 1:5,
                                             labels = c("Ninguno",
                                                        "Entre 1 y 5 personas",
                                                        "Entre 6 y 10 personas",
                                                        "Entre 11 y 20 personas",
                                                        "Más de 20 personas")))

estudio <- estudio %>% mutate(q0079 = factor(q0079, levels = 1:5,
                                             labels = c("Ninguno",
                                                        "Entre 1 y 5 personas",
                                                        "Entre 6 y 10 personas",
                                                        "Entre 11 y 20 personas",
                                                        "Más de 20 personas")))

ropa <- c("pantalon","polera","jeans","calcetin","calceta","ropa","poleron","traje","blazer","blusa","short",
          "boxer","calza","body","vestido","buzo","calzon$","calzone$","media","camisa","uniforme","chombas",         
          "chaqueta","chaleco","lenceria","falda","abrigo","poncho","tejido","delantal","vestuario","jean","jena",
          "corta viento","piletas","camiseta","panti","pants","faja")

platos_preparados <- c("humita","empanada","ensalada","cafe","anticucho","desayuno","comida","postre",
                       "almuerzo","^pie$","brocheta","sopa","hamburguesas","churrasco","comple","conpletos",
                       "ass","hass","italiano","comida","cubo","colacion","chaparrita","completo","jugos", 
                       "papas","handroll","ceviche","jugo","sandwich","hand roll","lomo","sanduich","desalluno",
                       "sandiuch", "guiso","fajita","churrasco","chapsui","chop","mote","huesillo","sushi",
                       "pizza","alfajor", "mermelada","ensalada","arepa","cazuela","mongoliana","^te$",
                       "arroz con leche","^pasteles de jaiba$","wafle","salchipapa","hans roll","jalea",
                       "empanda","naturale","arrollados primavera")

origen_vegetal <- c("palta","verdura","fruta","frutilla","uva","lechuga","apio","moras","choclo","limon", 
                    "repollo","sandia","arandano","durazno","zapallo","^papa$","pimenton","naranja",
                    "tomate","mora","melone","berrys","cebolla","lena")

origen_animal <- c("pescado","huevo","quezo","queso","miel","^mar$"," mar$","marisco","merluza","robado", "pescada",
                   "reineta","macha","jaiba","costillar","cecina","pollo","carne","camaron","carniceria","albacora",
                   "montino","lisa","corbina","gallina","pollito","lacteo","marino")

abarrotes <- c("comestible","bebida","agua","cereal","tallarin","harina","jugo","^leche", "aceite","yogurt","semola",
               "mantequilla","alimento","no perecible","abarrote")

panaderia <- c("pan$","pan ","sopapilla","sopai","pastele","queque","torta","churro","^masa$","^masas ",
               "tortilla","^calzones roto$",
               "berline","chilen","doblad","kutchen","galleta","cupcakes","tortas","reposteria")

confites <- c("helado","cabrita","csbrita","dulce","chocolate","algodon","azucar","confiteria",
              "turron", "barquillo","crunchi","chicle","heladi","menta$","super 8","merengue",
              "heladi","mentas","caramelo","bombon","palmera","cuchufl","csbritas","confite",
              "frutas bañada","sustancia","caluga")

bisuteria <- c("^aro","pulsera","reloj","bisuteria","jolla","joya","gorr","yoqueis","joquey","joquei","sombrero", 
               "lente","bandana", "collar","cadena","bufanda","cinturon","pulcera","cintillo", 
               "anillo","orfebreria","^accesorio$","plantilla","^coles ","^cole$","colet","turbante","cuello",
               "tobilleras","accesorios de ninaz","guante","mangas","panuelo","fantasia aro")

juguet <- c("juguete","jugueteria","jugete","peluche","muneca","ropa de muñeca","pistola",
            "regalos sorpresa","lego","disfrace","juquete","juegos de mesa","infantil","slime",
            "gatero")

malet <- c("bolso","banano","mochila","cartera","billetera","estuche","chauchero","monedero",
           "lonchera")

especias <- c("condimento","alino","aceituna","poroto","oregano","cochayuyo","cochallullo", "cilandro",
              "cilantro","hierba","yerba","canela","comino","condimenros","especia","legumbre",             
              "ahi","^aj","clavo","olor","infusiones","caldo","medicinale","condimenro")

plantas <- c("suculenta","planta","flore","arboles","cactu","frutales","tierra","macetero",
             "lilio$","rosa")

botilleria <- c("vino","cerveza","cervesa","licores","vaina","pisco","tabaco","tabaqueria","cigarr")

cosmeticos <- c("perfume","perfune","evon","avon","natura$","natura ","cosmetica","maquillaje","belleza",
                "cosmetico","labio","locione","base",
                "damas","colonia","crema","labial","jabon","masaje capilar","grema",
                "liquido de masaje","esika$","pestana","catalogo","calalogo","para el pelo",
                "cepillo","nuskin","mujere")

aseo <- c("confort","aseo","detergente","bolsa","basura","shampo","balsamo","cloro", "lavalosa",
          "servilleta", "nova","escob","higienico","pasta de diente","protectore","alusa",
          "pasta dental","pala","asea$")

celular <- c("celular","telefonico")

esteticos <- c("barberia","corte", "cabello","manicure","estilista","peluqueria","cosmetolo", "una",
               "tintura","peinado","alisado","depilacion","maquillaje","asesoramiento de imagen",
               "trenza","esmaltado","maniqiur","estetica")

plasticos <- c("fuentes","plastico","taper")
papeleria <- c("revista","libreta","libreria","libro","revista","papel", "pepeleria","diario","poster")
bazar <- c("bazar","parche","escolare","basar","lapi","cuadern","cordon","pinche","corrector","globo",
           "pinza","cierre","pegamento","magnetico","naipe","pañuelo","aroma","sale",
           "encendedor","lana","llavero","stiker","palillo","exoterico",
           "esoterico","inciencio","incienso","chapita","juegos de azar","accesorios","regalo",
           "portallave","vario","burbuja","cachureo","temporada","imane","linterna",
           "pila","tatuaje","selfie","botone","candado","laser","llaves","porta cd","binoculare",
           "pulpo","suvenir","lamina","piramide","importado","articulo","botes","cresencio","moledore","pipa")
deportivo <- c("balone","beisbol","casco","deport","guantes bicicleta","bicicleta")
hogar <- c("panos","cocina","hogar","ropa de casa","loza","cosas de casa","decorativo","casa","hogar",
           "sabana","menaje","taza","tazon","^olla","cristal","cojin","ensaladera","cortina","mueble",
           "cobertor","cuchara","cuchillo","lampara","adorno","espejo","ropa de casa nueva","guateros",
           "bares rustico","cuadro","silla","infusore","mantele","porta llave","domestico","genero",
           "pizarra","tostadore","trituradora","vicigio","linea blanca","peleadores","ventilador",
           "maquina de picar","colgador","piezera","piezaera","mortero","sababa","mordedores",
           "chispero","mordedor de cochalloyo","toalla")

artista <- c("musica","pendrive musicales","musica digital","circense","dibujo","caricatura","grabacion",
             "malabar","instrumental","show","audiovisual","videoclip","mastering","entretenimiento",
             "video","^cd$","entretencion")

artesania <- c("arte","cuero","artesania","manualidades","madera","albunes","artesania decorativa",
               "artesania en flores","artesania en genero","atrapa sueno","macrame","croche","barquito","vela",
               "aromatico","^tabl","chal$","mimbre","^platos pintado$"," pita$","pintado","tallado","bronce",
               "piedra","reciclado","antiguedade","peltre","yeso","manualid")

calzado <- c("zapatillas","zapatos","zapa","chala","calcado")

ferrete <- c("ferreteria","reciclaje de fierros","herramienta","ceramico","ceramica","aluminio","birrete",
             "maquina","manguera","pintura","soldadura","mezcla","puneta",
             "retablo","carro","carrito","cilindro","droca","sierra","repuestos","vidrio","construccion")

mascota <- c("mascota","arena","magica","perro", "gato","champion","canina","correa","pecera")

electronica <- c("electronic","tecnologia","ampolleta","cable","audifono","iluminacion","luces","enchufe",
                 "cargador","pendrive","antena","electrica","computacion","cargadoder","control","luce")

frutoseco <- c("^mani$","^mani ","almendra","^pasa$","nuez","nuece","semilla","cemilla","frutos seco")

servicios <- c("cortinaje","negocios","asesorias","clases","cocktail","cokteleria","coctele","confeccion","coffe break","copero",
               "costura","personalizado","encintado","taller","digital","aplicaciones","foto","gasfiteria","gafiteria",
               "instalaciones","limpia","subdivisione","informatico","reparacion","reiki","reiky"," masaje","relajacion",
               "rehabilitacion","actividad fisica","banquete","paginas web","descontracturante","diseno","hechuras",
               "masoterapia","arreglo","fangoterapia","jardines","marketing","planos","topografico","servicio",
               "arriendo","reciclaje","jardineria","lava autos","pasapelota","limpieza","pintar cara",
               "mantencion","plano","coctel","aberturas chapas y auto")
medicina <- c("remedio","medicin")
variedades <- list(servicios,mascota,hogar,ferrete,ropa,platos_preparados,deportivo,origen_vegetal,
                   origen_animal, abarrotes,panaderia,confites,juguet,malet,especias,
                   plantas,botilleria,cosmeticos,aseo,celular,esteticos,plasticos,artista,artesania,
                   calzado, electronica, frutoseco, papeleria,bisuteria,bazar,medicina)

names(variedades) <- c("Servicios","Articulos y servicios para mascota","Articulos de hogar","Ferreteria","Ropa",
                       "Platos preparados","Articulos deportivos","Alimentos origen vegetal",
                       "Alimentos origen animal","Abarrotes","Panaderia","Confites","Jugueteria",
                       "Maleteria","Especias (condimentos)","Plantas y Flores","Botilleria y Tabaco",
                       "Cosmeticos","Articulos de aseo","Articulos para celular",
                       "Servicios esteticos","Plasticos","Artisticos","Artesania","Calzado",
                       "Articulos electronicos","Frutos secos","Papeleria","Bisuteria","Bazar y varios",
                       "Productos medicinales")

estudio$word <- tolower(stri_trans_general(estudio$q0080_0001,"Latin-ASCII"))
estudio$word <- removeWords(estudio$word, c(get_stopwords(language = "es")$word,"no","n","kilos","nueva"))
estudio$word <- stripWhitespace(trimws(estudio$word))
estudio$word <- removePunctuation(as.character(estudio$word))
estudio$word <- removeNumbers(estudio$word)
estudio$word <- wordStem(tolower(estudio$word), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0080_0001r = ifelse(f == 1,as.character(q0080_0001),NA)) %>%
  fill(q0080_0001r) %>% ungroup(word) %>% select(-word,-f)
estudio$q0080_0001r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0080_0001r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),"Latin-ASCII")
for (i in 1:length(variedades)) {
  estudio$q0080_0001r <- ifelse(str_detect(estudio$q0080_0001r, paste(variedades[[i]],collapse = "|")), names(variedades)[i],estudio$q0080_0001r)
}
# estudio %>% filter(q0002 == 2) %>% group_by(q0080_0001r) %>% 
#    count(q0080_0001,q0080_0001r) %>% arrange(q0080_0001r) %>% print(n=nrow(estudio))
estudio$q0080_0001 <- estudio$q0080_0001r

estudio$word <- tolower(stri_trans_general(estudio$q0080_0002,"Latin-ASCII"))
estudio$word <- removeWords(estudio$word, c(get_stopwords(language = "es")$word,"no","n","kilos","nueva"))
estudio$word <- stripWhitespace(trimws(estudio$word))
estudio$word <- removePunctuation(as.character(estudio$word))
estudio$word <- removeNumbers(estudio$word)
estudio$word <- wordStem(tolower(estudio$word), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0080_0002r = ifelse(f == 1,as.character(q0080_0002),NA)) %>%
  fill(q0080_0002r) %>% ungroup(word) %>% select(-word,-f)
estudio$q0080_0002r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0080_0002r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),"Latin-ASCII")
for (i in 1:length(variedades)) {
  estudio$q0080_0002r <- ifelse(str_detect(estudio$q0080_0002r, paste(variedades[[i]],collapse = "|")), names(variedades)[i],estudio$q0080_0002r)
}
# estudio %>% filter(q0002 == 2) %>% group_by(q0080_0002r) %>%
#   count(q0080_0002,q0080_0002r) %>% arrange(q0080_0002r) %>% print(n=458)
estudio$q0080_0002 <- estudio$q0080_0002r

estudio$word <- tolower(stri_trans_general(estudio$q0080_0003,"Latin-ASCII"))
estudio$word <- removeWords(estudio$word, c(get_stopwords(language = "es")$word,"no","n","kilos","nueva"))
estudio$word <- stripWhitespace(trimws(estudio$word))
estudio$word <- removePunctuation(as.character(estudio$word))
estudio$word <- removeNumbers(estudio$word)
estudio$word <- wordStem(tolower(estudio$word), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0080_0003r = ifelse(f == 1,as.character(q0080_0003),NA)) %>%
  fill(q0080_0003r) %>% ungroup(word) %>% select(-word,-f)
estudio$q0080_0003r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0080_0003r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),"Latin-ASCII")
for (i in 1:length(variedades)) {
  estudio$q0080_0003r <- ifelse(str_detect(estudio$q0080_0003r, paste(variedades[[i]],collapse = "|")), names(variedades)[i],estudio$q0080_0003r)
}
# estudio %>% filter(q0002 == 2) %>% group_by(q0080_0003r) %>%
#   count(q0080_0003,q0080_0003r) %>% arrange(q0080_0003r) %>% print(n=nrow(estudio))
estudio$q0080_0003 <- estudio$q0080_0003r

estudio$q0082_o <- gsub("[^0-9]", "", estudio$q0082_other)
estudio$q0082_o <- removePunctuation(estudio$q0082_o)
estudio$q0082_o <- str_trim(estudio$q0082_o)
estudio$q0082_o <- as.numeric(estudio$q0082_o)

estudio$q0082 <- ifelse(estudio$q0082 ==0 & estudio$q0082_o == 0, 1,
                          ifelse(estudio$q0082 ==0 & estudio$q0082_o <= 250000, 2,
                            ifelse(estudio$q0082 ==0 & estudio$q0082_o <= 500000 & estudio$q0082_o > 250000, 3,
                                 ifelse(estudio$q0082 ==0 & estudio$q0082_o <= 1000000 & estudio$q0082_o > 500000, 4,
                                        ifelse(estudio$q0082 ==0 & estudio$q0082_o <= 2000000 & estudio$q0082_o > 1000000, 5,
                                               ifelse(estudio$q0082 ==0 & estudio$q0082_o > 2000000  , 6 , estudio$q0082))))))
estudio$q0082 <- ifelse(is.na(estudio$q0082),7,estudio$q0082)
estudio <- estudio %>% mutate(q0082 = factor(q0082, levels = 1:7,
                                               labels = c("Sin ventas ($0)", 
                                                          "Entre $1 y $250.000", 
                                                          "Entre $250.001 y $500.000",
                                                          "Entre $500.001 y $1.000.000",
                                                          "Entre $1.000.001 y $2.000.000",
                                                          "Más de $2.000.000",
                                                          "No es posible clasificar")))
#estudio %>% filter(q0002 == 2) %>% group_by(q0082) %>% count(q0082)

estudio <- estudio %>% mutate(q0084 = factor(q0084, levels = 1:4,
                                             labels = c("Menos de un 10% del monto vendido",
                                                        "Entre 11% y 30% del monto vendido",
                                                        "Entre 31% y 50% del monto vendido",
                                                        "Más del 50% del monto vendido")))

boca <- c("clientes por recomendacion","contacto","de voz en voz","boca a boca","pedido",
          "dato", "encargo","referido","via oral")
feria <- c("paseo bellamar","afuera de la casa en la feria","caleta pacheco arta mirano",
           "de repente en feria de las pulga", "feria","mercado","plaza")
puerta <- c("a domicilio","por casa","entrega domicilio","puerta a puerta","a dejar",
            "vehiculo","santiago","visita","en persona")
amigos <- c("amistade","amiga","en el trabajo","familia","amigo","oficina","conocido")
calle <- c("calle","centro de san antonio","centro","en un tren","playa","semaforo",
           "turismo","voz","lo promociona ella","promocion verbal","solo en el lugar",
           "si promociona")
fijos <- c("colegio","comerciantes, canastero y oublico","empanadas en la casa",
           "distribucion en negocios formale","establecido","fiesta","hospital","estilista",
           "estudio","juntas de vecino","local","casa","entidades","presencial","puesto",
           "terminal", "cementerio","servicio","taller","consulta por clienta")
rrss <- c("facebook","wasahp","pagina web")

var2 <- list(boca, feria, puerta, amigos, calle, fijos, rrss)
names(var2) <- c("De boca en boca","Ferias, plazas y paseos","Venta puerta a puerta",
                 "Venta a conocidos", "Venta en la calle","Venta en lugar establecido","Venta por redes sociales")

estudio$word <- tolower(stri_trans_general(estudio$q0085_other,"Latin-ASCII"))
estudio$word <- removeWords(estudio$word, c(get_stopwords(language = "es")$word,"no","recorriendo","clientes","n","kilos","nueva"))
estudio$word <- stripWhitespace(trimws(estudio$word))
estudio$word <- removePunctuation(as.character(estudio$word))
estudio$word <- removeNumbers(estudio$word)
estudio$word <- wordStem(tolower(estudio$word), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0085_r = ifelse(f == 1,as.character(q0085_other),NA)) %>%
  fill(q0085_r) %>% ungroup(word) %>% select(-word,-f)
estudio$q0085_r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0085_r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),
  "Latin-ASCII")
for (j in 1:length(var2)) {
  estudio$q0085_r <- ifelse(str_detect(estudio$q0085_r, 
                                           paste(var2[[j]],collapse = "|")), 
                                names(var2)[j],estudio$q0085_r)
}
estudio$q0085_other <- estudio$q0085_r
#estudio %>% filter(q0002 == 2) %>% group_by(q0085_other) %>% count(q0085_other) %>% print(n=nrow(estudio))

estudio$word <- tolower(stri_trans_general(estudio$q0087_other,"Latin-ASCII"))
estudio$word <- removeWords(estudio$word, c(get_stopwords(language = "es")$word,"no","recorriendo","clientes","n","kilos","nueva"))
estudio$word <- stripWhitespace(trimws(estudio$word))
estudio$word <- removePunctuation(as.character(estudio$word))
estudio$word <- removeNumbers(estudio$word)
estudio$word <- wordStem(tolower(estudio$word), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0087_r = ifelse(f == 1,as.character(q0087_other),NA)) %>%
  fill(q0087_r) %>% ungroup(word) %>% select(-word,-f)

estudio$q0087_r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0087_r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),
  "Latin-ASCII")

for (j in 1:length(var2)) {
  estudio$q0087_r <- ifelse(str_detect(estudio$q0087_r, 
                                       paste(var2[[j]],collapse = "|")), 
                            names(var2)[j],estudio$q0087_r)
}
estudio$q0087_other <- estudio$q0087_r
#estudio %>% filter(q0002 == 2) %>% group_by(q0087_other) %>% count(q0087_other) %>% print(n=nrow(estudio))


lab1 <- get_labels(estudio$q0090_0001)
estudio$q0090_0001 <- ifelse(estudio$q0089 == 1 & is.na(estudio$q0090_0001), 2, estudio$q0090_0001)
estudio$q0090_0001 <- factor(estudio$q0090_0001,levels = c(1,2),labels= c(lab1,"No"))
lab2 <- get_labels(estudio$q0090_0002)
estudio$q0090_0002 <- ifelse(estudio$q0089 == 1 & is.na(estudio$q0090_0002), 2, estudio$q0090_0002)
estudio$q0090_0002 <- factor(estudio$q0090_0002,levels = c(1,2),labels= c(lab2,"No"))
lab3 <- get_labels(estudio$q0090_0003)
estudio$q0090_0003 <- ifelse(estudio$q0089 == 1 & is.na(estudio$q0090_0003), 2, estudio$q0090_0003) 
estudio$q0090_0003 <- factor(estudio$q0090_0003,levels = c(1,2),labels= c(lab3,"No"))
lab4 <- get_labels(estudio$q0090_0004)
estudio$q0090_0004 <- ifelse(estudio$q0089 == 1 & is.na(estudio$q0090_0004), 2, estudio$q0090_0004) 
estudio$q0090_0004 <- factor(estudio$q0090_0004,levels = c(1,2),labels= c(lab4,"No"))


variedad <- c("variedad","calidad","cantidad","agregar","mercaderia","producto") 
financiamiento <- c("finan","capital","recurso","inversion","monetari",
                    "dinero","fondo","economico","de acuerdo a lo posible pueda ser cancelado")
ventas <- c("gamanci","ganancia","ingresos","masificar","venta","precio","ganar mas",
            "ingreso","vender mas","crecer")
publicidad <- c("marketing","publicidad","publicitar")
infraest <-c("infraestructura","lugar","taller","local","establec","casa",
             "estructura","ampliar","espacio","puesto","fuck track")
personal <- c("persona","voz","logistica","oderden","capacita","estabilidad","trabajo","ropa",
              "tranquiladad","organizacion","curso","todo","hogar propio")
Legalidad <- c("giro","rubro","permiso","formaliza","establec","permiso","sacar a los ambulante")
equipamiento <- c("herramienta","hidrolavadora","horno","carro","toldo","maquina","transporte",
                  "movilizacion","material","implemento")
innovac <- c("innovacion","inovacion","carrito saludable","comida al paso","humitas a la parmesana",
             "paseos en bote","prepara pasteleria ()ese es su fuerte","reposteria",
             "que la apoyen por el area de pasteleria","para hacer cosas manuale",
             "aportar basurero reciclable","gastronomia")
programas <- c("postular","emprendimiento","ficha","fosi")
vars3 <- list(variedad,financiamiento,ventas,infraest,personal,Legalidad,equipamiento, innovac,
              publicidad,programas)
names(vars3) <- c("Calidad y variedad de productos","Financiamiento","Mejorar ventas",
                  "Mejorar infraestructura","Mejoras para el personal",
                  "Mejorar terminos legales","Mejorar equipamiento","Innovacion","Publicidad",
                  "Ayuda en postulacion a programas")

estudio$word <- tolower(stri_trans_general(estudio$q0090_other,"Latin-ASCII"))
estudio$word <- removeWords(estudio$word, c(get_stopwords(language = "es")$word,"ex","no","recorriendo","clientes","n","kilos","nueva"))
estudio$word <- stripWhitespace(trimws(estudio$word))
estudio$word <- removePunctuation(as.character(estudio$word))
estudio$word <- removeNumbers(estudio$word)
estudio$word <- wordStem(tolower(estudio$word), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0090_r = ifelse(f == 1,as.character(q0090_other),NA)) %>%
  fill(q0090_r) %>% ungroup(word) %>% select(-word,-f)
estudio$q0090_r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0090_r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),
  "Latin-ASCII")
for (j in 1:length(vars3)) {
  estudio$q0090_r <- ifelse(str_detect(estudio$q0090_r, 
                                       paste(vars3[[j]],collapse = "|")), 
                            names(vars3)[j],estudio$q0090_r)
}
estudio$q0090_other <- estudio$q0090_r
#estudio %>% filter(q0002 == 2) %>% group_by(q0090_other) %>% count(q0090_other) %>% print(n=110)

lab1 <- get_labels(estudio$q0093_0001)
estudio$q0093_0001 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0001), 2, estudio$q0093_0001) 
estudio$q0093_0001 <- factor(estudio$q0093_0001,levels = c(1,2),labels= c(lab1,"No"))
lab2 <- get_labels(estudio$q0093_0002)
estudio$q0093_0002 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0002), 2, estudio$q0093_0002) 
estudio$q0093_0002 <- factor(estudio$q0093_0002,levels = c(1,2),labels= c(lab2,"No"))
lab3 <- get_labels(estudio$q0093_0003)
estudio$q0093_0003 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0003), 2, estudio$q0093_0003) 
estudio$q0093_0003 <- factor(estudio$q0093_0003,levels = c(1,2),labels= c(lab3,"No"))
lab4 <- get_labels(estudio$q0093_0004)
estudio$q0093_0004 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0004), 2, estudio$q0093_0004) 
estudio$q0093_0004 <- factor(estudio$q0093_0004,levels = c(1,2),labels= c(lab4,"No"))
lab5 <- get_labels(estudio$q0093_0005)
estudio$q0093_0005 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0005), 2, estudio$q0093_0005) 
estudio$q0093_0005 <- factor(estudio$q0093_0005,levels = c(1,2),labels= c(lab5,"No"))
lab6 <- get_labels(estudio$q0093_0006)
estudio$q0093_0006 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0006), 2, estudio$q0093_0006) 
estudio$q0093_0006 <- factor(estudio$q0093_0006,levels = c(1,2),labels= c(lab6,"No"))
lab7 <- get_labels(estudio$q0093_0007)
estudio$q0093_0007 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0007), 2, estudio$q0093_0007) 
estudio$q0093_0007 <- factor(estudio$q0093_0007,levels = c(1,2),labels= c(lab7,"No"))
lab8 <- get_labels(estudio$q0093_0008)
estudio$q0093_0008 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0008), 2, estudio$q0093_0008) 
estudio$q0093_0008 <- factor(estudio$q0093_0008,levels = c(1,2),labels= c(lab8,"No"))
lab9 <- get_labels(estudio$q0093_0009)
estudio$q0093_0009 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0009), 2, estudio$q0093_0009) 
estudio$q0093_0009 <- factor(estudio$q0093_0009,levels = c(1,2),labels= c(lab9,"No"))
lab10 <- get_labels(estudio$q0093_0010)
estudio$q0093_0010 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0010), 2, estudio$q0093_0010) 
estudio$q0093_0010 <- factor(estudio$q0093_0010,levels = c(1,2),labels= c(lab10,"No"))
lab11 <- get_labels(estudio$q0093_0011)
estudio$q0093_0011 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0011), 2, estudio$q0093_0011) 
estudio$q0093_0011 <- factor(estudio$q0093_0011,levels = c(1,2),labels= c(lab11,"No"))
lab12 <- get_labels(estudio$q0093_0012)
estudio$q0093_0012 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0012), 2, estudio$q0093_0012) 
estudio$q0093_0012 <- factor(estudio$q0093_0012,levels = c(1,2),labels= c(lab12,"No"))
lab13 <- get_labels(estudio$q0093_0013)
estudio$q0093_0013 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0013), 2, estudio$q0093_0013) 
estudio$q0093_0013 <- factor(estudio$q0093_0013,levels = c(1,2),labels= c(lab13,"No"))
lab14 <- get_labels(estudio$q0093_0014)
estudio$q0093_0014 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0014), 2, estudio$q0093_0014) 
estudio$q0093_0014 <- factor(estudio$q0093_0014,levels = c(1,2),labels= c(lab14,"No"))
lab15 <- get_labels(estudio$q0093_0015)
estudio$q0093_0015 <- ifelse(estudio$q0092 == 1 & is.na(estudio$q0093_0015), 2, estudio$q0093_0015) 
estudio$q0093_0015 <- factor(estudio$q0093_0015,levels = c(1,2),labels = c(lab15,"No"))

banig <- c("baniagualdad","banigualda","baniguslda")
fesper <- c("fondo esoeranza","fondo esperanza varios igualdad","fondo esperanza")
mascap <- c("mas","mascapaz")
semilla <- c("semilla")
words1 <- c("dos","mes","mas de","ano","meses","anos","atras","termino","como","recuerda","hace","carro","inapropiado")
delete <- c("si pero no recuerdo nombre de institucion","si recuerdo nombre institucion",
            "un carro inapropiado para el espacio","no lo recuerda","maquina de coser",
            "cafe","curso metalizado pintura")
var4 <- list(banig,fesper,mascap,semilla,delete)
names(var4) <- c("Banigualdad","Fondo esperanza","Mas capaz","Capital semilla","")

estudio$word1 <- tolower(stri_trans_general(estudio$q0094_other,"Latin-ASCII"))
estudio$word1 <- removeWords(estudio$word1, c(get_stopwords(language = "es")$word,words1,"ex","no","recorriendo","clientes","n","kilos","nueva"))
estudio$word1 <- removeNumbers(estudio$word1)
estudio$word1 <- stripWhitespace(trimws(estudio$word1))
estudio$word1 <- removePunctuation(as.character(estudio$word1))
estudio$word <- wordStem(tolower(estudio$word1), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0094_r = ifelse(f == 1,as.character(word1),NA)) %>%
  fill(q0094_r) %>% ungroup(word) %>% select(-word,-f)
estudio$q0094_r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0094_r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),
  "Latin-ASCII")
estudio$q0094_r <- removeWords(estudio$q0094_r, c(get_stopwords(language = "es")$word,words1,"ex","no","recorriendo","clientes","n","kilos","nueva"))
for (j in 1:length(var4)) {
  estudio$q0094_r <- ifelse(str_detect(estudio$q0094_r, 
                                       paste(var4[[j]],collapse = "|")), 
                            names(var4)[j],estudio$q0094_r)
}
estudio$q0094_other <- estudio$q0094_r
#estudio %>% filter(q0002 == 2) %>% group_by(q0094_other) %>% count(q0094_other) %>% print(n=110)

lab1 <- get_labels(estudio$q0096_0001)
estudio$q0096_0001 <- ifelse(estudio$q0095 == 1 & is.na(estudio$q0096_0001), 2, estudio$q0096_0001) 
estudio$q0096_0001 <- factor(estudio$q0096_0001,levels = c(1,2),labels= c(lab1,"No"))
lab2 <- get_labels(estudio$q0096_0002)
estudio$q0096_0002 <- ifelse(estudio$q0095 == 1 & is.na(estudio$q0096_0002), 2, estudio$q0096_0002) 
estudio$q0096_0002 <- factor(estudio$q0096_0002,levels = c(1,2),labels= c(lab2,"No"))
lab3 <- get_labels(estudio$q0096_0003)
estudio$q0096_0003 <- ifelse(estudio$q0095 == 1 & is.na(estudio$q0096_0003), 2, estudio$q0096_0003) 
estudio$q0096_0003 <- factor(estudio$q0096_0003,levels = c(1,2),labels= c(lab3,"No"))
lab4 <- get_labels(estudio$q0096_0004)
estudio$q0096_0004 <- ifelse(estudio$q0095 == 1 & is.na(estudio$q0096_0004), 2, estudio$q0096_0004) 
estudio$q0096_0004 <- factor(estudio$q0096_0004,levels = c(1,2),labels= c(lab4,"No"))

estudio$word <- tolower(stri_trans_general(estudio$q0096_other,"Latin-ASCII"))
estudio$word <- removeWords(estudio$word, c(get_stopwords(language = "es")$word,"no","recorriendo","clientes","n","kilos","nueva"))
estudio$word <- stripWhitespace(trimws(estudio$word))
estudio$word <- removePunctuation(as.character(estudio$word))
estudio$word <- removeNumbers(estudio$word)
estudio$word <- wordStem(tolower(estudio$word), language = "es")
estudio <- estudio %>%
  arrange(word) %>%
  group_by(word) %>%
  mutate(f = 1:n(),
         q0096_r = ifelse(f == 1,as.character(q0096_other),NA)) %>%
  fill(q0096_r) %>% ungroup(word) %>% select(-word,-f)
estudio$q0096_r <- stri_trans_general(stripWhitespace(trimws(tolower(
  str_replace_all(estudio$q0096_r, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)" ))))),
  "Latin-ASCII")
for (l in 1:length(vars3)) {
  estudio$q0096_r <- ifelse(str_detect(estudio$q0096_r, 
                                       paste(vars3[[l]],collapse = "|")), 
                            names(vars3)[l],estudio$q0096_r)
}
estudio$q0096_other <-  estudio$q0096_r
#estudio %>% filter(q0002 == 2) %>% group_by(q0096_other) %>% count(q0096_other) %>% print(n=110)

# for (var in inf_items$origvar[-1]){
#   print(var)
#   print(estudio %>% filter(q0002 == 2) %>% group_by((!!sym(var))) %>% count((!!sym(var)))  )
# }                              
#                               
                              
                              
                              
                              
                              
                              
