library(flextable)
library(officer)

########################################################
tablas <- function(tipo, varstipo, rubro = ""){
  
  if (tipo == "formal") {
    if (rubro != "") {
      data <- estudio %>% dplyr::select(varstipo$origvar) %>% filter(q0002 == 1, q0006_r == rubro) 
      q <- items %>% filter(encuesta == 0 | encuesta == 1, clase != "textuse") %>%
        mutate( origvar = ifelse(clase == "multiple" & !substr(origvar,6,9) %in% c("_oth") | clase == "combinada" | clase == "ranking" | clase == "textclasification",
                                 substr(origvar,1,5), origvar)) %>%
        dplyr::select(origvar,num,filtro,titulo) %>% 
        na.omit()
      nombre <- paste0("2. Resultados encuesta Empresas Formales, rubro ", rubro)
    } else {
      data <- estudio %>% dplyr::select(varstipo$origvar) %>% filter(q0002 == 1) 
      q <- items %>% filter(encuesta == 0 | encuesta == 1, clase != "textuse") %>%
        mutate( origvar = ifelse(clase == "multiple" & !substr(origvar,6,9) %in% c("_oth") | clase == "combinada" | clase == "ranking" | clase == "textclasification",
                                 substr(origvar,1,5), origvar)) %>%
        dplyr::select(origvar,num,filtro,titulo) %>% 
        na.omit()
      nombre <- "2. Resultados encuesta Empresas Formales"  
    }
  } else if (tipo == "empren"){
    data <- estudio %>% dplyr::select(varstipo$origvar) %>% filter(q0002 == 2)
    q <- items %>% filter(encuesta == 0 | encuesta == 2, clase != "textuse") %>%
      mutate(origvar = ifelse(clase == "multiple" & !substr(origvar,6,9) %in% c("_oth") | clase == "combinada" | clase == "ranking" | clase == "textclasification",
                              substr(origvar,1,5), origvar)) %>%
      dplyr::select(origvar,num,filtro,titulo) %>%
      na.omit()
    nombre <- "1. Resultados encuesta Emprendedores"
  } else if (tipo == "agro"){
  data <- estudio %>% dplyr::select(varstipo$origvar) 
  q <- items %>% filter(encuesta == 3, clase != "textuse") %>%
    mutate(origvar = ifelse(clase == "multiple" & !substr(origvar,6,9) %in% c("_oth") | clase == "combinada" | clase == "ranking" | clase == "textclasification",
                            substr(origvar,1,5), origvar)) %>%
    dplyr::select(origvar,num,filtro,titulo) %>%
    na.omit()
  nombre <- "1. Resultados encuesta Agropecuaria"
}
  
  multiple <- varstipo %>% filter(clase == "multiple") %>% dplyr::select(origvar)

  combinado <- varstipo %>% filter(clase == "combinada") %>% dplyr::select(origvar)

  ranking <- varstipo %>% filter(clase == "ranking") %>% dplyr::select(origvar)

  textmining <- varstipo %>% filter(clase == "textclasification") %>% dplyr::select(origvar)
 
  ab <- varstipo %>% filter(clase == "clasificada") %>% dplyr::select(origvar) 
  date <- Sys.Date()
  doc <- read_docx() %>%
    body_add_par(nombre, style = "Normal") 
  print(doc, target = paste0(dir,"freq_",tipo,substr(rubro,1,1),"_",date,".docx"))
  
  for (vars in q$origvar) {
    print(vars)
    num <- q %>% filter(origvar == vars) %>% dplyr::select(num)
    titulo <- q %>% filter(origvar == vars) %>% dplyr::select(titulo)
    
    
    if (any(grepl(vars,multiple$origvar, fixed = TRUE))) { #Multiple filtrada
      filtro <- q %>% filter(origvar == vars) %>% dplyr::select(filtro)
      if (filtro$filtro  == 1) {
        
        mult <- multiple$origvar[grepl(paste0("^",vars,"_*"),multiple$origvar, fixed = FALSE)]
        al1 <- rbind(c("", "Seleccionada", "", "No seleccionada", "", "Total", ""),
                     c("", "N", "%", "N", "%", "N", "%"))
        
        for (j in 1:length(mult)) {
          print(mult[j])
          datafilt <- data %>% dplyr::select(mult[j]) 
          
          t1 <- table(data[mult[j]])
          
          #if (any(levels(data.frame(t1)[,1]) %in% "1")) {
            
            dataf <- data %>% dplyr::select(mult[j]) %>% filter(eval(parse(text = paste0(mult[j]," != 9"))))
            
            a1 <- table(as_label(dataf))
            a2 <- round(prop.table(table(as_label(data[mult[j]])))*100,2)
            t1 <- sum(a1)
            t2 <- sum(prop.table(table(as_label(data[mult[j]])))*100)
            al <- cbind(a1, a2)
            if (nrow(al) == 1) al <- rbind(al,c(0,0))
            Total <- c(t1,t2)
            al <- rbind(al,Total)
            al <- cbind(rownames(al),al)
            colnames(al) <- c("cat","b1","b2")
            al <- rbind(c("","N", "%"),al)
            
            al1 <- rbind(al1,
                         c(al[2,1],al[2,2],al[2,3],al[3,2],al[3,3],al["Total",2],al["Total",3]))
          #}
        }
        
      } else {#Multiple no filtrada
        mult <- multiple$origvar[grepl(paste0("^",vars,"_*"),multiple$origvar, fixed = FALSE)]
        al1 <- rbind(c("", "Seleccionada", "", "No seleccionada", "", "Total", ""),
                     c("", "N", "%", "N", "%", "N", "%"))
        
        for (j in 1:length(mult)) {
          print(mult[j])
          datafilt <- data %>% dplyr::select(mult[j]) 
          t1 <- table(data[mult[j]], useNA = "ifany")
          
          #if (any(levels(data.frame(t1)[,1]) %in% "1")) {
            a1 <- table(as_label(data[mult[j]]), useNA = "ifany")
            a2 <- round(prop.table(table(as_label(datafilt), useNA = "ifany"))*100,2)
            t1 <- sum(a1)
            t2 <- sum(prop.table(table(as_label(datafilt), useNA = "ifany"))*100)
            al <- cbind(a1, a2)
            if (nrow(al) == 1) al <- rbind(al,c(0,0))
            Total <- c(t1,t2)
            al <- rbind(al,Total)
            al <- cbind(rownames(al),al)
            colnames(al) <- c("cat","b1","b2")
            al <- rbind(c("","N", "%"),al)
            
            al1 <- rbind(al1,
                         c(al[2,1],al[2,2],al[2,3],al[3,2],al[3,3],al[4,2],al[4,3]))
          #}
        }
      } 
      tbla <- flextable(data.frame(al1))
      tbla <- set_header_labels(tbla, X1 = paste0(num,". ", titulo))
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = 7, part = "header")
      tbla <- merge_h_range(tbla, i = 1, j1 = 2, j2 = 3, part = "body")
      tbla <- merge_h_range(tbla, i = 1, j1 = 4, j2 = 5, part = "body")
      tbla <- merge_h_range(tbla, i = 1, j1 = 6, j2 = 7, part = "body")
      tbla <- bg(tbla, bg = "white", part = "body")
      tbla <- bg(tbla, bg = "darkslateblue", part = "header")
      tbla <- bold(tbla, i = 1, part = "header")
      tbla <- bold(tbla, i = 1, part = "body")
      tbla <- bold(tbla, j = c(6,7), part = "body")
      tbla <- color(tbla, i = 1, color = "white", part = "header")
      tbla <- align(tbla, j = 1, align = "left",part = "body")
      tbla <- align(tbla, align = "left",part = "header")
      tbla <- align(tbla, j = c(2,4,6),align = "center",part = "body")
      tbla <- width(tbla, j = 1, width = 2)
      tbla <- width(tbla, j = c(2:7), width = 0.5)
      tbla <- font(tbla,fontname = "Calibri", part = "all")
      tbla <- fontsize(tbla, size = 10, part = "header")
      tbla <- fontsize(tbla, size = 10, part = "body")
      tbla <- border_remove(tbla)
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.right = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.top = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.bottom = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, j = 7, border.right = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, i = 1, j = 6, border.right = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, i = nrow(al1), border.bottom = fp_border(color = "black", width = 2))
      tbla <- border(tbla, i = c(2, 3), border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, j = c(3, 5), border.right = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = 2:nrow(al1), j = 1, border.right = fp_border(color = "black"), part = "body")
      
    } else if (any(grepl(paste0("^",vars,"$"),ab$origvar, fixed = FALSE))) {
      
      label <- get_label(data %>% dplyr::select(vars))
      datafilt <- data %>% dplyr::select(vars) %>% filter(eval(parse(text = paste0(vars," != \".\"")))) %>% 
        filter(eval(parse(text = paste0(vars," != \"No\"")))) %>% 
        filter(eval(parse(text = paste0(vars," != \"No aplica\"")))) %>% 
        filter(eval(parse(text = paste0(vars," != \"\"")))) 
      
      if (nrow(table(datafilt)) >= 20 ) filas <- 20 else filas <- nrow(table(datafilt))
      a1 <- sort(table(as_label(datafilt), useNA = "no"), decreasing = TRUE)[1:filas]
      a2 <- round(prop.table(sort(table(as_label(datafilt), useNA = "no"), decreasing = TRUE))[1:filas]*100,2)
      t1 <- sum(a1,na.rm = T)
      t2 <- ifelse(round(sum(a2,na.rm = T)) != 100, round(sum(a2,na.rm = T),1), round(sum(a2,na.rm = T)))
      a <- cbind(a1, a2)
      Total = c(t1,t2)
      a <- rbind(a,Total)
      colnames(a) <- c("b1","b2")
      a <- rbind(c("N", "%"),a)
      if (filas > 0) numer <- c("",seq(1,filas,1),"") else numer <- rep("",nrow(a))
      a <- cbind(numer,rownames(a),a)
      
      a <- na.omit(a)
      
      
      tbla <- flextable(data.frame(a))
      tbla <- set_header_labels(tbla, numer = paste0(num,". ",titulo," (Solo se muestran las ",filas," respuestas más comunes)"))
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = 4, part = "header")
      tbla <- bg(tbla, bg = "white", part = "body")
      tbla <- bg(tbla, bg = "darkslateblue", part = "header")
      tbla <- bold(tbla, i = 1, part = "header")
      tbla <- bold(tbla, i = 1, part = "body")
      tbla <- bold(tbla, i = nrow(a), part = "body")
      tbla <- font(tbla,fontname = "Calibri", part = "all")
      tbla <- color(tbla, i = 1, color = "white", part = "header")
      tbla <- align(tbla, j = c(1,2), align = "left",part = "body")
      tbla <- align(tbla, align = "left",part = "header")
      tbla <- width(tbla, j = 1, width = 0.3)
      tbla <- width(tbla, j = 2, width = 3)
      tbla <- width(tbla, j = c(3,4), width = 1)
      tbla <- fontsize(tbla, size = 10, part = "header")
      tbla <- fontsize(tbla, size = 10, part = "body")
      tbla <- border_remove(tbla)
      tbla <- border(tbla, i = 2, border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = nrow(a), border.top = fp_border(color = "black"))
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.right = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.top = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.bottom = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, j = 4, border.right = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, i = nrow(a), border.bottom = fp_border(color = "black", width = 2), part = "body")
      
    } else if (any(grepl(vars,combinado$origvar, fixed = TRUE))) {
      
      comb <- combinado$origvar[grepl(paste0("^",vars,"_*"),combinado$origvar, fixed = FALSE)]
      label <- get_labels(data[comb[1]])
      nlable <- eval(parse(text = paste0("label$",comb[1])))
      al1 <- c("","",nlable,"Total")
      
      for (j in 1:length(comb)) {
        if (comb[j] != "q0056_0005") {
          print(comb[j])
          datafilt <- data %>% dplyr::select(comb[j]) 
          
          a1 <- table(as_label(datafilt))
          a2 <- round(prop.table(table(as_label(datafilt)))*100,2)
          t1 <- sum(a1)
          t2 <- sum(prop.table(table(as_label(datafilt)))*100)
          
          a <- c(get_label(datafilt),"N",a1, t1)
          a <- rbind(a,c("","%",a2, t2))
          
          al1 <- rbind(al1,a) 
        }
      }
      colnames(al1) <- c("num", rep("",length(nlable) + 2))
      
      tbla <- flextable(data.frame(al1))
      tbla <- set_header_labels(tbla,  num = paste0(num,". ",titulo))
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = (length(nlable) + 3), part = "header")
      for (filas in seq(2, (nrow(al1) - 1), by = 2)) {
        tbla <- merge_at(tbla,  j = 1, i = c(filas:filas + 1), part = "body")
      }
      tbla <- bg(tbla, bg = "white", part = "body")
      tbla <- bg(tbla, bg = "darkslateblue", part = "header")
      tbla <- bold(tbla, i = 1, part = "header")
      tbla <- bold(tbla, i = 1, part = "body")
      tbla <- bold(tbla, j = 2, part = "body")
      tbla <- bold(tbla, j = (length(nlable) + 3), part = "body")
      tbla <- font(tbla,fontname = "Calibri", part = "all")
      tbla <- color(tbla, i = 1, color = "white", part = "header")
      tbla <- align(tbla, j = 1, align = "left",part = "body")
      tbla <- align(tbla, align = "left",part = "header")
      tbla <- align(tbla, j = 3:(length(nlable) + 3), align = "center",part = "body")
      tbla <- width(tbla, j = 1, width = 2)
      tbla <- width(tbla, j = 2, width = 0.3)
      tbla <- width(tbla, j = c(3:(length(nlable) + 3)), width = 0.7)
      tbla <- fontsize(tbla, size = 10, part = "header")
      tbla <- fontsize(tbla, size = 10, part = "body")
      tbla <- border_remove(tbla)
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, j = 1, border.right = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.top = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.bottom = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, i =  seq(2, (nrow(al1) - 1), by = 2), border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = nrow(al1), border.bottom = fp_border(color = "black", width = 2))
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, j = (length(nlable) + 3), border.right = fp_border(color = "black", width = 2), part = "body")
      
    } else if (any(grepl(vars,ranking, fixed = TRUE))) {
      
      rank <- ranking$origvar[grepl(paste0("^",vars,"_*"),ranking$origvar, fixed = FALSE)]
      label <- get_labels(data[,rank[1]])
      nlable <- eval(parse(text = paste0("label$",rank[1])))
      al1 <- data.frame(label)
      colnames(al1) <- "Pref"
      
      for (k in 1:length(rank)) {
        if (rank[k] != "q0053_0002") {
          print(rank[k])
          datafilt <- data %>% dplyr::select(rank[k]) 
          a1 <- data.frame(table(as_label(datafilt)))
          colnames(a1) <- c("Pref",get_label(datafilt))
          al1 <- full_join(al1,a1, By = "Pref")  
        }
      } 
      
      al1 <- al1 %>% gather(Prefer, Valor,-Pref) %>% 
        group_by(Pref) %>%
        mutate(n = 1:n_distinct(Prefer)) %>%
        group_by(Prefer) %>%
        spread(Pref,Valor) %>% 
        arrange(n) %>% dplyr::select(Prefer,n,get_labels(datafilt)[[1]])
      
      if (length(colnames(al1)) == 6) colnames(al1) <- c("Prefer","n","Pref1","Pref2","Pref3","Perdido") else colnames(al1) <- c("Prefer","n","Pref1","Pref2","Pref3")
      al1 <- al1 %>%
        mutate(PerRes1 = Pref1/sum(al1$Pref1, na.rm = TRUE)*100,
               PerRes2 = Pref2/sum(al1$Pref2, na.rm = TRUE)*100,
               PerRes3 = Pref3/sum(al1$Pref3, na.rm = TRUE)*100) %>%
        dplyr::select(Prefer,Pref1,PerRes1,Pref2,PerRes2,Pref3,PerRes3)
      al1["Total",] <- c("Total",colSums(al1[,-1],na.rm = TRUE))
      
      al1$PerRes1 <- as.character(round(as.numeric(al1$PerRes1),2))  
      al1$PerRes2 <- as.character(round(as.numeric(al1$PerRes2),2))  
      al1$PerRes3 <- as.character(round(as.numeric(al1$PerRes3),2))  
      
      al1 <- rbind(c(Prefer = "", Pref1 = nlable[1], PerRes1 = NA,
                     Pref2 =  nlable[2], PerRes2 = NA,
                     Pref3 =  nlable[3], PerRes3 = NA),
                   c(Prefer = "", Pref1 = "N", PerRes1 = "%",
                     Pref2 = "N", PerRes2 = "%",
                     Pref3 = "N", PerRes3 = "%"),al1)
      
      
      tbla <- flextable(data.frame(al1))
      tbla <- set_header_labels(tbla,  Prefer = paste0(num,". ",titulo))
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = (length(nlable)*2 - 1), part = "header")
      tbla <- merge_h_range(tbla, i = 1, j1 = 2, j2 = 3, part = "body")
      tbla <- merge_h_range(tbla, i = 1, j1 = 4, j2 = 5, part = "body")
      tbla <- merge_h_range(tbla, i = 1, j1 = 6, j2 = 7, part = "body")
      tbla <- bg(tbla, bg = "white", part = "body")
      tbla <- bg(tbla, bg = "darkslateblue", part = "header")
      tbla <- bold(tbla, i = 1, part = "header")
      tbla <- bold(tbla, i = 1, part = "body")
      tbla <- bold(tbla, i = 2, part = "body")
      tbla <- bold(tbla, i = nrow(al1), part = "body")
      tbla <- font(tbla,fontname = "Calibri", part = "all")
      tbla <- color(tbla, i = 1, color = "white", part = "header")
      tbla <- align(tbla, j = 1, align = "left",part = "body")
      tbla <- align(tbla, align = "left",part = "header")
      tbla <- align(tbla, j = 2:(length(nlable)*2 - 2), align = "center",part = "body")
      tbla <- width(tbla, j = 1, width = 2)
      tbla <- width(tbla, j = c(2:(length(nlable)*2 - 2)), width = 0.7)
      tbla <- fontsize(tbla, size = 10, part = "header")
      tbla <- fontsize(tbla, size = 10, part = "body")
      tbla <- border_remove(tbla)
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, j = 1, border.right = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.top = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.bottom = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, i =  c(3,nrow(al1)), border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = 1, j =  c(2,4), border.right = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, j =  c(3,5), border.right = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = nrow(al1), border.bottom = fp_border(color = "black", width = 2))
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, j = (length(nlable)*2 - 1), border.right = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, i = 1, j = (length(nlable)*2 - 2), border.right = fp_border(color = "black", width = 2), part = "body")
      
    } else if (any(grepl(vars,textmining$origvar, fixed = TRUE))) {
      
      minin <- textmining$origvar[grepl(paste0("^",vars,"_*"),textmining$origvar, fixed = FALSE)]
      for (l in 1:length(minin)) {
        minin <- c(minin,paste0(substr(minin[l],1,3),as.numeric(substr(minin[l],4,5)) + 1, substr(minin[l],6,10))  )
      }
      
      print(minin)
      
      datafilt <- data %>% dplyr::select(minin) 
      colnames(datafilt) <- c("P1_1","P1_2","P1_3","P2_1","P2_2","P2_3")
      
      if (vars == "q0036") serv <- "Insumo" else serv <- "Producto o servicio"
      
      datafil1 <- datafilt %>% dplyr::select(P1_1,P2_1) %>% gather(var,P1,-P2_1) %>% rename(P2 = P2_1)
      datafil2 <- datafilt %>% dplyr::select(P1_2,P2_2) %>% gather(var,P1,-P2_2) %>% rename(P2 = P2_2)
      datafil3 <- datafilt %>% dplyr::select(P1_3,P2_3) %>% gather(var,P1,-P2_3) %>% rename(P2 = P2_3)
      
      datafil <- datafil1 %>% bind_rows(datafil2) %>% bind_rows(datafil3) %>% 
        mutate(orden = ifelse(var == "P1_1", "Producto #1",
                              ifelse(var == "P1_2", "Producto #2",
                                     ifelse(var == "P1_3", "Producto #3","")))) %>% dplyr::select(-var)
      
      datafil <- datafil %>% mutate(word = P1) 
      datafil$word <- removeWords(datafil$word, c("No aplca","No aplica"))
      datafil$word <- removePunctuation(as.character(datafil$word))
      datafil$word <- removeNumbers(datafil$word)
      datafil$word <- wordStem(tolower(datafil$word), language = "es") 
      datafil$word <- removeWords(datafil$word, c(get_stopwords(language = "es")$word,"no","n","kilos"))
      datafil$word <- stripWhitespace(trimws(datafil$word))
      
      datafilrt <- datafil %>%
        filter(word != "") %>%
        arrange(P2,word) %>%
        group_by(P2,word) %>%
        mutate(f = 1:n(),
               P1_r = ifelse(f == 1,as.character(P1),NA)) %>%
        fill(P1_r) %>%
        group_by(orden,P2) %>%
        count(orden,P2, P1_r, sort = TRUE) %>%
        mutate(p = n/sum(n)*100)
      
      if (tipo == "agro"){
        a1 <- datafilrt %>% filter(orden == "Producto #1") %>% ungroup() %>% dplyr::select(P1_r,n,p)
        a2 <- datafilrt %>% filter(orden == "Producto #2") %>% ungroup() %>% dplyr::select(P1_r,n,p)
        a3 <- datafilrt %>% filter(orden == "Producto #3") %>% ungroup() %>% dplyr::select(P1_r,n,p)
        
        b1 <- datafilrt %>% filter(orden == "Producto #1") %>% ungroup() %>% dplyr::select(P1_r,n,p)
        b2 <- datafilrt %>% filter(orden == "Producto #2") %>% ungroup() %>% dplyr::select(P1_r,n,p)
        b3 <- datafilrt %>% filter(orden == "Producto #3") %>% ungroup() %>% dplyr::select(P1_r,n,p)
        
      }else {
        a1 <- datafilrt %>% filter(orden == "Producto #1", P2 == 1) %>% ungroup() %>% dplyr::select(P1_r,n,p)
        a2 <- datafilrt %>% filter(orden == "Producto #2", P2 == 1) %>% ungroup() %>% dplyr::select(P1_r,n,p)
        a3 <- datafilrt %>% filter(orden == "Producto #3", P2 == 1) %>% ungroup() %>% dplyr::select(P1_r,n,p)
        
        b1 <- datafilrt %>% filter(orden == "Producto #1", P2 == 2) %>% ungroup() %>% dplyr::select(P1_r,n,p)
        b2 <- datafilrt %>% filter(orden == "Producto #2", P2 == 2) %>% ungroup() %>% dplyr::select(P1_r,n,p)
        b3 <- datafilrt %>% filter(orden == "Producto #3", P2 == 2) %>% ungroup() %>% dplyr::select(P1_r,n,p)
        
      }
      
      if (nrow(table(a1)) >= 20 ) filas <- 20 else filas <- nrow(table(a1))
      if (filas > 0) numer <- c(seq(1,filas,1)) else numer <- rep("",nrow(a))
      a <- cbind(numer, a1[1:filas,],a2[1:filas,],a3[1:filas,])
      a <- rbind(a,
                 c("Total","",sum(a[1:filas,3], na.rm = TRUE),sum(a[1:filas,4], na.rm = TRUE),
                   "",sum(a[1:filas,6], na.rm = TRUE),sum(a[1:filas,7], na.rm = TRUE),
                   "",sum(a[1:filas,9], na.rm = TRUE),sum(a[1:filas,10], na.rm = TRUE)))
      a <- data.frame(a)
      a$p <- as.character(round(as.numeric(a$p),2))  
      a$p.1 <- as.character(round(as.numeric(a$p.1),2))  
      a$p.2 <- as.character(round(as.numeric(a$p.2),2)) 
      
      a <- rbind(c("Los produce su empresa",rep("",9)),c("",paste0(serv," #1"),"N","%",paste0(serv," #2"),"N","%",paste0(serv," #3"),"N","%"),a)
      
      tbla <- flextable(data.frame(a))
      tbla <- set_header_labels(tbla,  numer = paste0(num,"a. ",titulo," (Solo se muestran las ",filas," respuestas más comunes)"))
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = 10, part = "header")
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = 10, part = "body")
      tbla <- bg(tbla, bg = "white", part = "body")
      tbla <- bg(tbla, bg = "darkslateblue", part = "header")
      tbla <- bold(tbla, i = 1, part = "header")
      tbla <- bold(tbla, i = 1, part = "body")
      tbla <- bold(tbla, i = 2, part = "body")
      tbla <- bold(tbla, i = nrow(a), part = "body")
      tbla <- font(tbla,fontname = "Calibri", part = "all")
      tbla <- color(tbla, i = 1, color = "white", part = "header")
      tbla <- align(tbla, j = c(1,2,5,8), align = "left",part = "body")
      tbla <- align(tbla, align = "left",part = "header")
      tbla <- width(tbla, j = c(1), width = 0.5)
      tbla <- width(tbla, j = c(2,5,8), width = 1.5)
      tbla <- width(tbla, j = c(3:4,6:7,9:10), width = 0.5)
      tbla <- fontsize(tbla, size = 10, part = "header")
      tbla <- fontsize(tbla, size = 10, part = "body")
      tbla <- border_remove(tbla)
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, j = 1, border.right = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.top = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.bottom = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, i =  c(2,3,nrow(a)), border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, j =  c(1,4,7), border.right = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = nrow(a), border.bottom = fp_border(color = "black", width = 2))
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, j = 10, border.right = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, i = 1, j = 1, border.right = fp_border(color = "black", width = 2), part = "body")
      
      doc <- read_docx(paste0(dir,"freq_",tipo,substr(rubro,1,1),"_",date,".docx")) %>%
        body_add_par(" ") %>%
        body_add_flextable(tbla)
      print(doc, target = paste0(dir,"freq_",tipo,substr(rubro,1,1),"_",date,".docx"))
      
      if (nrow(table(b1)) >= 20 ) filas <- 20 else filas <- nrow(table(b1))
      if (filas > 0) numer <- c(seq(1,filas,1)) else numer <- rep("",nrow(b))
      b <- cbind(numer, b1[1:filas,],b2[1:filas,],b3[1:filas,])
      b <- rbind(b,
                 c("Total","",sum(b[1:filas,3], na.rm = TRUE),sum(b[1:filas,4], na.rm = TRUE),
                   "",sum(b[1:filas,6], na.rm = TRUE),sum(b[1:filas,7], na.rm = TRUE),
                   "",sum(b[1:filas,9], na.rm = TRUE),sum(b[1:filas,10], na.rm = TRUE)))
      b <- data.frame(b)
      b$p <- as.character(round(as.numeric(b$p),2))  
      b$p.1 <- as.character(round(as.numeric(b$p.1),2))  
      b$p.2 <- as.character(round(as.numeric(b$p.2),2)) 
      
      b <- rbind(c("No los produce su empresa",rep("",9)),c("",paste0(serv," #1"),"N","%",paste0(serv," #2"),"N","%",paste0(serv," #3"),"N","%"),b)
      
      tbla <- flextable(data.frame(b))
      tbla <- set_header_labels(tbla,  numer = paste0(num,"b. ",titulo," (Solo se muestran las ",filas," respuestas más comunes)"))
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = 10, part = "header")
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = 10, part = "body")
      tbla <- bg(tbla, bg = "white", part = "body")
      tbla <- bg(tbla, bg = "darkslateblue", part = "header")
      tbla <- bold(tbla, i = 1, part = "header")
      tbla <- bold(tbla, i = 1, part = "body")
      tbla <- bold(tbla, i = 2, part = "body")
      tbla <- bold(tbla, i = nrow(b), part = "body")
      tbla <- font(tbla,fontname = "Calibri", part = "all")
      tbla <- color(tbla, i = 1, color = "white", part = "header")
      tbla <- align(tbla, j = c(1,2,5,8), align = "left",part = "body")
      tbla <- align(tbla, align = "left",part = "header")
      tbla <- width(tbla, j = c(1), width = 0.5)
      tbla <- width(tbla, j = c(2,5,8), width = 1.5)
      tbla <- width(tbla, j = c(3:4,6:7,9:10), width = 0.5)
      tbla <- fontsize(tbla, size = 10, part = "header")
      tbla <- fontsize(tbla, size = 10, part = "body")
      tbla <- border_remove(tbla)
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, j = 1, border.right = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.top = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, border.bottom = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, i =  c(2,3,nrow(b)), border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, j =  c(1,4,7), border.right = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = nrow(b), border.bottom = fp_border(color = "black", width = 2))
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, j = 10, border.right = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, i = 1, j = 1, border.right = fp_border(color = "black", width = 2), part = "body")
      
    } else {
      
      datafilt <- data %>% dplyr::select(vars) 
      
      a1 <- table(as_label(datafilt))
      a2 <- round(prop.table(table(as_label(datafilt)))*100,2)
      t1 <- sum(a1)
      t2 <- sum(prop.table(table(as_label(datafilt)))*100)
      a <- cbind(cat = rownames(a1),a1, a2)
      a <- rbind(a,c("Total",t1,t2))
      colnames(a) <- c("cat","b1","b2")
      a <- rbind(c("","N", "%"),a)
      rownames(a) <- c()
      
      tbla <- flextable(data.frame(a))
      tbla <- set_header_labels(tbla, cat = paste0(num,". ",titulo))
      tbla <- merge_h_range(tbla, i = 1, j1 = 1, j2 = 3, part = "header")
      tbla <- bg(tbla, bg = "white", part = "body")
      tbla <- bg(tbla, bg = "darkslateblue", part = "header")
      tbla <- bold(tbla, i = 1, part = "header")
      tbla <- bold(tbla, i = 1, part = "body")
      tbla <- bold(tbla, i = nrow(a), part = "body")
      tbla <- font(tbla,fontname = "Calibri", part = "all")
      tbla <- color(tbla, i = 1, color = "white", part = "header")
      tbla <- align(tbla, j = 1, align = "left",part = "body")
      tbla <- align(tbla, align = "left",part = "header")
      tbla <- width(tbla, j = 1, width = 3)
      tbla <- width(tbla, j = c(2,3), width = 1)
      tbla <- fontsize(tbla, size = 10, part = "header")
      tbla <- fontsize(tbla, size = 10, part = "body")
      tbla <- border(tbla, border.right = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "header")
      tbla <- border(tbla, i = 2, border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, i = nrow(a), border.top = fp_border(color = "black"), part = "body")
      tbla <- border(tbla, j = 3, border.right = fp_border(color = "black", width = 2), part = "body")
      tbla <- border(tbla, j = 1, border.left = fp_border(color = "black", width = 2), part = "body")
    }
    
    
    doc <- read_docx(paste0(dir,"freq_",tipo,substr(rubro,1,1),"_",date,".docx")) %>%
      body_add_par(" ") %>%
      body_add_flextable(tbla)
    
    print(doc, target = paste0(dir,"freq_",tipo,substr(rubro,1,1),"_",date,".docx"))
    
  }
  print(paste0("Archivo creado en: ", dir,"freq_",tipo,substr(rubro,1,1),"_",date,".docx"))
}