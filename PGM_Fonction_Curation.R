#library(missMDA)
#library(stringdist)

######################################
# Lecture des datasets #
######################################

leyendoData = function(path="datasets_original"){
    lista_data_path = list.files(path, full.names=T)
    nombres = list.files(path)
    lista_data = lapply(lista_data_path, read.csv)
    names(lista_data) = nombres
    return(lista_data)
}

###################################
# echantillon du datasets #
###################################

dataSubSampler = function(db, size=0.8, train = T, seed = 314161){
    set.seed(seed)
    n = floor(size*nrow(db))
    ind_muestra = sample(1:nrow(db), size = n, replace = F)
    if(train) db = db[ind_muestra,]
    else db = db[-ind_muestra,]
    return(db)
}

#################################################
# reunification des datasets #
#################################################

workDescUnifier = function(lista_data){
    nombres = names(lista_data)
    ind_work_desc = grep(nombres, pattern = "work_desc", ignore.case = T, value = F)
    lista_work_desc = lista_data[ind_work_desc]
    nombres_work_desc = names(lista_work_desc)
    ind_map = grep(nombres_work_desc, pattern = "work_desc_map", ignore.case = T)
    code_wd_map = lista_work_desc[[ind_map]]
    lista_work_desc = lista_work_desc[-ind_map]
    for(i in 1:length(lista_work_desc)){
        #N_i = paste0("N",i)
        code_wd_n = lista_work_desc[[i]]
        nombre_wd = names(lista_work_desc)[i]
        ind_N1 = grepl(nombre_wd, pattern="n1", ignore.case=T)
        ind_N2 = grepl(nombre_wd, pattern="n2", ignore.case=T)
        if(!ind_N1 & !ind_N2){
            code_wd_map = merge(code_wd_map, code_wd_n, by.x = "N3", by.y="Code")
            ind_N = grep(colnames(code_wd_map), pattern = "N3")
            colnames(code_wd_map)[ind_N] = "Code"
            ind_libell = grep(colnames(code_wd_map), pattern = "Libell")
            colnames(code_wd_map)[ind_libell] = "N3"
        }
        if(ind_N1){
            code_wd_map = merge(code_wd_map, code_wd_n, by.x = "N1", by.y="Code")
            ind_N = grep(colnames(code_wd_map), pattern = "N1")
            code_wd_map = code_wd_map[,-ind_N]
            ind_libell = grep(colnames(code_wd_map), pattern = "Libell")
            colnames(code_wd_map)[ind_libell] = "N1"
        }
        if(ind_N2){
            code_wd_map = merge(code_wd_map, code_wd_n, by.x = "N2", by.y="Code")
            ind_N = grep(colnames(code_wd_map), pattern = "N2")            
            code_wd_map = code_wd_map[,-ind_N]
            ind_libell = grep(colnames(code_wd_map), pattern = "Libell")
            colnames(code_wd_map)[ind_libell] = "N2"
        }
    }
    lista_data = lista_data[-ind_work_desc]
    lista_data[[length(lista_data)+1]] = code_wd_map
    names(lista_data)[length(lista_data)] = "code_Work_desc.csv"
    return(lista_data)
}

#########################################
# reunification var géographique #
#########################################

geographicUnifier = function(x){
    nombres = names(lista_data)
    ind_city = grep(nombres, pattern = "^city", ignore.case = T, value = F)
    lista_city = lista_data[ind_city]    
    ind_dep = grep(nombres, pattern = "^depart", ignore.case = T, value = F)
    departments = lista_data[[ind_dep]]    
    ind_reg = grep(nombres, pattern = "^region", ignore.case = T, value = F)
    regions = lista_data[[ind_reg]]
    dep_reg = merge(departments, regions, by="Reg")
    i_reg = grep(colnames(dep_reg), pattern = "Reg")
    dep_reg = dep_reg[-i_reg]
    city_dep_reg = merge(dep_reg, lista_city[[1]], by="Dep")
    for(i in 1:2){
        city_dep_reg = merge(city_dep_reg, lista_city[[i+1]], by = "Insee")
    }
    ind_dep = grep(colnames(city_dep_reg), pattern = "Dep", ignore.case = T, value = F)
    city_dep_reg = city_dep_reg[,-ind_dep]
    ind_insee = grep(colnames(city_dep_reg), pattern = "Insee", ignore.case = T, value = F)
    colnames(city_dep_reg)[ind_insee] = "Code"
    lista_data = lista_data[-c(ind_city, ind_dep, ind_reg)]
    lista_data[[length(lista_data)+1]] = city_dep_reg
    names(lista_data)[length(lista_data)] = "code_Insee.csv"
    return(lista_data)
}

##################################################
# reunification de data learn et test #
##################################################


matcheandoData = function(lista_data, patron = "learn", working_status = "Any"){
    ind_patron = grep(names(lista_data), pattern = patron)
    lista_matches = lista_data[ind_patron]
    nrow_matches = unlist(lapply(lista_matches, nrow))
    print(nrow_matches)
    if(working_status == "Any"){
        ind_max = which.max(nrow_matches)
    }
    if(working_status == "Working"){
        ind_max = grep(names(lista_matches), pattern = "learn_job")
    }
    db = lista_matches[[ind_max]]
    lista_matches = lista_matches[-ind_max]
    for(i in 1:length(lista_matches)){
        db = merge(db, lista_matches[[i]], by="UID", all.x=T)
    }
    return(db)
}

#####################################################
# reunification avec les libellés des variables #
#####################################################

matcheandoCode = function(db, lista_data, patron = "code"){
    ind_codes = grep(names(lista_data), pattern = patron)
    lista_codes = lista_data[ind_codes]
    variables = colnames(db)
    # print(variables)
    variables_code = gsub(names(lista_codes), pattern="code_|\\.csv", replacement="")
    # print(variables_code)
    nombres_comunes = intersect(variables, variables_code)
    # print(nombres_comunes)
    ind_code_cruce = grep(variables_code, pattern = paste0(nombres_comunes, collapse="|"))
    columnas_interes = variables_code[ind_code_cruce]
    # print(columnas_interes)
    for(i in 1:length(columnas_interes)){
        columna_cruce = columnas_interes[i]
        #print(columna_cruce)
        #print(colnames(db))
        #print(columna_interes)
        # dist_jacc = stringdist::stringdist(columna_interes,
        #                                    colnames(db),
        #                                    method = "jaccard"
        #                                    )
        # ind_match = which(dist_jacc==min(dist_jacc))
        # columna_cruce = colnames(db)[ind_match]
        #print(columna_cruce)
        #print(head(lista_codes[[ind_code_cruce[i]]]))
        db = merge(db, 
                   lista_codes[[ind_code_cruce[i]]], 
                   by.x = columna_cruce,
                   by.y = "Code",
                   all.x=T
                   )
        ind_colin = grep(colnames(db), pattern = columna_cruce)
        db = db[,-ind_colin]
        ind_libel = grep(colnames(db), pattern = "Libell")
        colnames(db)[ind_libel] = columna_cruce
    }
    return(db)
}

#########################################
# dublons #
#########################################

duplicateDetector = function(lista_data){
    for(i in 1:length(lista_data)){
        ind_duplicados = which(duplicated(lista_data[[i]]))
        if(length(ind_duplicados)!=0) print(names(lista_data)[i])
    }
}

###########################
# NA #
###########################

buscandoNAS = function(lista_data){
    lista_df_na = list()
    par(cex = 0.6)
    for(i in 1:length(lista_data)){
        db = lista_data[[i]]
        # lista_nas = lapply(db, function(x){
        #                             n_na = length(which(is.na(x)))
        #                             n_vacios = length(which(x==""))
        #                             if(n_na + n_vacios != 0) df = data.frame(n_na, n_vacios)
        #                             else df = NULL
        #                             return(df)
        #                         }
        # )
        # lista_nas = lista_nas[!sapply(lista_nas,is.null)]
        # df_na = do.call("rbind", lista_nas)
        df_na = mice::md.pattern(db,
                                 rotate.names = T
                                 )
        lista_df_na[[i]] = df_na
    }
    names(lista_df_na) = names(lista_data)
    return(lista_df_na)
}

############################
# imputation #
############################

imputandoNAS = function(lista_data, nombre, parallel = FALSE){
    for(i in 1:length(lista_data)){
        db = lista_data[[i]]
        ind = grep(colnames(db),
                    pattern = "UID|F_name|LASTNAME"#|Work_desc" 
                    )
        db = db[,-ind]
        if (parallel){
            library(doParallel)
            cl <- makeCluster(6)
            registerDoParallel(cl)
        }
        miceObj = miceRanger::miceRanger(
                     db, 
                     m = 1, #3
                     maxiter = 1,#5
                     returnModels = TRUE, 
                     verbose=TRUE,
                     parallel = parallel
                    )
        if (parallel){
            stopCluster(cl)
            registerDoSEQ()
        }
        lista_data = miceRanger::completeData(miceObj)
        for(j in 1:length(lista_data)){
            write.csv(lista_data[[j]], 
                      paste0("datasets/data_imputada_", nombre, "_", j,".csv"), 
                      row.names=F)
        }
        # save(miceObj,file=nombre)
    }
    return(miceObj)
}
###########################################
# Analyse exploratoire #
###########################################

descriptivePlotsDbJob = function(db_job){
    variables = colnames(db_job)
    ind_var = grep(variables, pattern = "EMOLUMENT")
    # par(mfrow=c(1,2))
    par(mar = c(10,15,1,1))
    for(i in 1:length(variables)){
        # print(variables[i])
        col_i = db_job[,i]
        if(i == ind_var | any(is.na(col_i))) next
        form = paste0("EMOLUMENT ~", variables[i])
        if(class(col_i)!="numeric" & class(col_i)!="integer"){      
            boxplot(data = db_job, 
                    formula(form), 
                    main = form, 
                    las=2, 
                    col = RColorBrewer::brewer.pal(8, "Set1"),
                    horizontal=T
                    )
        }else{
            plot(data = db_job, 
                    formula(form), 
                    main = form, 
                    las=2, 
                    col = RColorBrewer::brewer.pal(8, "Set1")
                    )
        }
    }
}

###########################################
# Analyse exploratoire #
###########################################

descriptivePlots = function(lista_data, build_pdf = TRUE){
    nombres = gsub(names(lista_data), pattern = "\\.csv", replacement = "\\.pdf")
    for(i in 1:length(lista_data)){
        if(grepl(nombres[i], pattern = "code")) next
        db = lista_data[[i]]
        if(build_pdf) pdf(paste0("results/desc_",nombres[i]), width = 11, height = 7)
        par(mfrow = c(2,2),
            cex.axis = 0.6
            )
        ind_numeric = which(unlist(lapply(db, is.numeric)))
        categoricas = (colnames(db)[-ind_numeric])
        categoricas = categoricas[categoricas!="Nom.de.la.commune"]
        for(j in 2:ncol(db)){
            variable = db[,j]
            if(is.numeric(variable)){
                numerica = colnames(db)[j]
                hist(db[,j], 
                     main = numerica,
                     xlab="", 
                     col = rainbow(j),
                     las = 2
                     )
                # for(categorica in categoricas){
                # for(k in 1:length(categoricas)){
                # # for(k in 3:length(categoricas)){    
                #     # print(k)
                #     categorica = categoricas[k]
                #     form = paste0(numerica, "~", categorica)#, '+ SEX')
                #     boxplot(data = db, 
                #             formula(form), 
                #             main = form, 
                #             las=2, 
                #             col = RColorBrewer::brewer.pal(8, "Set1")
                #             )
                # }
            }else{
                barplot(table(variable), 
                        main = colnames(db)[j], 
                        col = hcl.colors(j, palette= "viridis"),
                        las = 2
                        )
            }
        }
        if(build_pdf) dev.off()
    }
}

#######################################################################
# XGboost #
#######################################################################

mod_XGboost = function(db, save = TRUE){
    ind_var_no = grep(colnames(db),
                       pattern = "Activity_type|Nom.de.la.commune|UID|F_name|LASTNAME")
    if(length(ind_var_no)!=0) db = db[,-ind_var_no]
    db_num = as.data.frame(lapply(db, as.numeric))
    db_num0 = as.matrix(db_num)
    mod_xgboost = xgboost::xgboost(
        data = db_num0[, -grep(colnames(db_num), pattern = "EMOLUMENT")],
        label = db_num0[, grep(colnames(db_num), pattern = "EMOLUMENT")],
        nrounds = 1000,
        objective = "reg:squarederror",
        early_stopping_rounds = 3,
        max_depth = 6,
        eta = .25,
        verbose = 0
    )
    # pdf("results/pesos_xgboost.pdf", width=10, height=7)
    # importance_matrix = xgboost::xgb.importance(model = mod_xgboost)
    # xgboost::xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
    if(save) save(mod_xgboost, file = "models/mod_xgboost_sinXY.RData")
    # dev.off()
    return(mod_xgboost)
}

###################################################
# NA sport #
###################################################

imputeSportsWorkingHoursOutlierHandling = function(learn_work){
    sports = as.character(learn_work$Sports)
    ind_na = which(is.na(learn_work$Sports))
    sports[ind_na] = "NO_SPORT"
    sports[-ind_na] = "SPORT"
    learn_work$Sports = as.factor(sports)
    learn_work = na.omit(learn_work)
    learn_work = learn_work[learn_work$EMOLUMENT>1200 & learn_work$EMOLUMENT<100000,]
    return(learn_work)
}

###################################################################
# Correlations #
###################################################################

mixedDataCorrelator = function(db, nombre){
    matriz = matrix(0, ncol(db), ncol(db))
    for(i in 1:(ncol(db)-1)){
        for(j in (i+1):ncol(db)){
            col_i = db[,i]
            col_j = db[,j]
            tipo_i = class(col_i)
            tipo_j = class(col_j)
            print(c(tipo_i, tipo_j))
            print(c(colnames(db)[i], colnames(db)[j]))
            if((tipo_i == "numeric" | tipo_i == "integer") & (tipo_j == "numeric" | tipo_j == "integer")){
                test = cor(as.numeric(col_i), as.numeric(col_j), method = "spearman")
            }
            if((tipo_i != "numeric" & tipo_i != "integer") & (tipo_j == "numeric" | tipo_j == "integer")){
                mod = MASS::glm.nb(scale(col_j) ~ col_i)
                m = summary(mod)
                test = m$adj.r.squared
            }
            if((tipo_i == "numeric" | tipo_i == "integer") & (tipo_j != "numeric" & tipo_j != "integer")){
                mod = MASS::glm.nb(scale(col_i) ~ col_j)
                m = summary(mod)
                test = m$adj.r.squared
            }
            if((tipo_i != "numeric" & tipo_i != "integer") & (tipo_j != "numeric" & tipo_j != "integer")){
                test = lsr::cramersV(col_i,col_j)
            }
            print(test)
            matriz[i,j] =  test
        }
    }
    colnames(matriz) = colnames(db)
    rownames(matriz) = colnames(db)
    write.csv(matriz, "datasets/matriz_correlacion.csv")
    library(corrplot)
    pdf("correlation_analysis.pdf")
    col4 = colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                            "cyan", "#007FFF", "blue", "#00007F"))
    colores = col4(10)
    corrplot(matriz, 
             method = "circle", 
             col = colores,
             title = "AnÃ¡lisis exploratorio",
             tl.col = "black",
             mar = c(0,0,3,2)#,
#                     p.mat = sig_pr,
#                     sig.level = 0.05)
            )
    dev.off()
    return(matriz)
}

########################################################################
# Rélation Emolument avec les autres variables #
########################################################################

emolumentVSall = function(db){
    ind_emo = grep(colnames(db), pattern = "EMOLUMENT")
    emolument = db[,ind_emo]
    db = db[,-ind_emo]
    ind_var_no = grep(colnames(db),
                       pattern = "Activity_type|Nom.de.la.commune|UID|F_name|LASTNAME")
    if(length(ind_var_no)!=0) db = db[,-ind_var_no]
    lista_test = list()
    for(i in 1:ncol(db)){
        col_j = db[,i]
        tipo_i = class(emolument)
        tipo_j = class(col_j)
        print(i)
        if((tipo_i == "numeric" | tipo_i == "integer") & (tipo_j == "numeric" | tipo_j == "integer")){
            m = MASS::glm.nb(emolument ~ col_j)
            test = with(summary(m), 1 - deviance/null.deviance)
        }
        if((tipo_i == "numeric" | tipo_i == "integer") & (tipo_j != "numeric" & tipo_j != "integer")){
            m = MASS::glm.nb(emolument ~ col_j)
            test = with(summary(m), 1 - deviance/null.deviance)
        }
        lista_test[[i]] = test
    }
    names(lista_test) = colnames(db)
    pesos = t(as.data.frame(lista_test))
    pesos0 = pesos[order(pesos, decreasing=F)]
    variables = rownames(pesos)[order(pesos, decreasing = F)]
    df_pesos = data.frame("variables" = variables, "pesos" = pesos0)
    write.csv(df_pesos, "datasets/pesos_predictores.csv")
    # pdf("results/pesos_predictores_bn_glm.pdf", width = 10, height = 7)
    # par(mar=c(2,10,1,1))
    # barplot(pesos0,
    #         names.arg = rownames(pesos)[order(pesos, decreasing=F)],
    #         horiz =T,
    #         border="white",
    #         las =2,
    #         main = "Pesos Predictores")
    # # dev.off()
    return(df_pesos)
}

#####################################
# Kmeans pour data mixte #
#####################################

mixedDataClusteringKmeans = function(db, k=5, importance=0.5) {
    db = db[, -grep(colnames(db),
            pattern = "Activity_type|Nom.de.la.commune|UID|F_name|LASTNAME|EMOLUMENT|Lat|long|^X|^Y|EMOLUMENT")]
    ind_num = which(unlist(lapply(db, is.numeric)))
    set.seed(100)
    mod = kamila::wkmeans(db[,ind_num],
                          as.data.frame(lapply(db[,-ind_num],as.factor)),
                          0.5,
                          k)
    return(mod)
}

#######################################
# A ne pas lancer #
#######################################

# adjustingNN = function(train){
#      require(caret)
#      db_dum = dummyVars(form,
#                         data = train)
#      mat = data.frame("EMOLUMENT"=train$EMOLUMENT, predict(db_dum, train))
#     nn = neuralnet::neuralnet( EMOLUMENT~.,
#                                 data = mat,
#                                 hidden = 5,
#                                 linear.output = TRUE)
#     save(nn, file = "models/modelo_nn.RData")
#     return(nn)
# }

#######################################
# A ne pas lancer #
#######################################

# Normalize the data
# dataNorm = function(data){
#     maxs <- apply(data, 2, max) 
#     mins <- apply(data, 2, min)
#     scaled <- as.data.frame(scale(data, center = mins, 
#                                 scale = maxs - mins))
#     return(scaled)
# }