# ----------------------------- Function to produce Figures--------------------------------------
#' Create Figure 1 
#' 
#' Compute the percentage of observed non-perennial months for european and South American
#' gauging stations for all observations and validation station-months during 1981-2019.
#' 
#' @param in_eu_all_dir the path to all observed European station-months 
#' @param in_sa_all_dir the path to all observed South American station-months 
#' @param in_eu_val_dir the path to observed European station-months in validation dataset
#' @param in_sa_val_dir the path to observed South American station-months in validation dataset
#' @param gauge_eu_dir the path to the European gauging stations shapefile
#' @param gauge_sa_dir the path to the South American gauging stations shapefile
#' @param out_dir the path to the output shapefiles for creating figure 1
#' 
#' @return NULL
#' 
create_fig1 <- function(in_eu_all_dir, in_sa_all_dir, in_eu_val_dir, in_sa_val_dir,
                        gauge_eu_dir, gauge_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the observed monthly time series of streamflow for all and validation datasets 
  in_eu_all <- qs::qread(file = in_eu_all_dir)
  in_sa_all <- qs::qread(file = in_sa_all_dir)
  data.table::setnames(in_sa_all, 'gaugeid', 'gauge_id')
  in_eu_val <- qs::qread(in_eu_val_dir)
  in_sa_val <- qs::qread(in_sa_val_dir)
  
  ##read the shapefiles
  eu_gauge <- sf::read_sf(gauge_eu_dir)
  eu_gauge <- eu_gauge %>% 
    rename(., gaugeid=gauge_d) %>% 
    dplyr::select(c('gaugeid', 'upa', 'gauge_name', 'DRYVER_RIV'))
  sa_gauge <- sf::read_sf(gauge_sa_dir)
  
  ##compute the number of NP for Europe
  ratio_eu_all <- in_eu_all[,round(sum(target!=0)/.N * 100, digits = 2), by='gaugeid'][order(gaugeid),]
  ratio_eu_val <- in_eu_val[,round(sum(target!=0)/.N * 100, digits = 2), by='gaugeid'][order(gaugeid),]
  eu_out <- data.table(gaugeid=ratio_eu_all[,gaugeid],ratio_all=ratio_eu_all[,V1],
                       ratio_val=ratio_eu_val[,V1])
  
  left_join(eu_gauge, eu_out, by='gaugeid') %>% 
    sf::write_sf(., file.path(out_dir, 'np_fraction_eu_stations_fig1ac.shp'))
  
  ##compute the number of NP for South America
  ratio_sa_all <- in_sa_all[,round(sum(target!=0)/.N * 100, digits = 2), by='gauge_id'][order(gauge_id),]
  ratio_sa_val <- in_sa_val[,round(sum(target!=0)/.N * 100, digits = 2), by='gauge_id'][order(gauge_id),]
  
  sa_out <- data.table(gauge_id=ratio_sa_all[,gauge_id],ratio_all=ratio_sa_all[,V1],
                       ratio_val=ratio_sa_val[,V1])
  
  left_join(sa_gauge, sa_out, by='gauge_id') %>% 
    sf::write_sf(., file.path(out_dir, 'np_fraction_sa_stations_fig1bd.shp'))
  
  return(NULL)
}


## create figure 2 -----
#' Create Figure 2 
#' 
#' Compute the ratio of the number of predicted non-perennial months to the number of
#' observed non-perennial months (P: perennial, NP: non-perennial) at the European and
#' South American gauging stations in the validation data sets for the model variants that 
#' were trained with all European gauging stations (I; first row), South American gauging 
#' stations (II; second row), and all European and South American gauging stations together
#' 
#' @param in_eu_val_dir the path to observed European station-months in validation dataset
#' @param in_sa_val_dir the path to observed South American station-months in validation dataset
#' @param model_i_dir the path to model variant I RF object, presenting European model
#' @param model_ii_dir the path to model variant II RF object, presenting South American model
#' @param model_iiia_dir the path to model variant I RF object, presenting "global" model
#' @param gauge_eu_dir the path to the European gauging stations shapefile
#' @param gauge_sa_dir the path to the South American gauging stations shapefile
#' @param out_dir the path to the output shapefiles for creating figure 2
#' 
#' @return NULL
#' 
create_fig2 <- function(in_eu_val_dir, in_sa_val_dir, model_i_dir,
                        model_ii_dir, model_iiia_dir, gauge_eu_dir, gauge_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the validation datasets
  val_data_eu <- qs::qread(in_eu_val_dir)
  val_data_sa <- qs::qread(in_sa_val_dir)
  
  val_data_eu_editted <- val_data_eu[,-c('target', 'gaugeid', 'dates', 'X', 'Y')]
  val_data_sa_editted <- val_data_sa[,-c('target', 'gauge_id')]
  
  ##read the shapefiles
  eu_gauge <- sf::read_sf(gauge_eu_dir)
  eu_gauge <- eu_gauge %>% 
    rename(., gaugeid=gauge_d) %>% 
    dplyr::select(c('gaugeid', 'upa', 'gauge_name', 'DRYVER_RIV'))
  sa_gauge <- sf::read_sf(gauge_sa_dir)
  
  ## inner function to apply the RF model on the validation datasets
  apply_on_val_datasets <- function(in_dt, in_model){
    
    execute_models_nets <- function(model_step1,
                                    data_dt, threshold = 0.5){
      
      res_step1 <- model_step1$predict_newdata(data_dt)
      res_step1 <- res_step1$set_threshold(threshold) %>% 
        as.data.table()
      
      out <- list(res_step1 = res_step1)
      return(out)
    }
    
    out <- execute_models_nets(in_model,
                               data_dt=in_dt)[[1]][,as.numeric(as.character(response))]
    return(out)
  }
  
  ## Model variant I on EU and SA datasets
  model_step1_i <- qs::qread(model_i_dir)
  eu_gstat_eu <- apply_on_val_datasets(in_dt = val_data_eu_editted,
                                       in_model = model_step1_i)
  eu_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                       in_model = model_step1_i)
  ## Model II on EU and SA datasets
  model_step1_ii <- qs::qread(model_ii_dir)
  sa_gstat_eu <- apply_on_val_datasets(in_dt = val_data_eu_editted,
                                       in_model = model_step1_ii)
  sa_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                       in_model = model_step1_ii)
  ## Model IIIa on EU and SA datasets
  model_step1_iiia <- qs::qread(model_iiia_dir)
  eusa_all_gstat_eu <- apply_on_val_datasets(in_dt = val_data_eu_editted,
                                             in_model = model_step1_iiia)
  eusa_all_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                             in_model = model_step1_iiia)
  
  ## combine the predicted SI status by each model variant into a table
  dt_eu <- data.table(gaugeid=val_data_eu$gaugeid, target=val_data_eu$target, var_I=eu_gstat_eu,
                      var_II=sa_gstat_eu, var_IIIa=eusa_all_gstat_eu)
  dt_sa <- data.table(gauge_id=val_data_sa$gauge_id,target=val_data_sa$target, var_I=eu_gstat_sa,
                      var_II=sa_gstat_sa, var_IIIa=eusa_all_gstat_sa)
  
  ## compute the ratio of the number of predicted NP months to the number of observed
  dt_eu_sta <- dt_eu[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gaugeid', .SDcols = -2]
  dt_sa_sta <- dt_sa[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gauge_id', .SDcols = -2]
  
  sa_gauge <- sa_gauge %>% 
    filter(gauge_id %in% dt_sa_sta$gauge_id)
  # Replace NaN with 400  and Inf with 4000
  for (col in names(dt_eu_sta)) {
    set(dt_eu_sta, i = which(is.nan(dt_eu_sta[[col]])), j = col, value = 400)
    set(dt_eu_sta, i = which(is.infinite(dt_eu_sta[[col]])), j = col, value = 4000)
  }
  
  for (col in names(dt_sa_sta)) {
    set(dt_sa_sta, i = which(is.nan(dt_sa_sta[[col]])), j = col, value = 400)
    set(dt_sa_sta, i = which(is.infinite(dt_sa_sta[[col]])), j = col, value = 4000)
  }
  
  ## write the results into two shapefiles for each continent
  left_join(eu_gauge[,c("gaugeid", "upa")], dt_eu_sta,  by='gaugeid') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_eu_stations_fig2ace.shp'))
  left_join(sa_gauge[,c("gauge_id", "upa")], dt_sa_sta,  by='gauge_id') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_sa_stations_fig2bdf.shp'))
  
  return(NULL)
}

## create figure 3 -----
#' Create Figure 3 
#' 
#' Compute fraction of months with at least one no-flow day during 1981-2019 for European and
#' South American reaches for the model variants that were trained with all European gauging 
#' stations (I; first row), South American gauging stations (II; second row), and all European 
#' and South American gauging stations together (IIIa; third row).
#' 
#' @param reaches_SI_status_dir the path to streamflow intermittence status simulated by the
#' model variants, here the model variants I, II and IIIa
#' @param reaches_eu_dir the path to the European gauging reaches shapefile
#' @param reaches_sa_dir the path to the South American gauging reaches shapefile
#' @param out_dir the path to the output shapefiles for creating figure 3
#' 
#' @return NULL
#'
create_fig3 <- function(reaches_SI_status_dir, reaches_eu_dir, reaches_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the reaches for EU and SA
  reaches_shp_eu <- sf::read_sf(reaches_eu_dir)
  reaches_shp_sa <- sf::read_sf(reaches_sa_dir)
  
  ## read the predicted SI status at the reaches in EU and SA for the three model variants
  pre_eu_model_I <- fst::read_fst(file.path(reaches_SI_status_dir,
                                            'res_nets_mat_dt_eu_allstat_on_eu.fst')) %>% as.data.table()
  pre_sa_model_I <- fst::read_fst(file.path(reaches_SI_status_dir,
                                            'res_nets_mat_dt_eu_allstat_on_sa.fst')) %>% as.data.table()
  pre_eu_model_II <- fst::read_fst(file.path(reaches_SI_status_dir,
                                             'res_nets_mat_dt_sa_allstat_on_eu.fst')) %>% as.data.table()
  pre_sa_model_II <- fst::read_fst(file.path(reaches_SI_status_dir,
                                             'res_nets_mat_dt_sa_allstat_on_sa.fst')) %>% as.data.table()
  pre_eu_model_IIIa <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'res_nets_mat_dt_eusa_all_gstat_on_eu.fst')) %>% as.data.table()
  pre_sa_model_IIIa <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'res_nets_mat_dt_eusa_all_gstat_on_sa.fst')) %>% as.data.table()
  ## Model variant I
  d <- rowSums(pre_eu_model_I[,-1] > 0)/468 * 100
  final_eu <- data.table(DRYVER_RIV = pre_eu_model_I$V1, frac=d)
  left_join(reaches_shp_eu, final_eu, by='DRYVER_RIV') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_I_on_eu_fig3a.shp'))
  
  d_sa <- rowSums(pre_sa_model_I[,-1] > 0)/468 * 100
  final_sa <- data.table(HYRIV_ID = pre_sa_model_I$V1, frac=d_sa)
  left_join(reaches_shp_sa[,c('HYRIV_ID','UPLAND_SKM')], final_sa, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_I_on_sa_fig3b.shp'))
  
  ## model variant II
  d <- rowSums(pre_eu_model_II[,-1] > 0)/468 * 100
  final_eu <- data.table(DRYVER_RIV = pre_eu_model_II$V1, frac=d)
  left_join(reaches_shp_eu, final_eu, by='DRYVER_RIV') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_II_on_eu_fig3c.shp'))
  
  d_sa <- rowSums(pre_sa_model_II[,-1] > 0)/468 * 100
  final_sa <- data.table(HYRIV_ID = pre_sa_model_II$V1, frac=d_sa)
  left_join(reaches_shp_sa[,c('HYRIV_ID','UPLAND_SKM')], final_sa, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_II_on_sa_fig3d.shp'))
  
  ## model variant IIIa
  d <- rowSums(pre_eu_model_IIIa[,-1] > 0)/468 * 100
  final_eu <- data.table(DRYVER_RIV = pre_eu_model_IIIa$V1, frac=d)
  left_join(reaches_shp_eu, final_eu, by='DRYVER_RIV') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_IIIa_on_eu_fig3e.shp'))
  
  d_sa <- rowSums(pre_sa_model_IIIa[,-1] > 0)/468 * 100
  final_sa <- data.table(HYRIV_ID = pre_sa_model_IIIa$V1, frac=d_sa)
  left_join(reaches_shp_sa[,c('HYRIV_ID','UPLAND_SKM')], final_sa, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_IIIa_on_sa_fig3f.shp'))
  
  return(NULL)
}

## create figure 4 ------
#' Create Figure 4 
#' 
#' Compute the ratio of the number of predicted non-perennial months to the number of 
#' observed non-perennial months (P: perennial, NP: non-perennial) at the European and 
#' South American gauging stations for the model variants that were trained with all European 
#' and South American gauging stations using the hyperparameters from the step 1 RF model 
#' of Döll et al. (2024) and without tuning (IIIb; first row) and the fraction of months with 
#' at least one no-flow day during 1981-2019 for European and South American reaches for 
#' variant IIIb (second row).
#'
#' @param in_eu_val_dir the path to observed European station-months in validation dataset
#' @param in_sa_val_dir the path to observed South American station-months in validation dataset
#' @param model_iiib_dir the path to model variant I RF object, presenting the global model with
#' tuned hyperparameters borowed from Döll et al. (2024) RF model.
#' @param reaches_SI_status_dir the path to streamflow intermittence status simulated by the
#' model variant, here the model variant IIIb
#' @param gauge_eu_dir the path to the European gauging stations shapefile
#' @param gauge_sa_dir the path to the South American gauging stations shapefile
#' @param reaches_eu_dir the path to the European gauging reaches shapefile
#' @param reaches_sa_dir the path to the South American gauging reaches shapefile
#' @param out_dir the path to the output shapefiles for creating figure 4
#' 
#' @return NULL
#'
create_fig4 <- function(in_eu_val_dir, in_sa_val_dir, model_iiib_dir, reaches_SI_status_dir,
                        gauge_eu_dir, gauge_sa_dir, reaches_eu_dir, reaches_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the validation datasets
  val_data_eu <- qs::qread(in_eu_val_dir)
  val_data_sa <- qs::qread(in_sa_val_dir)
  
  val_data_eu_editted <- val_data_eu[,-c('target', 'gaugeid', 'dates', 'X', 'Y')]
  val_data_sa_editted <- val_data_sa[,-c('target', 'gauge_id')]
  
  ##read the shapefiles
  eu_gauge <- sf::read_sf(gauge_eu_dir)
  eu_gauge <- eu_gauge %>% 
    rename(., gaugeid=gauge_d) %>% 
    dplyr::select(c('gaugeid', 'upa', 'gauge_name', 'DRYVER_RIV'))
  sa_gauge <- sf::read_sf(gauge_sa_dir)
  
  ## inner function to apply the RF model on the validation datasets
  apply_on_val_datasets <- function(in_dt, in_model){
    
    execute_models_nets <- function(model_step1,
                                    data_dt, threshold = 0.5){
      
      res_step1 <- model_step1$predict_newdata(data_dt)
      res_step1 <- res_step1$set_threshold(threshold) %>% 
        as.data.table()
      
      out <- list(res_step1 = res_step1)
      return(out)
    }
    
    out <- execute_models_nets(in_model,
                               data_dt=in_dt)[[1]][,as.numeric(as.character(response))]
    return(out)
  }
  
  ## Model variant I on EU and SA datasets
  model_step1_iiib <- qs::qread(model_iiib_dir)
  eu_gstat_eu <- apply_on_val_datasets(in_dt = val_data_eu_editted,
                                       in_model = model_step1_iiib)
  eu_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                       in_model = model_step1_iiib)
  
  ## combine the predicted SI status by each model variant into a table
  dt_eu <- data.table(gaugeid=val_data_eu$gaugeid, target=val_data_eu$target, var_IIIb=eu_gstat_eu)
  dt_sa <- data.table(gauge_id=val_data_sa$gauge_id,target=val_data_sa$target, var_IIIb=eu_gstat_sa)
  
  ## compute the ratio of the number of predicted NP months to the number of observed
  dt_eu_sta <- dt_eu[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gaugeid', .SDcols = -2]
  dt_sa_sta <- dt_sa[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gauge_id', .SDcols = -2]
  
  sa_gauge <- sa_gauge %>% 
    filter(gauge_id %in% dt_sa_sta$gauge_id)
  # Replace NaN with 400  and Inf with 4000
  for (col in names(dt_eu_sta)) {
    set(dt_eu_sta, i = which(is.nan(dt_eu_sta[[col]])), j = col, value = 400)
    set(dt_eu_sta, i = which(is.infinite(dt_eu_sta[[col]])), j = col, value = 4000)
  }
  
  for (col in names(dt_sa_sta)) {
    set(dt_sa_sta, i = which(is.nan(dt_sa_sta[[col]])), j = col, value = 400)
    set(dt_sa_sta, i = which(is.infinite(dt_sa_sta[[col]])), j = col, value = 4000)
  }
  
  ## write the results into two shapefiles for each continent
  left_join(eu_gauge[,c("gaugeid", "upa")], dt_eu_sta,  by='gaugeid') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_eu_stations_fig4a.shp'))
  left_join(sa_gauge[,c("gauge_id", "upa")], dt_sa_sta,  by='gauge_id') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_sa_stations_fig4b.shp'))
  
  ## part 4c and 4d ----
  ## read the reaches for EU and SA
  reaches_shp_eu <- sf::read_sf(reaches_eu_dir)
  reaches_shp_sa <- sf::read_sf(reaches_sa_dir)
  
  ## read the predicted SI status at the reaches in EU and SA for the three model variants
  pre_eu_model_IIIb <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'res_nets_mat_dt_eusa_doell_hyperparam_on_eu.fst')) %>% as.data.table()
  pre_sa_model_IIIb <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'res_nets_mat_dt_eusa_doell_hyperparam_on_sa.fst')) %>% as.data.table()
  
  ## Model variant IIIb
  d <- rowSums(pre_eu_model_IIIb[,-1] > 0)/468 * 100
  final_eu <- data.table(DRYVER_RIV = pre_eu_model_IIIb$V1, frac=d)
  left_join(reaches_shp_eu, final_eu, by='DRYVER_RIV') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_IIIb_on_eu_fig4c.shp'))
  
  d_sa <- rowSums(pre_sa_model_IIIb[,-1] > 0)/468 * 100
  final_sa <- data.table(HYRIV_ID = pre_sa_model_IIIb$V1, frac=d_sa)
  left_join(reaches_shp_sa[,c('HYRIV_ID','UPLAND_SKM')], final_sa, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_IIIb_on_sa_fig4d.shp'))
  
  return(NULL)
}
## create figure 5 ------
ggvimp <- function(in_rftuned_eu, in_rftuned_sa,in_rftuned_eusa,in_rftuned_eusa_iiib,
                   in_predvars, varnum = 10, spatial_rsp=FALSE) {
  
  rsmp_res_eu <- get_outerrsmp(in_rftuned_eu, spatial_rsp=spatial_rsp)
  rsmp_res_sa <- get_outerrsmp(in_rftuned_sa, spatial_rsp=spatial_rsp)
  rsmp_res_eusa <- get_outerrsmp(in_rftuned_eusa, spatial_rsp=spatial_rsp)
  #Get variable importance and format them
  varimp_basic_eu <- weighted_vimportance_nestedrf(rfresamp = rsmp_res_eu,
                                                   pvalue = FALSE) %>%
    merge(., in_predvars, by.x='varnames', by.y = "varname") %>%
    .[, `:=`(varnames = factor(varnames, varnames[order(-imp_wmean)]),
             Category = factor(Category,
                               levels = c('Anthropogenic', 'Climate', 'Geology', 'Hydrology', 
                                          'Lakes','Landcover', 'Physiography'))
    )] %>%
    setorder(-imp_wmean)
  
  varimp_basic_sa <- weighted_vimportance_nestedrf(rfresamp = rsmp_res_sa,
                                                   pvalue = FALSE) %>%
    merge(., in_predvars, by.x='varnames', by.y = "varname") %>%
    .[, `:=`(varnames = factor(varnames, varnames[order(-imp_wmean)]),
             Category = factor(Category,
                               levels = c('Anthropogenic', 'Climate', 'Geology', 'Hydrology', 
                                          'Lakes','Landcover', 'Physiography'))
    )] %>%
    setorder(-imp_wmean)
  
  varimp_basic_eusa <- weighted_vimportance_nestedrf(rfresamp = rsmp_res_eusa,
                                                     pvalue = FALSE) %>%
    merge(., in_predvars, by.x='varnames', by.y = "varname") %>%
    .[, `:=`(varnames = factor(varnames, varnames[order(-imp_wmean)]),
             Category = factor(Category,
                               levels = c('Anthropogenic', 'Climate', 'Geology', 'Hydrology', 
                                          'Lakes','Landcover', 'Physiography'))
    )] %>%
    setorder(-imp_wmean)
  
  base_pipeop <- in_rftuned_eusa_iiib$graph_model$pipeops$classif.ranger
  base_learner <- base_pipeop$learner_model
  imp <- base_learner$importance()
  varimp_basic_eusa_iiib <- data.table(varnames = names(imp),
                                       imp_wmean = as.vector(imp))
  varimp_basic_eusa_iiib <- varimp_basic_eusa_iiib[varimp_basic_eusa[,.(varnames,abbrevation, Category)],
                                                   on = 'varnames'] %>%
    setorder(-imp_wmean)
  #Plot step 1 RF for European model 
  outp_eu <- varimp_basic_eu[1:varnum] %>% 
    mutate(abbrevation = fct_reorder(abbrevation, desc(imp_wmean))) %>% 
    ggplot(. ,aes(x=abbrevation,
                  color =Category, fill=Category)) +
    geom_bar(aes(y=imp_wmean), stat = 'identity', alpha=0.7) +
    geom_errorbar(aes(ymin=imp_wmean-imp_wsd, ymax=imp_wmean+imp_wsd)) +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(x, width = 27)
    },
    limits=rev) +
    scale_fill_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                      drop=FALSE) +
    scale_color_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                       drop=FALSE) +
    theme_classic(18) +
    scale_y_continuous(expand=c(0,0), position = 'left') +
    labs(y = "", x='', title = '              I')+
    coord_flip(ylim=c(0, max(varimp_basic_eu[, max(imp_wmean+imp_wsd)+10], 100))) +
    theme(axis.text = element_text(color = 'black'), legend.position = 'none')
  
  outp_sa <- varimp_basic_sa[1:varnum] %>% 
    mutate(abbrevation = fct_reorder(abbrevation, desc(imp_wmean))) %>% 
    ggplot(. ,aes(x=abbrevation,
                  color =Category, fill=Category)) +
    geom_bar(aes(y=imp_wmean), stat = 'identity', alpha=0.7) +
    geom_errorbar(aes(ymin=imp_wmean-imp_wsd, ymax=imp_wmean+imp_wsd)) +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(x, width = 27)
    },
    limits=rev) +
    scale_fill_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                      drop=FALSE) +
    scale_color_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                       drop=FALSE) +
    theme_classic(18) +
    scale_y_continuous(expand=c(0,0), position = 'left') +
    labs(y = "", x='', title = '              II')+
    coord_flip(ylim=c(0, max(varimp_basic_sa[, max(imp_wmean+imp_wsd)+10], 100))) +
    theme(axis.text = element_text(color = 'black'), legend.position = 'none')
  
  outp_eusa <- varimp_basic_eusa[1:varnum] %>% 
    mutate(abbrevation = fct_reorder(abbrevation, desc(imp_wmean))) %>% 
    ggplot(. ,aes(x=abbrevation,
                  color =Category, fill=Category)) +
    geom_bar(aes(y=imp_wmean), stat = 'identity', alpha=0.7) +
    geom_errorbar(aes(ymin=imp_wmean-imp_wsd, ymax=imp_wmean+imp_wsd)) +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(x, width = 27)
    },
    limits=rev) +
    scale_fill_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                      drop=FALSE) +
    scale_color_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                       drop=FALSE) +
    theme_classic(18) +
    scale_y_continuous(expand=c(0,0), position = 'left') +
    labs(y = "", x='', title = '              IIIa')+
    coord_flip(ylim=c(0, max(varimp_basic_eusa[, max(imp_wmean+imp_wsd)+10], 100))) +
    theme(axis.text = element_text(color = 'black'), legend.position = 'none')
  
  outp_eusa_iiib <- varimp_basic_eusa_iiib[1:varnum] %>% 
    mutate(abbrevation = fct_reorder(abbrevation, desc(imp_wmean))) %>% 
    ggplot(. ,aes(x=abbrevation,
                  color =Category, fill=Category)) +
    geom_bar(aes(y=imp_wmean), stat = 'identity', alpha=0.7) +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(x, width = 27)
    },
    limits=rev) +
    scale_fill_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                      drop=FALSE) +
    scale_color_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                       drop=FALSE) +
    theme_classic(18) +
    scale_y_continuous(expand=c(0,0), position = 'left') +
    labs(y = "", x='', title = '              IIIb')+
    coord_flip(ylim=c(0, max(varimp_basic_eusa_iiib[, max(imp_wmean)+2000], 100))) +
    theme(axis.text = element_text(color = 'black'), legend.position = 'none')
  
  pout <- cowplot::plot_grid(outp_eu, outp_sa,outp_eusa, outp_eusa_iiib, label_size = 18) +
    cowplot::draw_label("Variable importance (actual impurity reduction)", 
                        x=0.5, y=  0, vjust=-0.5, angle= 0, size = 18) +
    cowplot::draw_label("Predictor", x=  0, y=0.5, vjust= 1.5, angle=90, size = 18)
  
  return(pout)
}

## create figure 6 -----
#' Create Figure 6 
#' 
#' Compute the fraction of months with at least one no-flow day during 1981-2019 for European 
#' and South American reaches for the model variants that were trained with only non-perennial 
#' European gauging stations (VII; first row), and only non-perennial South American gauging 
#' stations (VIII; second row).
#'
#' @param reaches_SI_status_dir the path to streamflow intermittence status simulated by the
#' model variants, here the model variant VII and VIII
#' @param reaches_eu_dir the path to the European gauging reaches shapefile
#' @param reaches_sa_dir the path to the South American gauging reaches shapefile
#' @param out_dir the path to the output shapefiles for creating figure 6
#' 
#' @return NULL
#'
create_fig6 <- function(reaches_SI_status_dir, reaches_eu_dir, reaches_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the reaches for EU and SA
  reaches_shp_eu <- sf::read_sf(reaches_eu_dir)
  reaches_shp_sa <- sf::read_sf(reaches_sa_dir)
  
  ## read the predicted SI status at the reaches in EU and SA for the three model variants
  pre_eu_model_VII <- fst::read_fst(file.path(reaches_SI_status_dir,
                                              'res_nets_mat_dt_eu_only_np_gstat_on_eu.fst')) %>% as.data.table()
  pre_sa_model_VII <- fst::read_fst(file.path(reaches_SI_status_dir,
                                              'res_nets_mat_dt_eu_only_np_gstat_on_sa.fst')) %>% as.data.table()
  pre_eu_model_VIII <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'res_nets_mat_dt_sa_only_np_gstat_on_eu.fst')) %>% as.data.table()
  pre_sa_model_VIII <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'res_nets_mat_dt_sa_only_np_gstat_on_sa.fst')) %>% as.data.table()
  ## Model variant VII
  d <- rowSums(pre_eu_model_VII[,-1] > 0)/468 * 100
  final_eu <- data.table(DRYVER_RIV = pre_eu_model_VII$V1, frac=d)
  left_join(reaches_shp_eu, final_eu, by='DRYVER_RIV') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_VII_on_eu_fig6a.shp'))
  
  d_sa <- rowSums(pre_sa_model_VII[,-1] > 0)/468 * 100
  final_sa <- data.table(HYRIV_ID = pre_sa_model_VII$V1, frac=d_sa)
  left_join(reaches_shp_sa[,c('HYRIV_ID','UPLAND_SKM')], final_sa, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_VII_on_sa_fig6b.shp'))
  
  ## model variant VIII
  d <- rowSums(pre_eu_model_VIII[,-1] > 0)/468 * 100
  final_eu <- data.table(DRYVER_RIV = pre_eu_model_VIII$V1, frac=d)
  left_join(reaches_shp_eu, final_eu, by='DRYVER_RIV') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_VIII_on_eu_fig6c.shp'))
  
  d_sa <- rowSums(pre_sa_model_VIII[,-1] > 0)/468 * 100
  final_sa <- data.table(HYRIV_ID = pre_sa_model_VIII$V1, frac=d_sa)
  left_join(reaches_shp_sa[,c('HYRIV_ID','UPLAND_SKM')], final_sa, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'np_frac_reaches_model_VIII_on_sa_fig6d.shp'))
  
  
  return(NULL)
}

## create figure S1 ----
#' Create Figure S1 
#' 
#' Compute the ratio of predicted to observed non-perennial months at the European and South American 
#' gauging stations is shown for the model variant IV (first row), which was trained using all European 
#' gauging stations and a subset of South American stations selected based on the station density 
#' in Africa. Additionally, this ratio is presented for two other model variants: variant V, 
#' trained using only the South American subset from variant IV, and variant VI, trained using a 
#' different random subset of South American stations that excludes those used in variant V.  
#' 
#' @param in_eu_val_dir the path to observed European station-months in validation dataset
#' @param in_sa_val_dir the path to observed South American station-months in validation dataset
#' @param model_iv_dir the path to model variant IV RF object, presenting European data plus 210
#' of South American station-months
#' @param model_v_dir the path to model variant V RF object, presenting the model that trained with
#' 210 of South American station-months similar to model variant IV
#' @param model_vi_dir the path to model variant I RF object, presenting the model that trained with
#' 210 of South American station-months, selected randomly
#' @param gauge_eu_dir the path to the European gauging stations shapefile
#' @param gauge_sa_dir the path to the South American gauging stations shapefile
#' @param out_dir the path to the output shapefiles for creating figure S1
#' 
#' @return NULL
#' 
create_figS1 <- function(in_eu_val_dir, in_sa_val_dir, model_iv_dir,
                         model_v_dir, model_vi_dir, gauge_eu_dir, gauge_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the validation datasets
  val_data_eu <- qs::qread(in_eu_val_dir)
  val_data_sa <- qs::qread(in_sa_val_dir)
  
  val_data_eu_editted <- val_data_eu[,-c('target', 'gaugeid', 'dates', 'X', 'Y')]
  val_data_sa_editted <- val_data_sa[,-c('target', 'gauge_id')]
  
  ##read the shapefiles
  eu_gauge <- sf::read_sf(gauge_eu_dir)
  eu_gauge <- eu_gauge %>% 
    rename(., gaugeid=gauge_d) %>% 
    dplyr::select(c('gaugeid', 'upa', 'gauge_name', 'DRYVER_RIV'))
  sa_gauge <- sf::read_sf(gauge_sa_dir)
  
  ## inner function to apply the RF model on the validation datasets
  apply_on_val_datasets <- function(in_dt, in_model){
    
    execute_models_nets <- function(model_step1,
                                    data_dt, threshold = 0.5){
      
      res_step1 <- model_step1$predict_newdata(data_dt)
      res_step1 <- res_step1$set_threshold(threshold) %>% 
        as.data.table()
      
      out <- list(res_step1 = res_step1)
      return(out)
    }
    
    out <- execute_models_nets(in_model,
                               data_dt=in_dt)[[1]][,as.numeric(as.character(response))]
    return(out)
  }
  
  ## Model variant IV on EU and SA datasets
  model_step1_iv <- qs::qread(model_iv_dir)
  eu_gstat_eu <- apply_on_val_datasets(in_dt = val_data_eu_editted,
                                       in_model = model_step1_iv)
  eu_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                       in_model = model_step1_iv)
  ## Model V on EU and SA datasets
  model_step1_v <- qs::qread(model_v_dir)
  sa_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                       in_model = model_step1_v)
  ## Model VI on EU and SA datasets
  model_step1_vi <- qs::qread(model_vi_dir)
  eusa_all_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                             in_model = model_step1_vi)
  
  ## combine the predicted SI status by each model variant into a table
  dt_eu <- data.table(gaugeid=val_data_eu$gaugeid, target=val_data_eu$target, var_IV=eu_gstat_eu)
  dt_sa <- data.table(gauge_id=val_data_sa$gauge_id,target=val_data_sa$target, var_IV=eu_gstat_sa,
                      var_V=sa_gstat_sa, var_VI=eusa_all_gstat_sa)
  
  ## compute the ratio of the number of predicted NP months to the number of observed
  dt_eu_sta <- dt_eu[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gaugeid', .SDcols = -2]
  dt_sa_sta <- dt_sa[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gauge_id', .SDcols = -2]
  
  sa_gauge <- sa_gauge %>% 
    filter(gauge_id %in% dt_sa_sta$gauge_id)
  # Replace NaN with 400  and Inf with 4000
  for (col in names(dt_eu_sta)) {
    set(dt_eu_sta, i = which(is.nan(dt_eu_sta[[col]])), j = col, value = 400)
    set(dt_eu_sta, i = which(is.infinite(dt_eu_sta[[col]])), j = col, value = 4000)
  }
  
  for (col in names(dt_sa_sta)) {
    set(dt_sa_sta, i = which(is.nan(dt_sa_sta[[col]])), j = col, value = 400)
    set(dt_sa_sta, i = which(is.infinite(dt_sa_sta[[col]])), j = col, value = 4000)
  }
  
  ## write the results into two shapefiles for each continent
  left_join(eu_gauge[,c("gaugeid", "upa")], dt_eu_sta,  by='gaugeid') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_eu_stations_figS1a.shp'))
  left_join(sa_gauge[,c("gauge_id", "upa")], dt_sa_sta,  by='gauge_id') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_sa_stations_figS1bcd.shp'))
  
  return(NULL)
}
## create figure S2 --------
#' Create Figure S2 
#' 
#' Compute the ratio of predicted to observed non-perennial months at the European and South American 
#' gauging stations is shown for the model variant IV (first row), which was trained using all European 
#' gauging stations and a subset of South American stations selected based on the station density 
#' in Africa. Additionally, this ratio is presented for two other model variants: variant V, 
#' trained using only the South American subset from variant IV, and variant VI, trained using a 
#' different random subset of South American stations that excludes those used in variant V.  
#' 
#' @param in_eu_val_dir the path to observed European station-months in validation dataset
#' @param in_sa_val_dir the path to observed South American station-months in validation dataset
#' @param model_vii_dir the path to model variant VII RF object, presenting the model that trained with
#' only non-perennial European gauging stations 
#' @param model_viii_dir the path to model variant VIII RF object, presenting the model that trained with
#' only non-perennial South American gauging stations
#' @param gauge_eu_dir the path to the European gauging stations shapefile
#' @param gauge_sa_dir the path to the South American gauging stations shapefile
#' @param out_dir the path to the output shapefiles for creating figure S2
#' 
#' @return NULL
#' 
create_figS2 <- function(in_eu_val_dir, in_sa_val_dir, model_vii_dir,
                         model_viii_dir, gauge_eu_dir, gauge_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the validation datasets
  val_data_eu <- qs::qread(in_eu_val_dir)
  val_data_sa <- qs::qread(in_sa_val_dir)
  
  val_data_eu_editted <- val_data_eu[,-c('target', 'gaugeid', 'dates', 'X', 'Y')]
  val_data_sa_editted <- val_data_sa[,-c('target', 'gauge_id')]
  
  ##read the shapefiles
  eu_gauge <- sf::read_sf(gauge_eu_dir)
  eu_gauge <- eu_gauge %>% 
    rename(., gaugeid=gauge_d) %>% 
    dplyr::select(c('gaugeid', 'upa', 'gauge_name', 'DRYVER_RIV'))
  sa_gauge <- sf::read_sf(gauge_sa_dir)
  
  ## inner function to apply the RF model on the validation datasets
  apply_on_val_datasets <- function(in_dt, in_model){
    
    execute_models_nets <- function(model_step1,
                                    data_dt, threshold = 0.5){
      
      res_step1 <- model_step1$predict_newdata(data_dt)
      res_step1 <- res_step1$set_threshold(threshold) %>% 
        as.data.table()
      
      out <- list(res_step1 = res_step1)
      return(out)
    }
    
    out <- execute_models_nets(in_model,
                               data_dt=in_dt)[[1]][,as.numeric(as.character(response))]
    return(out)
  }
  
  ## Model variant VII on EU and SA datasets
  model_step1_vii <- qs::qread(model_vii_dir)
  eu_gstat_eu <- apply_on_val_datasets(in_dt = val_data_eu_editted,
                                       in_model = model_step1_vii)
  eu_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                       in_model = model_step1_vii)
  ## Model VIII on EU and SA datasets
  model_step1_viii <- qs::qread(model_viii_dir)
  sa_gstat_eu <- apply_on_val_datasets(in_dt = val_data_eu_editted,
                                       in_model = model_step1_viii)
  sa_gstat_sa <- apply_on_val_datasets(in_dt = val_data_sa_editted,
                                       in_model = model_step1_viii)
  
  ## combine the predicted SI status by each model variant into a table
  dt_eu <- data.table(gaugeid=val_data_eu$gaugeid, target=val_data_eu$target, var_VII=eu_gstat_eu,
                      var_VIII=sa_gstat_eu)
  dt_sa <- data.table(gauge_id=val_data_sa$gauge_id,target=val_data_sa$target, var_VII=eu_gstat_sa,
                      var_VIII=sa_gstat_sa)
  
  ## compute the ratio of the number of predicted NP months to the number of observed
  dt_eu_sta <- dt_eu[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gaugeid', .SDcols = -2]
  dt_sa_sta <- dt_sa[,lapply(.SD, function(x) sum(x>0)/sum(target>0)), by='gauge_id', .SDcols = -2]
  
  sa_gauge <- sa_gauge %>% 
    filter(gauge_id %in% dt_sa_sta$gauge_id)
  # Replace NaN with 400  and Inf with 4000
  for (col in names(dt_eu_sta)) {
    set(dt_eu_sta, i = which(is.nan(dt_eu_sta[[col]])), j = col, value = 400)
    set(dt_eu_sta, i = which(is.infinite(dt_eu_sta[[col]])), j = col, value = 4000)
  }
  
  for (col in names(dt_sa_sta)) {
    set(dt_sa_sta, i = which(is.nan(dt_sa_sta[[col]])), j = col, value = 400)
    set(dt_sa_sta, i = which(is.infinite(dt_sa_sta[[col]])), j = col, value = 4000)
  }
  
  ## write the results into two shapefiles for each continent
  left_join(eu_gauge[,c("gaugeid", "upa")], dt_eu_sta,  by='gaugeid') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_eu_stations_figS2ac.shp'))
  left_join(sa_gauge[,c("gauge_id", "upa")], dt_sa_sta,  by='gauge_id') %>% 
    sf::write_sf(., file.path(out_dir, 'ratio_pre_over_obs_sa_stations_figS2bd.shp'))
  
  return(NULL)
}

## create figure S7 ----
#' Create Figure S7 
#' 
#' Compute the median probabilities for estimating perennial status in South American 
#' reaches for four model variants during 1981-2019. 
#'
#' @param reaches_SI_status_dir the path to streamflow intermittence status simulated by the
#' model variants, here the model variant IV, V, IIIa, and IIIb
#' @param reaches_eu_dir the path to the European gauging reaches shapefile
#' @param reaches_sa_dir the path to the South American gauging reaches shapefile
#' @param out_dir the path to the output shapefiles for creating figure S7
#' 
#' @return NULL
#'
create_figS7 <- function(reaches_SI_status_dir, reaches_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the reaches for EU and SA
  reaches_shp_sa <- sf::read_sf(reaches_sa_dir)
  
  ## read the predicted SI status at the reaches in SA for the three model variants
  pre_sa_model_IV <- fst::read_fst(file.path(reaches_SI_status_dir,
                                             'results_nets_prob_eu_plus_single_sa_on_sa.fst')) %>% as.data.table()
  pre_sa_model_V <- fst::read_fst(file.path(reaches_SI_status_dir,
                                            'results_nets_prob_sa_fixed_210_gstat_on_sa.fst')) %>% as.data.table()
  pre_sa_model_IIIa <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'results_nets_prob_eusa_all_gstat_on_sa.fst')) %>% as.data.table()
  pre_sa_model_IIIb <- fst::read_fst(file.path(reaches_SI_status_dir,
                                               'results_nets_prob_eusa_doell_hyperparam_on_sa.fst')) %>% as.data.table()
  ## compute the median
  pre_sa_model_IV[,prob_median := apply(.SD, 1, median), .SDcols=-1]
  pre_sa_model_V[,prob_median := apply(.SD, 1, median), .SDcols=-1]
  pre_sa_model_IIIa[,prob_median := apply(.SD, 1, median), .SDcols=-1]
  pre_sa_model_IIIb[,prob_median := apply(.SD, 1, median), .SDcols=-1]
  
  dtt_IV <- data.table(HYRIV_ID=pre_sa_model_IV[,V1], prob_median=pre_sa_model_IV[,prob_median])
  dtt_V <- data.table(HYRIV_ID=pre_sa_model_V[,V1], prob_median=pre_sa_model_V[,prob_median])
  dtt_IIIa <- data.table(HYRIV_ID=pre_sa_model_IIIa[,V1], prob_median=pre_sa_model_IIIa[,prob_median])
  dtt_IIIb <- data.table(HYRIV_ID=pre_sa_model_IIIb[,V1], prob_median=pre_sa_model_IIIb[,prob_median])
  
  left_join(reaches_shp_sa[,c('HYRIV_ID')], dtt_IV, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'prob_median_on_sa_figS7a.shp'))
  left_join(reaches_shp_sa[,c('HYRIV_ID')], dtt_V, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'prob_median_on_sa_figS7b.shp'))
  left_join(reaches_shp_sa[,c('HYRIV_ID')], dtt_IIIa, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'prob_median_on_sa_figS7c.shp'))
  left_join(reaches_shp_sa[,c('HYRIV_ID')], dtt_IIIb, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'prob_median_on_sa_figS7d.shp'))
  
  return(NULL)
}

## create figure S8 ----
#' Create Figure S8 
#' 
#' Compute the difference in the median probabilities for estimating perennial status at the 
#' South American reaches between the model variants IV and V 
#' 
#' @param reaches_SI_status_dir the path to streamflow intermittence status simulated by the
#' model variants, here the model variants IV, and V.
#' @param reaches_eu_dir the path to the European gauging reaches shapefile
#' @param reaches_sa_dir the path to the South American gauging reaches shapefile
#' @param out_dir the path to the output shapefiles for creating figure S8
#' 
#' @return NULL
#'
create_figS8 <- function(reaches_SI_status_dir, reaches_sa_dir, out_dir){
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  ## read the reaches for EU and SA
  reaches_shp_sa <- sf::read_sf(reaches_sa_dir)
  
  ## read the predicted SI status at the reaches in SA for the three model variants
  pre_sa_model_IV <- fst::read_fst(file.path(reaches_SI_status_dir,
                                             'results_nets_prob_eu_plus_single_sa_on_sa.fst')) %>% as.data.table()
  pre_sa_model_V <- fst::read_fst(file.path(reaches_SI_status_dir,
                                            'results_nets_prob_sa_fixed_210_gstat_on_sa.fst')) %>% as.data.table()
  ## compute the median of differences between the two variants in SA reaches
  sa_diff_prob <- data.table(HYRIV_ID=pre_sa_model_IV[,V1],
                             pre_sa_model_IV[,-1] - pre_sa_model_V[,-1]) 
  
  sa_diff_prob_med <- data.table(HYRIV_ID=pre_sa_model_IV[,V1],
                                 sa_diff_prob[,apply(.SD, 1, median), .SDcols=-1])
  
  
  left_join(reaches_shp_sa[,c('HYRIV_ID','UPLAND_SKM')], sa_diff_prob_med, by='HYRIV_ID') %>% 
    sf::write_sf(., file.path(out_dir, 'prob_median_diff_on_sa_figS8.shp'))
  
  return(NULL)
}

