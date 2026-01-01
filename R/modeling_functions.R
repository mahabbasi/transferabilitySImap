

#### -------------------- Develope RF model Step ONE ----------------
## classify 
classify_target <- function(tbl,target_col, breaks, labels){
  target_class <- tbl %>%
    pull(target_col) %>%
    cut(breaks = breaks,
        labels = labels)
  return(target_class)
}

## Combine the Eropean and South American observation data -----
combine_eusa_data <- function(root_dir, st_density=210, scenario='single'){
  
  eu_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_eu.qs'))
  sa_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_sa.qs'))
  
  eu_data[, dates :=NULL][, X :=NULL][, Y :=NULL]
  data.table::setnames(eu_data, 'gaugeid', 'gauge_id')
  cols_to_order <- names(eu_data)
  sa_data <- sa_data[,..cols_to_order]
  
  gauge_id <- sa_data[, unique(gauge_id)]
  
  if (scenario == 'single') {
    st_density <- st_density
  } else if (scenario == 'double') {
    st_density <- st_density * 2
  } else {
    st_density <- st_density * 3
  }
  
  set.seed(20250208)
  selected_rows <- sample(length(gauge_id), st_density)
  selected_stations <- gauge_id[selected_rows]
  
  out <- rbind(eu_data, sa_data[gauge_id %in% selected_stations])
  
  return(out)
}

# creating task for different problem type ---
create_task_stepone <- function(in_tbl, problem_type = "classif", ...){
  colnames(in_tbl) <- make.names(names(in_tbl),
                                 unique = TRUE)
  
  in_tbl = in_tbl[target > 0, target:=1]
  
  if (!inherits(in_tbl[,target], "factor")) {
    in_tbl[, target := as.factor(target)]
  }
  
  if (problem_type == "regr"){
    task <- TaskRegr$new(
      id = "regression_no_flow_days",
      backend = in_tbl,
      target = target_name
    )
  }else if (problem_type == "classif") {
    
    task <- TaskClassif$new(
      id = "binary_class",
      backend = in_tbl,
      target = "target")
    
  } else {
    stop("the problem type have to be one of regr and classif types,
         please check the problem_type argument.\n")
  }
  
  return(task)
}

#------ get_oversamp_ratio -----------------
#' Get oversample ratio
#'
#' Identify minority class and compute ratio between the number of observations
#' in the majority and the minority classes of a binary
#' \link[mlr3]{TaskClassif }.
#'
#' @param in_task binary \link[mlr3]{TaskClassif }
#'
#' @return named list with following items:
#' \describe{
#' \item{minoclass} - Value of minority class (e.g. '1' for intermittent rivers)
#' \item{ratio} - numeric ratio between number of items in majority and minority
#' class
#' }
#'
#' @examples
#' \dontrun{
#' in_dt <- data.table(intermittent=c(rep(0, 300), rep(1, 300)))
#' task = mlr3::TaskClassif$new(id = "in_dt", backend = in_dt, target ='intermittent')
#' get_oversamp_ratio(task)
#' }
#'
#' @export
get_oversamp_ratio <- function(in_task) {
  return(
    in_task$data()[, .N, by=get(in_task$target_names)] %>%
      setorder(N) %>%
      .[, list(minoclass=get[1], ratio=N[2]/N[1])]
  )
}

## create baselearners -----
create_baselearners <- function(in_task, ncores = parallel::detectCores()-3){
  lrns <- list()
  
  if (is.list(in_task)) {
    in_task <- in_task[[1]]
  }
  if (inherits(in_task, "TaskClassif")) {
    
    #Compute ratio of intermittent to perennial observations
    imbalance_ratio <- get_oversamp_ratio(in_task)$ratio
    
    lrns[['lrn_ranger']] <- mlr3::lrn('classif.ranger',
                                      num.trees = 800,
                                      sample.fraction = 0.632,
                                      replace = FALSE,
                                      splitrule = 'gini',
                                      predict_type = 'prob',
                                      importance = 'impurity_corrected',
                                      respect.unordered.factors = 'order', seed=42)
    
    set_threads(lrns[["lrn_ranger"]], n = ncores)
    
    po_oversampled <- mlr3pipelines::po("classbalancing", id = "oversampled", adjust = "minor",
                                        reference = "minor", shuffle = TRUE,
                                        ratio = imbalance_ratio)
    
    #Create mlr3 pipe operator to put a higher class weight on minority class
    # po_classweights <- mlr3pipelines::po("classweights", minor_weight = imbalance_ratio)
    #Create graph learners so that oversampling happens systematically upstream of all training
    lrns[['lrn_ranger_oversampled']] <- mlr3pipelines::GraphLearner$new(
      po_oversampled %>>% lrns[['lrn_ranger']])
    # lrns[['lrn_ranger_classweights']] <- mlr3pipelines::GraphLearner$new(
    #   po_classweights %>>% lrns[['lrn_ranger']])
  }
  lrns_out <- lrns[['lrn_ranger_oversampled']]
  return(lrns_out)
}

## set tuning parameters ------
set_tuning <- function(in_learner, in_measure, nfeatures,
                       insamp_nfolds, insamp_neval, insamp_nbatch) {
  
  if (is.list(in_learner)) {
    in_learner <- in_learner[[1]]
  }
  
  #Define paramet space to explore
  regex_tuneset <- function(in_learner) {
    prmset <- names(in_learner$param_set$tags)
    tune_rf <- ParamSet$new(list(
      ParamInt$new(grep(".*mtry", prmset, value=T)[1],
                   lower = floor(0.2*nfeatures),
                   upper = floor(0.8*nfeatures)), #80%  of features
      ParamDbl$new(grep(".*fraction", prmset, value=T),
                   lower = 0.2,
                   upper = 0.8)
    ))
    
    in_split =in_learner$param_set$get_values()[
      grep(".*split(rule|stat)", prmset, value=T)]
    
    if (in_split == 'maxstat') {
      tune_rf$add(
        ParamDbl$new(prmset[grep(".*alpha", prmset)],
                     lower = 0.01, upper = 0.1)
      )
      
    } else if (any(grepl(".*min.node.size", prmset))) {
      tune_rf$add(
        ParamInt$new(prmset[grep(".*min.node.size", prmset)],
                     lower = 1, upper = 10)
      )
    } else if (any(grepl(".*splitstat", prmset))) {
      tune_rf$add(
        ParamDbl$new(prmset[grep(".*alpha", prmset)],
                     lower = 0.01, upper = 0.1)
      )
    }
  }
  
  #Define inner resampling strategy
  rcv_rf = rsmp("cv", folds=insamp_nfolds) #aspatial CV repeated 10 times
  
  #Define termination rule
  evalsn = mlr3tuning::trm("evals", n_evals = insamp_neval) #termine tuning after insamp_neval rounds
  
  if (in_learner$task_type == 'classif') {
    if (inherits(in_measure, 'list')) {
      in_measure <- in_measure$classif
    }
    
    if (grepl('classif[.]cforest$', in_learner$id)) {
      learnertune <- in_learner
    } else if (grepl('classif[.]ranger$', in_learner$id)) {
      learnertune <- AutoTuner$new(learner= in_learner,
                                   resampling = rcv_rf,
                                   measure = in_measure,
                                   search_space = regex_tuneset(in_learner),
                                   terminator = evalsn,
                                   tuner =  tnr("random_search",
                                                batch_size = insamp_nbatch)) #batch_size determines level of parallelism
    } else{
      stop('The classification learner provided is not configurable with this workflow yet...')
    }
  } else if (in_learner$task_type == 'regr') {
    if (inherits(in_measure, 'list')) {
      in_measure <- in_measure$regr
    }
    
    learnertune <- AutoTuner$new(learner= in_learner,
                                 resampling = rcv_rf,
                                 measure = in_measure,
                                 search_space = regex_tuneset(in_learner),
                                 terminator = evalsn,
                                 tuner =  tnr("random_search",
                                              batch_size = insamp_nbatch))
  }
  
  #learnertune$store_tuning_instance = FALSE
  learnertune$id <- in_learner$id
  
  return(learnertune)
}

## set_cvresampling -----
set_cvresampling <- function(rsmp_id, in_task, outsamp_nrep, outsamp_nfolds) {
  #repeated_cv or repeated-spcv-coords
  outer_resampling = rsmp(rsmp_id,
                          repeats = outsamp_nrep,
                          folds = outsamp_nfolds)
  outer_resampling$instantiate(in_task)
  
  return(outer_resampling)
}

# dynamic_resample--------
# subsampling the data for dealing with imbalanced data
dynamic_resample <- function(in_task, in_learner, in_resampling, type,
                             store_models = FALSE) {
  if (is.list(in_learner)) {
    in_learner <- in_learner[[1]]
  }
  
  if (is.list(in_task)) {
    in_task <- in_task[[1]]
  }
  
  if (inherits(in_learner, 'BenchmarkResult')) {
    print(('BenchmarkResults was provided, getting the learner...'))
    in_learner <- in_learner$learners$learner[[1]]
  }
  
  #Make sure autotuner matches task (adjust mtry)
  if (inherits(in_learner, 'AutoTuner')) {
    in_learner <- reset_tuning(in_autotuner = in_learner,
                               in_task = in_task)
  }
  
  if ((in_learner$task_type == 'classif' & type=='classif') |
      (in_learner$task_type == 'regr' & type=='regr')) {
    resmp_rs <- mlr3::resample(learner = in_learner, task = in_task,
                               resampling = in_resampling, store_models = store_models
    )
    return(resmp_rs)
  }
}
# weighted_sd -----
weighted_sd <- function(x, w=NULL, na.rm=FALSE) {
  if (na.rm) {
    x <-  na.omit(x)
    if (length(w) > length(x)) {
      w <- w[-which(is.na(x))]
    }
  }
  
  if (length(w)==0) {
    w <- rep(1, length(x))
  }
  
  #Compute weighted standard deviation
  return(sqrt(sum((w) * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)))
}

# combine_bm -----------
combine_bm <- function(in_resampleresults, write_qs = NULL, inp_resdir = NULL) {
  #When tried as_benchmark_result.ResampleResult, got "Error in setcolorder(data, slots) :
  # x has some duplicated column name(s): uhash. Please remove or rename the
  # duplicate(s) and try again.". SO use this instead
  if (!file.exists(inp_resdir)) {
    dir.create(inp_resdir, recursive = TRUE)
  }
  
  # print('Converting to benchmark results...')
  # if (length(in_resampleresults) > 1) {
  #   bmres_list <- lapply(
  #     in_resampleresults[!sapply(in_resampleresults, is.null)],
  #     function(rsmpres) {
  #       print(rsmpres)
  #       if (!is.null(rsmpres)) {
  #         as_benchmark_result(rsmpres)
  #       }
  #     })
  #   #BenchmarkResult$new(rsmpres$data)})
  #   
  #   print('Combining...')
  #   bmrbase = bmres_list[[1]]
  #   for (i in 2:length(bmres_list)) {
  #     if (in_resampleresults[[i]]$task$task_type ==
  #         in_resampleresults[[1]]$task$task_type) {
  #       print(i)
  #       bmrbase$combine(bmres_list[[i]])
  #     } else {
  #       warning('ResampleResult #', i,
  #               'is not of the same task type as the first ResampleResult you provided, skipping...')
  #     }
  #   }
  # } 
  # else {
  #   warning('You provided only one resample result to combine_bm,
  #           simply returning output from as_benchmark_result...')
  #   bmrbase = as_benchmark_result(in_resampleresults)
  # }
  
  bmrbase = as_benchmark_result(in_resampleresults)
  print('Done combining, now writing to qs...')
  if (write_qs) {
    out_filen <- paste0('combine_bm', format(Sys.time(), '%Y%m%d%H%M%s'), '.qs')
    out_qs <- file.path(inp_resdir, out_filen)
  }
  
  qs::qsave(bmrbase, out_qs)
  
  return(out_filen)
}
# select_features ------
select_features <- function(in_bm, in_lrnid, in_task, pcutoff, inp_resdir = NULL) {
  
  #If path, read qs
  if (inherits(in_bm, "character")) {
    in_bm <- qs::qread(file.path(inp_resdir, in_bm))
  }
  
  #get desired resampled_results/learner
  if (inherits(in_bm, "BenchmarkResult")) {
    in_rf <- in_bm$filter(learner_ids = in_lrnid)
  } else {
    in_rf <- as_benchmark_result(in_bm)
  }
  
  #Apply feature/variable selection
  vimp <- weighted_vimportance_nestedrf(
    rfresamp = in_rf$resample_result(uhash=unique(as.data.table(in_rf)$uhash)),
    pvalue = TRUE) %>%
    .[,imp_wmeanper := imp_wmean/sum(imp_wmean)]
  
  task_featsel <- in_task$clone()$select(
    vimp[imp_pvalue <= pcutoff, as.character(varnames)])
  task_featsel$id <- paste0(in_task$id, '_featsel')
  
  return(list(in_task, task_featsel))
}
# weighted_vimportance_nestedrf ----------
weighted_vimportance_nestedrf <- function(rfresamp,
                                          pvalue = TRUE, pvalue_permutn = 50) {
  varnames <- rfresamp$task$feature_names
  rfresampdt <- as.data.table(rfresamp)
  
  vimportance_all <- rfresampdt[, extract_impperf_nestedrf(
    in_rflearner = learner, #Extract vimp and perf for each resampling instance
    in_task = task,
    imp=T, perf=T, pvalue=pvalue, pvalue_permutn), by=iteration] %>%
    cbind(., varnames)
  
  ####!!!!!!!!!!Adapt to allow for other measure than classif.bacc!!!!!!!!######
  out_vimportance <- vimportance_all[
    , list(imp_wmean = weighted.mean(importance, classif.bacc), #Compute weighted mean
           imp_wsd =  weighted_sd(x=importance, w=classif.bacc)), #Compute weighted sd
    by=varnames]
  
  if (pvalue) {
    out_vimportance <- cbind(
      out_vimportance,
      vimportance_all[,
                      list(imp_pvalue = weighted.mean(pvalue, classif.bacc)), #Compute weighted mean of pvalue
                      by=varnames][, !'varnames']
    )
  }
  
  return(out_vimportance)
}

# extract_impperf_nestedrf -----
extract_impperf_nestedrf <- function(in_rflearner, in_task,
                                     imp = TRUE, perf = TRUE,
                                     pvalue = TRUE, pvalue_permutn = 50) {
  
  in_task <- in_task[[1]]
  in_rflearner <- in_rflearner[[1]]
  
  if (inherits(in_rflearner, "AutoTuner")) {
    sublrn <- in_rflearner$model$learner
  } else {
    sublrn <- in_rflearner
  }
  
  print(paste0("Computing variable importance for resampling instance hash #",
               sublrn$hash))
  
  outobj <- cbind(
    if (imp) {
      ####################### IF GraphLearner ####################################
      if (inherits(sublrn, "GraphLearner")) {
        
        if ('classif.ranger' %in% names(sublrn$model)) {
          
          if (pvalue == TRUE) {
            in_formula <- as.formula(paste0(in_task$target_names, '~.'))
            
            importance_pvalues(
              sublrn$model$classif.ranger$model,
              method = "altmann",
              num.permutations = pvalue_permutn,
              data = in_task$data(),
              formula= in_formula
            )
            
          } else {
            data.table(importance=sublrn$model$classif.ranger$model$variable.importance)
          }
        }
        
        else if ('classif.cforest' %in% names(sublrn$model)) {
          if (pvalue == TRUE) {
            warning("p_value calculation is only available for ranger classification rf, ignoring p_value.
                    In addition, default parameters were used in partykit::varimp, adjust as needed.")
          }
          data.table(importance=
                       partykit::varimp(sublrn$model$classif.cforest$model,
                                        nperm = 1,
                                        OOB = TRUE,
                                        risk = "misclassification",
                                        conditional = FALSE,
                                        threshold = .2))
          }
      } else { ####################### IF direct model ####################################
        if (pvalue == TRUE) { #If want pvalue associated with predictor variables
          in_formula <- as.formula(paste0(in_task$target_names, '~.'))
          
          importance_pvalues(
            in_rflearner$model,
            method = "altmann",
            num.permutations = pvalue_permutn,
            data = in_task$data(),
            formula= in_formula
          )
        } else { #If pvalue == FALSE
          data.table(importance= in_rflearner$model$learner$importance())
        }
        
      }
    },
    if (perf) {
      perf_id <- in_rflearner$instance_args$measure$id
      outperf <- in_rflearner$tuning_result[, get(perf_id)]
      data.table(outperf) %>% setnames(perf_id)
    }
  )
  
  return(outobj)
}

# dynamic_resamplebm ----
dynamic_resamplebm <- function(in_task, in_bm, in_lrnid, in_resampling, type,
                               inp_resdir = NULL, store_models = FALSE) {
  #If path, read qs
  if (inherits(in_bm, "character")) {
    in_bm <- qs::qread(file.path(inp_resdir, in_bm))
  }
  
  #get desired resampled_results/learner
  in_rf <- in_bm$filter(learner_ids = in_lrnid)
  
  rsmp_out <- dynamic_resample(in_task = in_task,
                               in_learner = in_rf,
                               in_resampling = in_resampling,
                               type = type,
                               store_models = store_models)
  
  return(rsmp_out)
}
# reset_tuning -----
reset_tuning <- function(in_autotuner, in_task, in_lrnid = NULL) {
  if (inherits(in_autotuner, 'list') & !is.null(in_lrnid)) {
    in_autotuner <- in_autotuner[[
      which(unlist(lapply(in_autotuner, function(lrn) {lrn$id == in_lrnid})))
      ]]
  }
  
  tuneargs_ini <- in_autotuner$instance_args
  
  autotuner_new <- set_tuning(in_learner = tuneargs_ini$learner,
                              in_measure = tuneargs_ini$measure,
                              nfeatures = length(in_task$feature_names),
                              insamp_nfolds= tuneargs_ini$resampling$param_set$values$folds,
                              insamp_neval= tuneargs_ini$terminator$param_set$values$n_evals,
                              insamp_nbatch= in_autotuner$tuner$param_set$values$batch_size
  )
  
  return(autotuner_new)
}

# analyze_benchmark -----
analyze_benchmark <- function(in_bm, in_measure, inp_resdir=NULL) {
  
  #If path, read qs
  if (inherits(in_bm, "character")) {
    in_bm <- qs::qread(file.path(inp_resdir, in_bm))
  }
  
  print(paste('It took',
              in_bm$aggregate(mlr3::msr('time_both'))$time_both,
              'seconds to train and predict with the',
              in_bm$aggregate(msr('time_both'))$learner_id,
              'model...'))
  
  bmdt <- as.data.table(in_bm)
  
  if (in_bm$task_type == 'regr') {
    print(in_bm$aggregate(in_measure$regr))
    boxcomp <- mlr3viz::autoplot(in_bm, measure = in_measure$regr)
    
    preds <- lapply(seq_len(bmdt[,.N]), function(rsmp_i) {
      preds <- bmdt$prediction[[rsmp_i]] %>%
        as.data.table %>%
        .[, `:=`(outf = bmdt$iteration[[rsmp_i]],
                 task = bmdt$task[[rsmp_i]]$id,
                 task_type = in_bm$task_type,
                 learner = bmdt$learner[[rsmp_i]]$id)]
      return(preds)
    }) %>%
      do.call(rbind, .)
    
    if (!('prob.1' %in% names(preds)) & 'response' %in% names(preds)) {
      preds[, prob.1 := response]
    }
  }
  
  
  if (in_bm$task_type == 'classif_st') {
    print(in_bm$aggregate(measures=in_measure$classif))
    boxcomp <- mlr3viz::autoplot(in_bm, measure = in_measure$classif)
    
    preds <- lapply(seq_len(bmdt[,.N]), function(rsmp_i) {
      preds <- data.table(outf = bmdt$iteration[[rsmp_i]],
                          task = bmdt$task[[rsmp_i]]$id,
                          learner = bmdt$learner[[rsmp_i]]$id,
                          task_type = in_bm$task_type,
                          pred = list(bmdt$prediction[[rsmp_i]]))
      return(preds)
    }) %>%
      do.call(rbind, .)
  }
  
  tasklearner_unique <- preds[, expand.grid(unique(task), unique(learner))] %>%
    `colnames<-`(c('task', 'learner')) %>%
    setDT
  
  tasklearner_unique[, learner_format := dplyr::case_when(
    learner == 'classif.ranger'~'default RF',
    learner == 'oversample.classif.ranger'~'default RF - oversampled',
    learner == 'classweights.classif.ranger'~'default RF - weighted classes'
  )]
  
  glist <- lapply(1:nrow(tasklearner_unique), function(tsklrn) {
    print(tasklearner_unique[tsklrn,])
    subpred <- preds[task ==tasklearner_unique$task[tsklrn] &
                       learner == tasklearner_unique$learner[tsklrn],]
    
    ggmisclass_out <- ggmisclass_single(in_predictions = subpred)
    
    gout <- ggmisclass_out$plot +
      ggtitle(paste(tasklearner_unique$task[tsklrn],
                    tasklearner_unique$learner_format[tsklrn])) +
      labs(x='Threshold', y='Value')
    
    if (tsklrn < nrow(tasklearner_unique)) {
      gout <- gout +
        theme(legend.position = 'none')
    }
    
    return(list(plot = ggplotGrob(gout),
                interthres_dt = data.table(
                  learner = as.character(tasklearner_unique$learner[tsklrn]),
                  thresh = ggmisclass_out$interthresh
                )
    )
    )
  }) %>%
    unlist(recursive=F)
  
  
  return(list(
    bm_misclasscomp=do.call("grid.arrange",
                            list(grobs=glist[seq(1, length(glist), 2)])), #Get all plots out of the nested list
    bm_boxcomp = boxcomp,
    interthresh_dt = rbindlist(glist[seq(2, length(glist), 2)]) #Get all threshold data.table rows out of nested list
  ))
}

# get_outerrsmp ---------
get_outerrsmp <- function(in_rftuned, spatial_rsp=FALSE) {
  #Adapt whether return resample result or output from selecttrain_rf
  if (inherits(in_rftuned, 'list')) {
    #If there is more than one type of outer resampling
    if (length(in_rftuned$rf_outer$uhash) > 1) {
      #Check which resampling is spatial — only works if one is spatial
      sp_i <- which(unlist(lapply(in_rftuned$rf_outer, function(x) {
        grepl('.*Resampling.*Sp.*', x$resampling$format())
      })))
      
      #If user request that spatial resampling be used
      if (spatial_rsp==TRUE) {
        
        if (length(sp_i)>0) {
          rsmp_res <- in_rftuned$rf_outer[[min(sp_i)]]
        } else { #But if there is no spatial resampling provided
          stop("spatial_rsp==TRUE but the in_rftuned does not include
               any Spatial Resampling")
        }
        #If user didn't request spatial resampling to be used, grab the first
        #resampling that is not spatial
        } else {
          rsmp_res <- in_rftuned$rf_outer[[
            min((1:length(in_rftuned$rf_outer))[-sp_i])]]
        }
      
      #If there is only one type of outer resampling
    } else {
      print("Only one resampling result, ignoring spatial_rsp argument...")
      rsmp_res <- in_rftuned$rf_outer
    }
    #If in_rftuned is already a ResampleResult, simply return it
  } else if (inherits(in_rftuned, "ResampleResult")) {
    rsmp_res <- in_rftuned
  }
  return(rsmp_res)
}

# ggmisclass_single ---------
ggmisclass_single <- function(in_predictions=NULL, in_rftuned=NULL, spatial_rsp=FALSE) {
  #Get predicted probabilities of intermittency for each gauge
  # in_gaugestats[!is.na(cly_pc_cav), intermittent_predprob :=
  #                 as.data.table(in_predictions)[order(row_id), mean(prob.1), by=row_id]$V1]
  #Get misclassification error, sensitivity, and specificity for different classification thresholds
  #i.e. binary predictive assignment of gauges to either perennial or intermittent class
  
  #If provided resampling results rather than prediction table, extract 
  if (!is.null(in_rftuned)) {
    rsmp_res <- get_outerrsmp(in_rftuned, spatial_rsp=spatial_rsp)
    in_predictions <- rsmp_res$prediction()
  }
  
  #Get confusion matrices for range of thresholds (i.e., probability of flow intermittence
  #above which a watercourse is classified as non-perennial)
  threshold_confu_dt <- ldply(seq(0,1,0.01), threshold_misclass, in_predictions) %>%
    setDT
  
  #Get classification threshold at which sensitivity and specificity are the most similar
  balanced_thresh <- threshold_confu_dt[which.min(abs(spec-sens)),]
  print(paste('Sensitivity =', round(balanced_thresh$sens,2),
              'and Specificity =', round(balanced_thresh$spec,2),
              'at a classification threshold of', balanced_thresh$i))
  
  #Plot trends in confusion matrix metrics with increasing threshold
  gout <- ggplot(melt(threshold_confu_dt, id.vars='i'),
                 aes(x=i, y=value, color=variable, linetype=variable)) +
    geom_line(size=1.2) +
    geom_vline(xintercept=balanced_thresh$i, alpha=1/2) +
    geom_hline(yintercept=balanced_thresh$spec, alpha=1/2) +
    annotate('text', x=(balanced_thresh$i), y=0.4,
             label=balanced_thresh$i, angle=-90) +
    annotate('text', x=0.9, y=(balanced_thresh$spec),
             label=round(balanced_thresh$sens,2)) +
    scale_x_continuous(expand=c(0,0), name='Threshold') +
    scale_y_continuous(expand=c(0,0), name='Value') +
    scale_color_brewer(palette='Dark2',  #colorblind friendly
                       labels=c('Misclassification rate',
                                'Sensitivity (true positives)',
                                'Specificity (true negatives)')) +
    theme_bw()
  
  #Plot it
  return(list(plot = gout,
              interthresh = balanced_thresh$i))
}

# threshold_misclass ------
threshold_misclass <- function(i=0.5, in_preds) {
  #---- Get confusion matrix ----
  if (inherits(in_preds, 'PredictionClassif')) {
    confu <- as.data.table(in_preds$set_threshold(1-i)$confusion) #Get confusion matrix directly
  }
  
  if (is.data.table(in_preds)) {
    #If task associated with predictions is a classification and has records
    if (in_preds[task_type == 'classif_st',.N] > 0) {
      #For each CV repetition:
      #   1. set the probability threshold to compute a confusion matrix to 1-i
      #     (i being the threshold to classify something as 1, set_threshold
      #     being based on prob.0, not prob.1)
      #   2. Compute confusion matrix
      confu <- in_preds[, as.data.table(pred[[1]]$set_threshold(1-i)$confusion),
                        by=outf] %>%
        #Aggregate confusion matrices across repetitions
        .[, .(N=sum(N)), by=.(response, truth)]
    }
    
    #If task associated with predictions is a regression and has records
    if (in_preds[task_type == 'regr', .N] > 0) {
      #Reclassify continuous predictions into binary response across all records
      confu <- in_preds[, response := fifelse(prob.1>=i, '1', '0')] %>%
        .[, truth := as.character(truth)] %>%
        #Create aggregate confusion matrix
        .[, .N, by=.(response, truth)]
    }
  }
  
  #---- Compute statistics based on confusion matrix and format into data.table----
  outvec <- data.table(
    i,
    misclas = confu[truth != response, sum(N)] / confu[, sum(N)],
    sens = confu[truth == '1' & response == '1', N] / confu[truth=='1', sum(N)],
    spec  = confu[truth=='0' & response==0, N]/confu[truth=='0', sum(N)]
  )
  return(outvec)
}

# selecttrain_rf --------
selecttrain_rf <- function(in_rf, in_learnerid=NULL, in_task = NULL,
                           insamp_nfolds =  NULL, insamp_nevals = NULL) {
  
  outlist <- list()
  
  ######### Prepare autotuner for full training ####################
  # If a ResampleResult was provided
  if (inherits(in_rf, 'ResampleResult')) {
    in_bmsel <- in_rf$clone()
    iter_selected <- in_bmsel$score() %>% 
      as.data.table() %>%
      .[order(classif.ce)] %>%
      .[1,iteration]
    lrn_autotuner <- in_bmsel$learners[[iter_selected]]
    in_task <- in_bmsel$task
    outlist[['rf_outer']] <- in_rf
    
    # If a BenchmarkResult was provided
  } else if (inherits(in_rf, 'BenchmarkResult')) {
    in_bmsel <- in_rf$clone()$filter(learner_ids = in_learnerid,
                                     task_id = in_task)
    
    lrn_autotuner <- in_bmsel$clone()$learners$learner[[1]]
    in_task <-in_bmsel$tasks$task[[1]]
    
    #Return outer sampling object for selected model (or list of outer sampling objects)
    uhashes <- unique(as.data.table(in_bmsel)$uhash)
    if (length(uhashes) == 1) {
      outlist[['rf_outer']] <- in_bmsel$resample_result(uhash=uhashes)
    } else {
      outlist[['rf_outer']] <- lapply(uhashes, function(x) {
        in_bmsel$resample_result(uhash=x)
      })
    }
  } else if (inherits(in_rf, 'AutoTuner')) {
    lrn_autotuner <- in_rf
  }
  
  if (!is.null(insamp_nfolds)) {
    lrn_autotuner$instance_args$resampling$param_set$values$folds <- insamp_nfolds
  }
  
  if (!is.null(insamp_nevals)) {
    lrn_autotuner$instance_args$terminator$param_set$values$n_evals <- insamp_nevals
  }
  
  ######### Train it ####################
  # lrn_autotuner$model$learner$param_set$values = mlr3misc::insert_named(
  #   lrn_autotuner$model$learner$param_set$values,
  #   list(classif.ranger.importance = 'permutation')
  # )
  lrn_autotuner$train(in_task)
  
  outlist[['task']] <- in_task
  outlist[['rf_inner']] <- lrn_autotuner
  
  return(outlist)
}

# Create a data table of predictors' name and category
predname_df <- function(in_task, in_task_all = NULL, feat_name_vec = NULL, category = NULL){
  
  if (inherits(in_task, "Task")) {
    feat_name <- in_task$feature_names
    if (length(list(in_task_all)) == 1) {
      feat_name_all <- in_task_all$feature_names
    } else {
      feat_name_all <- in_task_all[[1]]$feature_names
    }
    
  } else
    feat_name <- feat_name_vec
  
  feat_name_abb <- c('Q', 'P_to_PET_ratio', 'Q_iav_cv', 'dor', 'drainage_area', 'glacier_frac',
                     'gwr_to_runoff_ratio', 'irri_frac_im', 'irri_frac', 'karst_frac', 'karst_status', 'land_cover',
                     'lake_frac', 'Q_mean_p12', 'Q_mean_p3', 'Q_min_p12', 'Q_min_p3', 'pot_nat_vegetation',
                     'pop_dens_im', 'pop_dens', 'Q_iav_sd', 'slope', 'wet_days')
  if (is.null(category)) {
    cat <- c("Hydrology", "Climate", "Hydrology", "Anthropogenic", "Physiography", "Landcover", "Hydrology",
             "Anthropogenic", "Anthropogenic", "Geology", "Geology",
             "Landcover", "Lakes",  "Hydrology", "Hydrology", "Hydrology", "Hydrology",
             "Landcover", "Anthropogenic", "Anthropogenic", "Hydrology", "Physiography", "Climate")
  } else
    cat <- category
  
  predvars <- cbind(feat_name, feat_name_abb, cat) %>%
    `colnames<-`(c("varname", 'abbrevation',"Category")) %>% 
    as.data.table()
  
  predvars_out <- predvars[varname %in% feat_name]
  
  return(predvars_out)
  
}

# extract_pd_nestedrf -----------
extract_pd_nestedrf <- function(learner_id=1, in_rftuned, datdf,
                                selcols, nvariate, ngrid=20) {
  in_mod <- as.data.table(in_rftuned)[eval(learner_id),] #Go through data.table format to have access to both tasks and learners
  
  #Get fold-specific performance measure
  foldperf <- extract_impperf_nestedrf(in_rflearner = in_mod$learner,
                                       in_task = in_mod$task,
                                       imp=F, perf=T, pvalue=F)
  
  # selcols <- in_vimp_plot$data %>% #Can use that if extracting from tunredrf is expensive
  #   setorder(-imp_wmean) %>%
  #   .[colnums, variable]
  
  
  if (inherits(in_mod$learner[[1]]$learner, "GraphLearner")) {
    in_fit <- in_mod$learner[[1]]$learner$model$classif.ranger$model
  } else {
    in_fit <- in_mod$learner[[1]]$learner$model
  }
  
  ngridvec <- c(ngrid, ngrid)
  
  #Make dataset of all combinations of selected column names, two at a time
  if (nvariate == 1) {
    pdcomb <- lapply(selcols, function(i) {
      print(i)
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i),
                                         n = ngridvec, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i)] %>%
        setnames(i, 'value1')
    }
    ) %>%
      do.call(rbind, .)
    
  } else if (nvariate == 2) {
    vargrid <- combn(selcols, 2, simplify=F) %>%
      do.call(rbind, .)
    
    #Get marginal distribution of the effect of two columns at a time
    pdcomb <- mapply(function(i, j) {
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i, j),
                                         n = ngridvec,
                                         interaction = TRUE, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i, var2=j)] %>%
        setnames(c(i,j), c('value1', 'value2'))
      
      
      return(pdout)
    }, vargrid[,1], vargrid[,2], SIMPLIFY = FALSE) %>%
      do.call(rbind, .)
  } else {
    print('Warning: function cannot yet work with more than two variables at a time')
  }
  
  return(pdcomb)
}
# save models 
save_model <- function(in_model, outdir){
  
  if (!file.exists(outdir)){
    dir.create(file.path(here::here(), 'results/modeldir'), recursive = TRUE)
  }
  
  if (inherits(in_model, 'list')){
    main_model <- in_model$rf_inner$model$learner
  }
  
  qs::qsave(main_model, file = file.path(here::here(), outdir))
}

# ----------------------- Functions for applying the sequantial model to reaches -------------
# costum functions to import data and the tuned models, and lastely execute the sequential models
import_data <- function(path_static, path_LR,
                        path_HR, num_mon,
                        num_year, continent='eu'){
  
  # Static predictors -----
  static_pred <- fst::read_fst(path = path_static,
                               columns = c("glacier_fraction", "land_cover", "slope", "drainage_area",
                                           "pot_nat_vegetation", "karst_fraction", "karst_status",
                                           'lka_pc_use', 'ppd_pk_cav', 'ppd_pk_uav', 'ire_pc_cse', 
                                           'ire_pc_use', 'dor_pc_pva',
                                           paste0("ai_", num_mon))) %>%
    as.data.table() %>% 
    rename(., ai = paste0("ai_", num_mon))
  
  upa <- static_pred %>% pull(drainage_area)
  
  # Low resolution predictors ----
  if (continent == 'eu'){
    path_qrdif_ql_ratio <-  file.path(path_LR, "gwr_to_runoff_ratio_eu.fst")
    path_wetdays <-  file.path(path_LR, "wetdays_net_eu.fst") 
  }else{
    path_qrdif_ql_ratio <-  file.path(path_LR, "gwr_to_runoff_ratio_sa.fst")
    path_wetdays <-  file.path(path_LR, "wetdays_net_sa.fst") 
  }
  
  qrdif_ql_ratio <- fst::read_fst(path = path_qrdif_ql_ratio,
                                  columns = paste0("gwr_", num_mon, "_", num_year)) %>%
    as.data.table() %>% 
    replace(is.na(.), 0) %>%
    `colnames<-`("gwr_to_runoff_ratio")
  
  wet_day <- fst::read_fst(path = path_wetdays,
                           columns = paste0("wetdays_", num_mon, "_", num_year)) %>%
    as.data.table() %>% 
    `colnames<-`("wet_days")
  
  if (continent == 'eu') {
    qrdif_ql_ratio$gwr_to_runoff_ratio <- qrdif_ql_ratio$gwr_to_runoff_ratio * 100
    wet_day$wet_days <- wet_day$wet_days * 100}
  
  # High resolution predictors ------
  if (continent == 'eu'){
    path_Q <- file.path(path_HR, "watergap_dis_net_eu.fst")
    path_mean_p3 <-  file.path(path_HR, "q_mean_p3_eu.fst")
    path_mean_p12 <-  file.path(path_HR, "q_mean_p12_eu.fst")
    path_min_p3 <-  file.path(path_HR, "q_min_p3_eu.fst")
    path_min_p12 <-  file.path(path_HR, "q_min_p12_eu.fst")
    path_cv <-  file.path(path_HR, "q_iav_cv_eu.fst")
    path_sd <-  file.path(path_HR, "q_iav_sd_eu.fst")  
  } else{
    path_Q <- file.path(path_HR, "watergap_dis_net_sa.fst")
    path_mean_p3 <-  file.path(path_HR, "q_mean_p3_sa.fst")
    path_mean_p12 <-  file.path(path_HR, "q_mean_p12_sa.fst")
    path_min_p3 <-  file.path(path_HR, "q_min_p3_sa.fst")
    path_min_p12 <-  file.path(path_HR, "q_min_p12_sa.fst")
    path_cv <-  file.path(path_HR, "q_iav_cv_sa.fst")
    path_sd <-  file.path(path_HR, "q_iav_sd_sa.fst")  
  }
  
  
  Q <- fst::read_fst(path = path_Q,
                     columns = paste0("Q_", num_mon, "_", num_year)) %>%
    as.data.table() %>% 
    `colnames<-`("Q")
  Q <- Q/upa
  mean_p3m <- fst::read_fst(path = path_mean_p3,
                            columns = paste0("mean_p3_", num_mon, "_", num_year)) %>% 
    as.data.table() %>% 
    `colnames<-`("mean_p3m")
  mean_p3m <- mean_p3m/upa
  mean_p12m <- fst::read_fst(path = path_mean_p12,
                             columns = paste0("mean_p12_", num_mon, "_", num_year)) %>% 
    as.data.table() %>% 
    `colnames<-`("mean_p12m")
  mean_p12m <- mean_p12m/upa
  min_p3m <- fst::read_fst(path = path_min_p3,
                           columns = paste0("min_p3_", num_mon, "_", num_year)) %>% 
    as.data.table() %>% 
    `colnames<-`("min_p3m")
  min_p3m <- min_p3m/upa
  min_p12m <- fst::read_fst(path = path_min_p12,
                            columns = paste0("min_p12_", num_mon, "_", num_year)) %>% 
    as.data.table() %>% 
    `colnames<-`("min_p12m")
  min_p12m <- min_p12m/upa
  q_cv <- fst::read_fst(path = path_cv,
                        columns = paste0("cv_", num_mon)) %>%
    as.data.table() %>%
    `colnames<-`("cv") %>%
    replace(is.na(.), 5)
  
  if (continent == 'eu'){
    
    q_sd <- fst::read_fst(path = path_sd,
                          columns = paste0("sd_", num_mon, "_", num_year)) %>%
      as.data.table() %>%
      `colnames<-`("sd")
  } else {
    q_sd <- fst::read_fst(path = path_sd,
                          columns = paste0("sd_", num_mon)) %>%
      as.data.table() %>%
      `colnames<-`("sd")
  }
  
  
  outdt <- cbind(Q, mean_p3m, mean_p12m, min_p3m, min_p12m, q_sd, q_cv,
                 wet_day, qrdif_ql_ratio, static_pred)
}


load_models <- function(path_model1){
  
  model_step1 <- qs::qread(file = path_model1)
  out <- list(model_step1 = model_step1)
}
execute_models_nets <- function(model_step1,
                                data_dt, threshold = 0.5){

  res_step1 <- model_step1$predict_newdata(data_dt)
  res_step1 <- res_step1$set_threshold(threshold) %>% 
    as.data.table()
  
  out <- list(res_step1 = res_step1)
  return(out)
}

runmodels_over_period <- function(path_model1,path_static,path_LR,path_HR,
                                  start_year=1981, end_year=2019, outdir,
                                  scenario = 'single', continent = 'eu'){
  if (!dir.exists(outdir)){
    dir.create(outdir, recursive = TRUE)
  }
  
  seq_models <- load_models(path_model1 = path_model1)
  
  # load the DRYvER_id of the reaches over Europe
  if (continent == 'eu'){
    id <- fst::read_fst(path = path_static,
                        columns = "DRYVER_RIVID") %>% 
      as.data.table()
    
    # create an empty matrix
    years <- start_year:end_year
    res_nets_mat <- matrix(NA, nrow = dim(id)[1], ncol = (length(years)*12+1))
    res_nets_mat[,1] <- id$DRYVER_RIVID
    
  }else {
    id <- fst::read_fst(path = path_static,
                        columns = "HYRIV_ID") %>% 
      as.data.table()
    
    # create an empty matrix
    years <- start_year:end_year
    res_nets_mat <- matrix(NA, nrow = dim(id)[1], ncol = (length(years)*12+1))
    res_nets_mat[,1] <- id$HYRIV_ID
  }
  
  # run sequential model for the whole historical period
  for(i in seq_along(years)){
    cat("The number of no-flow days for reaches in", as.character(years[i]),
        "is undergoing.\n")
    for(j in 1:12){
      
      data_dt <- import_data(path_static = path_static,
                             path_LR = path_LR,
                             path_HR = path_HR,
                             num_mon = j,
                             continent=continent,
                             num_year = years[i])
      
      results_net <- execute_models_nets(model_step1 = seq_models$model_step1,
                                         data_dt = data_dt)
      res_nets_mat[,((i-1)*12+j+1)] <- 
        as.numeric(levels(results_net$res_step1$response))[results_net$res_step1$response]
      res_nets_mat[results_net$rows_id_step2, ((i-1)*12+j+1)] <- 
        as.numeric(levels(results_net$res_step2$response))[results_net$res_step2$response]
    }
    
  }
  
  res_nets_mat_dt <- res_nets_mat %>% as.data.table()
  fst::write_fst(res_nets_mat_dt, 
                 path = file.path(outdir,paste0("res_nets_mat_dt_", scenario, ".fst")))
  
  return(res_nets_mat)
}

runmodels_over_period_withprob <- function(path_model1,path_static,path_LR,path_HR,
                                           start_year=1981, end_year=2019, outdir,
                                           scenario = 'single', continent = 'eu'){
  if (!dir.exists(outdir)){
    dir.create(outdir, recursive = TRUE)
  }
  
  seq_models <- load_models(path_model1 = path_model1)
  
  # load the DRYvER_id of the reaches over Europe
  if (continent == 'eu'){
    id <- fst::read_fst(path = path_static,
                        columns = "DRYVER_RIVID") %>% 
      as.data.table()
    
    # create an empty matrix
    years <- start_year:end_year
    res_nets_mat <- matrix(NA, nrow = dim(id)[1], ncol = (length(years)*12+1))
    res_nets_mat[,1] <- id$DRYVER_RIVID
    res_nets_mat_prob <- matrix(NA, nrow = dim(id)[1], ncol = (length(years)*12+1))
    res_nets_mat_prob[,1] <- id$DRYVER_RIVID
  }else {
    id <- fst::read_fst(path = path_static,
                        columns = "HYRIV_ID") %>% 
      as.data.table()
    
    # create an empty matrix
    years <- start_year:end_year
    res_nets_mat <- matrix(NA, nrow = dim(id)[1], ncol = (length(years)*12+1))
    res_nets_mat[,1] <- id$HYRIV_ID
    res_nets_mat_prob <- matrix(NA, nrow = dim(id)[1], ncol = (length(years)*12+1))
    res_nets_mat_prob[,1] <- id$HYRIV_ID
  }
  
  # run sequential model for the whole historical period
  for(i in seq_along(years)){
    cat("The number of no-flow days for reaches in", as.character(years[i]),
        "is undergoing.\n")
    for(j in 1:12){
      
      data_dt <- import_data(path_static = path_static,
                             path_LR = path_LR,
                             path_HR = path_HR,
                             num_mon = j,
                             continent=continent,
                             num_year = years[i])
      
      results_net <- execute_models_nets(model_step1 = seq_models$model_step1,
                                         data_dt = data_dt)
      res_nets_mat[,((i-1)*12+j+1)] <- 
        as.numeric(levels(results_net$res_step1$response))[results_net$res_step1$response]
      res_nets_mat[results_net$rows_id_step2, ((i-1)*12+j+1)] <- 
        as.numeric(levels(results_net$res_step2$response))[results_net$res_step2$response]
      
      res_nets_mat_prob[,((i-1)*12+j+1)] <- round(results_net$res_step1$prob.0, digits = 2)
    }
    
  }
  
  res_nets_mat_dt <- res_nets_mat %>% as.data.table()
  fst::write_fst(res_nets_mat_dt, 
                 path = file.path(outdir,paste0("res_nets_mat_dt_", scenario, ".fst")))
  res_nets_mat_prob_dt <- res_nets_mat_prob %>% as.data.table()
  fst::write_fst(res_nets_mat_prob_dt, 
                 path = file.path(outdir,paste0("results_nets_prob_",scenario, '.fst')))
  
  return(res_nets_mat)
}

# Validate the model with onde database -------
# compute the model performance only for reaches - if there is at least one record of intermittent
# the reach is considered as intermittent, otherwise, it is perennial.
# The class 2 and 3 are meant to be intermittent. otherwise, the reach is perennial. 
# also we remove the records with the phase of 4. 
validate_modelwithonde <- function(path_reachdt, path_ondedt, path_reach_shp, path_onde_shp, outdir){
  
  start_date <<- start_date
  end_date <<- end_date
  
  #import the predicted (for the European reaches) and observed (ONDE reaches) data
  reach_data <- fst::read_fst(path = file.path(path_reachdt, "res_nets_mat_dt.fst"))
  onde_data <- data.table::fread(file.path(path_ondedt, 'onde_france_merge.csv'))
  #read the ONDE geofile
  dryver_riv <- sf::st_read(file.path(path_onde_shp, 'onde_stations_dryverids.shp')) %>% 
    sf::st_drop_geometry()
  
  #read the European reaches geofile
  net_eu_id_dt <-  sf::st_read(dsn = file.path(path_reach_shp, '01_dryver_net_eu.shp'))
  
  #exclude some of irrelavent fields
  dryver_riv <- dryver_riv %>%
    select(-c('DRYVER_R_1', 'from_node', 'to_node', 'Shape_Leng',
              'LENGTH_GEO', 'im_frac', 'upa', 'OBJECTID', 'Join_Count', 'TARGET_FID',
              'Join_Cou_1', 'TARGET_F_1', 'OBJECTID_1',
              'F_CdSite_1', 'F_CdSite_2', 'DRYVER_RIV')) %>% 
    as.data.table()
  # select only the ONDE reaches from European reaches
  onde_reaches <- net_eu_id_dt %>% filter(DRYVER_RIV %in% dryver_riv$DRYVER_R_2)
  
  data.table::setnames(reach_data, 'V1', 'ids')
  
  #select only a few columns of onde observed data
  onde_data_filtered <- onde_data %>% 
    dplyr::select(c('<CdSiteHydro>', '<DtRealObservation>', '<RsObservationNat>')) %>% 
    `colnames<-`(c('site_code', 'dates', 'status')) %>% 
    .[status != 4,] # remove the phase 4.
  
  #create a vector of dates as character 
  date_char <- seq.Date(start_date, 
                        as.Date('2019-12-01'), 'month') %>% 
    as.character()
  
  #filter only ONDE reach-months
  predicted_onde_reaches <- reach_data %>%
    dplyr::filter(ids %in% dryver_riv$DRYVER_R_2) %>%  # filter the onde reaches
    `colnames<-`(c('ids', date_char)) # set the cols of dt with date 
  
  #reframe the ONDE predicted reach-months
  predicted_onde_reaches_melted <- predicted_onde_reaches %>% 
    as.data.table() %>% 
    melt(., id.var ='ids') %>% 
    .[,variable := as.Date(variable)]
  
  predicted_onde_reaches_melted <- predicted_onde_reaches_melted %>%
    `colnames<-`(c('DRYvER_RIV', 'dates', 'response'))
  
  #filter only reach-month within the study period
  onde_data_filtered <- onde_data_filtered[site_code %in% dryver_riv$F_CdSiteHy] %>% 
    .[dates <= end_date]
  #merged the observed data with shapefile of the reaches
  merged_dt <- merge(onde_data_filtered, dryver_riv, by.x='site_code',by.y = 'F_CdSiteHy')
  #set the date to the first day of month
  merged_dt[, dates := as.Date(paste0(format(dates, "%Y-%m"), "-01"))]
  merged_dt <- merged_dt %>%
    rename(., DRYvER_RIV = DRYVER_R_2)
  #merged the observed and predicted data
  dd <- merge(merged_dt, predicted_onde_reaches_melted, by=c('DRYvER_RIV', 'dates'))
  
  #based on onde definition, if status is 1, it is perennial, otherwise in intermittent
  dd[, status := ifelse(status == 1, 0, 1)] 
  dd[,response := ifelse(response == 0, 0, 1)]
  dd_selected <- dd[,.(DRYvER_RIV,site_code,dates,status, response, upa_1)]
  dd_selected[,status := as.factor(status)][,response := as.factor(response)]
  
  #Compute the ONDE reaches' classes to compute predictive accuracy
  classes_onde <- dd_selected %>% 
    group_by(DRYvER_RIV) %>% 
    dplyr::summarise(count = n(),
                     obs = sum(status == 1),
                     pre = sum(response == 1),
                     DRYvER_RIV = unique(DRYvER_RIV)) %>% 
    ungroup() %>% 
    mutate(class=case_when(
      obs > 0 &   pre == 0 ~ 1,
      obs > 0 &   pre > 1 ~ 2,
      obs == 0 &   pre == 0 ~ 3,
      obs == 0 &   pre > 1 ~ 4
    ))
  # Correctly classified reachs
  onde_correct <- dd_selected %>% 
    group_by(site_code) %>% 
    dplyr::summarise(count = n(), 
                     class_correct = sum(status == response)/ count * 100,
                     DRYvER_RIV = unique(DRYvER_RIV))
  #bias and ratio
  dd_selected[, c('count', 'no_flow_obs', 'no_flow_pre') := 
                .(.N, sum(status == 1), sum(response == 1)), by = DRYvER_RIV]
  
  dd_selected[, c('ratio_no_flow_obs', 'ratio_no_flow_pre') := 
                .((no_flow_obs / count) * 100, (no_flow_pre / count) * 100), by = .(DRYvER_RIV)][
                  ,c('ratio') := .(ratio_no_flow_pre / ratio_no_flow_obs),  by = .(DRYvER_RIV)
                  ][,c('bias') := .(no_flow_pre - no_flow_obs), by = .(DRYvER_RIV)]
  
  distincted_dt <- unique(dd_selected, by = 'DRYvER_RIV') %>%
    .[, .(DRYvER_RIV, site_code, count, no_flow_obs, no_flow_pre, 
          ratio_no_flow_obs, ratio_no_flow_pre, ratio, bias, upa_1)]
  
  distincted_dt[is.na(ratio), ratio := 9999]
  distincted_dt[is.infinite(ratio), ratio := 999999]
  
  #Save the shapefiles for creating maps
  
  merge(onde_reaches, distincted_dt, by.x='DRYVER_RIV', by.y='DRYvER_RIV') %>% 
    sf::write_sf(., file.path(outdir, "onde_ratioandbias_preoverobs.shp"))
  
  merge(onde_reaches, classes_onde, by.x='DRYVER_RIV', by.y='DRYvER_RIV') %>% 
    sf::write_sf(., file.path(outdir, "onde_classes_all_reportrun.shp"))
  
  merge(onde_reaches, onde_correct, by.x='DRYVER_RIV', by.y='DRYvER_RIV') %>% 
    sf::write_sf(., file.path(outdir, "onde_classification_correct.shp"))
  
}

# the intermittent fraction of the reaches over Europe
compute_intermittency_fraction <- function(in_matrix, path_reach_shp, path_static, outdir){
  
  #read the upstream area
  upa <- fst::read_fst(path = path_static,
                       columns = "drainage_area") %>% 
    as.data.table()
  
  #read the European reaches geofile
  net_eu_id_dt <-  sf::st_read(dsn = file.path(path_reach_shp, '01_dryver_net_eu.shp'))
  
  #compute the fraction of intermittency between 1981-2019
  d <- rowSums(in_matrix[,-1] > 0)/468 * 100
  
  res_nets_mat_dt <- in_matrix %>% 
    data.table::as.data.table()
  
  reach_im_frac <- cbind(id = res_nets_mat_dt$V1,
                         im_frac = d,
                         upa = upa$drainage_area) %>% as.data.table()
  
  merge(net_eu_id_dt, reach_im_frac,
        by.y = "id", by.x = "DRYVER_RIV") %>% 
    sf::st_as_sf() %>%
    sf::write_sf(., file.path(outdir, "intermittency_frac_reaches_eu.shp"))
}


# ----------------------------- Function to produce Figures--------------------------------------

# create the variable important plot for both steps ---> Figure 7. ------------
ggvimp <- function(in_rftuned_step1, in_rftuned_step2, in_predvars, varnum = 23, spatial_rsp=FALSE) {
  
  rsmp_res_step1 <- get_outerrsmp(in_rftuned_step1, spatial_rsp=spatial_rsp)
  rsmp_res_step2 <- get_outerrsmp(in_rftuned_step2, spatial_rsp=spatial_rsp)
  #Get variable importance and format them
  varimp_basic_step1 <- weighted_vimportance_nestedrf(rfresamp = rsmp_res_step1,
                                                      pvalue = FALSE) %>%
    merge(., in_predvars, by.x='varnames', by.y = "varname") %>%
    .[, `:=`(varnames = factor(varnames, varnames[order(-imp_wmean)]),
             Category = factor(Category,
                               levels = c('Anthropogenic', 'Climate', 'Geology', 'Hydrology', 
                                          'Lakes','Landcover', 'Physiography'))
    )] %>%
    setorder(-imp_wmean)
  
  varimp_basic_step2 <- weighted_vimportance_nestedrf(rfresamp = rsmp_res_step2,
                                                      pvalue = FALSE) %>%
    merge(., in_predvars, by.x='varnames', by.y = "varname") %>%
    .[, `:=`(varnames = factor(varnames, varnames[order(-imp_wmean)]),
             Category = factor(Category,
                               levels = c('Anthropogenic', 'Climate', 'Geology', 'Hydrology', 
                                          'Lakes','Landcover', 'Physiography'))
    )] %>%
    setorder(-imp_wmean)
  #Plot step 1 RF
  outp_step1 <- varimp_basic_step1[1:dim(in_predvars)[1]] %>% 
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
    theme(axis.text = element_text(color = 'black')) +
    scale_y_continuous(expand=c(0,0), position = 'left') +
    labs(y = "", x='', title = '              Step 1 RF')+
    coord_flip(ylim=c(0, max(varimp_basic_step1[, max(imp_wmean+imp_wsd)+10], 100))) 
  #Plot step 2
  outp_step2 <- varimp_basic_step2[1:dim(in_predvars)[1]] %>% 
    mutate(abbrevation = fct_reorder(abbrevation, desc(imp_wmean))) %>% 
    ggplot(. ,aes(x=abbrevation,
                  color =Category, fill=Category)) +
    geom_bar(aes(y=imp_wmean),position = position_dodge(), stat = 'identity', alpha=0.7) +
    
    geom_errorbar(aes(ymin=imp_wmean-imp_wsd, ymax=imp_wmean+imp_wsd)) +
    scale_y_reverse () +
    # scale_x_discrete() +
    coord_flip () +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(x, width = 27)
    },
    limits=rev, position = "top") +
    scale_fill_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                      drop=FALSE) +
    scale_color_manual(values=c('#FFCCFF', '#fdb462', '#696868','#3399FF', '#80b1d3','#b3de69', '#bc80bd'),
                       drop=FALSE) +
    theme_classic(18) +
    theme(legend.position = 'none', axis.text = element_text(color = 'black')) +
    labs(y = "", x = '', title = '      Step 2 RF')
  
  pout <- cowplot::plot_grid(outp_step1, outp_step2, rel_widths = c(2, 1), label_size = 18) +
    cowplot::draw_label("Variable importance (actual impurity reduction)", 
                        x=0.5, y=  0, vjust=-0.5, angle= 0, size = 18) +
    cowplot::draw_label("Predictor", x=  0, y=0.5, vjust= 1.5, angle=90, size = 18) +
    cowplot::draw_label("Predictor", x=  1, y=0.5, vjust= -1.5, angle=90, size = 18)
  
  return(pout)
}

# Create a data table of predictors' name and category
predname_df <- function(in_task, in_task_all = NULL, feat_name_vec = NULL, category = NULL){
  
  if (inherits(in_task, "Task")) {
    feat_name <- in_task$feature_names
    if (length(list(in_task_all)) == 1) {
      feat_name_all <- in_task_all$feature_names
    } else {
      feat_name_all <- in_task_all[[1]]$feature_names
    }
    
  } else
    feat_name <- feat_name_vec
  
  feat_name_abb <- c('Q', 'P_to_PET_ratio', 'Q_iav_cv', 'dor', 'drainage_area', 'glacier_frac',
                     'gwr_to_runoff_ratio', 'irri_frac_im', 'irri_frac', 'karst_frac', 'karst_status', 'land_cover',
                     'lake_frac', 'Q_mean_p12', 'Q_mean_p3', 'Q_min_p12', 'Q_min_p3', 'pot_nat_vegetation',
                     'pop_dens_im', 'pop_dens', 'Q_iav_sd', 'slope', 'wet_days')
  if (is.null(category)) {
    cat <- c("Hydrology", "Climate", "Hydrology", "Anthropogenic", "Physiography", "Landcover", "Hydrology",
             "Anthropogenic", "Anthropogenic", "Geology", "Geology",
             "Landcover", "Lakes",  "Hydrology", "Hydrology", "Hydrology", "Hydrology",
             "Landcover", "Anthropogenic", "Anthropogenic", "Hydrology", "Physiography", "Climate")
  } else
    cat <- category
  
  predvars <- cbind(feat_name, feat_name_abb, cat) %>%
    `colnames<-`(c("varname", 'abbrevation',"Category")) %>% 
    as.data.table()
  
  predvars_out <- predvars[varname %in% feat_name]
  
  return(predvars_out)
  
}
# extract_pd_nestedrf -----------
extract_pd_nestedrf <- function(learner_id=1, in_rftuned, datdf,
                                selcols, nvariate, ngrid=20) {
  in_mod <- as.data.table(in_rftuned)[eval(learner_id),] #Go through data.table format to have access to both tasks and learners
  
  #Get fold-specific performance measure
  foldperf <- extract_impperf_nestedrf(in_rflearner = in_mod$learner,
                                       in_task = in_mod$task,
                                       imp=F, perf=T, pvalue=F)
  
  # selcols <- in_vimp_plot$data %>% #Can use that if extracting from tunredrf is expensive
  #   setorder(-imp_wmean) %>%
  #   .[colnums, variable]
  
  
  if (inherits(in_mod$learner[[1]]$learner, "GraphLearner")) {
    in_fit <- in_mod$learner[[1]]$learner$model$classif.ranger$model
  } else {
    in_fit <- in_mod$learner[[1]]$learner$model
  }
  
  ngridvec <- c(ngrid, ngrid)
  
  #Make dataset of all combinations of selected column names, two at a time
  if (nvariate == 1) {
    pdcomb <- lapply(selcols, function(i) {
      print(i)
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i),
                                         n = ngridvec, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i)] %>%
        setnames(i, 'value1')
    }
    ) %>%
      do.call(rbind, .)
    
  } else if (nvariate == 2) {
    vargrid <- combn(selcols, 2, simplify=F) %>%
      do.call(rbind, .)
    
    #Get marginal distribution of the effect of two columns at a time
    pdcomb <- mapply(function(i, j) {
      pdout <- edarf::partial_dependence(fit = in_fit, vars = c(i, j),
                                         n = ngridvec,
                                         interaction = TRUE, data = datdf) %>% #Warning: does not work with data_table
        setDT %>%
        .[,(names(foldperf)) := foldperf] %>%
        .[, `:=`(var1=i, var2=j)] %>%
        setnames(c(i,j), c('value1', 'value2'))
      
      
      return(pdout)
    }, vargrid[,1], vargrid[,2], SIMPLIFY = FALSE) %>%
      do.call(rbind, .)
  } else {
    print('Warning: function cannot yet work with more than two variables at a time')
  }
  
  return(pdcomb)
}

# ggpartialdep ---------- 
ggpartialdep <- function (in_rftuned, in_predvars, colnums, ngrid, nodupli=T,
                          nvariate = 2, model_data,
                          parallel=T, spatial_rsp=FALSE) {
  
  in_predvars$unites <- c('(m3 sec-1 km-2)', '(-)', '(-)', '(% * 10)', '(km2)',
                          '(%)', '(-)', '(% * 100)', '(% * 100)', '(%)', '(-)', '(-)', '(% * 100)',
                          '(m3 sec-1 km-2)',
                          '(m3 sec-1 km-2)', '(m3 sec-1 km-2)', '(m3 sec-1 km-2)', '(-)', '(People per km2)',
                          '(People per km2)', '(m3 sec-1 km-2)', '(deg/100)', '(days/month * 10000)')
  
  in_predvars[, combined := paste(abbrevation, unites, sep = " ")]
  model_data[, gwr_to_runoff_ratio := gwr_to_runoff_ratio/100][
    ,wet_days := wet_days/100
    ][
      ,dor_pc_pva := dor_pc_pva/1000
      ][, lka_pc_use := lka_pc_use/100][, ire_pc_cse := ire_pc_cse/100][,ire_pc_use:=ire_pc_use/100]
  
  #Get outer resampling of interest
  rsmp_res <- get_outerrsmp(in_rftuned, spatial_rsp=spatial_rsp)
  
  #Get partial dependence across all folds and repeats
  nlearners <-with(rsmp_res$resampling$param_set$values, folds*repeats)
  datdf <- as.data.frame(model_data) #This may be shortened
  varimp <- weighted_vimportance_nestedrf(rsmp_res, pvalue=FALSE) %>%
    setorder(-imp_wmean)
  
  if (length(colnums) > nrow(varimp)) {
    colnums <- colnums[1:nrow(varimp)]
    cat('colnums argument exceeded the number of variables,
        reduced it to ', nrow(varimp), ' variables')
  }
  
  if (nodupli) {
    selcols <- as.character(
      varimp$varnames[!duplicated(substr(varimp$varnames, 1,3))][colnums])
  } else {
    selcols <- as.character(
      varimp$varnames[colnums])
  }
  
  if (parallel) {
    print(paste("Computing partial dependence with future.apply across", nlearners,
                "CV folds"))
    pd <- future.apply::future_lapply(seq_len(nlearners),
                                      extract_pd_nestedrf,
                                      in_rftuned = rsmp_res,
                                      datdf = datdf,
                                      selcols = selcols,
                                      nvariate = nvariate,
                                      ngrid = ngrid,
                                      future.scheduling = structure(TRUE,ordering = "random"),
                                      future.packages = c("data.table","edarf","ranger"))
    
  } else {
    print(paste("Computing partial dependence iteratively across", nlearners,
                "CV folds"))
    pd <- lapply(seq_len(nlearners),
                 extract_pd_nestedrf,
                 in_rftuned = rsmp_res,
                 datdf = datdf,
                 selcols = selcols,
                 nvariate = nvariate,
                 ngrid = ngrid)
  }
  
  #Get weighted mean
  varvec <- paste0('var', 1:nvariate)
  valvec <- paste0('value', 1:nvariate)
  
  pdformat <- do.call(rbind, pd) %>%
    setDT %>%
    .[, list(mean1 = weighted.mean(`1`, classif.bacc)),
      by= c(varvec, valvec)] %>%
    .[, variables := var1] %>%
    merge(., in_predvars, by.x='var1', by.y='varname')
  
  datdf2 <- as.data.table(datdf)[, target_class := as.numeric(as.character(target))]
  not_log_variables <- c('ai','gwr_to_runoff_ratio', 'pot_nat_vegetation', 'land_cover', 'cv', 'glacier_fraction',
                         'karst_fraction', 'karst_status', 'lka_pc_use', 'ppd_pk_cav', 'ppd_pk_uav', 'ire_pc_cse',
                         'ire_pc_use', 'slope', 'wet_days')
  if (nvariate ==1) {
    
    tileplots_list1 <- pdformat[!(var1 %in% not_log_variables),
                                list(list(ggplotGrob(
                                  ggplot(.SD, aes(x=value1, y=mean1)) +
                                    geom_line() +
                                    geom_rug(data=datdf2,
                                             aes_string(x=eval(var1),y='target_class'),
                                             alpha=1/3) +
                                    scale_y_continuous(name='Partial dependence (probability of intermittency)',
                                                       limits= c(min(mean1)-0.01, max(mean1)+0.01),  #c(0.25, 0.425),
                                                       expand=c(0,0))+
                                    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                  labels = trans_format("log10", math_format(10^.x)),
                                                  name=stringr::str_wrap(eval(combined), width = 30)) +
                                    theme_classic() +
                                    theme(text = element_text(size=16),
                                          axis.title.y = element_blank())
                                ))), by=.(var1)]
    
    tileplots_list2 <- pdformat[(var1 %in% not_log_variables),
                                list(list(ggplotGrob(
                                  ggplot(.SD, aes(x=value1, y=mean1)) +
                                    geom_line() +
                                    geom_rug(data=datdf2,
                                             aes_string(x=eval(var1),y='target_class'),
                                             alpha=1/3) +
                                    scale_y_continuous(name='Partial dependence (probability of intermittency)',
                                                       limits= c(min(mean1)-0.01, max(mean1)+0.01),  #c(0.25, 0.425),
                                                       expand=c(0,0)) +
                                    scale_x_continuous(name=stringr::str_wrap(eval(combined), width = 30)) +
                                    theme_classic() +
                                    theme(text = element_text(size=16),
                                          axis.title.y = element_blank())
                                ))), by=.(var1)]
    
    tileplots_l <- rbind(tileplots_list1, tileplots_list2)
    setkey(tileplots_l, var1)
    tileplots_l <- tileplots_l[varimp$varnames]
    
  }
  
  pagelayout <-   lapply(1:(nrow(tileplots_l) %/% 9), function(p_i) {
    (p_i-1)*9+(1:9)
  })
  if (nrow(tileplots_l) %% 9 > 0) {
    pagelayout[[nrow(tileplots_l) %/% 9 + 1]] <-
      (nrow(tileplots_l) %/% 9)*9+(1:(nrow(tileplots_l) %% 9))
  }
  
  
  tileplots_multipl <- lapply(pagelayout, function(page) {
    print(page)
    return(do.call("grid.arrange",list(
      grobs=(tileplots_l[page,V1]),
      left = textGrob('Probability that the station-month is non-perennial',
                      gp = gpar(fontface = "bold", fontsize = 16), rot = 90))))
  })
  return(tileplots_multipl)
}
