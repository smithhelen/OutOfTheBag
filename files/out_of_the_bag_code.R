## R code to accompany the file "Out of (the) bag - encoding categorical predictors impacts out-of-bag samples"
# by HL Smith, PJ Biggs, NP French, ANH Smith, JC Marshall 2024

# load libraries
library(tidyverse)
library(ranger)


# set seed
seed=123
set.seed(seed)


# calculate oob and independent misclasification rates
oob_mc <- function(n=200, p=3, k=35, ntrees=500, ordered=TRUE){
  
  # generate data
  data <- data.frame(
    id = 1:n, 
    class = sample(1:p, n, TRUE), 
    var1 = sample(1:k, n, TRUE)) |> 
    mutate(across(everything(), factor))
  
  # split data into train and test sets
  dat.train <- data  |> slice_sample(prop = 0.8) |> mutate(across(everything(), droplevels))
  dat.test <- data |> filter(!id %in% dat.train$id) |> mutate(across(everything(), droplevels))
  
  # 1. target-based encoding
  # create ranger model on training data
  mod <- ranger(class ~ ., data=dat.train |> select(-id), respect.unordered.factors = "order", oob.error = TRUE)

  # test ranger model 
  preds <- data.frame(prediction = predict(mod, data=dat.test, predict.all = FALSE)$predictions, truth = dat.test$class) |> table()

  # calculate OOB and misclassification error rates
  tb.oob = mod$prediction.error
  tb.misclass = 1-sum(diag(preds))/sum(preds)
  
  # 2. target-agnostic encoding
  # create ranger model on training data
  mod <- ranger(class ~ ., data=dat.train |> select(-id), respect.unordered.factors = "ignore", oob.error = TRUE)

  # test ranger model and calculate misclassification error rate
  preds <- data.frame(prediction = predict(mod, data=dat.test, predict.all = FALSE)$predictions, truth = dat.test$class) |> table()

  ta.oob = mod$prediction.error
  ta.misclass = 1-sum(diag(preds))/sum(preds)
  
  # output results
  list(tb.oob=tb.oob, tb.misclass=tb.misclass,
       ta.oob=ta.oob, ta.misclass=ta.misclass)
       
}


# function to permute variable column
permute <- function(var, data) {
  data |> mutate({{var}} := sample(.data[[{{var}}]]))
}

# function to calculate misclassification rates
mc <- function(mod, dat){
  preds <- data.frame(prediction = predict(mod, data=dat, predict.all = FALSE)$predictions, truth = dat$class) |> table()
  misclass <- 1-sum(diag(preds))/sum(preds)
  misclass
}

# calculate variable importance using a range of parameters
oob_vim <- function(n=200, p=3, k=3, ntrees=500, importance = "permutation"){
  
  # generate data
  data <- data.frame(
    id = 1:n, 
    class = sample(1:p, n, TRUE), 
    var1 = sample(1:k, n, TRUE)) |> 
    mutate(across(everything(), factor))
  
  # split data into train and test sets
  dat.train <- data  |> slice_sample(prop = 0.8) |> mutate(across(everything(), droplevels))
  dat.test <- data |> filter(!id %in% dat.train$id) |> mutate(across(everything(), droplevels))
  
  switch(
    importance,
    
    independent = {
      # split data into two cv folds
      dat.one <- data  |> slice_sample(prop = 0.5) |> mutate(across(everything(), droplevels))
      dat.two <- data |> filter(!id %in% dat.one$id) |> mutate(across(everything(), droplevels))
      
      # 1. target-based encoding
      ## 1st fold
      # run default ranger with ordered method
      tb.mod_1 <- ranger(class ~ ., data=dat.one |> select(-id), respect.unordered.factors = "order")
      
      # calculate misclassification rates
      misclass_1 <- mc(tb.mod_1, dat.two)
      
      # permute variable in dat.two
      vars_1 <- dat.two |> select(starts_with("var")) |> colnames()
      list.perm.dat_1 <- map(as.list(vars_1), permute, data=dat.two)
      misclass.perm_1 <- list.perm.dat_1 |> map(mc, mod=tb.mod_1)
      names(misclass.perm_1) <- vars_1
      
      # calculate fold 1 permutation importance
      vi_1 <- misclass.perm_1 |> map(\(x) {misclass_1 - x}) |> unlist()
      
      ## 2nd fold
      # run default ranger with ordered method
      tb.mod_2 <- ranger(class ~ ., data=dat.two |> select(-id), respect.unordered.factors = "order")
      
      # calculate misclassification rates
      misclass_2 <- mc(tb.mod_2, dat.one)
      
      # permute variable in dat.one
      vars_2 <- dat.one |> select(starts_with("var")) |> colnames()
      list.perm.dat_2 <- map(as.list(vars_2), permute, data=dat.one)
      misclass.perm_2 <- list.perm.dat_2 |> map(mc, mod=tb.mod_2)
      names(misclass.perm_2) <- vars_2
      
      # calculate fold 2 permutation importance
      vi_2 <- misclass.perm_2 |> map(\(x) {misclass_2 - x}) |> unlist()
      
      # calculate variable importance
      target_based <- mean(vi_1, vi_2)
      
      # 2. target-agnostic encoding
      ## 1st fold
      # run default ranger with ordered method
      ta.mod_1 <- ranger(class ~ ., data=dat.one |> select(-id), respect.unordered.factors = "ignore")
      
      # calculate misclassification rates
      misclass_1 <- mc(ta.mod_1, dat.two)
      
      # permute variable in dat.two
      vars_1 <- dat.two |> select(starts_with("var")) |> colnames()
      list.perm.dat_1 <- map(as.list(vars_1), permute, data=dat.two)
      misclass.perm_1 <- list.perm.dat_1 |> map(mc, mod=ta.mod_1)
      names(misclass.perm_1) <- vars_1
      
      # calculate fold 1 permutation importance
      vi_1 <- misclass.perm_1 |> map(\(x) {misclass_1 - x}) |> unlist()
      
      ## 2nd fold
      # run default ranger with ordered method
      ta.mod_2 <- ranger(class ~ ., data=dat.two |> select(-id), respect.unordered.factors = "ignore")
      
      # calculate misclassification rates
      misclass_2 <- mc(ta.mod_2, dat.one)
      
      # permute variable in dat.one
      vars_2 <- dat.one |> select(starts_with("var")) |> colnames()
      list.perm.dat_2 <- map(as.list(vars_2), permute, data=dat.one)
      misclass.perm_2 <- list.perm.dat_2 |> map(mc, mod=ta.mod_2)
      names(misclass.perm_2) <- vars_2
      
      # calculate fold 2 permutation importance
      vi_2 <- misclass.perm_2 |> map(\(x) {misclass_2 - x}) |> unlist()
      
      # calculate variable importance
      target_agnostic <- mean(vi_1, vi_2)
    },
    
    
    holdout = {
      # 1. target-based encoding
      tb.mod <- holdoutRF(class ~ ., data=dat.train |> select(-id), respect.unordered.factors = "order")
      target_based <- tb.mod$variable.importance
      
      # 2. target-agnostic encoding
      ta.mod <- holdoutRF(class ~ ., data=dat.train |> select(-id), respect.unordered.factors = "ignore")
      target_agnostic <- ta.mod$variable.importance
    },
    
    {
      # 1. target-based encoding
      tb.mod <- ranger(class ~ ., data=dat.train |> select(-id), respect.unordered.factors = "order", importance = importance)
      target_based <- tb.mod$variable.importance
      
      # 2. target-agnostic encoding
      ta.mod <- ranger(class ~ ., data=dat.train |> select(-id), respect.unordered.factors = "ignore", importance = importance)
      target_agnostic <- ta.mod$variable.importance
    }
    
  )
  
  # output results
  list(target_based=target_based, 
       target_agnostic=target_agnostic)
  
}





# run over a range of parameter values and repeat
n <- list(30,50,100,150,200,400) ## n=10 and n=20 throw an error for holdout sample ##
names(n) <- as.character(n)
k <- list(1,5,10,35,50,100,150,200)
names(k) <- as.character(k)
importance <- list("permutation", "impurity", "impurity_corrected", "holdout", "independent")
names(importance) <- importance


# misclassification simulation
set.seed(seed)
mc_results <- map_dfr(1:99, function(z) {map_dfr(n, function(x) {map_dfr(k, ~oob_mc(n=x, k=.), .id = "k")}, .id = "n")}) |> 
  group_by(n,k) |> summarise(target_based.oob=mean(tb.oob), target_based.misclass=mean(tb.misclass),
                             target_agnostic.oob=mean(ta.oob), target_agnostic.misclass=mean(ta.misclass)) 
# for plotting
mc_plot <- mc_results |> 
  pivot_longer(cols = target_based.oob:target_agnostic.misclass, names_to = c("ordered", "method"), names_pattern = "(.*)\\.(.*)", values_to = "error") |> 
  ungroup() |> mutate(across(c(n,k), as.numeric))

mc_plot |> 
  ggplot(aes(x=k, y=error, colour=method, size=n, shape=method, fill=stage(method, after_scale = alpha(colour,0.1)))) + 
  geom_point() + 
  geom_hline(yintercept=2/3, linetype="dotted") + 
  theme_bw(base_size = 14)+ 
  scale_y_continuous("Misclassification rate",expand=c(0,0),limits=c(0,1)) +
  scale_colour_manual(values=c("#d34728","#193d87"),labels = c("Independent test data", "Out-Of-Bag")) + 
  scale_fill_manual(values=c("#d34728","#193d87"),labels = c("Independent test data", "Out-Of-Bag"), aesthetics = c("fill")) + 
  scale_shape_manual(values=c(21,23),labels = c("Independent test data", "Out-Of-Bag"))+
  labs(x="Number of factor levels", colour="Method of calculation", shape="Method of calculation", fill="Method of calculation", size="Sample size") + 
  facet_wrap(~ordered, labeller = labeller(ordered=c(`target_agnostic`="Target-agnostic encoding",`target_based`="Target-based encoding"))) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 3)), 
         shape = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         size = guide_bins(direction = "vertical")) + 
  theme_bw(base_size = 10.5, base_family = 'serif') +
  scale_radius(limits = c(10, NA), range = c(0.5, 5),breaks=c(20,50,100,150,200,400)) + 
  theme(axis.title.x = element_text(margin = ggplot2::margin(t=10)), axis.title.y = element_text(margin = ggplot2::margin(r=5)))


# variable importance simulation
set.seed(seed)
vim_results <- map_dfr(1:99, function(z) {
  map_dfr(n, function(x) {
    map_dfr(k, function(y) {
      map_dfr(importance, function(i){oob_vim(n=x, k=y, importance=i)}, .id="importance")}, .id = "k"
      )}, .id = "n"
    )})


# for plotting
vim_plot <- vim_results |> 
  group_by(importance, n,k) |> summarise(target_based =mean(target_based ), target_agnostic=mean(target_agnostic)) |>
  mutate(n = as.numeric(n), k=as.numeric(k)) |> ungroup() |> 
  mutate(importance = factor(importance, levels = c("permutation","holdout","independent","impurity","impurity_corrected"))) |> 
  pivot_longer(cols = starts_with("target"), names_to = "Encoding", values_to = "VI")

vim_plot |>
  ggplot(aes(x=k, y=VI, size=n, shape=Encoding, colour=Encoding, fill=stage(Encoding, after_scale = alpha(colour,0.1)))) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_bw(base_size = 14)+
  facet_wrap(~importance, scales = "free", 
             labeller = labeller(importance=c("permutation"="Mean Decrease Accuracy",
                                              "holdout"="Holdout",
                                              "independent"="Independent Holdout",
                                              "impurity"="Mean Decrease Impurity",
                                              "impurity_corrected"="Actual Impurity Reduction"
                                              )), as.table = FALSE) + 
  ggh4x::facetted_pos_scales(y = list(
    scale_y_continuous("Variable Importance", limits = c(0,0.5), expand=expansion(mult=c(0.1,0.08))),
    scale_y_continuous(limits = c(0,0.5), expand=expansion(mult=c(0.1,0.08))),
    scale_y_continuous(limits = c(0,0.5), expand=expansion(mult=c(0.1,0.08))),
    scale_y_continuous(limits = c(0,150), expand=expansion(mult=c(0.1,0.1))),
    scale_y_continuous(limits = c(0,70), expand=expansion(mult=c(0.1,0.05)))
  )) +
  #scale_y_continuous("Variable Importance",expand=expansion(mult=c(0.1,0.1))) +
  scale_colour_manual(values=c("#F98400","#354823"),labels = c("Target-agnostic", "Target-based")) +
  scale_fill_manual(values=c("#F98400","#354823"),labels = c("Target-agnostic", "Target-based"), aesthetics = c("fill")) + 
  scale_shape_manual(values=c(21,23),labels = c("Target-agnostic", "Target-based"))+
  labs(x="Number of factor levels", colour="Method of encoding", fill="Method of encoding", shape="Method of encoding", size="Sample size") +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 3)),
         shape = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         size = guide_bins(direction = "vertical")) +
  theme_bw(base_size = 10.5, base_family = 'serif') +
  scale_radius(limits = c(10, NA), range = c(0.5, 5),breaks=c(20,50,100,150,200,400)) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t=10)), 
        axis.title.y = element_text(margin = ggplot2::margin(r=5)),
        legend.position = c(0.8,0.8))
