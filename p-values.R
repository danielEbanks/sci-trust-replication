# p-values.R
# This script computes all p-values reported in the manuscript by
# deriving effect sizes (in percentage points) and their standard errors
# directly from the survey data.

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(gt)


### 1. Data preparation --------------------------------------------------------
# read SPSS files
nov22 <- read_sav("Data/CalTech_November_2022.sav")
jun23 <- read_sav("Data/CalTech_June_2023.sav")
dec23 <- read_sav("Data/caltech_climate_dec23.sav")

# helper: prepare 2023 waves by harmonising variable names
prepare_wave <- function(df, wave, d=1) {
  if (wave %in% c(1)) {
    df <- df %>%  select(x,weight,presvote20post,Q16,race,gender3,region,age4,educ4,pid3,faminc,acsownrent)     %>%
      mutate(id=1:n(),Q10_binary=as.numeric(Q16==d),rand_ab3=x) %>%
      mutate(treat=case_when(rand_ab3%in%c(4) ~1, TRUE ~0),
             treat_dem=case_when(rand_ab3%in%c(5) ~1, TRUE ~0),
             treat_rep=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
             gender3=as.factor(gender3),
             race=as.factor(race),
             age4=as.factor(age4),
             pid3=as.factor(pid3),
             acsownrent=as.factor(acsownrent),
             region=as.factor(region),
             faminc=as.factor(faminc),
             educ4=as.factor(educ4),
             wave=1)%>%
      select(-weight,-Q16,-id,-x)
    
  } else if (wave %in% c(2)) {
    df <- df %>%    select(z,weight,Q11,presvote20post,Q6,race,gender4,region,age4,educ4,pid3,faminc,ownrent) %>%
      mutate(id=1:n(),Q10_binary=as.numeric(Q11==d),treat=z,rand_ab3=z) %>%
      mutate(treat=case_when(rand_ab3%in%c(4) ~1, TRUE ~0),
             treat_dem=case_when(rand_ab3%in%c(5) ~1, TRUE ~0),
             treat_rep=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
             gender3=as.factor(gender4),
             race=as.factor(race),
             age4=as.factor(age4),
             pid3=as.factor(pid3),
             acsownrent=as.factor(ownrent),
             region=as.factor(region),
             faminc=as.factor(faminc),
             educ4=as.factor(educ4),
             wave=wave)%>%
      select(-weight,-Q6,-Q11,-id,-ownrent,-z)
  }
  
}



# prepare pooled 2023 dataset
jun23_prep <- prepare_wave(jun23, wave = 1)
dec23_prep <- prepare_wave(dec23, wave = 2)
pooled23   <- bind_rows(jun23_prep, dec23_prep) %>%
  mutate(wave = as.factor(wave))

#Distrust
jun23_prepd <- prepare_wave(jun23, wave = 1,d=2)
dec23_prepd <- prepare_wave(dec23, wave = 2,d=2)
pooled23d   <- bind_rows(jun23_prepd, dec23_prepd) %>%
  mutate(wave = as.factor(wave))


# prepare 2022 dataset in similar form
nov22_prep <- nov22 %>%
  select(rand_ab3,weight,Q10,presvote20post,race,gender3,educ4,region,age4,educ,pid3,faminc,acsownrent)     %>%
  mutate(id=1:n(),Q10_binary=as.numeric(Q10==1)) %>%
  mutate(treat=case_when(rand_ab3%in%c(6) ~1, TRUE ~0),
         treat_dem=case_when(rand_ab3%in%c(3) ~1, TRUE ~0),
         treat_rep=case_when(rand_ab3%in%c(1) ~1, TRUE ~0),
         gender3=as.factor(gender3),
         race=as.factor(race),
         age4=as.factor(age4),
         pid3=as.factor(pid3),
         educ4=as.factor(educ4),
         acsownrent=as.factor(acsownrent),
         region=as.factor(region),
         faminc=as.factor(faminc),
         educ=as.factor(educ))%>%
  select(-weight,-Q10,-id)

### 2. Effect estimation -------------------------------------------------------

# estimate means and differences by voter group and treatment
estimate_effects <- function(df, vote_subset, treat_ids_dem, treat_ids_rep, neutral_ids) {
  dsub <- df %>% filter(presvote20post %in% vote_subset)
  # mean trust for each treatment
  means <- dsub %>%
    group_by(rand_ab3) %>%
    summarise(Trust = mean(Q10_binary, na.rm = TRUE),
              sd    = sd(Q10_binary, na.rm = TRUE),
              n     = sum(!is.na(Q10_binary)),
              se    = sd / sqrt(n)) %>%
    ungroup()
  # collapse to three arms: Democratic, Republican, Neutral
  dem_mean  <- means %>% 
    filter(rand_ab3 %in% treat_ids_dem)%>%
    mutate(treat=case_when(rand_ab3==treat_ids_dem[1] ~"people",
                           rand_ab3==treat_ids_dem[2] ~"sci")) %>%
    pivot_wider(names_from = treat, values_from = c(Trust,se,sd,n,rand_ab3))
  rep_mean  <- means %>% 
    filter(rand_ab3 %in% treat_ids_rep)%>%
    mutate(treat=case_when(rand_ab3==treat_ids_rep[1] ~"people",
                           rand_ab3==treat_ids_rep[2] ~"sci"))  %>%
   pivot_wider(names_from = treat, values_from = c(Trust,se,sd,n,rand_ab3)) 
  neut_mean <- means %>% 
    filter(rand_ab3 %in% neutral_ids)%>%
    mutate(treat=case_when(rand_ab3==neutral_ids[1] ~"people",
                           rand_ab3==neutral_ids[2] ~"sci"))  %>%
    pivot_wider(names_from = treat, values_from = c(Trust,se,sd,n,rand_ab3))
  
  list(dem   = dem_mean,
       rep   = rep_mean,
       neut  = neut_mean)
}

# apply Bonferroni correction by multiplying by n_tests (cap at 1)
adjust_p <- function(pvals, m) {
  pmin(pvals * m, 0.999)
}

# compute t-statistics and p-values for Democratic, Republican, and total effects

# n_tests: the number of hypothesis tests.  A Bonferroni correction multiplies
#          each raw p-value by n_tests and caps the result at one.  Standard
#          errors and t-statistics are unaffected by this adjustment.


compute_tstats <- function(effects, n_tests = 1) {
  # differences (neutral minus partisan) in percentage points
  neut_diff  <- (effects$neut$Trust_sci - effects$neut$Trust_people) * 100
  rep_diff  <- (effects$rep$Trust_sci - effects$rep$Trust_people) * 100
  dem_diff  <- (effects$dem$Trust_sci - effects$dem$Trust_people) * 100
  
  dem_latent_diff <- neut_diff - abs(dem_diff)
  rep_latent_diff <- neut_diff - abs(rep_diff)
  # pooled standard errors in percentage points
  n_rep  <- effects$rep$n_people+effects$rep$n_sci
  n_dem  <- effects$dem$n_people+effects$dem$n_sci
  n_neut <- effects$neut$n_people+effects$neut$n_sci
  n_latent_dem <- n_neut + n_dem
  n_latent_rep <- n_neut + n_rep
  
  neut_se    <- ((effects$neut$se_sci)*(effects$neut$n_sci/n_neut) +
                               (effects$neut$se_people)*(effects$neut$n_people/n_neut)) * 100
  dem_se    <- ((effects$dem$se_sci)*(effects$dem$n_sci/n_dem) + 
                      (effects$dem$se_people)*(effects$dem$n_people/n_dem)) * 100
  rep_se    <- ((effects$rep$se_sci)*(effects$rep$n_sci/n_rep) + 
                      (effects$rep$se_people)*(effects$rep$n_people/n_rep)) * 100
  
  latent_rep_se <- ((effects$neut$se_sci)*(n_neut/n_latent_rep) +
                               (effects$rep$se_people)*(effects$rep$n_people/n_latent_rep)) * 100
  
  latent_dem_se <- ((effects$neut$se_sci)*(n_neut/n_latent_dem) +
                               (effects$dem$se_people)*(effects$dem$n_people/n_latent_dem)) * 100
  # t-statistics
  t_neut    <- neut_diff / neut_se
  t_dem     <- dem_diff / dem_se
  t_rep     <- rep_diff / rep_se
  
  t_latent_rep  <- rep_latent_diff / latent_rep_se
  t_latent_dem  <- dem_latent_diff / latent_dem_se
  # total co‑partisan effect (difference between dem_diff and rep_diff)
  total_diff <- dem_diff - rep_diff
  total_se   <- (dem_se + rep_se)/2
  t_total    <- total_diff / total_se
  # compute two-sided p-values using large-sample approximation
  p_dem   <- 2 * pt(abs(t_dem), df = sum(effects$dem$n_people,
                                         effects$dem$n_sci), lower.tail = FALSE)
  p_rep   <- 2 * pt(abs(t_rep), df = sum(effects$rep$n_people,
                                         effects$rep$n_sci), lower.tail = FALSE)
  p_neut <- 2 * pt(abs(t_neut), df = sum(effects$neut$n_people,
                                          effects$neut$n_sci), lower.tail = FALSE)
  p_total <- 2 * pt(abs(t_total), df = sum(effects$dem$n_people,
                                          effects$dem$n_sci,
                                          effects$rep$n_people,
                                          effects$rep$n_sci
                                          ), lower.tail = FALSE)
  p_latent_rep <- 2 * pt(abs(t_latent_rep), df = n_latent_rep, lower.tail = FALSE)
  p_latent_dem <- 2 * pt(abs(t_latent_dem), df = n_latent_dem, lower.tail = FALSE)
  
  
## apply correction for multiple hypothesis tests
  p_dem       <- adjust_p(p_dem, n_tests)
  p_rep       <- adjust_p(p_rep, n_tests)
  p_neut       <- adjust_p(p_neut, n_tests)
  p_total      <- adjust_p(p_total, n_tests)
  p_latent_rep <- adjust_p(p_latent_rep, n_tests)
  p_latent_dem <- adjust_p(p_latent_dem, n_tests)
  
  
  return(list(dem_diff   = dem_diff,  dem_se = dem_se,  dem_t = t_dem,  dem_p = p_dem,
              rep_diff   = rep_diff,  rep_se = rep_se,  rep_t = t_rep,  rep_p = p_rep,
              neut_diff   = neut_diff,  neut_se = neut_se,  neut_t = t_neut,  neut_p = p_neut,
              total_diff = total_diff,total_se= total_se,total_t= t_total,total_p= p_total,
              latent_rep_diff = rep_latent_diff, latent_rep_se = latent_rep_se, latent_rep_t = t_latent_rep, latent_rep_p = p_latent_rep,
              latent_dem_diff = dem_latent_diff, latent_dem_se = latent_dem_se, latent_dem_t = t_latent_dem, latent_dem_p = p_latent_dem))
}


# ---- Helper function to reshape each tstat list ----
extract_tstats <- function(lst, study, condition) {
  # group names = unique prefixes
  groups <- unique(str_remove(names(lst), "_(diff|se|t|p)$"))
  
  df <- map_dfr(groups, function(g) {
    tibble(
      study     = study,
      condition = condition,
      group     = g,
      diff      = lst[[paste0(g, "_diff")]],
      se        = lst[[paste0(g, "_se")]],
      t         = lst[[paste0(g, "_t")]],
      p         = lst[[paste0(g, "_p")]]
    )
  })
  
  return(df)
}

# Helper function to relabel for table
relabel_groups <- function(df) {
  df %>%
    mutate(
      group = recode(
        group,
        dem        = "Direct Democratic Effect",
        rep        = "Direct Republican Effect",
        neut       = "Total Treatment Effect",
        total = "Partisan Difference",
        latent_rep = "Latent Republican Effect",
        latent_dem = "Latent Democratic Effect"
        # 'total' intentionally left as-is (not specified to rename)
      ),
      group = factor(
        group,
        levels = c(
          "Direct Democratic Effect",
          "Direct Republican Effect",
          "Total Treatment Effect",
          "Partisan Difference",                       # stays as provided
          "Latent Democratic Effect",
          "Latent Republican Effect"
        )
      )
    )
}


### 3. Apply to Study 1 (Nov 2022 wave) ----------------------------------------
# Note: treat_ids are based on how the treatments were coded in 2022:
#       3 = Democratic messenger, 1 = Republican messenger, 5/6 = neutral messages.

#H1
study1_effects_all   <- estimate_effects(nov22_prep, vote_subset = c(1, 2),
                                         treat_ids_dem = c(4,3), treat_ids_rep = c(2,1),
                                         neutral_ids = c(5,6))



#H2
study1_effects_biden <- estimate_effects(nov22_prep, vote_subset = 1,
                                         treat_ids_dem = c(4,3), treat_ids_rep = c(2,1),
                                         neutral_ids = c(5,6))
study1_effects_trump <- estimate_effects(nov22_prep, vote_subset = 2,
                                         treat_ids_dem = c(4,3), treat_ids_rep = c(2,1),
                                         neutral_ids = c(5,6))

#H3
study1_effects_all_high_ed   <- estimate_effects(nov22_prep %>%filter(educ4 %in% c(3,4)), 
                                                 vote_subset = c(1, 2),
                                                 treat_ids_dem = c(4,3), treat_ids_rep = c(2,1),
                                                 neutral_ids = c(5,6))

study1_effects_all_low_ed   <- estimate_effects(nov22_prep %>%filter(educ4 %in% c(1,2)), 
                                                vote_subset = c(1, 2),
                                                treat_ids_dem = c(4,3), treat_ids_rep = c(2,1),
                                                neutral_ids = c(5,6))


# H1
study1_tstats_all   <- compute_tstats(study1_effects_all,n_tests = 6)

# We run three statistical tests for effects here

#H2

study1_tstats_biden <- compute_tstats(study1_effects_biden, n_tests = 6)
study1_tstats_trump <- compute_tstats(study1_effects_trump, n_tests = 6)


#H3

# Two tests (latent effects for high and low education groups)
study1_tstats_all_high_ed <-compute_tstats(study1_effects_all_high_ed, n_tests = 2)
study1_tstats_all_low_ed  <-compute_tstats(study1_effects_all_low_ed, n_tests = 2)  

### 4. Apply to Study 2 (pooled June/Dec 2023 data) ----------------------------
# In 2023 waves, IDs 2,5=Democrat, 3,6=Republican, 1,4=Neutral.

#H1
study2_effects_all   <- estimate_effects(pooled23, vote_subset = c(1, 2),
                                         treat_ids_dem = c(2,5), treat_ids_rep = c(3,6),
                                         neutral_ids = c(1,4))

#H2
study2_effects_biden <- estimate_effects(pooled23, vote_subset = 1,
                                         treat_ids_dem = c(2,5), treat_ids_rep = c(3,6),
                                         neutral_ids = c(1,4))
study2_effects_trump <- estimate_effects(pooled23, vote_subset = 2,
                                         treat_ids_dem = c(2,5), treat_ids_rep = c(3,6),
                                         neutral_ids = c(1,4))

#H2.Distrust
study2_effects_bidend <- estimate_effects(pooled23d, vote_subset = 1,
                                         treat_ids_dem = c(2,5), treat_ids_rep = c(3,6),
                                         neutral_ids = c(1,4))
study2_effects_trumpd <- estimate_effects(pooled23d, vote_subset = 2,
                                         treat_ids_dem = c(2,5), treat_ids_rep = c(3,6),
                                         neutral_ids = c(1,4))


#H3
study2_effects_all_high_ed   <- estimate_effects(pooled23 %>%filter(educ4 %in% c(3,4)), vote_subset = c(1, 2),
                                                 treat_ids_dem = c(2,5), treat_ids_rep = c(3,6),
                                                 neutral_ids = c(1,4))

study2_effects_all_low_ed   <- estimate_effects(pooled23 %>%filter(educ4 %in% c(1,2)), vote_subset = c(1, 2),
                                                treat_ids_dem = c(2,5), treat_ids_rep = c(3,6),
                                                neutral_ids = c(1,4))

#H1
study2_tstats_all   <- compute_tstats(study2_effects_all,n_tests = 6)

#H2
study2_tstats_biden <- compute_tstats(study2_effects_biden,n_tests = 2)
study2_tstats_trump <- compute_tstats(study2_effects_trump,n_tests = 2)


#H2.Distrust

study2_tstats_bidend <- compute_tstats(study2_effects_bidend,n_tests = 4)
study2_tstats_trumpd <- compute_tstats(study2_effects_trumpd,n_tests = 4)


#H3
study2_tstats_all_high_ed <-compute_tstats(study2_effects_all_high_ed,n_tests = 6)
study2_tstats_all_low_ed  <-compute_tstats(study2_effects_all_low_ed,n_tests = 6)  

### 5. Combine and label p-values ---------------------------------------------

p_values <- list(
  # Study 1 (Nov 2022)
  study1_all_neut_p  = study1_tstats_all$neut_p,
  study1_all_dem_p    = study1_tstats_all$dem_p,
  study1_all_rep_p    = study1_tstats_all$rep_p,
  study1_all_total_p  = study1_tstats_all$total_p,
  
  study1_biden_dem_p   = study1_tstats_biden$dem_p,
  study1_biden_rep_p   = study1_tstats_biden$rep_p,
  study1_biden_total_p = study1_tstats_biden$total_p,
  study1_biden_neut_p  = study1_tstats_biden$neut_p,
  
  study1_trump_dem_p  = study1_tstats_trump$dem_p,
  study1_trump_rep_p  = study1_tstats_trump$rep_p,
  study1_trump_total_p= study1_tstats_trump$total_p,
  study1_trump_neut_p= study1_tstats_trump$neut_p,
  
  
  # Study 2 (Jun/Dec 2023 pooled)
  study2_all_neut_p    = study2_tstats_all$neut_p,
  study2_all_dem_p    = study2_tstats_all$dem_p,
  study2_all_rep_p    = study2_tstats_all$rep_p,
  study2_all_total_p  = study2_tstats_all$total_p,
  
  study2_biden_dem_p  = study2_tstats_biden$dem_p,
  study2_biden_rep_p  = study2_tstats_biden$rep_p,
  study2_biden_total_p= study2_tstats_biden$total_p,
  study2_biden_neut_p= study2_tstats_biden$neut_p,
  
  study2_trump_dem_p  = study2_tstats_trump$dem_p,
  study2_trump_rep_p  = study2_tstats_trump$rep_p,
  study2_trump_total_p= study2_tstats_trump$total_p,
  study2_trump_neut_p= study2_tstats_trump$neut_p,
  
  # Core Latent Effects
  ##Overall
  study2_all_latent_dem_p    = study2_tstats_all$latent_dem_p,
  study2_all_latent_rep_p    = study2_tstats_all$latent_rep_p,
  ## Biden
  study2_biden_latent_dem_p  = study2_tstats_biden$latent_dem_p,
  study2_biden_latent_rep_p  = study2_tstats_biden$latent_rep_p,
  ## Trump
  study2_trump_latent_dem_p  = study2_tstats_trump$latent_dem_p,
  study2_trump_latent_rep_p  = study2_tstats_trump$latent_rep_p
  
)

print(p_values)



# ---- Build tables for study 1 ----
df1_all   <- extract_tstats(study1_tstats_all,   "Study 1", "All")

df1_biden <- extract_tstats(study1_tstats_biden, "Study 1", "Biden")
df1_trump <- extract_tstats(study1_tstats_trump, "Study 1", "Trump")

df1_highed <- extract_tstats(study1_tstats_all_high_ed,   "Study 1", "College +")
df1_lowed  <-extract_tstats(study1_tstats_all_low_ed,   "Study 1", "HS or Less")




# ---- Build tables for Pooled Study ----
df2_all   <- extract_tstats(study2_tstats_all,   "Pooled", "All")

df2_biden <- extract_tstats(study2_tstats_biden, "Pooled", "Biden")
df2_trump <- extract_tstats(study2_tstats_trump, "Pooled", "Trump")

df2_bidend <- extract_tstats(study2_tstats_bidend, "Pooled - Distrust", "Biden")
df2_trumpd <- extract_tstats(study2_tstats_trumpd, "Pooled - Distrust", "Trump")

df2_highed <- extract_tstats(study2_tstats_all_high_ed,   "Pooled", "College +")
df2_lowed  <-extract_tstats(study2_tstats_all_low_ed,   "Pooled", "HS or Less")

# If you also have study2_tstats_all/biden/trump, repeat here
# df2_all   <- extract_tstats(study2_tstats_all,   "Study 2", "All")
# ...

# ---- Combine ----
results_df <- bind_rows(df1_all, df1_biden, df1_trump,
                        df2_all, df2_biden, df2_trump,
                        df1_highed,df1_lowed,df2_highed,
                        df2_lowed, df2_bidend,df2_trumpd ) %>%
  mutate(
    diff_se = sprintf("%.2f (%.2f)", diff, se),
    p_fmt   = ifelse(p < .001, formatC(p, format = "e", digits = 2),
                     sprintf("%.3f", p))
  ) %>%
  select(study, condition, group, diff_se, t, p_fmt)

results_df <- relabel_groups(results_df)

# ---- Pretty table ----
results_table <- results_df %>%
  gt(rowname_col = "group") %>%
  tab_header(
    title    = "Treatment Effects by Study and Condition",
    subtitle = "Estimates with Standard Errors, t-statistics, and p-values"
  ) %>%
  cols_label(
    study     = "Study",
    condition = "Population",
    diff_se   = "Estimate (SE)",
    t         = "t-statistic",
    p_fmt     = "p-value"
  )


print(results_table)

cat(as.character(gt::as_latex(results_table)))
gt::gtsave(ed_latent_results_table1, "Tables/latent_effects_ALL.tex")


# Pooled H1 and H2

# ---- Combine ----
results_df2p <- bind_rows(df2_all, df2_biden, df2_trump,) %>%
  mutate(
    diff_se = sprintf("%.2f (%.2f)", diff, se),
    p_fmt   = ifelse(p < .001, formatC(p, format = "e", digits = 2),
                     sprintf("%.3f", p))
  ) %>%
  filter(condition=="All") |>
  select(study, condition, group, diff_se, t, p_fmt)

  results_df2p <- relabel_groups(results_df2p)
  
results_df2p <- relabel_groups(results_df2p)

# ---- Pretty table ----
results_table2p <- results_df2p %>%
  gt(rowname_col = "group") %>%
  tab_header(
    title    = "Treatment Effects by Study and Condition",
    subtitle = "Estimates with Standard Errors, t-statistics, and p-values"
  ) %>%
  cols_label(
    study     = "Study",
    condition = "Population",
    diff_se   = "Estimate (SE)",
    t         = "t-statistic",
    p_fmt     = "p-value"
  )


print(results_table2p)
cat(as.character(gt::as_latex(results_table2p)))
gt::gtsave(ed_latent_results_table2, "Tables/latent_effects_pooled.H1.H2.tex")


## H3 Table

#Study 1
ed_latent_results_df1 <- bind_rows(
                        df1_highed,df1_lowed) %>%
  mutate(
    diff_se = sprintf("%.2f (%.2f)", diff, se),
    p_fmt   = ifelse(p < .001, formatC(p, format = "e", digits = 2),
                     sprintf("%.3f", p))
  ) %>%
  select(study, condition, group, diff_se, t, p_fmt)

ed_latent_results_df1 <- relabel_groups(ed_latent_results_df1) |> 
  filter(group%in%c("Latent Republican Effect","Latent Democratic Effect"))

# ---- Pretty table ----
ed_latent_results_table1 <- ed_latent_results_df1 %>%
  gt(rowname_col = "group") %>%
  tab_header(
    title    = "Treatment Effects by Study and Condition",
    subtitle = "Estimates with Standard Errors, t-statistics, and p-values"
  ) %>%
  cols_label(
    study     = "Study",
    condition = "Population",
    diff_se   = "Estimate (SE)",
    t         = "t-statistic",
    p_fmt     = "p-value"
  )

print(ed_latent_results_table1)

# Pooled

ed_latent_results_df2 <- bind_rows(
  df2_highed,df2_lowed) %>%
  mutate(
    diff_se = sprintf("%.2f (%.2f)", diff, se),
    p_fmt   = ifelse(p < .001, formatC(p, format = "e", digits = 2),
                     sprintf("%.3f", p))
  ) %>%
  select(study, condition, group, diff_se, t, p_fmt)

ed_latent_results_df2 <- relabel_groups(ed_latent_results_df2) |> 
  filter(group%in%c("Latent Republican Effect","Latent Democratic Effect"))

# ---- Pretty table ----
ed_latent_results_table2 <- ed_latent_results_df2 %>%
  gt(rowname_col = "group") %>%
  tab_header(
    title    = "Treatment Effects by Study and Condition",
    subtitle = "Estimates with Standard Errors, t-statistics, and p-values"
  ) %>%
  cols_label(
    study     = "Study",
    condition = "Population",
    diff_se   = "Estimate (SE)",
    t         = "t-statistic",
    p_fmt     = "p-value"
  )

print(ed_latent_results_table2)


cat(as.character(gt::as_latex(ed_latent_results_table1)))
gt::gtsave(ed_latent_results_table1, "Tables/latent_effects_study1.tex")

cat(as.character(gt::as_latex(ed_latent_results_table2)))
gt::gtsave(ed_latent_results_table2, "Tables/latent_effects_pooled.tex")


# Distrust

results_df2d <- bind_rows(
  df2_bidend,df2_trumpd) %>%
  mutate(
    diff_se = sprintf("%.2f (%.2f)", diff, se),
    p_fmt   = ifelse(p < .001, formatC(p, format = "e", digits = 2),
                     sprintf("%.3f", p))
  ) %>%
  select(study, condition, group, diff_se, t, p_fmt)

results_df2d <- relabel_groups(results_df2d) |> 
  filter(group%in%c("Latent Republican Effect","Latent Democratic Effect"))

# ---- Pretty table ----
results_table2d <- results_df2d %>%
  gt(rowname_col = "group") %>%
  tab_header(
    title    = "Treatment Effects by Study and Condition",
    subtitle = "Estimates with Standard Errors, t-statistics, and Bonferroni Corrected p-values"
  ) %>%
  cols_label(
    study     = "Study",
    condition = "Population",
    diff_se   = "Estimate (SE)",
    t         = "t-statistic",
    p_fmt     = "Bonferroni Corrected p-value"
  )

print(results_table2d)
cat(as.character(gt::as_latex(results_table2d)))
gt::gtsave(ed_latent_results_table2, "Tables/latent_effects_pooled_distrust.tex")


