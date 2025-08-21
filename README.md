# Replication README for Caltech Climate Study Analysis

This README provides a detailed overview of the data, code, and steps required to reproduce the figures and tables in the Caltech climate study replication.  It draws on the cleaned and re‐ordered R script (`figs.R`), the associated survey data files (three `.sav` files), and the LaTeX manuscript.  The goal of the script is to reproduce the descriptive statistics and treatment effect estimates used in the paper.

## 1. Data sources

The replication relies on three survey waves collected by the California Institute of Technology (Caltech) as part of a study on **trust in science and science communicators**.  Each wave contains participants randomly assigned to one of six messages about climate change, presented by a particular messenger and party affiliation.  The datasets used are:

| File                        | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `CalTech_November_2022.sav` | The **November 2022 wave**, fielded shortly after the 2022 mid‑term elections.  Each respondent was exposed to one of six messages about climate change: scientists from the Democratic Party, scientists from the Republican Party, people from the Democratic Party, people from the Republican Party, scientists (party unspecified), or people (party unspecified).  Key variables include: `rand_ab3` (treatment arm), `weight` (survey weights), `Q10` (trust outcome), `presvote20post` (2020 presidential vote: 1 = Joe Biden, 2 = Donald Trump), and demographics (`race`, `gender3`, `region`, `age4`, `educ`, `pid3`, `faminc`, `acsownrent`). |
| `CalTech_June_2023.sav`     | The **June 2023 wave**.  This wave used variables `x` (treatment assignment), `Q16` (trust outcome) and replaced `Q10` with `Q16` and `rand_ab3` with `x`.  The outcome is coded to `Q10_binary` in the script.  Demographic variables largely match the November 2022 wave.                                                                                                                                                                                                                                                                                                                                                                              |
| `caltech_climate_dec23.sav` | The **December 2023 wave**.  Variables include `z` (treatment assignment), `Q11` (trust outcome), and a set of demographic variables.  The script renames `Q11` to `Q10` and `z` to `rand_ab3` for consistency.  An additional variable `ownrent` distinguishes home ownership.                                                                                                                                                                                                                                                                                                                                                                           |

Each `.sav` file is a SPSS file.  The replication script uses the `haven` package to load them into R.  Survey weights are provided in each file and are used to compute representative averages.

## 2. Project structure

The replication repository contains:

* **`figs.R`** – the fully ordered R script that prepares the data, computes treatment effects and latent effects, generates all figures (Figures 3a–7d), performs robustness checks, and saves results as PDF plots and LaTeX tables.  The original code has been reorganised so that figures appear in the order they are referenced in the paper.
* **`Data/`** – a directory expected to hold the three `.sav` data files.  This directory must be created manually and populated with the survey data.
* **`Plots/`** – a directory where the script writes PDF figures.  Each figure corresponds to a panel in the manuscript (e.g., `study1.levels.soils.pdf`, `biden.study1_treatment_effects_soils.pdf`, etc.).  If the directory does not exist, R will create it when running the script.
* **`Tables/`** – (optional) the script writes LaTeX table files (via `xtable`) into this directory.  These can be included in the manuscript with `\input{}` commands.
* **`main.tex`** – the LaTeX manuscript (not included in this repository listing).  It refers to the generated figures and tables.  After running the R script, compile this file with `pdflatex` to produce the final paper.

## 3. Software requirements

To reproduce the analysis, you need:

1. **R (≥ 4.2)** with the following packages.  Install them with `install.packages()` if they are not already present:

   * `haven` – reads SPSS `.sav` files.
   * `survey` – handles survey design objects and weighted statistics.
   * `tidyverse` (including `dplyr`, `ggplot2`, `forcats`, etc.) – data manipulation and plotting.
   * `RColorBrewer` – colour palettes for figures.
   * `gtsummary` and `gt` – summary tables (optional).
   * `margins` – computing marginal effects (used in parts of the original script).
   * `latex2exp` – renders LaTeX expressions in plot annotations.
   * `labelled` – manages labelled variables from SPSS files.
   * `xtable` – exports tables to LaTeX.
     You can install all dependencies in one call:

   ```R
   install.packages(c(
     "haven", "survey", "tidyverse", "RColorBrewer", "gtsummary", 
     "margins", "latex2exp", "labelled", "xtable"
   ))

   ```

   1. **LaTeX** distribution (e.g., TeX Live, MiKTeX) to compile `main.tex`. The LaTeX file expects that the PDF figures and any tables generated by the R script are available in the appropriate subdirectories.

   ## 4. Running the analysis

   Follow these steps to reproduce the figures and tables:

   1. **Upon Release of the Data** – place `CalTech_November_2022.sav`, `CalTech_June_2023.sav` and `caltech_climate_dec23.sav` in a directory called `Data/` at the project root. The script assumes this path when reading the files.
   2. **Launch R** – open an R session in the project root.
   3. **Source the script** – run the script to load, process, and analyse the data:

      ```
      source("figs.R")

      ```

      The script will load the data, recode variables to ensure consistency across waves, compute survey-weighted means and treatment effects, and generate a series of figures. Each figure corresponds to a panel in the paper:

      * **Figure 3a** – Levels of trust and distrust (November 2022 wave) for each treatment arm, separated by messenger type (scientists/people) and party affiliation (Democratic/Republican/neutral).
      * **Figure 3b** – Estimated treatment effects of co‑partisan and cross‑partisan messages on trust (all voters, Study 1), showing point estimates, confidence intervals, and the total treatment effect.
      * **Figure 3c** – Latent effects (differences of differences) for all voters (Study 1).
      * **Figure 4** – Levels of trust by 2020 vote choice (Biden vs. Trump voters) for each treatment arm (Study 1).
      * **Figures 5a and 5b** – Treatment and latent effects for **Biden voters** in the 2022 wave.
      * **Figures 5c and 5d** – Treatment and latent effects for **Trump voters** in the 2022 wave.
      * **Figure 6** – Levels of trust and distrust in the pooled 2023 dataset (June 2023 and December 2023 waves combined).
      * **Pooled all‐voters plots** – Following Figure 6, the script computes treatment and latent effects for all voters in the pooled 2023 data (no figure number in the manuscript). It generates `study2_treatment_effects_soils.pdf` and `study2_latent_effects_soils.pdf`.
      * **Figures 7a and 7b** – Treatment and latent effects for **Biden voters** in the pooled 2023 data.
      * **Figures 7c and 7d** – Treatment and latent effects for **Trump voters** in the pooled 2023 data.
        Two additional robustness plots are created at the end of the script:
      * **robust.swap.levels.votechoice2020.soils.first.pdf** – uses only the June 2023 wave and examines whether respondents answered the attention check (`Q20`) consistently with the trust question (`Q16`). It recomputes trust levels after dropping mismatched responses.
      * **robust.swap.levels.votechoice2020.soils.repeat.pdf** – repeats the analysis using a different wording of the trust question (`Q20`) in the June 2023 wave, ensuring that results are not driven by question order or wording.
        All PDF files are saved into the `Plots/` directory. If the directory does not exist, R will create it automatically. The script also writes tables of regression results (e.g., `pooled_all.tex`, `pooled_biden.tex`, `pooled_trump.tex`) into `Tables/` if the corresponding code blocks are uncommented.
   4. **Compile the manuscript** (optional) – after running the script, compile `main.tex` with a LaTeX engine. The file will include the generated figures and tables via `\includegraphics` and `\input` commands. Ensure that the relative paths (`Plots/...` and `Tables/...`) are correct.

   ## 5. Understanding the code

   The R script follows a clear structure:

   1. **Setup and data loading** – loads libraries and reads the `.sav` files using `haven::read_sav()`. Creates survey design objects with `survey::svydesign()` and constructs an initial data frame (`df`) for Study 1.
   2. **Balance testing** (commented out in the re‑ordered script) – initial sections compute balance statistics to check random assignment across demographic variables. These are not run by default but can be used to replicate the balance tables.
   3. **Study 1 analysis** (November 2022 wave):

      * *Figure 3a* – calculates weighted means of trust and distrust for each treatment arm and plots them by messenger type and party. Uses LaTeX expressions (via `latex2exp`) to annotate the bars.
      * *Figure 3b* – fits linear models (using `lm()`) to estimate the effect of being exposed to a Democratic messenger vs. neutral, a Republican messenger vs. neutral, and a neutral messenger vs. co‑partisan. It computes differences of means and standard errors manually and produces a bar plot showing direct and latent components of the treatment effect.
      * *Figure 3c* – isolates the latent effects (differences‐in‐differences) and displays them separately with confidence intervals.
      * *Figure 4* – repeats the trust and distrust analysis but splits the sample by 2020 vote choice (Biden vs. Trump voters). Treatment arms are grouped into non‑partisan, co‑partisan and cross‑partisan categories.
      * *Figures 5a–5d* – replicates the above analyses separately for Biden voters (5a / 5b) and Trump voters (5c / 5d), producing treatment and latent effects for each group.
   4. **Study 2 analysis** (pooled June 2023 + December 2023 waves):

      * *Data preparation* – recodes the June and December files to align variable names (`Q16`/`Q11` → `Q10`, `x`/`z` → `rand_ab3`). It merges the two waves and assigns a wave indicator (`wave = 1` for June and `wave = 2` for December).
      * *Figure 6* – computes trust levels by treatment and party in the pooled data.
      * *Pooled all-voters effects* – estimates treatment and latent effects across all voters. Although not explicitly numbered in the manuscript, these plots complement Figure 6.
      * *Figures 7a–7d* – performs the same treatment and latent effect estimation for Biden (7a / 7b) and Trump (7c / 7d) voters in the pooled data.
   5. **Robustness checks** – two blocks at the end of the script reproduce the levels analysis using only the June 2023 wave. The first (attention check) drops respondents whose answers to the attention check (`Q20`) do not match the trust question (`Q16`). The second redefines the trust outcome using `Q20` instead of `Q16`. These analyses ensure that the main results are not sensitive to question wording or inattentive respondents.

   ## 6. Interpretation of variables

   To aid replication, here is a short description of key variables used in the script:

   | VariableMeaning                                                                                   |                                                                                                                                                                                                                                                                                                                                                                                          |
   | ------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
   | `rand_ab3` / `x` / `z`                                                                            | Treatment assignment. Values 1–6 correspond to combinations of messenger type (scientists vs. people) and party affiliation (Democratic vs. Republican vs. neutral). In the script, `treat` is 1 when `rand_ab3` equals 4 (co‑partisan neutral), `treat_dem` is 1 when `rand_ab3` equals 5 (Democratic messenger), and `treat_rep` is 1 when `rand_ab3` equals 6 (Republican messenger). |
   | `Q10`, `Q16`, `Q11`                                                                               | Outcome variables measuring trust in the messenger. Coded such that 1 indicates **trust** and any other response indicates **distrust**. The script recodes this into `Q10_binary` (1 = trust; 0 = distrust).                                                                                                                                                                            |
   | `presvote20post`                                                                                  | Self‑reported 2020 presidential vote: 1 = Joe Biden voter, 2 = Donald Trump voter. Observations where respondents did not vote or preferred not to answer are generally excluded from the analyses.                                                                                                                                                                                      |
   | `weight`                                                                                          | Survey weight. Used with the `survey` package to compute weighted means and standard errors.                                                                                                                                                                                                                                                                                             |
   | `race`, `gender3`, `gender4`, `region`, `age4`, `educ`, `pid3`, `faminc`, `acsownrent`, `ownrent` | Demographic covariates. They are converted to factors for modelling and, in some analyses, included in balance checks (commented out).                                                                                                                                                                                                                                                   |
   | `Q20`                                                                                             | Attention check question used in June 2023. In the robustness check, respondents with mismatching answers on `Q20` and `Q16` are removed. In the second robustness check, `Q20` itself is used as the trust outcome.                                                                                                                                                                     |

   ## 7. Troubleshooting and tips

   * **Missing packages** – If you encounter errors such as *“there is no package called 'xyz'”*, install the missing package using `install.packages("xyz")`.
   * **Directory structure** – Ensure that the `Data/`, `Plots/` and `Tables/` directories exist at the top level of the project. The script references relative paths; failing to set up these folders will result in errors when saving files.
   * **SPSS file encoding** – The `haven` package should handle labelled SPSS files automatically. If you see encoding warnings, you can set `options(haven_show_nonlabelled = FALSE)` to suppress them. Use `as_factor()` from the `labelled` package to convert labelled variables to factors.
   * **Survey design** – For descriptive plots (Figures 3a, 4 and 6), the script computes means by simply weighting observations. For treatment effect estimates (Figures 3b–3c, 5a–7d), it fits linear models using base R’s `lm()` rather than the `survey` package. This approach mirrors the paper but treats the survey weights as frequency weights. If you wish to incorporate the complex survey design, replace `lm()` with `svyglm()`.
   * **Reproducing tables** – The script includes commented code sections that use the `balance_regression()` function (not defined in the script) and `xtable` to generate balance tables. Uncomment these sections if you wish to replicate the balance tests and save LaTeX tables.
   * **LaTeX compilation** – After running the script, compile `main.tex` with a LaTeX engine. Ensure that your LaTeX distribution can find the `Plots/` and `Tables/` directories. Use `pdflatex main.tex` or `latexmk` for automated compilation.

   ## 8. License and acknowledgements

   This replication code is provided for educational and research purposes. The data were collected by the California Institute of Technology (Caltech) as part of a study on climate communication. Please cite the original paper if you use this code or data in your own research.

   If you encounter any issues or have questions about this replication, please contact the authors or open an issue in the project repository.
