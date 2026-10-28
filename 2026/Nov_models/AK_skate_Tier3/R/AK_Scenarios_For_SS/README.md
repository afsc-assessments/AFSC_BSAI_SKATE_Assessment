 # Alaska Projection Scenarios for Stock Synthesis

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

Version: December 1, 2025

Created by Steve Barbeaux  
E-mail: steve.barbeaux@noaa.gov  
Phone: ‪(206) 526-4211‬

This function performs the 7 Alaska projection scenarios for Stock Synthesis 3 for tier 3 models. It runs the scenarios, saves them to the working directory, and produces stock figures for the projections. However, please note that this function could use some work on generalization and making the figures more suitable for assessment purposes.

## Usage

In the `starter.ss` file, make sure to change it to read from the converged `.par` file:
  1 # 0=use init values in control file; 1=use ss.par  
 
Assumptions:
- You have already specified the forecast parameters appropriately in the `forecast.ss` file for scenario 1.
- There is no catch or F already specified in the forecast file.

Parameters:
- `DIR`: Model directory
- `CYR`: Model current year
- `SYR`: Start year for the model
- `SEXES`: Number of sexes in the model
- `fleets`: The fleet number/s in SS for your fisheries
- `Scenario2`: Indicates whether you wish to have a different catch for scenario 2 (1 = FmaxABC, 2 = F as S2_F, 3 = specified catch from a formatted CSV saved in the root directory named 'Scenario2_catch.csv', must have an entry for each year, season, and fleet for the years that differ from Fmaxabc with columns "Year, Seas, Fleet, Catch_or_F")
- `s4_F` is the F for scenario 4, defaults to 0.75, should be 0.65 for some species check your requirments
- `do_fig`: Whether to plot figures
- `do_mark`  whether to make markdown tables
- `URL` is the url address of the previous stock assessment document
- `pdf_tab` is the table number in the pdf to collect executive summary table data
- `init_dir` is the director of the AK_SCENARIOS_FOR_SS files.

## Output

<B>SSB</B><br> 
This provides all output from the model start year to the final projection year as specified in the forecast file for each scenario. Table with columns Yr, TOT, SUMM, SSB, std, F, Catch, SSB_unfished, and model. Yr = year, TOT = total biomass, SUMM= Summary biomass as specified in the starter file, SSB is the female spawning biomass, std = standard deviation of the female spawning biomass, F = sum apical F, Catch = total catch, SSB_unfished =  unfished female spawning biomass, and model = Scenario.    

<B>CATCH</B><br>
This provides projected catch estimates for each scenario. Table with columns Yr, Catch, Catch_std, and model, Yr = year, Catch = total catch, Catch_std = standard deviation of the catch, and model = scenario. 

<B>Two_year</B><br>
Table with projections out two years. Table with columns Yr, SSB, SSB_PER, SB100, SB40, SB35, F40, F35, C_ABC, C_OFL. 

<B>Tables</B><br>
List with two three tables including Catch, F, and SSB corresponsing to the standard projection tables included in the stock assessment 

<B>FIGS</B><br>
List with two sets of figures including standard projection figures showing spawning biomass and catch projections. 

Example figures:<br>
<B><I>SSB</I></B><br>
![image](https://github.com/afsc-assessments/AK_Scenarios_For_SS/assets/5395237/379a0331-8757-486f-81c0-5f38228a0bfc)

![image](https://github.com/afsc-assessments/AK_Scenarios_For_SS/assets/5395237/baf2959e-9d0a-4e7b-87f0-d79ae783152f)

<B><I>Catch</I></B><br>
![image](https://github.com/afsc-assessments/AK_Scenarios_For_SS/assets/5395237/e1130b51-b94b-416e-b383-69741d71b030)
![image](https://github.com/afsc-assessments/AK_Scenarios_For_SS/assets/5395237/bb202bdc-b566-4ffb-9401-02b46eec726a)

Example usage:
```R
profiles <- Do_AK_TIER_3_Scenarios(DIR = "Model_23.1.0.d", CYR = 2023, SYR = 1977,  SEXES = 1, FLEETS = 1, Scenario2 = 1, S2_F = 0.4,
					s4_F = 0.75, do_fig = TRUE, do_mark=TRUE,URL="https://apps-afsc.fisheries.noaa.gov/Plan_Team/2022/EBSpcod.pdf",
					pdf_tab=1, init_dir=" C:/Users/steve.barbeaux/Work/GitHub/AK_Scenarios_For_SS")
