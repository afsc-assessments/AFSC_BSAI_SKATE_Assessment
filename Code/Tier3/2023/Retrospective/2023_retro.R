library(r4ss)
library(readr)

# Run retrospective analysis----
# Note: this was run in a separate folder following guidance from package instructions, copied to .Rproj directory
M14_2_ret <-retro(dir = paste0(getwd(), "/M14_2_update"),
                      oldsubdir = "",
                      newsubdir = "retrospectives",
                      subdirstart = "retro",
                      years = 0:-10,
                      verbose = TRUE,
                      exe = "ss_win.exe")


# Mohns Rho calc ----
setwd(paste0(getwd(), "/M14_2_update/retrospectives"))
ret_out <- SSgetoutput(dirvec = c("retro0","retro-1", "retro-2", 'retro-3', 'retro-4', 'retro-5', 'retro-6',
                                  'retro-7', 'retro-8', 'retro-9', 'retro-10'))
ret_summ <- SSsummarize(ret_out)

MR_out <- SSmohnsrho(ret_summ)
MR_out <- as.data.frame(unlist(MR_out))
MR_out$Metric <- rownames(MR_out)
write_csv(MR_out, paste0(getwd(), "/Mohns_Rho_M14_2.csv"))

# Plot retrospective----
endyrvec <- ret_summ[["endyrs"]]+ 0:-10
SSplotComparisons(ret_summ,
                  endyrvec = endyrvec,
                  print = TRUE,
                  plotdir = here::here(getwd()),
                  subplots = c(1, 9),
                  legend = F)


