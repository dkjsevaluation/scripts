library(taskscheduleR)
path <- "C:/testo/Monitoring_parse_outlook_cal.R"
taskscheduler_create(taskname = "test_run", rscript = path,
                     schedule = c("ONCE"), starttime = format(Sys.time() + 15, "%H:%M"))
taskscheduler_delete("test_run")
