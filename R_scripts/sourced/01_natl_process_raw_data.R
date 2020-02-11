# Script to read in excel sheet of raw data, which has 
# and turn it into a _processed.csv file, with
# all SETs in one table


# file_path determined in calling script


# read in all sheets and append them to each other
dat <- file_path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df( ~ read_excel(path = file_path, 
                             sheet = .x, 
                             col_types = "text"), 
                .id = "sheet")

# check to make sure the SET ID that was entered matches the name of each sheet
mismatches <- dat$set_id != dat$sheet

# make this whole script stop if something doesn't match
if(sum(mismatches) > 0){
        print(dat[mismatches, c("sheet", "set_id", "year", "month", "arm_position")])
        stop("There are SET IDs that do not match the sheet name. Please check and correct the rows printed above before proceeding.")
        }else{
# if no problem, do everything else

# first, format the data:
# get rid of the "sheet" column
# make sure date is date format; 
# several columns should be character: set_id, arm_position, 
# and anything that ends in qaqc_code
dat_formatted <- dat %>% 
        select(-sheet) %>% 
        mutate_at(c("set_id", "arm_position"), as.character) %>% 
        mutate_at(vars(ends_with("qaqc_code")), as.character)


# have to change column names first because there are too many underscores
names(dat_formatted) <- gsub("qaqc_code", "qaqccode", names(dat_formatted))
names(dat_formatted) <- gsub("pin_", "pin", names(dat_formatted))
names(dat_formatted) <- gsub("height_", "height", names(dat_formatted))

# set up to pivot
spec <- dat_formatted %>% 
        build_longer_spec(
                cols = starts_with("pin1_height"):"pin9_qaqccode",
                names_to = c("pinnumber", ".value"),
                names_sep = "_"
        )

# pivot to longer
dat_long <- dat_formatted %>% 
        pivot_longer_spec(spec = spec)

# put underscores back in the names
names(dat_long) <- gsub("pin", "pin_", names(dat_long))
dat_long$pin_number <- gsub("pin", "pin_", dat_long$pin_number)
names(dat_long) <- gsub("qaqccode", "qaqc_code", names(dat_long))
names(dat_long) <- gsub("height", "height_", names(dat_long))

# format and arrange before output
dat_long <- dat_long %>% 
        select(set_id, year, month, day, arm_position, arm_qaqc_code, 
               pin_number, starts_with("height"), qaqc_code, everything()) %>% 
        arrange(set_id, year, month, day, arm_position, pin_number)

# generate output and write out file
file_in <- str_extract(file_path, "[:alpha:]+\\.xls")
file_out <- paste0(substr(file_in, 1, nchar(file_in)-4), "_processed.csv")
out_path <- processed_path  # defined in calling script
file_out <- paste0(out_path, "/", file_out)
write.csv(dat_long, file_out, row.names = FALSE)
        }

