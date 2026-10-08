# Lab 2: is this file ready to analyze?

library(tidyverse)


# you're a junior analyst at a state library association. a colleague
# downloaded the 2024 Public Libraries Survey and wants to compare
# library systems on visits, circulation, and programs.

# your supervisor wants an audit before anyone analyzes anything.

# nothing today is new. you've done every piece of this already.


# grain and key

libraries <- read_csv(
  "data/raw/PLS_FY24_AE_pud24i.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

glimpse(libraries)

# one row is one ______.
##one row is a specific library systems repsonse to the survey
# the download also has an "outlet" file. it has more rows.
# why would it? (the user's guide will tell you.)
##there is some data that is confidential
# which column should identify a row?
n_distinct(libraries$FSCSKEY)
n_distinct(libraries$LIBID)
##FSCSKEY
# what did those tell you?
##Your FSCSKEY is unique
# what would it have meant if the count() came back with rows?
##?

# let's focus on VISITS for now. what does this column mean?
##Library Visits
#VISITS counts visits to the library in a year.
# before you run anything: what values would be impossible? negative numbers

libraries |>
  summarise(
    min_visits = min(VISITS, na.rm = TRUE),
    max_visits = max(VISITS, na.rm = TRUE)
  )

libraries |>
  filter(VISITS < 0) |>
  count(VISITS)

# how many different negative values? how many rows of each?
##two different negative values (-3, -1). There are 5 rows of -1 and 69 rows of -3
# is a negative number here bad data, missing data, or a code?
# can you tell from the data alone?
##You can not tell from the data alone but if you look at the user guide it says that -1 is a missing value and -3 is a temporarily closed admin
# does R think any of these are missing?
##no R does not think they are missing
is.na(libraries$VISITS) |>
  table()

# on tuesday, NA told us THAT something was missing but not WHY.
# what's different here?
##Since they are using -1 to represent a missing value there are not going to be any NAs in our data

# go to the user's guide. for each negative value:
# what does it mean, in the guide's words? where did you find it?
##-1 means missing value and -3 means temporarily closed administrative entity.
# do the two codes mean the same thing?
#No

# every numeric column has a flag column. find the one for VISITS.
flag <- libraries |> 
  select(VISITS, F_VISITS)
n_distinct(libraries$F_VISITS)
table(libraries$F_VISITS)
# what does the flag tell you? does it tell the two codes apart?
##It tells you if the data was imputed and why, and if the data was not imputed and why.
##Yes, it tells you if something was just not reported or if the admin was temporarily closed.
# make a clean version. VISITS stays exactly as it is.

libraries <- libraries |>
  mutate(visits_clean = if_else(VISITS %in% c(-1, -3), NA_real_, VISITS))

# why list the codes instead of writing VISITS < 0?
##To distinguish the reason for the the missing value.
# both codes just turned into NA. what did we lose?
# where can we still find it?
##The reason for it being missing, we can still find it in F_VISITS.
n_distinct(libraries$FSCSKEY)
is.na(libraries$visits_clean) |>
  table()
# did it do what we meant? three questions:
# same number of library systems? Yes 
# did every code become NA? No
# did anything else become NA? No

# does any of this matter? 
##Yes, because if we had included negative values it could have messed up how we analyzed the dataset.

# why is the raw mean lower? what's in each denominator?
##Because the raw has the negative numbers as codes lowering the avg.
##9249 is in the denom for the raw
##9175 is in the denom for the cleaned version

# your turn :)
# get in your group project groups

# do that process for each of the following:
# TOTATTEN - program attendance
# TOTPRO - number of programs
# TOTCIR - total circulation

# you're not solving a new problem. same steps, different variable.
# everything you need is in the VISITS section.

# what should it measure? what would be impossible?
##TOTATTEN measures the total attendance of synchronous programs. Negative should be impossible
##TOTPRO measures the total number of synchronous program sessions. Negatives should be impossible
##TOTCIR measures the Total annual circulation transactions. Negatives should be impossible
# range. any odd values? how many of each?
##TOTATTEN: Yes, -1 (401 instances) and -3 (5 instances)
##TOTPRO: Yes, -1 (354 instances) and -3 (5 instances)
##TOTCIR
libraries |>
  summarise(
    min_atten = min(TOTATTEN, na.rm = TRUE),
    max_atten = max(TOTATTEN, na.rm = TRUE)
  )

libraries |>
  filter(TOTATTEN < 0) |>
  count(TOTATTEN)

libraries |>
  summarise(
    min_pro = min(TOTPRO, na.rm = TRUE),
    max_pro = max(TOTPRO, na.rm = TRUE)
  )

libraries |>
  filter(TOTPRO < 0) |>
  count(TOTPRO)
# what does the user's guide say they mean? -1 means missing value and -3 means temporarily closed administrative entity
# ("we couldn't find it" is an answer. "we assumed" isn't.)


# what's the flag column? (the names get shortened. look for it.)
##F_TOTATT

# clean version. keep the raw column.
libraries <- libraries |>
  mutate(totatten_clean = if_else(TOTATTEN %in% c(-1, -3), NA_real_, TOTATTEN))

# check it: same rows? every code became NA? nothing else did?
n_distinct(libraries$FSCSKEY)
is.na(libraries$totatten_clean) |>
  table()
##Yes, no, nothing else did
# mean before and after. big change or small? Fairly big change.
libraries |> 
  summarise(
    mean(TOTATTEN)
  )
libraries |> 
  summarise(
    mean(totatten_clean, na.rm = TRUE)
  )
# where are they? 

# recoding to NA fixes the number. does it finish the job?

# are the coded rows spread out, or do they bunch up?

# if someone compares circulation across states, what goes wrong?
##IDK what the last four questions are asking :)
#DA JANKEES LOSE

# this is for you to answer
# is this file ready to analyze as is? 3-4 sentences.
##No, this file would not be ready to analyze. There are several other columns I would need to inspect and most likely clean.
##It would also depend on the specific work I am doing. For certain projects I might only need what I have already cleaned, but in general it needs more cleaning.