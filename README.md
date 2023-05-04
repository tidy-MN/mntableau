### Prerequisites

- Python installed with tableau-api-lib and tableauhyperio packages installed
- Set Python interpreter in RStudio

### `RStudio`: Install the *mntableau* package from Gitlab 

- Open RStudio
- Run the script below in the RStudio console

``` r
# First install the 'rstudioapi', 'remotes' and 'git2r' packages
## These let you access MDH's Gitlab
install.packages("rstudioapi", "remotes", "git2r")

# Install the mntableau package from Gitlab
remotes::install_git("https://gitlab.health.state.mn.us/r-time/mntableau",
                     credentials = git2r::cred_user_pass(
                       rstudioapi::askForPassword("Gitlab username"),
                       rstudioapi::askForPassword("Gitlab password")))
```
