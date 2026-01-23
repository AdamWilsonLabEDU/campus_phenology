# GitHub Setup Commands for R
# Run these commands in your R console to configure GitHub access

# Install required packages if needed
# install.packages(c("usethis", "gitcreds"))

library(usethis)
library(gitcreds)

# 1. Check current git configuration
# NOTE: If you use a fine-grained GitHub token, `usethis::git_sitrep()` can error
# because GitHub may not return token "scopes" metadata.
# If that happens, either switch to a classic PAT (recommended below) or use the
# fallback checks at the bottom of this script.
usethis::git_sitrep()

# 2. Set up your git config (replace with your info)
usethis::use_git_config(
  user.name = "Your Name",
  user.email = "your.email@example.com"
)

# 3. Create a GitHub Personal Access Token (PAT)
# This opens the classic PAT page with suggested scopes.
# Make sure you create a *Tokens (classic)* PAT if you want `git_sitrep()` to work.
usethis::create_github_token()

# 4. After creating the token, store it
# Optionally delete the old token first (e.g., if you previously stored a fine-grained token)
# gitcreds::gitcreds_delete(url = "https://github.com")
gitcreds::gitcreds_set()

# 5. Verify everything is set up correctly
usethis::git_sitrep()

# 6. If you need to change the remote URL to use SSH instead of HTTPS:
# usethis::use_git_remote(
#   name = "origin",
#   url = "git@github.com:AdamWilsonLabEDU/campus_phenology.git",
#   overwrite = TRUE
# )

# 7. Pull latest changes
# system("git pull origin main")

# 8. Push changes
# system("git push origin main")

# --- Fallback checks (useful if `git_sitrep()` errors) ---
# system("git remote -v")
# system("git status")
# (Optional) confirm token works for API calls:
# gh::gh_whoami()
