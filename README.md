# AlgebraicTemplate.jl

[![Stable Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](https://AlgebraicJulia.github.io/AlgebraicTemplate.jl/stable)
[![Development Documentation](https://img.shields.io/badge/docs-dev-blue.svg)](https://AlgebraicJulia.github.io/AlgebraicTemplate.jl/dev)
[![Code Coverage](https://codecov.io/gh/AlgebraicJulia/AlgebraicTemplate.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/AlgebraicJulia/AlgebraicTemplatee.jl)
[![CI/CD](https://github.com/AlgebraicJulia/AlgebraicTemplate.jl/actions/workflows/julia_ci.yml/badge.svg)](https://github.com/AlgebraicJulia/AlgebraicTemplate.jl/actions/workflows/julia_ci.yml)

A template repository for making a new AlgebraicJulia package.

## 🛠️ Usage

1. Use the "Use this template" dropdown to select "Create a new repository"
2. In the new page select "AlgebraicJulia" as the owner, give the repository a name such as "AlgebraicX.jl", and create a new repository from the template
3. Set up Codecov credentials for code coverage (If you have trouble, reach out to an AlgebraicJulia organization owner to help with this)

   1. Log into [Codecov](https://codecov.io) with your GitHub account (this requires that you are a member of the AlgebraicJulia organization)
   2. Navigate to the [AlgebraicJulia organization](https://app.codecov.io/gh/AlgebraicJulia)
   3. Select your new repository from the list (e.x. "AlgebraicX")
   4. Note down the `CODECOV_TOKEN` value (It may be in the "Settings" tab if it doesn't show up immediately)
   5. Navigate back to your new GitHub repository and go to the Settings tab
   6. Go to "Security", "Secrets and variables", and "Actions" and click the "New repository secret" button
   7. Give the secret name `CODECOV_TOKEN` and the Secret value is the value you noted from the Codecov settings
   8. Click "Add secret"

4. Clone the new repository, for example in the terminal:
   ```sh
   git clone https://github.com/AlgebraicJulia/AlgebraicX.jl.git
   cd AlgebraicX.jl
   ```
5. Rename the file `src/AlgebraicTemplate.jl` to match the name of your new package (e.x. "AlgebraicX")
   ```sh
   mv src/AlgebraicTemplate.jl src/AlgebraicX.jl
   ```
6. Replace all instances of the word "AlgebraicTemplate" with your new package name (e.x. "AlgebraicX")
   ```sh
   # On linux
   git grep -l 'AlgebraicTemplate' | xargs sed -i 's/AlgebraicTemplate/AlgebraicX/g'
   # On Mac OS X
   git grep -l 'AlgebraicTemplate' | xargs sed -i '' -e 's/AlgebraicTemplate/AlgebraicX/g'
   ```
7. Generate a new random version 4 UUID (you can get one here: https://www.uuidgenerator.net/version4)
   - We will assume for this example that your new UUID is `<UUID>`
8. Replace all instances of the template's UUID, "b66562e1-fa90-4e8b-9505-c909188fab76", with your new UUID (e.x. "<UUID>")
   ```sh
   # On linux
   git grep -l 'b66562e1-fa90-4e8b-9505-c909188fab76' | xargs sed -i 's/b66562e1-fa90-4e8b-9505-c909188fab76/<UUID>/g'
   # On Mac OS X
   git grep -l 'b66562e1-fa90-4e8b-9505-c909188fab76' | xargs sed -i '' -e 's/b66562e1-fa90-4e8b-9505-c909188fab76/<UUID>/g'
   ```
9. Commit these new changes to your repository
   ```sh
   git commit -am "Set up skeleton for AlgebraicX.jl"
   git push
   ```
10. Go back to your repository and wait until the tests have passed, you can check the status by going to the "Actions" tab in the repository

### 📔 Set Up GitHub Pages (Public Repos Only)

1. Follow the Usage steps above to set up a new template, make sure all initial GitHub Actions have passed
2. Navigate to the repository settings and go to "Code and automation", "Pages"
3. Make sure the "Source" dropdown is set to "Deploy from a branch"
4. Set the "Branch" dropdown to "gh-pages", make sure the folder is set to "/ (root)", and click "Save"
5. Go back to the main page of your repository and click the gear to the right of the "About" section in the right side column
6. Under "Website" check the checkbox that says "Use your GitHub Pages website" and click "Save changes"
7. You will now see a URL in the "About" section that will link to your package's documentation

### 🛡️ Set Up Branch Protection (Public Repos Only)

1. Follow the Usage steps above to set up a new template, make sure all initial GitHub Actions have passed
2. Navigate to the repository settings and go to "Code and automation", "Branches"
3. Click "Add branch protection rule" to start adding branch protection
4. Under "Branch name pattern" put `main`, this will add protection to the main branch
5. Make sure to set the following options:
   - Check the "Require a pull request before merging"
   - Check the "Request status checks to pass before merging" and make sure the following status checks are added to the required list:
     - CI / Documentation
     - CI / Julia 1 - ubuntu-latest - x64 - push
     - CI / Julia 1 - ubuntu-latest - x86 - push
     - CI / Julia 1 - windows-latest - x64 - push
     - CI / Julia 1 - windows-latest - x86 - push
     - CI / Julia 1 - macOS-latest - x64 - push
   - Check the "Restrict who can push to matching branches" and add `algebraicjuliabot` to the list of people with push access
6. Click "Save changes" to enable the branch protection
