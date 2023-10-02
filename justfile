
commit_description := ''
commit_name := ''

# this gets executed on vtjust start, use it to set up your environment
default:
    echo 'Hello, world!'
    just stg-report-commit-name-and-desc

stg-init:
    stg init
stg-status:
    stg status
stg-new:
    stg new "{{commit_name}}" -m "{{commit_description}}"
    just stg-report-commit-name-and-desc
stg-series:
    stg series -d
stg-add:
    stg add .
stg-refresh:
    stg refresh
stg-push:
    stg push
    just stg-report-commit-name-and-desc
stg-pop:
    stg pop
    just stg-report-commit-name-and-desc
stg-refresh--index:
    stg refresh --index
stg-spill-r:
    stg spill -r
stg-spill:
    stg spill
git-stash:
    git stash --include-untracked
git-stash-apply:
    git stash apply
git-stash-pop:
    git stash pop
stg-delete:
    stg delete -t
    just stg-report-commit-name-and-desc
stg-diff:
    stg diff
cabal-build:
    cabal build
git-status:
    git status
git-checkout-main:
    git checkout main
git-checkout-devel:
    git checkout devel
git-push-force:
    bash -c "git push --force origin HEAD"
cabal-install:
    cabal install --overwrite-policy=always
git-reset-devel:
    -git checkout main
    -stg branch --delete --force devel
    stg branch --create devel
    git checkout devel
stg-clean:
    stg clean
stg-rename:
    stg rename "{{commit_name}}"
stg-report-commit-name-and-desc:
    stg series -d | awk -f stgCommitNameAndDesc.sh > vtjust.fifo
stg-report-commit-name-and-desc-pipe:
    stg series -d | awk -f stgCommitNameAndDesc.sh