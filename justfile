
commit_description := 'new patch full of dust'
commit_name := 'n'

default:
    echo 'Hello, world!'
stg-init:
    stg init
stg-status:
    stg status
stg-new:
    stg new "{{commit_name}}" -m "{{commit_description}}"
stg-series:
    stg series -d
stg-add:
    stg add .
stg-refresh:
    stg refresh
stg-push:
    stg push
stg-pop:
    stg pop
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
