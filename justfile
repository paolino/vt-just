
x := '1'

default:
    echo 'Hello, world!'
stg-init:
    stg init
stg-status:
    stg status
stg-new:
    stg new -m "new patch"
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
