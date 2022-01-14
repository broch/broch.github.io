#!/usr/bin/env bash

REMOTE="git@github.com:broch/broch.github.io.git"
DEPLOY="public"

info() {
  printf "  \033[00;32m+\033[0m $1\n"
}

success() {
  printf "  \033[00;32m+\033[0m $1\n"
}

fail() {
  printf "  \033[0;31m-\033[0m $1\n"
  exit
}

if [ ! -f "config.toml" ]; then
  fail "not at root dir"
fi

rm -rf $DEPLOY
mkdir $DEPLOY

info "created $DEPLOY"
pushd $DEPLOY

git init -q
info "initialized git"
#git checkout -b site -q
#info "created site branch"
git remote add origin $REMOTE
info "set git remote"
git fetch origin site
git checkout site
info "checked out site"
success "setup complete"

popd
hugo --minify --environment production

pushd $DEPLOY

git add --all .
info "added files to git"

git commit -m "Deploy" -q
info "committed site"

popd
echo "cd $DEPLOY squash, and force push site branch to deploy"
