#!/bin/bash
if [ -z "$(git status --porcelain)" ]; then
  # Working directory clean
  quarto render --use-freezer --to html
  git add --force docs/*
  git stash
  git checkout gh-pages
  git pull
  git rm -r docs
  git stash apply
  git commit
  git push
  git checkout main
else
  # Uncommitted changes
  echo "You have uncommitted changes."
fi
