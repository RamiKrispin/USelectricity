#!/usr/bin/env bash
echo $2
#Rscript -e "source('./functions/data_update.R');if(update_data()){rmarkdown::render_site()}"


if [[ "$(git status --porcelain)" != "" ]]; then
    git add docs/*
    git add data/*
    git commit -m "Auto update of the $1 data"
    git push origin main
else
echo "Nothing to commit..."
fi