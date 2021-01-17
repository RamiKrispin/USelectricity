echo "Build the docker"

docker build . -t rkrispin/us_electricity_rstudio:dev

# if [[ $? = 0 ]] ; then
# echo "Pushing docker..."
# docker push rkrispin/us_electricity:dev.0.0.0.9000
# else
# echo "Docker build failed"
# fi