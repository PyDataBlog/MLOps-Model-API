# The "share" dir is what Vagrant exposes as the "services\mongodb" folder
cd share

# Create a db dir (if it does not already exist), so we can mount that
# into the docker image as a volume where we can write our data to
mkdir db

# Build the docker image under the tag of "sabhiram/mongodb"
docker build -t sabhiram/mongodb .

# Run the docker image, expose the db port and the admin port along
# with mounting the "db" dir as the "/data/db" volume
docker run \
    -p 27017:27017 \
    -p 28017:28017 \
    -v /home/core/share/db:/data/db \
    -d sabhiram/mongodb
