# Base Image
FROM nvidia/cuda:11.7.0-base-ubuntu22.04

# Set the working directory
WORKDIR /src

# Install pip
# RUN apt-get update && apt-get install -y pip

# Copy the pkg requirements
COPY requirements.txt .

# Upgrade pip & install packge requirements
RUN pip install -r requirements.txt

# Copy the source code to the working directory
COPY . .

# Run the app
CMD ["python3", "app/main.py"]
