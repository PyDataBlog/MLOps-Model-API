# snapshot-check.rb
# Version:  1.0.0
# Author:   Jason Boeshart
# Date:     10/22/2015
# Notes:    Initial script creation. This script will go through and get the servers that have drives with no snapshots
#           and the volumes that haven't been snapshotted within the specified number of days. It outputs to the user
#           with a list of both.

require 'aws-sdk'
require 'time'
require 'time_difference'
require 'optparse'

# Some variable initialization
region = 'us-east-1'
max_age = 7
filter = ''
tag = ''
no_snap = {}
snap_ages = {}

# Get options from the command line
OptionParser.new do |opts|
  opts.banner = "Usage: snapshot-check.rb [options]"
  opts.on('-t STRING', '--tag', 'Tag that you want to filter against') { |v| tag = v }
  opts.on('-f STRING', '--filter', 'Filter to use for the tag') { |v| filter = v }
  opts.on('-a STRING', '--age', 'Age in days of the most recent snapshot') { |v| max_age = v }
  opts.on('-r REGION', '--region', 'AWS region to query') { |v| region = v }
end.parse!

# Set up EC2 connection
ec2 = Aws::EC2::Resource.new(region: region)

# Get the instances based on the specified filter
instances = ec2.instances(
  filters: [
    {
      name: "tag:#{tag}",
      values: [filter]
    }
  ]
)

# Get all the instance/volume/snapshot details
# Loop through each instance
instances.each do |instance|
  if instance.state.name == 'running'
    puts "---------------------------------"
    # Initialize a few variables
    drives_without_snap = 0
    @name = ''
    # Loop through each tag on the instance to pull out the name of the server
    instance.tags.each do |t|
      if t.key == tag
        puts "#{tag}: " + t.value
        @name = t.value
        # Add the server to the array we're using to verify recent snapshots
        snap_ages[@name] = {}
      end
    end
    puts "Instance ID: " + instance.id
    # Loop through each volume attached to the instance
    instance.volumes.each do |volume|
      puts "\tVolume ID: " + volume.id
      snap_count = 0
      # Loop through each snapshot associated with a volume
      volume.snapshots.each do |snapshot|
        # Add to a counter to track the number of snapshots
        snap_count += 1
        puts "\t\tSnapshot ID: " + snapshot.id
        puts "\t\tSnapshot Description: " + snapshot.description
        puts "\t\tSnapshot Start Time: " + snapshot.start_time.to_s
        # Get the age of the snapshot
        age = TimeDifference.between(Time.now.utc, snapshot.start_time).in_days.to_s
        puts "\t\tSnapshot Age: " + age
        # Store the age of the most recent snapshot
        if snap_count == 1
          # If it's the first time through, set the age to current age
          snap_ages[@name][volume.id] = age
        else
          # If the age in the hash is greater than the current age, replace it with the current age
          if snap_ages[@name][volume.id] > age
            snap_ages[@name][volume.id] = age
          end
        end
      end
      if snap_count == 0
        drives_without_snap += 1
        puts "\tWARNING! No snapshots for this volume!"
        no_snap[volume.id] = @name
      end
    end
    if drives_without_snap > 0
      puts "WARNING! This system has #{drives_without_snap} drive(s) without snapshots!"
    end
  end
end

# Output the summary from script execution
puts ''
puts '*********************************'
puts "Summary"
puts '*********************************'
puts "There were #{no_snap.size} volumes on #{no_snap.values.uniq.size} servers found without snapshots:"
no_snap.each do |vol, serv|
  puts "Server: #{serv}, Volume: #{vol}"
end
puts ''

puts "The following volumes have no snapshots in the past #{max_age} days:"
# Loop through each item in the snap_ages hash and determine if it's older than max_age
snap_ages.each do |key, value|
  value.each do |k, v|
    if v.to_i > max_age.to_i
      puts "Server: #{key}, Volume: #{k}, Latest Snapshot Age: #{v} days"
    end
  end
end
