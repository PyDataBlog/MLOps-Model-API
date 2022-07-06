#!/bin/bash
#####################################
# Chef Client Downloader
# Ryan Moon
# Created 08/29/16
# Version 0.0.1 on 08/29/16
#####################################
# CURRENTLY REQUIRES WGET
#####################################
# This script iterates thru two arrays
# chef_versions and os_versions, to
# download specific chef-client versions.
# The DMG is mounted, the PKG is copied,
# then the DMG is removed.
# Currently I'm only testing 12.8.1-1 and
# 12.13.37-1 in OS 10.9, 10.10, and 10.11
# You can uncomment the ones you'd prefer
# using.
# On success you will see chef-$version_number-1.pkg in
# the chef_client_versions/$os_version_number
#####################################

main(){
chef_versions=(
  12.8.1
  # 12.9.38
  # 12.9.41
  # 12.10.24
  # 12.11.18
  # 12.12.15
  # 12.13.30
  12.13.37
  )

os_versions=(
  10.11
  10.10
  10.9
  )

for ((i=0; i<${#chef_versions[@]}; i++)); do
  for ((f=0; f<${#os_versions[@]}; f++)); do
    if [[ ! -f ../chef_client_versions/${os_versions[$f]}/chef-${chef_versions[$i]}-1.pkg ]]; then
      echo "Downloading -- MacOS ${os_versions[$f]} - Chef ${chef_versions[$i]}"
          /usr/local/bin/wget -q https://packages.chef.io/stable/mac_os_x/${os_versions[$f]}/chef-${chef_versions[$i]}-1.dmg -O ../chef_client_versions/${os_versions[$f]}/chef-${chef_versions[$i]}-1.dmg
          /usr/bin/hdiutil mount ../chef_client_versions/${os_versions[$f]}/chef-${chef_versions[$i]}-1.dmg -quiet
          cp -r /Volumes/Chef\ Client/chef-${chef_versions[$i]}-1.pkg ../chef_client_versions/${os_versions[$f]}/chef-${chef_versions[$i]}-1.pkg
          hdiutil unmount /Volumes/Chef\ Client -quiet
          rm -rf ../chef_client_versions/${os_versions[$f]}/chef-${chef_versions[$i]}-1.dmg
          echo "Finished -- OS ${os_versions[$f]} - Chef ${chef_versions[$i]}"
        else
          echo "MacOS ${os_versions[$f]} - Chef ${chef_versions[$i]}-1.pkg already exists."
    fi
  done
done
}

# Script
main "$@"
