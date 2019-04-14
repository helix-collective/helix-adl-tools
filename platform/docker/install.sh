#!/bin/bash
set -e

echo "deb mirror://mirrors.ubuntu.com/mirrors.txt bionic main restricted universe multiverse" > /etc/apt/sources.list
echo "deb mirror://mirrors.ubuntu.com/mirrors.txt bionic-updates main restricted universe multiverse" >> /etc/apt/sources.list
echo "deb mirror://mirrors.ubuntu.com/mirrors.txt bionic-security main restricted universe multiverse" >> /etc/apt/sources.list

# Update first prior to installing packages from the standard repos
apt-get update

# Add curl and apt-transport-https for installing from https repositories
apt-get install -y curl apt-transport-https gnupg

# Add source repository for yarn
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list

# Add source repository for recent node
curl -sL https://deb.nodesource.com/setup_8.x | bash -

# Update again so we can install packages from third party repos
apt-get update

# Install packages
apt-get install -y \
  nodejs \
  yarn \
  unzip \
  zip \
  libgmp-dev
