#!/bin/bash -xe

echo "installing dotnet-sdk-6.0.420"
curl -Lo dotnet.tar.gz https://download.visualstudio.microsoft.com/download/pr/b521d7d2-108b-43d9-861a-58b2505a125a/0023553690a68328b33bc30a38f151db/dotnet-sdk-6.0.420-linux-x64.tar.gz
mkdir -p $HOME/dotnet && tar zxf dotnet-sdk-6.0.420-linux-x64.tar.gz -C $HOME/dotnet
export DOTNET_ROOT=$HOME/dotnet
export PATH=$PATH:$HOME/dotnet
dotnet --version
