#!/bin/bash -xe

echo "installing dotnet-sdk-6.0.420"
curl -Lo dotnet.tar.gz https://download.visualstudio.microsoft.com/download/pr/b521d7d2-108b-43d9-861a-58b2505a125a/0023553690a68328b33bc30a38f151db/dotnet-sdk-6.0.420-linux-x64.tar.gz
mkdir -p /dotnet && tar zxf dotnet.tar.gz -C /dotnet
export DOTNET_ROOT=/dotnet
export PATH=$PATH:/dotnet
dotnet --version
