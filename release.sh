#!/bin/sh

set -e
set -x

VERSION=$1
IMAGE_NAME=registry.virtual-void.net/jrudolph/json-auto-typer:$VERSION

git tag -a $VERSION -m "Released $VERSION"
docker build --tag $IMAGE_NAME .
docker push $IMAGE_NAME