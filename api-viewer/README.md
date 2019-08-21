This is a react application to show the openapi specification for an
ADL specified http API.

# Instructions

1) Checkout a clean copy of this repo.

2) Replace the prototype, baked in typescript ADL with that for your project.

Assuming that the absolute path to this repo is `$REPO`, and the absolute
path to your adl tree is `$ADLDIR`

```
ADLGEN=$REPO/api-viewer/client/src/adl-gen
rm -rf $ADLGEN/*
ADLFILES="$(find $ADLDIR -name '*.adl') $($REPO/scripts/dockerized-adlc-stdlib)"
$REPO/scripts/dockerized-adlc typescript \
  --include-resolver \
  --include-rt --runtime-dir=runtime \
  --excluded-ast-annotations="" \
  -O $ADLGEN -I $ADLDIR $ADLFILES
```

3) Start the application locally

```
(cd $REPO/api-viewer/client; yarn; yarn start)
```
