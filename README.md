# Baselang API

Haskell API client for Baselang.

## Methods

```haskell
hours :: IO Hours -- get all hours for all teachers
```

## Build

`stack build`

## Setup

You need an account and to grab your `cartalyst_sentry` cookie from the web app.

```
export BASELANG_TOKEN=${cartalyst_sentry}
```

## Run

`stack exec baselang`
