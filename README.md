# Internet connection checker

This is a simple SAFE application to continuously monitor your internet connection.

## Install pre-requisites

You'll need to install the following pre-requisites in order to build SAFE applications

* The [.NET Core SDK](https://www.microsoft.com/net/download)
* [FAKE 5](https://fake.build/) installed as a [global tool](https://fake.build/fake-gettingstarted.html#Install-FAKE)
* The [Yarn](https://yarnpkg.com/lang/en/docs/install/) package manager (you an also use `npm` but the usage of `yarn` is encouraged).
* [Node LTS](https://nodejs.org/en/download/) installed for the front end components.
* If you're running on OSX or Linux, you'll also need to install [Mono](https://www.mono-project.com/docs/getting-started/install/).

## Work with the application

To concurrently run the server and the client components in watch mode use the following command:

```bash
fake build -t Run
```

## Deployment

1. Follow the documentation in https://safe-stack.github.io/docs/template-azure-registration/
2. Create a `release.sh` script:

```bash
#!/bin/bash

# In case you want to deploy the same environment every time (optional)
export environment={some random string, for example '231g1a'}
# from step 1
export subscriptionId={subscriptionIdFromAzure}
# from step 1
export clientId={applicationIdFromAzure}
# start deployment
fake build target AppService
```

3. now run the script and authenticate against azure (follow the screen instructions)

## SAFE Stack Documentation

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/docs/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)
* [Fulma](https://mangelmaxime.github.io/Fulma/)

If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).
