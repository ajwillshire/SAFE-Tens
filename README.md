# SAFE-Tens
This is a learn-by-doing project involving the [SAFE Stack](https://safe-stack.github.io/) and [Akka.Net](https://getakka.net/). It was originally inspired by [this Elmish project](https://github.com/rommsen/tens) built by [Roman Sachse](https://github.com/rommsen) as documented in [this](https://youtu.be/jeYlmH2NlTw) video.

Virtually all of the game logic has been moved server-side and each player has their own set of Actors created to run their instance of the game. You can have multiple client sessions occurring simultaneously, and high scores across players are tracked. There is an automatic player that can be set to play the game unsupervised. It's not infallible, however!

You can see the other players that are active, you can kill them or, if a player has abandoned their Actors (i.e., closed their session) you can adopt that system.

The interface still needs work - it's functional but not very pretty. But I'm getting there!

Any suggestions for improvements gratefully received.


## Install pre-requisites

You'll need to install the following pre-requisites in order to build SAFE applications

* The [.NET Core SDK](https://www.microsoft.com/net/download)
* [FAKE 5](https://fake.build/) installed as a [global tool](https://fake.build/fake-gettingstarted.html#Install-FAKE)
* The [Yarn](https://yarnpkg.com/lang/en/docs/install/) package manager (you can also use `npm` but the usage of `yarn` is encouraged).
* [Node LTS](https://nodejs.org/en/download/) installed for the front end components.
* If you're running on OSX or Linux, you'll also need to install [Mono](https://www.mono-project.com/docs/getting-started/install/).

## Work with the application

To concurrently run the server and the client components in watch mode use the following command:

```bash
fake build --target run
```


## SAFE Stack Documentation

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/docs/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)
* [Feliz](https://github.com/Zaid-Ajaj/Feliz)

If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).

## Troubleshooting

* **fake not found** - If you fail to execute `fake` from command line after installing it as a global tool, you might need to add it to your `PATH` manually: (e.g. `export PATH="$HOME/.dotnet/tools:$PATH"` on unix) - [related GitHub issue](https://github.com/dotnet/cli/issues/9321)
