# metaLampBot
Echo chat bot creatred as requested by MetaLamp. Implemented for TG and VK.
## Setting up and running
1. Download and install `stack` as described [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Clone this repository as described [here](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) or with `git clone metalampBot` command if you have `git` installed.
3. To run app you need your own Telegram bot token, wich you can get [here](https://telegram.me/BotFather) and/or VK access token and group id, details on setting up you VK group with chat bot can be found [here](https://dev.vk.com/api/bots/getting-started).
4. After you got those replace field `token` in `config/bot.properties`. Other possible configurations are described there.
5. `stack setup` for downloading compiler, 
   `stack build` for building app and downloading dependencies.
6. `stack exec metaLampBot` for running your bot.

You can run tests of logic with `stack test`

## Project structer
* `app/Main.hs` - Parses config, runs an appropriate bot implementation.
* `src/Handlers/` - Contains logic for bot and logger.
* `src/Logger/` - Contains implementaion for logger.
* `src/Bot/` - Contains differend bot implementations with following structer:
    * `ImplName/Implement.hs` - All functions need for bot logic to work (`getUpdates`, `sendMes`, ...).
    * `ImplName/Types.hs` - Types for different responses and JSON instances for them using `aeson` package.
* `src/Exceptions/` - Mostly educational hierchy of exception types, for web exceptions and parse exceptions. 
* `src/Internal/` - Commonly used functions and types, mainly Http connection and parsing functions in `Req.hs`.
* `src/Config.hs/` - Type for app's config and tools for parsing it provided by `conferer` package.