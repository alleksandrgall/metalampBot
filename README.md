# metaLampBot
Echo chat bot creatred as requested by MetaLamp. Implemented for TG and VK.
## Setting up and running
1. Download and install `stack` as described [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
2. Clone this repository as described [here](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) or with `git clone https://github.com/alleksandrgall/metalampBot` command if you have `git` installed.
3. To run app you need your own Telegram bot token, which you can get [here](https://telegram.me/BotFather) and/or VK access token and group id,details on setting up you VK group with chat bot can be found [here](https://dev.vk.com/api/bots/getting-started). 
VK Long Poll API version is 5.131.
4. Rename `config/config_template.properties` to `config/bot.properties`, that will be an actuall config where the programm will read from.
   In `config/config_template.properties` you can also find information about additional configurations.
5. Place information you got on step (3) in corresponding fields in `config/bot.properties`.
6. `stack setup` for downloading compiler, 
   `stack build` for building app and downloading dependencies.
7. `stack exec metaLampBot-exe` for running your bot.

You can run tests of logic with `stack test`.

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
