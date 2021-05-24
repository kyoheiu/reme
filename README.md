A simple CLI to create slack reminder.

<script id="asciicast-fqLCIbepnV9K10YVUrWu5ImjF" src="https://asciinema.org/a/fqLCIbepnV9K10YVUrWu5ImjF.js" async></script>

## Installation
```
git clone https://github.com/kyoheiu/reme.git
cd reme
cabal install
```

You need `~/.config/reme.dhall` file to store the slack authentication token.

```dhall
{ slackToken = "xoxp-xxxxxx..."
}
```

If you'd like to change the file path, feel free to replace `path` in Main.hs.

For the token and arguments detail, see [reminders.add method | Slack](https://api.slack.com/methods/reminders.add).
