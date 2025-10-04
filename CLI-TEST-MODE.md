# CLI Test Mode

The bot now supports a CLI test mode for local testing without needing a Telegram connection.

## Usage

Run the bot with the `-test` flag:

```bash
lein run -- -test
```

This will start an interactive session where you can type commands directly:

```
Starting CLI test mode...
Type commands (e.g., 'roll 3d6' or 'goat, mem')
Press Ctrl+C to exit

> roll 3d6
TestUser: 3d6:2,5,4:11 Total:11

> toss
Heads.

> calc 5+3*2
11
```

## How It Works

- Input from stdin is treated as messages from a test user ("TestUser")
- All text-based commands work normally
- Image-based responses show `[IMAGE: Cannot display images in CLI mode]`
- Messages are processed through the same module system as Telegram messages
- The bot pretends all messages are private messages (chatId: 123456)

## Testing Modules

This is useful for:
- Quick iteration on module logic
- Testing command parsing
- Debugging without Telegram overhead
- Automated testing via piped input

Example automated test:
```bash
echo -e "roll 3d6\ntoss\ncalc 10*5" | lein run -- -test
```

## Implementation

- `CLIConnection.java` - Replaces `ServerConnection` in test mode
- `InputHandler` - Reads from stdin, creates Message objects
- `OutputHandler` - Reads from outqueue, writes to stdout
- No Telegram API dependencies required in test mode
