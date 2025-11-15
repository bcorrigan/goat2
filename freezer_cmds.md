Here is a detailed specification for the freezer management chatbot's text interface. I've broken it down into core concepts, a command reference, and some advanced features for a better user experience.

# FreezerBot Text Interface Specification

1. Core Concepts

    - Freezers: Users can have multiple, named freezers (e.g., "garage", "kitchen", "upstairs"). Commands can target a specific freezer. If no freezer is named, the bot will use a "default" freezer.
    - Items: An item is defined by its Name (e.g., "peas", "chicken thighs"), Quantity (a number), and an optional Unit (e.g., "bags", "boxes", "fillets").
    - Inventory IDs: When listing items, each unique item entry is given a short, temporary ID (e.g., #1, #2). This allows for quick and unambiguous removal of items.

2. Command Reference

Here are the primary commands, designed to be flexible with natural language parsing. Keywords are in bold, optional parts in [square brackets], and user-provided values in <angle brackets>.
A. Adding Items

    Command Keywords: add, put, store, freeze

    Full Syntax:
    <keyword> <quantity> [unit] of <item name> [to/in <freezer name>]

    Examples:
        add 2 bags of peas to garage freezer (The full, unambiguous command)
        put 4 chicken breasts in kitchen (The unit is part of the name)
        freeze 1 loaf of bread (Uses the default freezer)
        add strawberries (Defaults to quantity 1, uses the default freezer)

    Bot Responses:
        Success (New Item): ✅ Added 2 bags of peas to the Garage Freezer.
        Success (Existing Item): ✅ Added 2 bags of peas to the Garage Freezer. You now have 5.
        Error (No Default Freezer): Hmm, which freezer should I put that in? You can tell me with 'add peas to garage' or set a default with 'set default freezer garage'.
        Error (Freezer Not Found): I can't find a freezer named "cellar". Your freezers are: Garage, Kitchen.

B. Removing/Taking Items

    Command Keywords: take, remove, use, get

    Syntax:
        take <quantity> of <item name> [from <freezer name>]
        take <quantity> of <ID> [from <freezer name>] (Using the ID is the most reliable method)

    Examples:
        inventory garage (User first checks the inventory)

            Inventory for: 🧊 Garage Freezer
            #1: 5x bags of peas
            #2: 1x tub of ice cream (chocolate)
            #3: 4x chicken breasts

        take 2 of #1 (Removes 2 bags of peas)
        use #2 (Removes the entire tub of ice cream)
        remove 1 chicken breast from garage (Works, but is less precise than using the ID)

    Bot Responses:
        Success (Partial Removal): 👌 Removed 2 bags of peas from the Garage Freezer. 3 remaining.
        Success (Full Removal): 👌 Removed the last tub of ice cream from the Garage Freezer.
        Helpful Warning (Taking more than exists): You asked to take 5 chicken breasts, but there are only 4 in the Garage Freezer. I've removed all of them for you.
        Error (Item not found): I couldn't find item "#5" in the Garage Freezer.
        Error (Ambiguous Name): I found "chicken breasts" and "chicken nuggets". Please use the item ID from the inventory list to be more specific.

C. Viewing Inventory

    Command Keywords: inventory, list, show, check, whats in

    Syntax:
        <keyword> [<freezer name>]
        <keyword> all

    Examples:
        inventory garage
        list (Shows the default freezer)
        show all

    Bot Responses:
        Single Freezer:

            Inventory for: 🧊 Garage Freezer
            #1: 3x bags of peas
            #2: 4x chicken breasts
            #3: 1x leftover chili (added 2 days ago)

        All Freezers:

            🧊 Garage Freezer:
            #1: 3x bags of peas
            ...
            ---
            🧊 Kitchen Freezer:
            #1: 2x pizzas (pepperoni)
            #2: 1x bag of corn

        Empty Freezer: The Garage Freezer is empty! 🎉

D. Freezer Management

    Commands:
        add freezer <name> / create freezer <name>
        remove freezer <name> / delete freezer <name>
        rename freezer <old name> to <new name>
        list freezers
        set default freezer <name>

    Bot Responses:
        ✅ New freezer "Upstairs" created.
        Your freezers are: Garage (default), Kitchen, Upstairs.
        "Garage" is now your default freezer.
        Confirmation on Delete: Are you sure you want to delete the "Upstairs" freezer and all its contents? This cannot be undone. (Reply yes/no)

E. Help

    Command: help
    Response: A summary of the most common commands and examples.

        Hi! I'm FreezerBot. Here's what I can do:
        • **add 2 bags of peas to garage** - Add items.
        • **inventory garage** - See what's inside.
        • **take 1 of #2** - Remove an item (use the ID from the inventory!).
        • **list freezers** - Manage your freezers.
        • **set default freezer garage** - Save some typing!

3. User Experience Enhancements (The "Necessary Extras")

    Context Awareness: If you just ran inventory garage, the bot should assume your next command (take #1) is for the "garage" freezer without you having to specify it.
    Date Stamping: When an item is added, the bot automatically records the date. The inventory can then display how long an item has been frozen.

        #3: 1x leftover chili (frozen for 45 days)

    Expiry Reminders (Advanced): Allow users to add expiry information.
        add 1kg mince meat to kitchen expires in 3 months
        The bot could then proactively warn about items nearing their expiry date.

            ⚠️ Heads up! The "mince meat" in your Kitchen Freezer expires in 7 days.

    Search/Find Command:
        find chicken
        Response: Found "chicken" in: 🧊 Garage Freezer (#2: 4x chicken breasts), 🧊 Kitchen Freezer (#3: 2x chicken Kievs).
    Shopping List Integration: When an item is fully removed, the bot can offer to add it to a shopping list.
        👌 Removed the last bag of peas... Shall I add "peas" to your shopping list? (yes/no)

# Example Dialogue Flow

Here's how a full conversation might look:

User: add freezer garage
Bot: ✅ New freezer "Garage" created. I've set it as your default.

User: add 2 bags of peas
Bot: ✅ Added 2 bags of peas to the Garage Freezer.

User: store 1 tub of ben & jerrys
Bot: ✅ Added 1 tub of ben & jerrys to the Garage Freezer.

User: inventory
Bot:

    Inventory for: 🧊 Garage Freezer (default)
    #1: 2x bags of peas (frozen today)
    #2: 1x tub of ben & jerrys (frozen today)

User: add freezer kitchen
Bot: ✅ New freezer "Kitchen" created.

User: put 4 salmon fillets in kitchen
Bot: ✅ Added 4 salmon fillets to the Kitchen Freezer.

User: list all
Bot:

    🧊 Garage Freezer (default):
    #1: 2x bags of peas (frozen today)
    #2: 1x tub of ben & jerrys (frozen today)
    ---
    🧊 Kitchen Freezer:
    #1: 4x salmon fillets (frozen today)

User: take #2
Bot: 👌 Removed the last tub of ben & jerrys from the Garage Freezer. Shall I add "ben & jerrys" to your shopping list? (yes/no)

User: yes
Bot: 👍 I've added it to your shopping list.

