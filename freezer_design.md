# Freezer Management Module - Design Document

## Overview

The Freezer Management module will provide a comprehensive inventory tracking system for multiple freezers via Telegram chat interface. Users can add, remove, view, and manage freezer contents with natural language commands.

## Architecture

### Database Design

**Database Location**: `resources/freezer.db`

**Tables**:

1. **freezers**
   - `freezer_id` INTEGER PRIMARY KEY AUTOINCREMENT
   - `user_id` TEXT NOT NULL (Telegram username)
   - `chat_id` INTEGER NOT NULL
   - `freezer_name` TEXT NOT NULL (e.g., "garage", "kitchen")
   - `is_default` BOOLEAN DEFAULT 0
   - `created_at` DATETIME
   - UNIQUE INDEX on (user_id, freezer_name)

2. **items**
   - `item_id` INTEGER PRIMARY KEY AUTOINCREMENT
   - `freezer_id` INTEGER NOT NULL (FK to freezers.freezer_id)
   - `item_name` TEXT NOT NULL (e.g., "peas", "chicken breasts")
   - `quantity` REAL NOT NULL DEFAULT 1
   - `unit` TEXT (e.g., "bags", "boxes", "fillets") - can be NULL
   - `added_date` DATETIME NOT NULL
   - `expiry_date` DATETIME (optional, for future enhancement)
   - `notes` TEXT (optional, e.g., "chocolate", "leftover chili")
   - FOREIGN KEY (freezer_id) REFERENCES freezers(freezer_id) ON DELETE CASCADE
   - INDEX on freezer_id

3. **user_context** (for context awareness)
   - `user_id` TEXT PRIMARY KEY
   - `last_freezer_id` INTEGER (FK to freezers.freezer_id)
   - `last_command_time` DATETIME
   - FOREIGN KEY (last_freezer_id) REFERENCES freezers(freezer_id) ON DELETE SET NULL

### Module Structure

**Namespace**: `org.goat.module.Freezer`

**Dependencies**:
- `org.goat.core.macros` - for `defmodule` macro
- `org.goat.core.message` - for message wrappers
- `org.goat.db.freezer` - database operations
- `clojure.string` - text parsing
- `clojure.java.jdbc` - SQLite operations

### Database Layer

**Namespace**: `org.goat.db.freezer`

**Core Functions**:

Database Management:
- `create-db` - Initialize database and tables
- `tbl-exists?` - Check if table exists (from util)

Freezer Operations:
- `get-freezers` [user-id] - Get all freezers for user
- `get-default-freezer` [user-id] - Get user's default freezer
- `add-freezer` [user-id chat-id freezer-name is-default?] - Create new freezer
- `delete-freezer` [freezer-id] - Delete freezer and all items
- `rename-freezer` [freezer-id new-name] - Rename a freezer
- `set-default-freezer` [user-id freezer-id] - Set default freezer
- `get-freezer-by-name` [user-id freezer-name] - Find freezer by name
- `get-freezer-by-id` [freezer-id] - Get freezer by ID

Item Operations:
- `add-item` [freezer-id item-name quantity unit notes] - Add/update item
- `remove-item` [item-id quantity] - Remove quantity, delete if zero
- `get-items` [freezer-id] - Get all items in freezer
- `get-item-by-id` [item-id] - Get specific item
- `search-items` [user-id search-term] - Search across all freezers
- `get-item-age` [item-id] - Get days since item was added

Context Operations:
- `set-user-context` [user-id freezer-id] - Remember last freezer used
- `get-user-context` [user-id] - Get last freezer used (within timeout)

### Command Parser

**Namespace**: `org.goat.module.freezer.parser`

Natural language parsing functions:

**Pattern Matching**:
- `parse-add-command` - Extract: quantity, unit, item-name, freezer-name
  - Patterns: "add <qty> [unit] of <item> [to/in <freezer>]"
  - Keywords: add, put, store, freeze
- `parse-remove-command` - Extract: quantity, item-id or name, freezer-name
  - Patterns: "take <qty> of <item|#id> [from <freezer>]"
  - Keywords: take, remove, use, get
- `parse-inventory-command` - Extract: freezer-name or "all"
  - Keywords: inventory, list, show, check, "whats in"
- `parse-freezer-command` - Extract: action, freezer-name(s)
  - Actions: add/create/remove/delete/rename/list/set default
- `parse-search-command` - Extract: search term
  - Keywords: find, search

**Helper Functions**:
- `extract-quantity` - Parse quantity (default: 1)
- `extract-unit` - Identify unit in text
- `extract-item-id` - Parse #N format
- `normalize-freezer-name` - Case-insensitive, trim spaces

### Message Handler

**Namespace**: `org.goat.module.Freezer`

**Command Routing**:
Using `defmodule` macro, define `process-channel-message` that:
1. Parses message text with parser functions
2. Routes to appropriate handler
3. Handles errors gracefully
4. Uses context when freezer not specified

**Handler Functions**:
- `handle-add-item` - Add item to freezer
- `handle-remove-item` - Remove item from freezer
- `handle-inventory` - Show freezer contents
- `handle-freezer-mgmt` - Manage freezers
- `handle-search` - Search for items
- `handle-help` - Display help

**Response Formatting**:
- `format-success` - Success messages with emojis
- `format-error` - Error messages
- `format-inventory` - Format inventory display
- `format-item-age` - Format "frozen for X days"
- `format-freezer-list` - Format freezer list

## Phased Implementation Plan

### Phase 1: Core Database & Infrastructure (Foundation)

**Objective**: Set up database layer and basic infrastructure

**Tasks**:
1. Create `src/clj/org/goat/db/freezer.clj`
2. Define database schema (freezers, items tables only)
3. Implement `create-db` with table creation
4. Implement basic freezer CRUD:
   - `add-freezer`
   - `get-freezers`
   - `get-freezer-by-name`
   - `set-default-freezer`
   - `get-default-freezer`
5. Implement basic item CRUD:
   - `add-item` (simple version, no update logic)
   - `get-items`
   - `get-item-by-id`
   - `remove-item`
6. Write unit tests for database layer

**Deliverable**: Working database layer with basic operations tested

**Estimated Complexity**: Low-Medium
**Dependencies**: None

---

### Phase 2: Command Parsing (Text Understanding)

**Objective**: Build natural language parser for commands

**Tasks**:
1. Create `src/clj/org/goat/module/freezer/parser.clj`
2. Implement extraction helpers:
   - `extract-quantity`
   - `extract-unit`
   - `extract-item-id`
   - `normalize-freezer-name`
3. Implement command parsers:
   - `parse-add-command`
   - `parse-remove-command`
   - `parse-inventory-command`
   - `parse-freezer-command`
4. Write unit tests for parser with various input formats

**Deliverable**: Robust parser handling various natural language inputs

**Estimated Complexity**: Medium
**Dependencies**: Phase 1 complete

---

### Phase 3: Basic Commands (MVP)

**Objective**: Implement core functionality with simple commands

**Tasks**:
1. Create `src/clj/org/goat/module/Freezer.clj`
2. Use `defmodule` macro to define module structure
3. Implement handlers:
   - `handle-add-item` (simple version)
   - `handle-remove-item` (by item ID only)
   - `handle-inventory` (single freezer)
4. Implement response formatters:
   - `format-success`
   - `format-error`
   - `format-inventory` (basic)
5. Register commands: `:freezer-add`, `:freezer-remove`, `:freezer-list`
6. Integration testing with Telegram bot

**Deliverable**: Working MVP with add/remove/list functionality

**Estimated Complexity**: Medium
**Dependencies**: Phase 1 & 2 complete

---

### Phase 4: Freezer Management (Multi-Freezer Support)

**Objective**: Enable users to manage multiple freezers

**Tasks**:
1. Update database layer:
   - `delete-freezer`
   - `rename-freezer`
2. Implement freezer management handler:
   - Create freezer
   - Delete freezer (with confirmation)
   - Rename freezer
   - List freezers
   - Set default freezer
3. Update parser for freezer commands:
   - `parse-freezer-command` (full version)
4. Update inventory handler to show all freezers
5. Add default freezer logic to add/remove handlers
6. Format freezer list output

**Deliverable**: Full freezer management capabilities

**Estimated Complexity**: Medium
**Dependencies**: Phase 3 complete

---

### Phase 5: Smart Item Management (Enhanced UX)

**Objective**: Improve item handling with smart features

**Tasks**:
1. Update database layer:
   - Enhance `add-item` to update quantity if item exists
   - Add `get-item-age` for date calculations
2. Enhance `handle-add-item`:
   - Check for existing items
   - Update quantity vs create new
   - Return "You now have X" message
3. Enhance `handle-remove-item`:
   - Support removal by item name (with ambiguity detection)
   - Warning when removing more than available
   - "Removed last X" messaging
4. Enhance inventory formatting:
   - Show item age ("frozen for X days")
   - Display temporary IDs (#1, #2, etc.)
   - Format with emojis and structure
5. Error handling improvements:
   - Ambiguous item detection
   - Item not found errors
   - No default freezer errors

**Deliverable**: Polished item management with smart behaviors

**Estimated Complexity**: Medium-High
**Dependencies**: Phase 4 complete

---

### Phase 6: Context Awareness (Smart Defaults)

**Objective**: Remember user's last freezer for better UX

**Tasks**:
1. Add `user_context` table to database
2. Implement context functions:
   - `set-user-context`
   - `get-user-context` (with timeout logic)
3. Update all handlers to:
   - Save context after each command
   - Use context when freezer not specified
4. Add context timeout (e.g., 5 minutes)
5. Test context across command sequences

**Deliverable**: Smart defaults based on recent commands

**Estimated Complexity**: Low-Medium
**Dependencies**: Phase 5 complete

---

### Phase 7: Search & Help (Discoverability)

**Objective**: Help users find items and learn commands

**Tasks**:
1. Implement search functionality:
   - `search-items` in database layer
   - `handle-search` in module
   - `parse-search-command` in parser
   - Format search results across freezers
2. Implement comprehensive help:
   - `handle-help` with examples
   - Command summary
   - Usage tips
3. Register `:freezer-find` and `:freezer-help` commands

**Deliverable**: Search and help functionality

**Estimated Complexity**: Low
**Dependencies**: Phase 6 complete

---

### Phase 8: Advanced Features (Nice-to-Have)

**Objective**: Add advanced features for power users

**Tasks**:
1. Expiry tracking:
   - Add expiry_date support in add-item
   - Parse "expires in X months/days" syntax
   - Implement expiry warning checker
   - Add command to show expiring items
2. Shopping list integration (if shopping module exists):
   - Prompt to add removed items to shopping list
   - Y/N confirmation handling
3. Statistics:
   - Most frozen items
   - Oldest items
   - Freezer utilization

**Deliverable**: Advanced features for enhanced UX

**Estimated Complexity**: Medium-High
**Dependencies**: Phase 7 complete
**Note**: Optional - assess based on user feedback

---

## Technical Considerations

### Database Patterns

Following existing patterns from `org.goat.db.users` and `org.goat.db.urls`:

1. **Connection Definition**:
```clojure
(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/freezer.db"})
```

2. **Table Creation Pattern**:
```clojure
(when-not (util/tbl-exists? db :freezers)
  (sql/db-do-commands db
    (sql/create-table-ddl :freezers [...]))
  (sql/execute! db "create unique index ..."))
```

3. **Query Pattern**:
```clojure
(-> (sql/query db ["SELECT ... WHERE ...=?" param])
    first
    :field)
```

4. **Insert/Update Pattern**:
```clojure
(sql/execute! db ["INSERT ... ON CONFLICT ... DO UPDATE ..." params])
```

### Module Patterns

Following `org.goat.module.CoreCommands`:

1. **Use defmodule macro** for cleaner code
2. **Message wrapping** is automatic
3. **Command routing** via `case` on `(msg/command m)`
4. **Response** via `(msg/reply m text)`
5. **HTML formatting** supported (e.g., `<b>bold</b>`)

### Error Handling

1. **Graceful degradation** - don't crash on bad input
2. **Helpful errors** - guide user to correct usage
3. **Validation** - check freezer exists, item exists, etc.
4. **Confirmation** - for destructive operations (delete freezer)

### Testing Strategy

1. **Unit tests** for database layer (each CRUD operation)
2. **Unit tests** for parser (various input formats)
3. **Integration tests** for full command flow
4. **Manual testing** via Telegram

## User Experience Flow Examples

### First Time Setup
```
User: /freezer add freezer garage
Bot: ✅ New freezer "Garage" created. I've set it as your default.

User: add 2 bags of peas
Bot: ✅ Added 2 bags of peas to the Garage Freezer.
```

### Adding to Existing Items
```
User: add 3 bags of peas
Bot: ✅ Added 3 bags of peas to the Garage Freezer. You now have 5.
```

### Viewing Inventory
```
User: inventory
Bot:
Inventory for: 🧊 Garage Freezer (default)
#1: 5x bags of peas (frozen 2 days ago)
#2: 1x tub of ice cream (frozen today)
```

### Removing Items
```
User: take 2 of #1
Bot: 👌 Removed 2 bags of peas from the Garage Freezer. 3 remaining.

User: use #2
Bot: 👌 Removed the last tub of ice cream from the Garage Freezer.
```

### Error Handling
```
User: take peas
Bot: Hmm, which freezer should I look in? You can specify with 'take peas from garage' or set a default with '/freezer set default garage'.

User: take chicken
Bot: I found "chicken breasts" and "chicken nuggets" in the Garage Freezer. Please use the item ID from the inventory list to be more specific.
```

## API Summary

### Commands to Register
- `:freezer` - Main command (shows help)
- `:freezer-add` or natural "add", "put", "store", "freeze"
- `:freezer-remove` or natural "take", "remove", "use", "get"
- `:freezer-inventory` or natural "inventory", "list", "show"
- `:freezer-find` or natural "find", "search"

### Message Handling
- **receive-messages**: `:all` (to catch natural language)
- **wants-private**: `true` (works in DMs and channels)

## Migration & Deployment

1. **Database creation** happens automatically on first run
2. **No migration needed** - new feature
3. **Module registration** - add to module list in bot configuration
4. **No impact** on existing modules

## Future Enhancements (Post-MVP)

1. Image support - photo of frozen items
2. Barcode scanning integration
3. Recipe integration - "Do I have ingredients for X?"
4. Shared freezers - multiple users per freezer
5. Export to CSV/PDF
6. Analytics - freezer usage over time
7. Mobile app integration
8. Voice command support via Telegram voice messages

## Success Metrics

1. **Functionality**: All commands work as specified
2. **Usability**: Natural language commands parsed correctly
3. **Reliability**: No data loss, proper error handling
4. **Performance**: Quick response times (<500ms)
5. **Adoption**: Users create freezers and track items

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Complex NLP parsing | Medium | Start with simple patterns, expand incrementally |
| Database corruption | High | Regular backups, transaction safety, CASCADE deletes |
| Ambiguous commands | Medium | Clear error messages, use item IDs as fallback |
| Context confusion | Low | Short timeout, clear feedback on which freezer used |
| Scale (many items) | Low | Use LIMIT/OFFSET for large inventories, pagination |

## Open Questions

1. **Command prefix**: Should we use `/freezer` prefix for all, or allow natural language everywhere?
   - **Recommendation**: Hybrid - support both `/freezer add` and natural "add"

2. **Quantity precision**: Support decimals (0.5 kg) or integers only?
   - **Recommendation**: Use REAL type, support decimals

3. **Multi-user freezers**: Share a freezer between household members?
   - **Recommendation**: Phase 9+ feature, start single-user

4. **Confirmation prompts**: How to handle Y/N responses?
   - **Recommendation**: Phase 8+, use simple text matching

## Conclusion

This design provides a solid foundation for a freezer management module that integrates seamlessly with the existing Goat chatbot architecture. The phased approach allows for incremental development and testing, with each phase delivering tangible value. The module follows established patterns in the codebase while providing a rich, user-friendly experience through natural language processing.

**Recommended Start**: Begin with Phase 1-3 to deliver an MVP, then gather user feedback before proceeding with advanced features.
