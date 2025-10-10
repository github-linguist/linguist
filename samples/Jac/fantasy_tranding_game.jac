"""
Fantasy Trading Game - An interactive RPG trading simulation using byLLM
Demonstrates byLLM character generation, conversation, and transaction systems
"""

import from byllm { Model }
import from os { get_terminal_size }


glob llm = Model(model_name="gpt-4o");


glob person_record: dict = {};

obj InventoryItem {
    has name: str;
    has price: float;
}


obj Person {
    has name: str;
    has age: int;
    has hobby: str;
    has description: str;
    has money: float;
    has inventory: list[InventoryItem];
}


"""
A Chat object contains the person's name and the message.
"""
obj Chat {
    has person: str;
    has message: str;
}

"""
Makes a transaction between buyer and seller for the specified item.
Returns true if successful, false otherwise. The price is optional,
if not provided, the item's price is used (if they negotiate to a different price that should be given here
otherwiwse the price is optional and None will be used as a default parameter.
"""
def make_transaction(buyer_name: str, seller_name: str, item_name: str, price: int| None = None) -> bool {
    buyer = person_record[buyer_name];
    seller = person_record[seller_name];

    # Find the item in seller's inventory
    item_to_buy = None;
    item_index = -1;

    for i in range(len(seller.inventory)) {
        if seller.inventory[i].name.lower() == item_name.lower() {
            item_to_buy = seller.inventory[i];
            item_index = i;
            break;
        }
    }

    price = price or item_to_buy.price;

    # Check if item exists and buyer has enough money
    if not item_to_buy or buyer.money < price {
        return False;
    }

    # Transfer item and money
    buyer.money -= price;
    seller.money += price;
    buyer.inventory.append(item_to_buy);
    seller.inventory.pop(item_index);

    return True;
}


"""
Generates the player character for a fantasy RPG game.
"""
def make_player() -> Person
    by llm();


"""
Generates a random npc person with a name, age, favourite pet and hobby.
The person should be a fantasy character, like an elf, dwarf, orc, etc.
"""
def make_random_npc() -> Person
    by llm();


"""
Generates the next line of dialogue from the given NPC in an ongoing
conversation with the player. If no chat history is provided, generates
the NPC's initial greeting. The NPC's response should reflect their
personality, background, and any prior context from the chat history.

Before making a transaction, the NPC confirm with the player and after
they said yes, the transaction is made. Make sure the NPC doesn't give
an item way way less than its price, but they can negotiate a bit.
"""
def chat_with_player(player: Person, npc: Person, chat_history: list[Chat]) -> Chat
    by llm(tools=[make_transaction]);


    def clear_screen() {
        # ANSI escape sequence to clear screen and move cursor to top-left
        print("\033[2J\033[H", end="");
    }

    def print_inventory_table() {
        # Get terminal dimensions
        terminal_size = get_terminal_size();
        terminal_width = terminal_size.columns;
        terminal_height = terminal_size.lines;

        # Use full terminal width
        separator = "=" * terminal_width;
        print(separator);
        print("INVENTORY STATUS".center(terminal_width));
        print(separator);

        # Header
        player_header = "  PLAYER: " + player.name;
        npc_header = "NPC: " + npc.name;
        player_money = "  Money: $" + str(player.money);
        npc_money = "Money: $" + str(npc.money);

        # Calculate column widths based on terminal width
        half_width = (terminal_width - 2) // 2;
        print(f"{player_header.ljust(half_width)}{npc_header.ljust(half_width)}");
        print(f"{player_money.ljust(half_width)}{npc_money.ljust(half_width)}");
        print("-" * terminal_width);

        # Get max inventory length for proper formatting
        player_len = len(player.inventory);
        npc_len = len(npc.inventory);
        max_items = player_len if player_len > npc_len else npc_len;

        for i in range(max_items) {
            player_item = "";
            npc_item = "";

            if i < len(player.inventory) {
                item = player.inventory[i];
                player_item = "  " + item.name + " - $" + str(item.price);
            }

            if i < len(npc.inventory) {
                item = npc.inventory[i];
                npc_item = item.name + " - $" + str(item.price);
            }

            # Use dynamic column widths
            print(f"{player_item.ljust(half_width)}{npc_item.ljust(half_width)}");
        }

        print("=" * terminal_width);
        print("");  # Add spacing after inventory

        return 7 + max_items;  # Return number of lines used for inventory
    }

    def display_chat_history() {

        # Get terminal dimensions
        terminal_size = get_terminal_size();
        terminal_width = terminal_size.columns;
        terminal_height = terminal_size.lines;


        print("CONVERSATION".center(terminal_width));
        print("-" * terminal_width);

        # Calculate available space for chat messages
        inventory_lines = 7 + max(len(player.inventory), len(npc.inventory));
        available_lines = terminal_height - inventory_lines - 4;  # Reserve lines for input prompt and headers

        # Calculate how many messages we can show based on available space
        # Each message takes approximately 5-6 lines (speaker + bubble)
        lines_per_message = 6;
        max_messages = max(1, available_lines // lines_per_message);

        # Always show only the most recent messages that fit
        recent_messages = chat_display_history[-max_messages:] if len(chat_display_history) > max_messages else chat_display_history;

        for chat_msg in recent_messages {
            if chat_msg["type"] == "npc" {
                print_speech_bubble_inline(chat_msg["speaker"], chat_msg["message"], True);
            } else {
                print_speech_bubble_inline(chat_msg["speaker"], chat_msg["message"], False);
            }
        }
    }

    def print_speech_bubble_inline(speaker: str, message: str, is_npc: bool = True) {

        # Get terminal dimensions
        terminal_size = get_terminal_size();
        terminal_width = terminal_size.columns;
        terminal_height = terminal_size.lines;


        # Create speech bubble effect
        lines = [];
        words = message.split();
        current_line = "";
        # Use terminal width to determine max width for speech bubbles
        max_width = min(60, terminal_width - 20);  # Leave some margin

        for word in words {
            test_line = current_line + " " + word if current_line else word;
            if len(test_line) <= max_width {
                current_line = test_line;
            } else {
                if current_line {
                    lines.append(current_line);
                }
                current_line = word;
            }
        }
        if current_line {
            lines.append(current_line);
        }

        # Calculate bubble width based on terminal width
        bubble_width = min(64, terminal_width - 16);  # Adaptive bubble width

        if is_npc {
            # NPC speech bubble (left side)
            print(speaker + ":");
            print("." + "-" * (bubble_width - 2) + ".");
            for line in lines {
                padding_needed = bubble_width - 4 - len(line);
                padding = " " * padding_needed if padding_needed > 0 else "";
                print("| " + line + padding + " |");
            }
            print("'" + "-" * (bubble_width - 2) + "'");
        } else {
            # Player input bubble (right side, heavily indented)
            indent_size = max(20, terminal_width - bubble_width - 4);  # Push to right side
            indent = " " * indent_size;
            print(indent + speaker + ":");
            print(indent + "." + "-" * (bubble_width - 2) + ".");
            for line in lines {
                padding_needed = bubble_width - 4 - len(line);
                padding = " " * padding_needed if padding_needed > 0 else "";
                print(indent + "| " + line + padding + " |");
            }
            print(indent + "'" + "-" * (bubble_width - 2) + "'");
        }
        print("");  # Add spacing between messages
    }

    def render_ui() {

        # Get terminal dimensions
        terminal_size = get_terminal_size();
        terminal_width = terminal_size.columns;
        terminal_height = terminal_size.lines;


        clear_screen();
        inventory_lines = print_inventory_table();
        display_chat_history();

        # Fill remaining space to push input to bottom
        used_lines = inventory_lines + 2;  # +2 for conversation header
        chat_messages_count = len(chat_display_history);
        if chat_messages_count > 0 {
            # Calculate space used by recent messages
            available_lines = terminal_height - inventory_lines - 4;
            lines_per_message = 6;
            max_messages = max(1, available_lines // lines_per_message);
            displayed_messages = min(chat_messages_count, max_messages);
            used_lines += displayed_messages * lines_per_message;
        }

    }

with entry {

    # Example hardcoded characters (commented out for AI generation)
    # player = Person(name="Arin", age=24, hobby="swordsmanship", description="A brave and agile warrior skilled with the blade, ready to face any challenge.", money=150.0, inventory=[InventoryItem(name="Iron Sword", description="A sturdy iron sword, balanced and reliable.", price=100.0), InventoryItem(name="Leather Armor", description="Lightweight armor offering decent protection.", price=75.0), InventoryItem(name="Healing Potion", description="Restores health when consumed.", price=25.0)]);
    # npc = Person(name="Thalor", age=137, hobby="herbalism", description="An ancient elf who loves tending to mystical plants and caring for his pet raven.", money=80.0, inventory=[InventoryItem(name="Herb Pouch", description="A collection of rare herbs for potions.", price=40.0), InventoryItem(name="Raven Feather", description="A magical feather from his pet raven.", price=30.0)]);

    # Generate AI-powered characters
    player = make_player();
    npc = make_random_npc();

    person_record[player.name] = player;
    person_record[npc.name] = npc;

    history = [];
    chat_display_history = [];  # Store chat messages for display

    # Initial screen render
    render_ui();

    while True {
        # Generate NPC response and add to history
        chat = chat_with_player(player, npc, history);
        history.append(chat);

        # Add NPC message to display history
        chat_display_history.append({
            "type": "npc",
            "speaker": npc.name,
            "message": chat.message
        });

        # Re-render UI with new message
        render_ui();

        # Get player input
        inp = input("\nPlayer: ");
        if inp {
            # Add player message to display history
            chat_display_history.append({
                "type": "player",
                "speaker": player.name,
                "message": inp
            });

            history.append(Chat(person=player.name, message=inp));

            # Re-render UI with player response
            render_ui();
        }

    }

}