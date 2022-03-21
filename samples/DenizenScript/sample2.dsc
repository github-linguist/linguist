player_chooses_new_owner_of_faction:
    type: world
    events:
        on player clicks item in faction_action_danger_zone_transfer_ownership_player_list_inventory:
            - define new_owner <context.item.flag[player_id]>

            - define faction <player.proc[get_player_faction]>

            - define faction_owner <player[<proc[get_owner].context[<[faction]>]>]>

            - if !<player.has_flag[has_ownership_offer]>:
                - if <[new_owner].has_flag[faction]>:
                    - if <[new_owner].uuid> == <[faction_owner].uuid>:
                        - narrate "<&color[#1569EA]>You can't transfer ownership to yourself! You are already the owner." format:faction_action_format
                        - inventory close
                    - flag player waiting_for_owner_transfer_request_acceptance:<[new_owner]> expire:10m
                    - inventory close
                - else:
                    - flag player waiting_for_owner_transfer_request_acceptance:<[new_owner]> expire:10m
                    - inventory close

                    - flag <[new_owner]> has_ownership_offer:<[faction]> expire:10m
                    - runlater out_of_time_for_transfer delay:5s def.new_owner:<[new_owner]>
                    - narrate targets:<[new_owner]> "<green><player.name>, wants you to become the new owner of their faction<reset>, <[faction].proc[get_display_name]>. <green>To accept, please issue the command<reset>: <yellow>accept<reset>. <green>To deny, please issue the command<reset>: <yellow>deny <green>or, do nothing. This request will expire in 10 minutes." format:faction_action_format
            - else:
                - narrate "<[new_owner].name> already has a valid offer. Please try again soon." format:faction_action_format

player_accepts_or_denies_offer_to_be_new_owner:
    type: world
    events:
        on player chats flagged:has_ownership_offer:
            - define old_owner <player[<player.flag[has_ownership_offer].proc[get_owner]>]>
            - define faction <player.flag[has_ownership_offer]>
            - if <context.message.to_lowercase> == accept:
                - determine passively cancelled

                # Removes both the old owner and new owner from their factions to avoid any errors.
                - if <[old_owner].has_flag[faction]>:
                    - flag server factions.<[old_owner].flag[faction]>.members:<-:<[old_owner].uuid>
                    - flag <[old_owner]> faction:!
                - if <player.has_flag[faction]>:
                    - flag server factions.<player.flag[faction]>.members:<-:<player.uuid>
                    - flag player faction:!

                - flag server factions.<[faction]>.owner:<player.uuid>
                - flag server factions.<[faction]>.members:->:<player.uuid>
                - flag player FACTION:<[faction]>

                # Re-adds the owner to the faction.
                - flag server factions.<[faction]>.members:->:<[old_owner].uuid>
                - flag <[old_owner]> faction:<[faction]>

                - flag <player> has_ownership_offer:!
                - flag <[old_owner]> waiting_for_owner_transfer_request_acceptance:!

                - narrate "You are now the owner of <player.flag[has_ownership_offer].proc[get_display_name]>!" format:faction_action_format
                - narrate "<player.name> has accepted the offer to become owner! You are now no longer the owner of <[faction].proc[get_display_name]>, but are now a regular member." targets:<[old_owner]> format:faction_action_format

            - else if <context.message.to_lowercase> == deny:
                - determine passively cancelled

                - flag <player> has_ownership_offer:!
                - flag <[old_owner]> waiting_for_owner_transfer_request_acceptance:!

                - narrate "You have denyed the offer." format:faction_action_format
                - narrate "<player.name> <red>has declined your offer to become owner of your faction." targets:<[old_owner]> format:faction_action_format
            - else:
                - determine cancelled

out_of_time_for_transfer:
    type: task
    definitions: new_owner
    script:
        - if <player.has_flag[has_ownership_offer]>:
            - narrate "Sorry, the ownership offer has expired." targets:<[new_owner]> format:faction_action_format
        - else:
            - stop