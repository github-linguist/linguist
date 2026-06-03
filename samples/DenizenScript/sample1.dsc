# Main faction task.
faction:
    type: task
    script:
        - narrate Claim! format:faction_action_format
    edit:
        - narrate Edit! format:faction_action_format
    invite:
        - narrate Invite! format:faction_action_format
    settings:
        - narrate Settings! format:faction_action_format
    transfer_ownership:
        - if !<player.has_flag[waiting_for_owner_transfer_request_acceptance]>:
            - inventory open d:faction_action_danger_zone_transfer_ownership_player_list_inventory
        - else:
            - narrate "You are still waiting for <player.flag[waiting_for_owner_transfer_request_acceptance].name> to accept the ownership!" format:faction_action_format
    create:
        - if !<server.has_flag[factions]>:
            - flag server factions:<map[]>
            - flag server "factions.all claims:<list[]>"

        - flag server FACTION_IDS:++

        - define FACTION_UUID faction_<server.flag[FACTION_IDS]>

        - flag player FACTION:<[FACTION_UUID]>

        - definemap default_faction_data:
            owner: <player.uuid>
            members: <list[<player.uuid>]>
            name: <player.name>'s Faction
            settings:
                permissions:
                    # This is just place holder stuff ;0
                    - OwnersGetAllPerms
                    - MembersGetBasicPerms
                    - OutsidersDoNotGetPerms
                color: <white>
            display name: <white><player.name>'s Faction
            rivalries: <list[]>
            allies: <list[]>
            power: 100
            claims: <list[<player.location.chunk.cuboid>]>

        - note <player.location.chunk.cuboid> as:faction_cuboid_<[FACTION_UUID]>
        - flag server "factions.all claims:->:<player.location.chunk.cuboid>"
        - flag server factions.<[FACTION_UUID]>:<[default_faction_data]>

        - narrate "<green>Faction created! Check it out by using '/faction'!" format:faction_action_format
    delete:
        - define FACTION_UUID <player.flag[FACTION]>

        - define members <[FACTION_UUID].proc[get_members]>
        - foreach <[members]> as:m:
            - flag <player[<[m]>]> faction:!

        - foreach <proc[get_all_claims]> as:cl:
            - flag server "factions.all claims:<-:<[cl]>"

        - flag server factions:<-:<[FACTION_UUID]>

        - narrate "<&[success]>Successfully deleted the faction!" format:faction_action_format
    leave:
        - define faction <player.flag[faction]>
        - if <player.uuid> == <[faction].proc[get_owner]>:
            - narrate "Sorry, but you are going to have to tranfer ownership of your faction before you can leave it. You can do so in the <red>Danger Zone<reset> section of the Faction Action Inventory. (<bold>/f<reset>)" format:faction_action_format
            - stop
        - if <[faction].proc[get_members]> == <list[<player.uuid>]>:
            - narrate "You have successfully left the faction." format:faction_action_format
            - inject faction.delete
    wipe:
        - foreach <server.offline_players.include[<server.online_players>]> as:p:
            - if <[p].has_flag[FACTION]>:
                - flag <[p]> FACTION:!
        - foreach <proc[get_factions]> as:i:
            - flag server factions:<-:<[i]>
        - flag server "factions:<-:all claims"
        - flag server factions:!
        - foreach <server.notes[cuboids]> as:n:
            - if <[n].advanced_matches_text[*faction_cuboid_*]>:
                - note remove as:<[n].note_name>
        - flag server FACTION_IDS:-1
        - narrate Wiped. format:faction_action_format