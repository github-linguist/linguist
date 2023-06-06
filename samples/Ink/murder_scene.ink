
/*

    This is a long example of a crime scene investigation, from the Writing with Ink chapter on Lists.

*/

-> murder_scene

// Helper function: popping elements from lists
=== function pop(ref list)
   ~ temp x = LIST_MIN(list) 
   ~ list -= x 
   ~ return x

//
//  System: items can have various states
//  Some are general, some specific to particular items
//


LIST OffOn = off, on
LIST SeenUnseen = unseen, seen

LIST GlassState = (none), steamed, steam_gone
LIST BedState = (made_up), covers_shifted, covers_off, bloodstain_visible

//
// System: inventory
//

LIST Inventory = (none), cane, knife

=== function get(x)
    ~ Inventory += x

//
// System: positioning things
// Items can be put in and on places
//

LIST Supporters = on_desk, on_floor, on_bed, under_bed, held, with_joe

=== function move_to_supporter(ref item_state, new_supporter) ===
    ~ item_state -= LIST_ALL(Supporters)
    ~ item_state += new_supporter


// System: Incremental knowledge.
// Each list is a chain of facts. Each fact supersedes the fact before 
//

VAR knowledgeState = ()

=== function reached (x) 
   ~ return knowledgeState ? x 

=== function between(x, y) 
   ~ return knowledgeState? x && not (knowledgeState ^ y)

=== function reach(statesToSet) 
   ~ temp x = pop(statesToSet)
   {
   - not x: 
      ~ return false 

   - not reached(x):
      ~ temp chain = LIST_ALL(x)
      ~ temp statesGained = LIST_RANGE(chain, LIST_MIN(chain), x)
      ~ knowledgeState += statesGained
      ~ reach (statesToSet)     // set any other states left to set
      ~ return true            // and we set this state, so true
 
    - else:
      ~ return false || reach(statesToSet) 
    }   

//
// Set up the game
//

VAR bedroomLightState = (off, on_desk)

VAR knifeState = (under_bed)


//
// Knowledge chains
//


LIST BedKnowledge = neatly_made, crumpled_duvet, hastily_remade, body_on_bed, murdered_in_bed, murdered_while_asleep

LIST KnifeKnowledge = prints_on_knife, joe_seen_prints_on_knife,joe_wants_better_prints, joe_got_better_prints

LIST WindowKnowledge = steam_on_glass, fingerprints_on_glass, fingerprints_on_glass_match_knife


//
// Content
//

=== murder_scene ===
    The bedroom. This is where it happened. Now to look for clues.
- (top)
    { bedroomLightState ? seen:     <- seen_light  }
    <- compare_prints(-> top)

    *   (dobed) [The bed...]
        The bed was low to the ground, but not so low something might not roll underneath. It was still neatly made.
        ~ reach (neatly_made)
        - - (bedhub)
        * *     [Lift the bedcover]
                I lifted back the bedcover. The duvet underneath was crumpled.
                ~ reach (crumpled_duvet)
                ~ BedState = covers_shifted
        * *     (uncover) {reached(crumpled_duvet)}
                [Remove the cover]
                Careful not to disturb anything beneath, I removed the cover entirely. The duvet below was rumpled.
                Not the work of the maid, who was conscientious to a point. Clearly this had been thrown on in a hurry.
                ~ reach (hastily_remade)
                ~ BedState = covers_off
        * *     (duvet) {BedState == covers_off} [ Pull back the duvet ]
                I pulled back the duvet. Beneath it was a sheet, sticky with blood.
                ~ BedState = bloodstain_visible
                ~ reach (body_on_bed)
                Either the body had been moved here before being dragged to the floor - or this is was where the murder had taken place.
        * *     {BedState !? made_up} [ Remake the bed ]
                Carefully, I pulled the bedsheets back into place, trying to make it seem undisturbed.
                ~ BedState = made_up
        * *     [Test the bed]
                I pushed the bed with spread fingers. It creaked a little, but not so much as to be obnoxious.
        * *     (darkunder) [Look under the bed]
                Lying down, I peered under the bed, but could make nothing out.

        * *     {TURNS_SINCE(-> dobed) > 1} [Something else?]
                I took a step back from the bed and looked around.
                -> top
        - -     -> bedhub

    *   {darkunder && bedroomLightState ? on_floor && bedroomLightState ? on}
        [ Look under the bed ]
        I peered under the bed. Something glinted back at me.
        - - (reaching)
        * *     [ Reach for it ]
                I fished with one arm under the bed, but whatever it was, it had been kicked far enough back that I couldn't get my fingers on it.
                -> reaching
        * *     {Inventory ? cane} [Knock it with the cane]
                -> knock_with_cane

        * *     {reaching > 1 } [ Stand up ]
                I stood up once more, and brushed my coat down.
                -> top

    *   (knock_with_cane) {reaching && TURNS_SINCE(-> reaching) >= 4 &&  Inventory ? cane } [Use the cane to reach under the bed ]
        Positioning the cane above the carpet, I gave the glinting thing a sharp tap. It slid out from the under the foot of the bed.
        ~ move_to_supporter( knifeState, on_floor )
        * *     (standup) [Stand up]
                Satisfied, I stood up, and saw I had knocked free a bloodied knife.
                -> top

        * *     [Look under the bed once more]
                Moving the cane aside, I looked under the bed once more, but there was nothing more there.
                -> standup

    *   {knifeState ? on_floor} [Pick up the knife]
        Careful not to touch the handle, I lifted the blade from the carpet.
        ~ get(knife)

    *   {Inventory ? knife} [Look at the knife]
        The blood was dry enough. Dry enough to show up partial prints on the hilt!
        ~ reach (prints_on_knife)

    *   [   The desk... ]
        I turned my attention to the desk. A lamp sat in one corner, a neat, empty in-tray in the other. There was nothing else out.
        Leaning against the desk was a wooden cane.
        ~ bedroomLightState += seen

        - - (deskstate)
        * *     (pickup_cane) {Inventory !? cane}  [Pick up the cane ]
                ~ get(cane)
              I picked up the wooden cane. It was heavy, and unmarked.

        * *    { bedroomLightState !? on } [Turn on the lamp]
                -> operate_lamp ->

        * *     [Look at the in-tray ]
                I regarded the in-tray, but there was nothing to be seen. Either the victim's papers were taken, or his line of work had seriously dried up. Or the in-tray was all for show.

        + +     (open)  {open < 3} [Open a drawer]
                I tried {a drawer at random|another drawer|a third drawer}. {Locked|Also locked|Unsurprisingly, locked as well}.

        * *     {deskstate >= 2} [Something else?]
                I took a step away from the desk once more.
                -> top

        - -     -> deskstate

    *     {(Inventory ? cane) && TURNS_SINCE(-> deskstate) <= 2} [Swoosh the cane]
        I was still holding the cane: I gave it an experimental swoosh. It was heavy indeed, though not heavy enough to be used as a bludgeon.
        But it might have been useful in self-defence. Why hadn't the victim reached for it? Knocked it over?

    *   [The window...]
        I went over to the window and peered out. A dismal view of the little brook that ran down beside the house.

        - - (window_opts)
        <- compare_prints(-> window_opts)
        * *     (downy) [Look down at the brook]
                { GlassState ? steamed:
                    Through the steamed glass I couldn't see the brook. -> see_prints_on_glass -> window_opts
                }
                I watched the little stream rush past for a while. The house probably had damp but otherwise, it told me nothing.
        * *     (greasy) [Look at the glass]
                { GlassState ? steamed: -> downy }
                The glass in the window was greasy. No one had cleaned it in a while, inside or out.
        * *     { GlassState ? steamed && not see_prints_on_glass && downy && greasy }
                [ Look at the steam ]
                A cold day outside. Natural my breath should steam. -> see_prints_on_glass ->
        + +     {GlassState ? steam_gone} [ Breathe on the glass ]
                I breathed gently on the glass once more. { reached (fingerprints_on_glass): The fingerprints reappeared. }
                ~ GlassState = steamed

        + +     [Something else?]
                { window_opts < 2 || reached (fingerprints_on_glass) || GlassState ? steamed:
                    I looked away from the dreary glass.
                    {GlassState ? steamed:
                        ~ GlassState = steam_gone
                        <> The steam from my breath faded.
                    }
                    -> top
                }
                I leant back from the glass. My breath had steamed up the pane a little.
               ~ GlassState = steamed

        - -     -> window_opts

    *   {top >= 5} [Leave the room]
        I'd seen enough. I {bedroomLightState ? on:switched off the lamp, then} turned and left the room.
        -> joe_in_hall

    -   -> top


= operate_lamp
    I flicked the light switch.
    { bedroomLightState ? on:
        <> The bulb fell dark.
        ~ bedroomLightState += off
        ~ bedroomLightState -= on
    - else:
        { bedroomLightState ? on_floor: <> A little light spilled under the bed.} { bedroomLightState ? on_desk : <> The light gleamed on the polished tabletop. }
        ~ bedroomLightState -= off
        ~ bedroomLightState += on
    }
    ->->


= compare_prints (-> backto)
    *   { between ((fingerprints_on_glass, prints_on_knife),     fingerprints_on_glass_match_knife) } 
[Compare the prints on the knife and the window ]
        Holding the bloodied knife near the window, I breathed to bring out the prints once more, and compared them as best I could.
        Hardly scientific, but they seemed very similar - very similiar indeed.
        ~ reach (fingerprints_on_glass_match_knife)
        -> backto

= see_prints_on_glass
    ~ reach (fingerprints_on_glass)
    {But I could see a few fingerprints, as though someone hadpressed their palm against it.|The fingerprints were quite clear and well-formed.} They faded as I watched.
    ~ GlassState = steam_gone
    ->->

= seen_light
    *   {bedroomLightState !? on} [ Turn on lamp ]
        -> operate_lamp ->

    *   { bedroomLightState !? on_bed  && BedState ? bloodstain_visible }
        [ Move the light to the bed ]
        ~ move_to_supporter(bedroomLightState, on_bed)

        I moved the light over to the bloodstain and peered closely at it. It had soaked deeply into the fibres of the cotton sheet.
        There was no doubt about it. This was where the blow had been struck.
        ~ reach (murdered_in_bed)

    *   { bedroomLightState !? on_desk } {TURNS_SINCE(-> floorit) >= 2 }
        [ Move the light back to the desk ]
        ~ move_to_supporter(bedroomLightState, on_desk)
        I moved the light back to the desk, setting it down where it had originally been.
    *   (floorit) { bedroomLightState !? on_floor && darkunder }
        [Move the light to the floor ]
        ~ move_to_supporter(bedroomLightState, on_floor)
        I picked the light up and set it down on the floor.
    -   -> top

=== joe_in_hall
    My police contact, Joe, was waiting in the hall. 'So?' he demanded. 'Did you find anything interesting?'
- (found)
    *   {found == 1} 'Nothing.'
        He shrugged. 'Shame.'
        -> done
    *   { Inventory ? knife } 'I found the murder weapon.'
        'Good going!' Joe replied with a grin. 'We thought the murderer had gotten rid of it. I'll bag that for you now.'
        ~ move_to_supporter(knifeState, with_joe)

    *   {reached(prints_on_knife)} { knifeState ? with_joe }
        'There are prints on the blade[.'],' I told him.
        He regarded them carefully.
        'Hrm. Not very complete. It'll be hard to get a match from these.'
        ~ reach (joe_seen_prints_on_knife)
    *   { reached((fingerprints_on_glass_match_knife, joe_seen_prints_on_knife)) }
        'They match a set of prints on the window, too.'
        'Anyone could have touched the window,' Joe replied thoughtfully. 'But if they're more complete, they should help us get a decent match!'
        ~ reach (joe_wants_better_prints)
    *   { between(body_on_bed, murdered_in_bed)}
        'The body was moved to the bed at some point[.'],' I told him. 'And then moved back to the floor.'
        'Why?'
        * *     'I don't know.'
                Joe nods. 'All right.'
        * *     'Perhaps to get something from the floor?'
                'You wouldn't move a whole body for that.'
        * *     'Perhaps he was killed in bed.'
                'It's just speculation at this point,' Joe remarks.
    *   { reached(murdered_in_bed) }
        'The victim was murdered in bed, and then the body was moved to the floor.'
        'Why?'
        * *     'I don't know.'
                Joe nods. 'All right, then.'
        * *     'Perhaps the murderer wanted to mislead us.'
                'How so?'
            * * *   'They wanted us to think the victim was awake[.'], I replied thoughtfully. 'That they were meeting their attacker, rather than being stabbed in their sleep.'
            * * *   'They wanted us to think there was some kind of struggle[.'],' I replied. 'That the victim wasn't simply stabbed in their sleep.'
            - - -   'But if they were killed in bed, that's most likely what happened. Stabbed, while sleeping.'
                    ~ reach (murdered_while_asleep)
        * *     'Perhaps the murderer hoped to clean up the scene.'
                'But they were disturbed? It's possible.'

    *   { found > 1} 'That's it.'
        'All right. It's a start,' Joe replied.
        -> done
    -   -> found
-   (done)
    {
    - between(joe_wants_better_prints, joe_got_better_prints):
        ~ reach (joe_got_better_prints)
        <> 'I'll get those prints from the window now.'
    - reached(joe_seen_prints_on_knife):
        <> 'I'll run those prints as best I can.'
    - else:
        <> 'Not much to go on.'
    }
    -> END


