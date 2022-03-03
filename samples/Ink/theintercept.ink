// Character variables. We track just two, using a +/- scale
VAR forceful = 0
VAR evasive = 0


// Inventory Items
VAR teacup = false
VAR gotcomponent = false


// Story states: these can be done using read counts of knots; or functions that collect up more complex logic; or variables
VAR drugged = false
VAR hooper_mentioned = false

VAR losttemper = false
VAR admitblackmail = false

// what kind of clue did we pass to Hooper?
CONST NONE = 0
CONST STRAIGHT = 1
CONST CHESS = 2
CONST CROSSWORD = 3
VAR hooperClueType = NONE

VAR hooperConfessed = false

CONST SHOE = 1
CONST BUCKET = 2
VAR smashingWindowItem = NONE

VAR notraitor = false
VAR revealedhooperasculprit = false
VAR smashedglass = false
VAR muddyshoes = false

VAR framedhooper = false

// What did you do with the component?
VAR putcomponentintent = false
VAR throwncomponentaway = false
VAR piecereturned = false
VAR longgrasshooperframe = false


// DEBUG mode adds a few shortcuts - remember to set to false in release!
VAR DEBUG = false
{DEBUG:
	IN DEBUG MODE!
	*	[Beginning...]	-> start
	*	[Framing Hooper...] -> claim_hooper_took_component
	*	[In with Hooper...] -> inside_hoopers_hut
- else:
	// First diversion: where do we begin?
 -> start
}

 /*--------------------------------------------------------------------------------
	Wrap up character movement using functions, in case we want to develop this logic in future
--------------------------------------------------------------------------------*/


 === function lower(ref x)
 	~ x = x - 1

 === function raise(ref x)
 	~ x = x + 1

/*--------------------------------------------------------------------------------

	Start the story!

--------------------------------------------------------------------------------*/

=== start === 

//  Intro
	- 	They are keeping me waiting. 
		*	Hut 14[]. The door was locked after I sat down. 
		I don't even have a pen to do any work. There's a copy of the morning's intercept in my pocket, but staring at the jumbled letters will only drive me mad. 
		I am not a machine, whatever they say about me.

	- (opts)
		{|I rattle my fingers on the field table.|}
 		* 	(think) [Think] 
 			They suspect me to be a traitor. They think I stole the component from the calculating machine. They will be searching my bunk and cases. 
			When they don't find it, {plan:then} they'll come back and demand I talk. 
			-> opts
 		*	(plan) [Plan]
 			{not think:What I am is|I am} a problem—solver. Good with figures, quick with crosswords, excellent at chess. 
 			But in this scenario — in this trap — what is the winning play?
 			* * 	(cooperate) [Co—operate] 
	 				I must co—operate. My credibility is my main asset. To contradict myself, or another source, would be fatal. 
	 				I must simply hope they do not ask the questions I do not want to answer.
		 			~ lower(forceful)
	 		* * 	[Dissemble] 
		 			Misinformation, then. Just as the war in Europe is one of plans and interceptions, not planes and bombs. 
		 			My best hope is a story they prefer to the truth. 
		 			~ raise(forceful)
	 		* * 	(delay) [Divert] 
		 			Avoidance and delay. The military machine never fights on a single front. If I move slowly enough, things will resolve themselves some other way, my reputation intact.
		 			~ raise(evasive)
		*	[Wait]		
	- 	-> waited

= waited 
	-	Half an hour goes by before Commander Harris returns. He closes the door behind him quickly, as though afraid a loose word might slip inside.
		"Well, then," he begins, awkwardly. This is an unseemly situation. 
		*	"Commander."
			He nods. <>
		*	(tellme) {not start.delay} "Tell me what this is about."
			He shakes his head. 
			"Now, don't let's pretend."
		*	[Wait]
			I say nothing.
	-	He has brought two cups of tea in metal mugs: he sets them down on the tabletop between us.
		*	{tellme} [Deny] "I'm not pretending anything."
			{cooperate:I'm lying already, despite my good intentions.}
			Harris looks disapproving. -> pushes_cup
		*	(took) [Take one]
			~ teacup = true
			I take a mug and warm my hands. It's <>
		*	(what2) {not tellme} "What's going on?"
			"You know already."
			-> pushes_cup
		*	[Wait]
			I wait for him to speak. 
			- - (pushes_cup) He pushes one mug halfway towards me: <>
	-	a small gesture of friendship. 
		Enough to give me hope?
 		* 	(lift_up_cup) {not teacup} [Take it] 
 				I {took:lift the mug|take the mug,} and blow away the steam. It is too hot to drink. 
 				Harris picks his own up and just holds it.
 				~ teacup = true
 				~ lower(forceful)
 		* 	{not teacup} [Don't take it] 
 				Just a cup of insipid canteen tea. I leave it where it is.
 				~ raise(forceful)
				
		*	{teacup} 	[Drink] 
				I raise the cup to my mouth but it's too hot to drink.

		*	{teacup} 	[Wait] 		
			I say nothing as -> lift_up_cup

- 	"Quite a difficult situation," {lift_up_cup:he|Harris} begins{forceful <= 0:, sternly}. I've seen him adopt this stiff tone of voice before, but only when talking to the brass. "I'm sure you agree."
 		* 	[Agree] 
 				"Awkward," I reply
 		* 	(disagree) [Disagree] 
 				"I don't see why," I reply
				 ~ raise(forceful)
				 ~ raise(evasive)
 		* 	[Lie] -> disagree
 		* 	[Evade] 
 				"I'm sure you've handled worse," I reply casually
 				~ raise(evasive)
	- 	{ teacup:
 			~ drugged  = true
			<>, sipping at my tea as though we were old friends
 	  	}
		<>.
 		
 	-
 		*	[Watch him]
			His face is telling me nothing. I've seen Harris broad and full of laughter. Today he is tight, as much part of the military machine as the device in Hut 5. 

 		*	[Wait]
 			I wait to see how he'll respond. 

 		*	{not disagree} [Smile]
 			I try a weak smile. It is not returned.
 			~ lower(forceful)
		
// Why you're here
	- 	
		"We need that component," he says.
		
	-	//"There's no alternative, of course," he continues.
		{not missing_reel:
			-> missing_reel -> harris_demands_component
		}
	-	
 		* 	[Yes]
 			"Of course I do," I answer. 
 		* (no) [No]
 			"No I don't. And I've got work to do..."
			"Work that will be rather difficult for you to do, don't you think?" Harris interrupts. 
			
 		* 	[Evade]
 				-> here_at_bletchley_diversion
 		* 	[Lie] 
 				-> no
 	-	-> missing_reel -> harris_demands_component

=== missing_reel === 
	*	[The stolen component...]
	*	[Shrug]
		I shrug. 
		->->
	- 	The reel went missing from the Bombe this afternoon. The four of us were in the Hut, working on the latest German intercept. The results were garbage. It was Russell who found the gap in the plugboard. 
	-	Any of us could have taken it; and no one else would have known its worth.
 	
 		*	{forceful <= 0 }[Panic] They will pin it on me. They need a scapegoat so that the work can continue. I'm a likely target. Weaker than the rest. 
 			~ lower(forceful)
 		*	[Calculate] My odds, then, are one in four. Not bad; although the stakes themselves are higher than I would like.
 			~ raise(evasive)
 		*	{evasive >= 0} [Deny] But this is still a mere formality. The work will not stop. A replacement component will be made and we will all be put back to work. We are too valuable to shoot. 
 			~ raise(forceful)
 	-	->->


=== here_at_bletchley_diversion
	"Here at Bletchley? Of course."
 	~ raise(evasive)
 	~ lower(forceful)
	"Here, now," Harris corrects. "We are not talking to everyone. I can imagine you might feel pretty sore about that. I can imagine you feeling picked on. { forceful < 0:You're a sensitive soul.}"

 	* (fine) "I'm fine[."]," I reply. "This is all some misunderstanding and the quicker we have it cleared up the better."
 		~ lower(forceful)
		"I couldn't agree more." And then he comes right out with it, with an accusation. 
	
	*	{forceful < 0}	"What do you mean by that?"

 	* (sore) { forceful >= 0 } "Damn right[."] I'm sore. Was it one of the others who put you up to this? Was it Hooper? He's always been jealous of me. He's..."
 		~ raise(forceful)
 		~ hooper_mentioned = true
		The Commander moustache bristles as he purses his lips. "Has he now? Of your achievements, do you think?" 
 		It's difficult not to shake the sense that he's { evasive > 1 :mocking|simply humouring} me.
		"Or of your brain? Or something else?"
 		* * 	"Of my genius.["] Hooper simply can't stand that I'm cleverer than he is. We work so closely together, cooped up in that Hut all day. It drives him to distraction. To worse."
				"You're suggesting Hooper would sabotage this country's future simply to spite you?" Harris chooses his words like the military man he is, each lining up to create a ring around me.
 					* * * 	[Yes] 			
	 							"{ forceful > 0:He's petty enough, certainly|I wouldn't put it past him}. He's a creep." { teacup : I set the teacup down.|I wipe a hand across my forehead.}
	 							~ raise(forceful)
	 							~ teacup = false
 					* * * 	[No] 			
	 							"No, { forceful >0:of course not|I suppose not}." { teacup :I put the teacup back down on the table|I push the teacup around on its base}. 
							 	~ lower(forceful)
								~ teacup = false
 					* * * 	[Evade] 		
	 							"I don't know what I'm suggesting. I don't understand what's going on."
	 							~ raise(evasive)
								"But of course you do." Harris narrows his eyes. 
								-> done

					- - - 	(suggest_its_a_lie) "All I can say is, ever since I arrived here, he's been looking to ways to bring me down a peg. I wouldn't be surprised if he set this whole affair up just to have me court—martialled."
							"We don't court—martial civilians," Harris replies. "Traitors are simply hung at her Majesty's pleasure."
 					* * * 	"Quite right[."]," I answer smartly.
 					* * * 	(iamnotraitor) "I'm no traitor[."]," I answer{forceful > 0 :smartly|, voice quivering. "For God's sake!"}
 					* * * 	[Lie] -> iamnotraitor
					- - - He stares back at me. 

 		* * 	"Of my standing.["] My reputation." { forceful > 0:I'm aware of how arrogant I must sound but I plough on all the same.|I don't like to talk of myself like this, but I carry on all the same.} "Hooper simply can't bear knowing that, once all this is over, I'll be the one receiving the knighthood and he..."
				"No—one will be getting a knighthood if the Germans make landfall," Harris answers sharply. He casts a quick eye to the door of the Hut to check the latch is still down, then continues in more of a murmur: "Not you and not Hooper. Now answer me." 
				For the first time since the door closed, I wonder what the threat might be if I do <i>not</i>. 
 				
 		* * 	[Evade] 				
 				~ teacup = false
 				~ raise(forceful)
 				"How should I know?" I reply, defensively. { teacup :I set the teacup back on the table.}  -> suggest_its_a_lie
 				

 	* [Be honest] 	-> sore
 	* [Lie] 		-> fine

-	(done) -> harris_demands_component


=== harris_demands_component ===
	"{here_at_bletchley_diversion:Please|So}. Do you have it?" Harris is {forceful > 3:sweating slightly|wasting no time}: Bletchley is his watch. "Do you know where it is?"
	 	* 	[Yes]
	 		"I do." 
	 		-> admitted_to_something
	 	* (nope) [No] "I have no idea." 
	 					-> silence
	 	* [Lie] 		-> nope
	 	* [Evade] 		
	 		"The component?"
			 ~ raise(evasive)
			 ~ lower(forceful)
			"Don't play stupid," he replies. "{ not missing_reel:The component that went missing this afternoon. }Where is it?"
			
	- 	{ not missing_reel:
			-> missing_reel -> 
		}
 		* 	[Co-operate] "I know where it is."
 			-> admitted_to_something
 		* (nothing) [Delay] "I know nothing about it." My voice shakes{ forceful > 0:  with anger|; I'm unaccustomed to facing off against men with holstered guns}. 

 		* [Lie] -> nothing
 		* [Evade] 

			"I don't know what gives you the right to pick on me. { forceful > 0:I demand a lawyer.|I want a lawyer.}"

			"This is time of war," Harris answers.  "And by God, if I have to shoot you to recover the component, I will. Understand?" He points at the mug,-> drinkit

		-	(silence) There's an icy silence. { forceful > 2:I've cracked him a little.|{ evasive > 2:He's tiring of my evasiveness.}} 

		// Drink tea and talk
		- (drinkit) "Now drink your tea and talk."
		 * { teacup  }   	[Drink] 			-> drinkfromcup
		 * { teacup  }   	[Put the cup down] 
		 		I set the cup carefully down on the table once more.
				~ teacup = false
				~ raise(forceful)
				-> whatsinit
		
		 * { not teacup  }  [Take the cup] 
		 		- - (drinkfromcup) I lift the cup { teacup :to my lips }and sip. He waits for me to swallow before speaking again.
			 		~ drugged  = true
			 		~ teacup    = true
		 * { not teacup  }  [Don't take it] 
		 		I leave the cup where it is. 
				~ raise(forceful)
				- - (whatsinit) "Why?" I ask coldly. "What's in it?"
				
	- 	"Lapsang Souchong," he {drinkfromcup:remarks|replies}, placing his own cup back on the table untouched. "Such a curious flavour. It might almost not be tea at all. You might say it hides a multitude of sins. As do you. Isn't that right?"
 
		 * (suppose_i_have) [Agree] 
			 	// Regrets
				"I suppose so," I reply. "I've done things I shouldn't have done." 
				 ~ lower(forceful)
				 -> harris_presses_for_details

		* (nothing_ashamed_of) { not drugged  }   [Disagree]
		 		"I've done nothing that I'm ashamed of."
 				-> harris_asks_for_theory

		 * (cant_talk_right) { drugged  }   [Disagree] 
			 	I open my mouth to disagree, but the words I want won't come. It is like Harris has taken a screwdriver to the sides of my jaw.
 				-> admitted_to_something.ive_done_things

 		 * {drugged} [Lie] 	-> cant_talk_right
 		 * {not drugged} [Lie] 	-> nothing_ashamed_of
		 * { drugged  }   [Evade] -> cant_talk_right

		 * { not drugged  }   [Evade] 
		 		"None of us are blameless, Harris. { forceful > 1:But you're not my priest and I'm not yours|But I've done nothing to deserve this treatment}. Now, please. Let me go. I'll help you find this damn component, of course I will." 
				//   Who do you blame?
				He appears to consider the offer. 
				 -> harris_asks_for_theory



=== harris_presses_for_details
// Open to Blackmail
	"You mean you've left yourself open," Harris answers. "To pressure. Is that what you're saying?"
	 	* [Yes] -> admit_open_to_pressure
	 	* { not drugged  } [No] 
	 			"I'm not saying anything of the sort," I snap back. "What is this, Harris? You're accusing me of treachery but I don't see a shred of evidence for it! Why don't you put your cards on the table?"
			 	~ raise(forceful)
			 	
				
	 	* {drugged} [No] 
	 			I shake my head violently, to say no, that's not it, but whatever is wrong with tongue is wrong with neck too. I look across at the table at Harris' face and realise with a start how sympathetic he is. Such a kind, generous man. How can I hold anything back from him?
			 	~ lower(forceful)
				I take another mouthful of the bitter, strange—tasting tea before answering.
			 	-> admit_open_to_pressure


	 	* { not drugged  } [Evade] 
	 			"You're the one applying pressure here," I answer { forceful > 1:smartly|somewhat miserably}. "I'm just waiting until you tell me what is really going on."
				 ~ raise(evasive)
	 	* { drugged  } [Evade] 				 
	 			"We're all under pressure here."
	 			He looks at me with pity. -> harris_has_seen_it_before

 	-	"It's simple enough," Harris says. -> harris_has_seen_it_before

= admit_open_to_pressure
	"That's it," I reply. "There are some things... which a man shouldn't do."
	 ~ admitblackmail  = true
	Harris doesn't stiffen. Doesn't lean away, as though my condition might be infectious. I had thought they trained them in the army to shoot my kind on sight. 
	He offers no sympathy either. He nods, once. His understanding of me is a mere turning cog in his calculations, with no meaning to it.
 	-> harris_has_seen_it_before


=== admitted_to_something
	// Admitting Something
	{ not drugged  :
		Harris stares back at me. { evasive == 0:He cannot have expected it to be so easy to break me.}
	- else:
		Harris smiles with satisfaction, as if your willingness to talk was somehow his doing.
	}
	"I see." 
	There's a long pause, like the delay between feeding a line of cypher into the Bombe and waiting for its valves to warm up enough to begin processing. 
	"You want to explain that?"
		 * 	[Explain] 
		 	I pause a moment, trying to choose my words. To just come out and say it, after a lifetime of hiding... that is a circle I cannot square.
		 	* * 	[Explain] 	-> ive_done_things
		 	* * 	{drugged} [Say nothing] 	-> say_nothing
		 	* * 	{not drugged} [Lie] 	-> claim_hooper_took_component

		 * { not drugged  }   [Don't explain]
		 		"There's nothing to explain," I reply stiffly. -> i_know_where
				
		 * { not drugged  }   [Lie] -> claim_hooper_took_component
		 * { not drugged  }   [Evade]
		 	"Explain what you should be doing, do you mean, rather than bullying me? Certainly." I fold my arms. -> i_know_where

		 * (say_nothing) { drugged  }   [Say nothing]
		 	I fold my arms, intended firmly to say nothing. But somehow, watching Harris' face, I cannot bring myself to do it. I want to confess. I want to tell him everything I can, to explain myself to him, to earn his forgiveness. The sensation is so strong my will is powerless in the face of it. 
			Something is wrong with me, I am sure of it. There is a strange, bitter flavour on my tongue. I taste it as words start to form.
		 	-> ive_done_things

= i_know_where
	"I know where your component is because it's obvious where your component is. That doesn't mean I took it, just because I can figure out a simple problem, any more than it means I'm a German spy because I can crack their codes."
	-> harris_asks_for_theory


= ive_done_things
	 "I've done things," I begin{harris_demands_component.cant_talk_right: helplessly}. "Things I didn't want to do. I tried not to. But in the end, it felt like cutting off my own arm to resist."
	-> harris_presses_for_details




=== harris_asks_for_theory
"Tell me, then," he asks. "What's your theory? You're a smart fellow — as smart as they come around here, and that's saying something. What's your opinion on the missing component? Accident, perhaps? Or do you blame one of the other men? { hooper_mentioned :Hooper?}"
 	* [Blame no—one] 
 		-> an_accident
 	* [Blame someone] -> claim_hooper_took_component

= an_accident
	"An accident, naturally." I risk a smile. "That damned machine is made from spare parts and string. Even these Huts leak when it rains. It wouldn't take more than one fellow to trip over a cable to shake out a component. Have you tried looking under the thing?"
	"Do you believe we haven't?"
	In a sudden moment I understand that his reply is a threat. 
	"Now," he continues. "Are you sure there isn't anything you want to tell me?"

	 * [Co-operate]
	 	"All right." With a sigh, your defiance collapses. "If you're searched my things then I suppose you've found { evasive > 1: what you need|my letters. Haven't you? In fact, if you haven't, don't tell me}.
		 ~ admitblackmail  = true
		Harris nods once. 
		<> -> harris_has_seen_it_before

	 * {evasive > 0} [Evade] "Only that you're being unreasonable, and behaving like a swine."
		// Loses temper
		"You imbecile," Harris replies, with sudden force. He is half out of his chair. "You know the situation as well as I do. Why the fencing? The Hun are poised like rats, ready to run all over this country. They'll destroy everything. You understand that, don't you? You're not so locked up inside your crossword puzzles that you don't see that, are you? This machine we have here — you men — you are the best and only hope this country has. God help her."
			~ losttemper  = true
			I sit back, startled by the force of his outburst. His carefully sculpted expression has curled to angry disgust. <i>He really does hate me</i>, I think. <i>He'll have my blood for the taste of it.</i>
		* * [Placate]
			"Now steady on," I reply, gesturing for him to be calm.
	 		
		* * [Mock] 
			"I can imagine how being surrounded by clever men is pretty threatening for you, Commander," I reply with a sneer. "They don't train you to think in the Armed Forces."
			 ~ raise(forceful)
			
		* * [Dismiss]
			"Then I'll be going, on and getting on with my job of saving her, shall I?" I even rise half to my feet, before he slams the tabletop.
			
		- - "Talk," Harris demands. "Talk now. Tell me where you've hidden it or who you passed it to. Or God help me, I'll take your wretched pansy body to pieces looking for it."
	 		-> harris_demands_you_speak




=== harris_has_seen_it_before
	"I've seen it before. A young man like you — clever, removed. The kind that doesn't go to parties. Who takes himself too seriously. Who takes things too far."
	He slides his thumb between two fingers.
	"Now they own you."
	
	 * [Agree] 
	 	"What could I do?" I'm shaking now. The night is cold and the heat—lamp in the Hut has been removed. "{ forceful > 2:I won't|I don't want to} go to prison."
	 	"Smart man," he replies. "You wouldn't last.  
		
	 * [Disagree] 
		 "I can still fix this."
		Harris shakes his head. "You'll do nothing. This is beyond you now. You may go to prison or may go to firing squad - or we can change your name and move you somewhere where your indiscretions can't hurt you. But right now, none of that matters. What happens to you doesn't matter. All that matters is where that component is. 
		
	 * { not drugged  }   [Lie] 
	 	"I wanted to tell you," I tell him. "I thought I could find out who they were. Lead you to them."
		Harris looks at me with contempt. "You wretch. You'll pay for what you've done to this country today. If a single man loses his life because of your pride and your perversions then God help your soul. 

	*  {drugged} {forceful < 0} [Apologise]
		"Harris, I..."
		~lower(forceful)
		"Stop it," he interrupts. "There's no jury here to sway.  And there's no time. 

- 	(tell_me_now) <> So why don't you tell me, right now. Where is it?"
	-> harris_demands_you_speak




=== harris_demands_you_speak
	His eyes bear down like carbonised drill—bits.
 * [Confess] 
	 	{ forceful > 1 :
		"You want me to tell you what happened? You'll be disgusted."
		-else:
			"All right. I'll tell you what happened." And never mind my shame.
		}
		"I can imagine how it starts," he replies.

 * { not drugged  } [Dissemble] -> claim_hooper_took_component
 * { drugged  } [Dissemble]
	 	My plan now is to blame Hooper, but I cannot seem to tell the story. Whatever they put in my tea, it rules my tongue. { forceful >1:I fight it as hard as I can but it does no good.|I am desperate to tell him everything. I am weeping with shame.} 
	 	
		 ~ lower(forceful)
-  -> i_met_a_young_man

 	


=== i_met_a_young_man
	//  Explain Story
	*	[Talk]
		"There was a young man. I met him in the town. A few months ago now. We got to talking. Not about work. And I used my cover story, but he seemed to know it wasn't true. That got me wondering if he might be one of us."
	-	Harris is not letting me off any more. 
		"You seriously entertained that possibility?"
	 * [Yes]
	 	"Yes, I considered it. <>
	 * [No] 
		"No. Not for more than a moment, of course. Everyone here is marked out by how little we would be willing to say about it."
		"Only you told this young man more than a little, didn't you?"
		I nod. "<>
	* [Lie] 
		"I was quite certain, after a while. After we'd been talking. <>
- 	He seemed to know all about me. He... he was quite enchanted by my achievements."
	The way Harris is staring I expect him to strike me, but he does not. He replies, "I can see how that must have been attractive to you," with such plain—spokeness that I think I must have misheard.

	 *  [Yes] "It's a lonely life in this place," I reply. "Lonely - and still one never gets a moment to oneself."
		"That's how it is in the Service," Harris answers. 
		* *	[Argue] "I'm not in the Service."
			Harris shakes his head. "Yes, you are."  
		* * [Agree] "Perhaps. But I didn't choose this life." 
			Harris shakes his head. "No. And there's plenty of others who didn't who are suffering far worse." 
		- - Then he waves the thought aside. 

	 * (nope) { not drugged  }  [No] "The boy was a pretty simpleton. Quite inferior. His good opinion meant nothing to be. Harris, do not misunderstand. I was simply after his body."
			 ~ raise(evasive)
			Harris, to his credit, doesn't flinch; but I can see he will have nightmares of this moment later tonight. I'm tempted to reach out and take his hand to worsen it for him.
	
	 * { drugged  }   		[No] 
	 	"It wasn't," I reply. "But I doubt you'd understand."
	 	He simply nods. 
	 * { not drugged  }   	[Lie] -> nope

-  "Go on with your confession."
- (paused) 
	 { not nope:
		That gives me pause. I hadn't thought of it as such. But I suppose he's right. I am about to admit what I did.
	}
	"There's not much else to say. I took the part from Bombe computing device. You seem to know that already. I had to. He was going to expose me if I didn't."
	//  So blackmail?
	"This young man was blackmailing you over your affair?"

	~ temp harris_thinks_youre_drugged = drugged

	 { drugged:
	 	~ drugged = false
		As Harris speaks I find myself suddenly sharply aware, as if waking from a long sleep. The table, the corrugated walls of the hut, everything seems suddenly more tangible than a moment before. 
		Whatever it was they put in my drink is wearing off. 
	}
	 
	 * (yes) [Yes] 
	 	"Yes. I suppose he was their agent. I should have realised but I didn't. Then he threatened to tell you. I thought you would have me locked up: I couldn't bear the thought of it. I love working here. I've never been so happy, so successful, anywhere before. I didn't want to lose it."
		"So what did you do with the component?" Harris talks urgently. He grips his gloves tightly in one hand, perhaps prepared to lift them and strike if it is required. "Have you passed it to this man already? Have you left it somewhere for him to find?" 
		* * (still_have)	[I have it] 	
				"I still have it. Not on me, of course. -> reveal_location_of_component

		* * (dont_have) 	[I don't have it] 	-> i_dont_have_it
		* * [Lie] 							-> dont_have
		* * [Tell the truth] 				-> still_have

	 * (notright) [No] 
	 	"No, Harris. The young man wasn't blackmailing me." I take a deep breath. "It was Hooper."
		{ not hooper_mentioned:
			"Hooper!" Harris exclaims, in surprise. {harris_thinks_youre_drugged:He does not doubt me for a moment.}
		- else:
			"Now look here," Harris interrupts. "Don't start that again."
		}
		 "It's the truth, Harris. If I'm going to jail, so be it, but I won't hang at Traitor's Gate. Hooper was the one who told the boy about our work. Hooper put the boy on to me. { forceful < 2:I should have realised, of course. These things don't happen by chance. I was a fool to think they might.} And then, once he had me compromised, he demanded I steal the part from the machine."
		 ~ revealedhooperasculprit  = true
		"Which you did." Harris leans forward. "And then what? You still have it? You've stashed it somewhere?"
		* * (didnt_have_long) [Yes] 
			"Yes. I only had a moment. -> reveal_location_of_component

		* * (passed_on) [No] -> passed_onto_hooper
		* * [Lie] 			-> passed_on
		* * [Evade] 		
			"I can't remember."
			He draws his gun and lays it lightly on the field table.
			"I'm sorry to threaten you, friend. But His Majesty needs that brain of yours, and that brain alone. There are plenty of other parts to you that our country could do better without. Now I'll ask you again. Did you hide the component?"
			* * * [Yes] -> didnt_have_long
			* * * (nope_didnt_hide) [No] 
			 		"Very well then." I swallow nervously, to make it look more genuine. -> passed_onto_hooper
			* * * [Lie] -> nope_didnt_hide 

			* * * [Evade] -> i_dont_have_it

	 * [Tell the truth] 	-> yes
	 * [Lie] 				-> notright

= i_dont_have_it
	"I don't have it any more. I passed it through the fence to my contact straight after taking it, before it was discovered to be missing. It would have been idiocy to do differently. It's long gone, I'm afraid."
	"You fool, Manning," Harris curses, getting quickly to his feet. "You utter fool. Do you suppose you will be any better off living under Hitler? It's men like you who will get us all killed. Men too feeble, too weak in their hearts to stand up and take a man's responsibility for the world. You're happier to stay a child all your life and play with your little childish toys."
	 * [Answer back]
	 	"Really, Commander," I reply. "It rather sounds like you want to spank me."
		"For God's sake," he declares with thick disgust, then swoops away out of the room.

	 * [Say nothing] 
	 	I say nothing. It's true, isn't it? I can't deny that I know there is a world out there, a complicated world of pain and suffering. And I can't deny that I don't think about it a moment longer than I have to. What use is thinking on a problem that cannot be solved? It is precisely our ability to avoid such endless spirals that makes us human and not machine.
		"God have mercy on your soul," Harris says finally, as he gets to his feet and heads for the door. "I fear no—one else will." 

	- -> left_alone

= passed_onto_hooper
	~ hooper_mentioned = true
	"No. I passed it on to Hooper."
	"I see. And what did he do with it?"
	 * [Evade] 
	 	"I don't know."
		"You can do better than that. Remember, there's a hangman's noose waiting for traitors."
		* * 	[Theorise] 
				"Well, then," I answer, nervously. "What would he do? Either get rid of it straight away — or if that wasn't possible, which it probably wouldn't be, since he'd have to arrange things with his contacts — so most likely, he'd hide it somewhere and wait, until you had the rope around my neck and he could be sure he was safe."
 				-> claim_hooper_took_component.harris_being_convinced

		* * [Shrug] -> claim_hooper_took_component.its_your_problem

	 * [Tell the truth] 
	 	"I don't think Hooper could have planned this in advance. So he'd need to get word to whoever he's working with, and that would take time. So I think he would have hidden it somewhere, and be waiting to make sure I soundly take the fall. That way, if anything goes wrong, he can arrange for the part to be conveniently re—found."
 		-> claim_hooper_took_component.harris_being_convinced

	 * [Lie]
		"I'm sure I saw him this evening, talking to someone by the fence on the woodland side of the compound. He's probably passed it on already. You'll have to ask him."

		 -> claim_hooper_took_component.harrumphs


/*--------------------------------------------------------------------------------
	Trying to frame Hooper
--------------------------------------------------------------------------------*/


=== claim_hooper_took_component
//  Blame Hooper
	"I saw Hooper take it."
	 ~ hooper_mentioned  = true
	 { losttemper  :
		"Did you?" 
		The worst of his rage is passing; he is now moving into a kind of contemptuous despair. I can imagine him wrapping up our interview soon, leaving the hut, locking the door, and dropping the key down the well in the yard. 
		And why wouldn't he? With my name tarnished they will not let me back to work on the Bombe — if there is the slightest smell of treachery about my name I would be lucky not be locked up for the remainder of the war.
	- else:
		 "I see." He is starting to lose his patience. I have seen Harris angry a few times, with lackeys and secretaries. But never with us. With the 'brains' he has always been cautious, treating us like children. 
		 And now I see that, like a father, he wants to smack us when we disobey him.
	}
	"Just get to the truth, man. Every <i>minute</i> matters."
	 * { admitblackmail  }   [Persist with this]
	 		"I know what you're thinking. If I've transgressed once then I must be guilty of everything else... But I'm not. We were close to cracking the 13th's intercept. We were getting correlations in the data. Then Hooper disappeared for a moment, and next minute the machine was down."
	 		
	 * [Tell the truth] 
	 		"Very well. I see there's no point in covering up. You know everything anyway."
			Harris nods, and waits for me to continue.
			 -> i_met_a_young_man

	 * { not admitblackmail }   [Persist with this]
	 			"This is the truth."
	 
	- 	I have become, somehow, an accustomed liar — the words roll easily off my tongue. Perhaps I am a traitor, I think, now that I dissemble as easily as one.
		"Go on," Harris says, giving me no indication of whether he believes my tale.
		 * 	[Assert] "I saw him take it," I continue. "Collins was outside having a cigarette. Peterson was at the table. But I was at the front of the machine. I saw Hooper go around the side. He leant down and pulled something free. I even challenged him. I said, 'What's that? Someone put a nail somewhere they shouldn't have?' He didn't reply."
		 	Harris watches me for a long moment.
		 	
		 * 	[Imply] "At the moment the machine halted, Peterson was at the bench and Collins was outside having a smoke. I was checking the dip—switches. Hooper was the only one at the back of the Bombe. No—one else could have done it."
				"That's not quite the same as seeing him do it," Harris remarks.
				 * * 	[Logical]
				 		"When you have eliminated the impossible..." I begin, but Harris cuts me off.
		 			
				 * * 	[Persuasive] 
				 		"You have to believe me." 
				 		"We don't have to believe anyone," Harris returns. "I will only be happy with the truth, and your story doesn't tie up. We know you've been leaving yourself open to pressure. We've been watching your activities for some time. But we thought you were endangering the reputation of this site; not risking the country herself. Perhaps I put too much trust in your intellectual pride."
						He pauses for a moment, considering something. Then he continues:
						"It might have been Hooper. It might have been you. -> we_wont_guess

				 * * 	[Confident] 
					"Ask the others," I reply, leaning back. "They'll tell you. If they haven't already, that's only because they're protecting Hooper. Hoping he'll come to his senses and stop being an idiot. I hope he does too. And if you lock him up in a freezing hut like you've done me, I'm sure he will."
						"We have," Harris replies simply. 
						It's all I can do not to gape.
						-> hoopers_hut_3

	- "We are left with two possibilities. You, or Hooper." The Commander pauses to smooth down his moustache. <>
	- (hoopers_hut_3) "Hooper's in Hut 3 with the Captain, having a similar conversation."
	 	
		 * 	"And the other men?["] Do we have a hut each? Are there  enough senior officers to go round?"
			"Collins was outside when it happened, and Peterson can't get round the machine in that chair of his," Harris replies. "That leaves you and Hooper.
		 * 	"Then you know I'm right.["] You knew all along. Why did you threaten me?"
			"All we know is that we have a traitor, holding the fate of the country in his hands. 
	- (we_wont_guess) <> We're not in the business of guessing here at Bletchley. We are military intelligence. We get answers." Harris points a finger. "And if that component has left these grounds, then every minute is critical."
	 * [Co-operate] 
			"I'd be happy to help," I answer, leaning forwards. "I'm sure there's something I could do."
			"Like what, exactly?"
			* * 	"Put me in with Hooper."
					 -> putmein
			* * 	"Tell Hooper I've confessed.["] Better yet. Let him see you marching me off in handcuffs. Then let him go, and see what he does. Ten to one he'll go straight to wherever he's hidden that component and his game will be up."
					Harris nods slowly, chewing over the idea. It isn't a bad plan even — except, of course, Hooper has <i>not</i> hidden the component, and won't lead them anywhere. But that's a problem I might be able to solve once I'm out of this place; and once they're too busy dogging Hooper's steps from hut to hut.
					"Interesting," the Commander muses. "But I'm not so sure he'd be that stupid. And if he's already passed the part on, the whole thing will only be a waste of time."
					* * * 	"Trust me. He hasn't.["] If I know that man, and I do, he'll be wanting to keep his options open as long as possible. If the component's gone then he's in it up to his neck. He'll take a week at least to make sure he's escaped suspicion. Then he'll pass it on."
							"And if we keep applying pressure to him, you think the component will eventually just turn up?"
							* * * * "Yes.["] Probably under my bunk."
									Harris smiles wryly. "We'll know that for a fake, then. We've looked there already. 
							* * * * "Or be thrown into the river." 
									"Hmm." Harris chews his moustache thoughtfully. "Well, that would put us in a spot, seeing as how we'd never know for certain. We'd have to be ready to change our whole approach just in case the part had got through to the Germans. 
							- - - -	 <> I don't mind telling you, this is a disaster, this whole thing. What I want is to find that little bit of mechanical trickery. I don't care where. In your luncheon box or under Hooper's pillow. Just somewhere, and within the grounds of this place."
							* * * * "Then let him he think he's off the hook.["] Make a show of me. And then you'll get your man."
									<i>Somehow</i>, I think. But that's the part I need to work.
									 -> harris_takes_you_to_hooper

							* * * * "Then you'd better get searching[."]," I reply, tiring of his complaining. A war is a war, you have to expect an enemy. -> its_your_problem

					* * * 	"You're right. Let me talk to him[."], then. As a colleague. Maybe I can get something useful out of him."
	 						-> putmein

					* * * "You're right." -> shake_head

	 * [Block] -> its_your_problem


= harris_being_convinced
	"Makes sense," Harris agrees, cautiously. { evasive > 1:I can see he's still not entirely convinced by my tale, as well he might not be — I've hardly been entirely straight with him.|I can see he's still not certain whether he can trust me.} "Which means the question is, what can we do to rat him out?"
	 * [Offer to help] 
	 	"Maybe I can help with that."
		"Oh, yes? And how, exactly?"
		 * * 	"I'll talk to him." 
				"What?"
				"Put me in with Hooper with him. Maybe I can get something useful out of him."
			 	-> putmein
		 * * 	"We'll fool him.["] He's waiting to be sure that I've been strung up for this, so let's give him what he wants. If he sees me taken away, clapped in irons — he'll go straight to that component and set about getting rid of it."
	 			-> harris_takes_you_to_hooper

	 * [Don't offer to help]
	 	I lean back.  -> its_your_problem

= putmein
	Harris shakes his head. 
	"He despises you. I don't see why he'd give himself up to you."
	 * [Insist] "Try me. Just me and him." 
	 	-> go_in_alone
	 * [Give in] "You're right." 
	 	-> shake_head


= shake_head
	// Can't help
	<> I shake my head. "You're right. I don't see how I can help you. So there's only one conclusion."
	"Oh, yes? And what's that?"
	 -> its_your_problem


= its_your_problem
// Won't Help
	"It's your problem. Your security breach. So much for your careful vetting process." 
	I lean back in my chair and fold my arms so the way they shake will not be visible. 
	"You'd better get on with solving it, instead of wasting your time in here with me."
 	-> harrumphs

= harrumphs
	Harris harrumphs. He's thinking it all over.
 	* { putmein  }   	[Wait] 
 		"All right," he declares, gruffly. "We'll try it. But if this doesn't work, I might just put the both of you in front of a firing squad and be done with these games. Worse things happen in time of war, you know."
		"Alone," I add.
		 -> go_in_alone

 	* { not putmein  }  [Wait] 
	 	"No," Harris declares, finally. "I think you're lying about Hooper. I think you're a clever, scheming young man — that's why we hired you — and you're looking for the only reasonable out this situation has to offer. But I'm not taking it. We know you were in the room with the machine, we know you're of a perverted persuasion, we know you have compromised yourself. There's nothing more to say here. Either you tell me what you've done with that component, or we will hang you and search just as hard. It's your choice."
	 -> harris_threatens_lynching


= go_in_alone
	"Alone?"
	"Alone."
	Harris considers it. I watch his eyes, flicking backwards and forwards over mine, like a ribbon—reader loading its program.
	* 	[Patient] "Well?"
	* 	[Impatient] "For God's sake, man, what do you have to lose?" 
	 	~ raise(forceful)
	- 	"We'll be outside the door," Harris replies, seriously. "The first sign of any funny business and we'll have you both on the floor in minutes. You understand? The country needs your brain, but it's not too worried about your legs. Remember that."
		Then he gets to his feet, and opens the door, and marches me out across the yard. The evening is drawing in and there's a chill in the air. My mind is racing. I have one opportunity here — a moment in which to put the fear of God into Hooper and make him do something foolish that places him in harm's way. But how to achieve it?
		"You ready?" Harris demands.
 	* (yes) [Yes]
 			"Absolutely."
 	* 	[No]
 			"No."
			"Too bad." 
 	* 	[Lie] -> yes

	- 	-> inside_hoopers_hut


/*--------------------------------------------------------------------------------
	Quick visit to see Hooper
--------------------------------------------------------------------------------*/

=== harris_takes_you_to_hooper
	// Past Hooper
	Harris gets to his feet. "All right," he says. "I should no better than to trust a clever man, but we'll give it a go." 
	Then, he smiles, with all his teeth, like a wolf. 
	 { claim_hooper_took_component.hoopers_hut_3:
		"Especially since this is a plan that involves keeping you in handcuffs. I don't see what I have to lose."
	- else:
		"Hooper's in Hut 3 being debriefed by the Captain. Let's see if we can't get his attention somehow."
	}
	// Leading you past Hooper
	He raps on the door for the guard and gives the man a quick instruction. He returns a moment later with a cool pair of iron cuffs. 
	"Put 'em up," Harris instructs, and I do so. The metal closes around my wrists like a trap. I stand and follow Harris willingly out through the door.
	But whatever I'm doing with my body, my mind is scheming. <i>Somehow,</i> I'm thinking, <i>I have to get away from these men long enough to get that component behind Hut 2 and put it somewhere Hooper will go. Or, otherwise, somehow get Hooper to go there himself...</i>
	Harris marches me over to Hut 3, and gestures for the guard to stand aside. Pushing me forward, he opens the door nice and wide. 
	// Hut 3
	"Captain. Manning talked. If you'd step out for a moment?"
	 * 	[Play the part, head down]
	 	From where he's sitting, I know Hooper can see me, so I keep my head down and look guilty as sin. The bastard is probably smiling.
 		

	 * 	[Look inside the hut]
		I look in through the door and catch Hooper's expression. I had half expected him to be smiling be he isn't. He looks shocked, almost hurt. "Iain," he murmurs. "You couldn't..."
		 
	 * 	(shouted) [Call to Hooper] 
	 	I have a single moment to shout something to Hooper before the door closes.
		"I'll get you Hooper, you'll see!" I cry. Then:
		 
		 	* * "Queen to rook two, checkmate!"[] I call, then laugh viciously, as if I am damning him straight to hell.
			 	~ hooperClueType = CHESS
			- - (only_catch) I only catch Hooper's reaction for a moment — his eyebrow lifts in surprise and alarm. Good. If he thinks it is a threat then he just might be careless enough to go looking for what it might mean.
				 
		 	* * "Ask not for whom the bell tolls!"
			He stares back at me, as if were a madman and perhaps for a split second I see him shudder.
			 

		 	* * "Two words: messy, without one missing!"[] I cry, laughing. It isn't the best clue, hardly worthy of The Times, but it will have to do.
		 		~ hooperClueType = CROSSWORD
 			-> only_catch

- 	The Captain comes outside, pulling the door to. "What's this?" he asks. "A confession? Just like that?"
	"No," the Commander admits, in a low voice. "I'm afraid not. Rather more a scheme. The idea is to let Hooper go and see what he does. If he believes we have Manning here in irons, he'll try to shift the component."
	"If he has it."
	"Indeed."
	The Captain peers at me for a moment, like I was some kind of curious insect.
	"Sometimes, I think you people are magicians," he remarks. "Other times you seem more like witches. Very well." 
	With that he opens the door to the Hut and goes back inside. The Commander uses the moment to hustle me roughly forward.
	 { shouted  :
		"And what was all that shouting about?" he hisses in my ear as we move towards the barracks. "Are you trying to pull something? Or just make me look incompetent?"
	- else:
		"This scheme of yours had better come off," he hisses in my ear. "Otherwise the Captain is going to start having men tailing <i>me</i> to see where I go on Saturdays."
	}
	* 	[Reassure] 
		{ not shouted :
			"It will. Hooper's running scared," I reply, hoping I sound more confident than I feel.
		- else:
			"Just adding to the drama," I tell him, confidently. "I'm sure you can understand that."
		}
		"I think we've had enough drama today already," Harris replies. "Let's hope for a clean kill."
		
	* 	[Dissuade] 
		{ not shouted:
			"The Captain thought it was a good scheme. You'll most likely get a promotion."
		- else:
			"I'm not trying to do anything except save my neck."
		}
		"Let's hope things work out," Harris agrees darkly.
		
	* 	[Evade] 
		"We're still in ear—shot if they let Hooper go. Best get us inside and then we can talk, if we must."
		"I've had enough of your voice for one day," Harris replies grimly. <>
		
	* 	[Say nothing]
		I let him have his rant. <> 
- 	He hustles me up the steps of the barracks, keeping me firmly gripped as if I had any chance of giving him, a trained military man, the slip. It's all I can do not to fall into the room.
 	-> slam_door_shut_and_gone




=== inside_hoopers_hut
	-  	Harris opens the door and pushes me inside. "Captain," he calls. "Could I have a moment?"
		The Captain, looking puzzled, steps out. The door is closed. Hooper stares at me, open—mouthed, about to say something. I probably have less than a minute before the Captain storms back in and declares this plan to be bunkum.
	 *	 [Threaten]
	 		"Listen to me, Hooper. We were the only men in that hut today, so we know what happened. But I want you to know this. I put the component inside a breeze—block in the foundations of Hut 2, wrapped in one of your shirts. They're going to find it eventually, and that's going to be what tips the balance. And there's nothing you can do to stop any of that from happening."
	 		~ hooperClueType = STRAIGHT
	 		
		His eyes bulge with terror. "What did I do, to you? What did I ever do?"
		 * * 	[Tell the truth] 
		 		"You treated me like vermin. Like something abhorrent."
				"You are something abhorrent."
				"I wasn't. Not when I came here. And I won't be, once you're gone."
				
		 * * 	[Lie] 
		 		"Nothing," I reply. "You're just the other man in the room. One of us has to get the blame."
 				
		 * * 	[Evade] 
		 		"It doesn't matter. Just remember what I said. I've beaten you, Hooper. Remember that."
		- - 	I get to my feet and open the door of the Hut. The Captain storms back inside and I'm quickly thrown out. 		-> hustled_out


	 * [Bargain] 
		 "Hooper, I'll make a deal with you. We both know what happened in that hut this afternoon. I know because I did it, and you know because you know you didn't. But once this is done I'll be rich, and I'll split that with you. I'll let you have the results, too. Your name on the discovery of the Bombe. And it won't hurt the war effort — you know as well as me that the component on its own is worthless, it's the wiring of the Bombe, the usage, that's what's valuable. So how about it?"
		Hooper looks back at me, appalled. "You're asking me to commit treason?"
		 * * 	[Yes]
		 		"Yes, perhaps. But also to ensure your name goes down in the annals of mathematics. -> back_of_hut_2
		 * * 	[No] 
			 	"No. It's not treason. It's a trade, plain and simple."
	 			
		 * * 	(lie) [Lie] 
		 		"I'm suggesting you save your own skin. I've wrapped that component in one of your shirts, Hooper. They'll be searching this place top to bottom. They'll find it eventually, and when they do, that's the thing that will swing it against you. So take my advice now. Hut 2."
				 ~ hooperClueType = STRAIGHT

		 * * 	[Evade] -> lie
		- - 	 -> no_chance

	 * [Plead] 
		"Please, Hooper. You don't understand. They have information on me.  I don't need to tell you what I've done, you know. Have a soul. And the component — it's nothing. It's not the secret of the Bombe. It's just a part. The German's think it's a weapon — a missile component. Let them have it. Please, man. Just help me."
		"Help you?" Hooper stares. "Help you? You're a traitor. A snake in the grass. And you're <i>queer</i>."
		 * * 	[Deny] 
		 		"I'm no traitor. You <i>know</i> I'm not. How much work have I done here against the Germans? I've given my all. And you know as well as I do, if the Reich were to invade, I would be a dead man. Please, Hooper. I'm not doing any of this lightly."
 				
		 * * 	[Accept]
		 		"I am what I am," I reply. "I'm the way I was made. But they'll hang me unless you help, Hooper. Don't let them hang me."
 				
		 * * 	[Evade] 
		 		"That's not important now. What matters is what you do, this evening."

		 - - 	"Assuming I wanted to help you," he replies, carefully. "Which I don't. What would I do?"
				"Nothing. Almost nothing. 
				-> back_of_hut_2

= back_of_hut_2
	<> All you have to do is go to the back of Hut 2. There's a breeze—block with a cavity. That's where I've put it. I'll be locked up overnight. But you can pick it up and pass it to my contact. He'll be at the south fence around two AM."
	~ hooperClueType = STRAIGHT
	 -> no_chance

= no_chance
	"If you think I'll do that then you're crazy," Hooper replies. 
	At that moment the door flies open and the Captain comes storming back inside.
	 -> hustled_out

= hustled_out
	// To Barracks
	Harris hustles me over to the barracks. "I hope that's the end of it," he mutters.
	"Just be sure to let him out," I reply. "And then see where he goes."
	 -> slam_door_shut_and_gone



/*--------------------------------------------------------------------------------
	Left alone overnight
--------------------------------------------------------------------------------*/


=== slam_door_shut_and_gone
	Then they slam the door shut, and it locks.
	{ hooperClueType == NONE :
		<> How am I supposed to manage anything from in here?
		*   [Try the door] -> try_the_door
		* 	[Try the windows] -> try_the_windows

	- else:
		I can only hope that Hooper bites. If he thinks I'm bitter enough to have framed him, and arrogant enough to have taunted him with {hooperClueType > STRAIGHT:a clue to} where the damning evidence is hidden... 
		If he hates me enough, and is paranoid enough, then he might {hooperClueType > STRAIGHT:unravel my little riddle and} go searching around Hut 2. 
	}

	 * 	[Wait] 	-> night_falls


= try_the_door
	I try the door. It's locked, of course. 
	 -> from_outside_heard

= from_outside_heard
	From outside, I hear a voice. Hooper's. He's haranguing someone.
	- (opts)
	*  (listened) [Listen at the keyhole] 
			I put my ear down to the keyhole, but there's nothing now. Probably still a guard outside, of course, but they're keeping mum.
			-> opts

	* { not try_the_windows  }   [Try the window] -> try_the_windows
	* { not try_the_door  } {listened}   [Try the door] -> try_the_door
	* { try_the_windows  }   [Smash the window] -> try_to_smash_the_window
	* { try_the_door  && try_the_windows  }   [Wait] 
	 		It's useless. There's nothing I can do but hope. I sit down on one corner of the bunk to wait.
 			-> night_falls

= try_the_windows
	I go over to the window and try to jimmy it open. Not much luck, but in my struggling I notice this window only backs on the thin little brook that runs down the back of the compound. Which means, if I smashed it, I might get away with no—one seeing.	 
	 -> from_outside_heard


= try_to_smash_the_window
	The window is my only way out of here. I just need a way to smash it.
	 * [Punch it] 
	 	I suppose my fist would do a good enough job. But I'd cut myself to ribbons, most likely. <>

	 * (use_bucket) [Find something] 
			 ~ smashingWindowItem = BUCKET
			I cast around the small room. There's a bucket in one corner for emergencies — I suppose I could use that. I pick it up but it's not very easy to heft. <>
	 * [Use something you've got] 
		 	I pat down my pockets but all I'm carrying is the intercept, which is no good at all.
			* * [Something you're wearing?] 
					Ah, but of course! I slip off one shoe and heft it by the toe. The heel will make a decent enough hammer, if I give it enough wallop.
					 ~ smashingWindowItem  = SHOE
					But I'll cut my hand to ribbons doing it. <>
			* * [Look around] -> use_bucket
	- 	And the noise would be terrible. There must be a way of making this easier. I'm supposed to be a thief now. What would a burglar do?
	 	* [Work slowly] 
	 		Work carefully? It's difficult to work carefully when all one's has is { smashingWindowItem == BUCKET :a bucket. It's rather like the sledgehammer for the proverbial nut|{ smashingWindowItem == SHOE :a shoe|nothing but brute force}}. 
			 * * 	[Just do it] -> time_to_move_now
			 * * 	[Look around for something] 
	 	* [Find something to help] 
	- -> find_something_to_smash_window


= time_to_move_now
	Enough of this. There isn't any time to lose. Right now they'll be following Hooper as he goes to bed, and goes to sleep; and then that's it. The minute he closes his eyelids and drifts off that's the moment that this trap swings shut on me.
	So I punch out the glass with my { smashingWindowItem == BUCKET :bucket|{ smashingWindowItem == SHOE :shoe|fist}} and it shatters with a terrific noise. Then I stop, and wait, to see if anyone will come in through the door.
	Nothing.
	 * (pause) [Wait a little longer] 
		 I pause for a moment longer. It doesn't do to be too careless...
	 * [Clear the frame of shards]
		With my jacket wrapped round my arm, I sweep out the remaining shards of glass. It's not a big window, but I'm not a big man. If I was Harris, I'd be stuffed, but as it is...

	 -	Then the door locks turns. The door opens. Then Jeremy — one of the guards, rather — sticks his head through the door. "I thought I heard..." 
		He stops. Looks for a moment. { smashingWindowItem ==BUCKET :Sees the bucket in my hand.|Sees the broken window.} Then without a moment's further thought he blows his shrill whistles and hustles into the hut, grabbing me roughly by my arms.
		{ pause:
			I'll never know if I hadn't have waited that extra moment — maybe I still could have got away. But, how far?
		}
		I'm hustled into one of the huts. Nowhere to sleep, but they're not interested in my comfort any longer. Harris comes in with the Captain.
		"So," Harris remarks. "Looks like your little trap worked. Only it worked to show <i>you</i> out for what you are."
		* 	[Tell the truth] 
			 { i_met_a_young_man  :
				"Please, Harris. You can't understand the pressure they put me under. You can't understand what it's like, to be in love but be able to do nothing about it..."
			- else:
				"Harris. They were blackmailing me. They knew about... certain indiscretions. You can understand, can't you, Harris? I was in an impossible bind..."
			}
		* 	[Lie]
			 "I had to get out, Harris. I had to provoke Hooper into doing something that would incriminate himself fully. He's too clever, you see..."
			 
		* 	[Evade] 
		 	"This proves nothing," I reply stubbornly. "You still don't have the component and without it, I don't see what you can hope to prove."

	 -	"Be quiet, man. We know all about your and your sordid affairs." The Captain curls his lip. "Don't you know there's a war on? Do you know the kind of place they would have sent you if it haven't had been for that brain of yours? Don't you think you owe it to your country to use it a little more?"
		 
		<i>Do I</i>, I wonder? <i>Do I owe this country anything, this country that has spurned who and what am I since the day I became a man?</i>
		 * [Yes] 
			 	My anger deflates like a collapsing equation, all arguments cancelling each other out. The world, of course, owes me nothing; and I owe it everything.
			 
		 * 	(alone) [No] 
				 <i>Of course not. I am alone; that is what they wanted me to be, because of who and what I love. So I have no nation, no country.</i>
				 

		 * [Lie] 	-> alone
		 * [Evade] 	
		 		<i>But what is a country, after all? A country is not a concept, not an ideal. Every country falls, its borders shift and move, its language disappears to be replaced by another. Neither the Reich nor the British Empire will survive forever, so what use is my loyalty to either? </i>
				<i>I may as well, therefore, look after myself. Something I have attempted, but failed miserably, to do.</i>
				 
	- //  Tell us where
		"I'm afraid we have only one option, Manning," Harris says. "Please, man. Tell us where the component is."
		 ~ notraitor  = true
		 ~ losttemper = false
		 * [Tell them]
		 	~ revealedhooperasculprit = false
		 	"All right." I am beaten, after all. "<>-> reveal_location_of_component

		 * [Say nothing] -> my_lips_are_sealed

= find_something_to_smash_window
	Let me see. There's the bunk, { not smashingWindowItem == BUCKET :a bucket,} nothing else. I have my jacket but nothing in the pockets — no handkerchief, for instance.
	- (opts)
	*   [The bunk] 	
		The bunk has a solid metal frame, a blanket, a pillow, nothing more.
		- - (bunk_opts)
		* *  [The frame]
			 	The frame is heavy and solid. I couldn't lift it or shift it without help from another man. And it wouldn't do me any good here anyway. I can reach the window perfectly well.
				 -> bunk_opts
		* * [The blanket] 
		 		The blanket. Perfect. I scoop it up off the bed and hold it in place over the window. -> smash_the_window
		* * [The pillow] 
		 		The pillow is fat and fluffy. I could put it over the window and it would muffle the sound of breaking glass, certainly; but I wouldn't be able to break any glass through it either. 
				 -> bunk_opts

		* * {bunk_opts > 1} [Something else] -> opts

	* [The jacket] 
			I slip off my jacket and hold it with one hand over the glass. -> smash_the_window
	* { not smashingWindowItem == BUCKET  }   [The bucket] 
	 		The bucket? Hardly. The bucket might do some good if I wanted to sweep up the glass afterwards, but it won't help me smash the glass quietly.
		 	-> opts


=== smash_the_window
	//  Smashing glass
		Then I heft { smashingWindowItem == BUCKET :up the bucket — this really is quite a fiddly thing to be doing in cuffs — |{ smashingWindowItem == SHOE : my shoe by its toe, |back my arm, }} and take a strong swing, trying to imagine it's Harris' face on the other side.
	 	~ smashedglass  = true
	 	~ smashingWindowItem = NONE
		*	[Smash!]
	-	The sound of the impact is muffled. With my arm still covered, I sweep out the remaining glass in the frame. 
	-	I'm ready to escape. The only trouble is — when they look in on me in the morning, there will be no question what has happened. It won't help me one jot with shifting suspicion off my back.
		* [Wait]
		 		So perhaps I should wait it out, after all. Who knows? I might have a better opportunity later.
			 	-> night_passes
		* [Slip out] 
		 		Moving quickly and quietly, I hoist myself up onto the window—frame and worm my way outside into the freezing night air. Then I am away, slipping down the paths between the Huts, sticking to the shadows, on my way to Hut 2.
	// Out at night
	-
		 * [Go the shortest way] 
			 	There's no time to lose. Throwing caution to the wind I make my way quickly to Hut 2, and around the back. I don't think I've been seen but if I have it is too late. My actions are suspicious enough for the noose. I have no choice but to follow through.
		 * [Take a longer route]
		 		In case I'm being followed, I divert around the perimeter of the compound. It's a much longer path, and it takes me across some terrain that's difficult to negotiate in the dark — muddy, and thick with thistles and nestles.
				~ muddyshoes  = true
				Still, I can be confident no—one is behind me. I crouch down behind the rear wall of Hut 2. <>
	- 	The component is still there, wrapped in a tea—towel and shoved into a cavity in a breeze—block at the base of the Hut wall.
	 	* [Take it] 
	 		Quickly, I pull it free, and slip it into the pocket of my jacket.
			~ gotcomponent  = true
		
	 	* [Leave it] 
	 		Still there means no—one has found it, which means it is probably well—hidden. And short of skipping the compound now, I can afford to leave it hidden there a while longer. So I leave it in place.
	-  Where now?
	 	* 	[Back to the barracks] -> return_to_room_after_excursion
	 	* 	{ gotcomponent  }  [Go to Hooper's dorm] -> go_to_hoopers_dorm
 		* 	[Escape the compound] 
			Enough of this place. Time for me to get moving. I can get to the train station on foot, catch the postal train to Scotland and be somewhere else before anyone realises that I'm gone.
			 
			Of course, then they'll be looking for me in earnest. { not framedhooper :As a confirmed traitor.|Perhaps not as a traitor — they might take the idea that Hooper was involved with the theft — but certainly as a valuable mind, one containing valuable secrets and all too easily threatened. They will think I am running away because of my indiscretions. I suppose, in fairness, that I am.}
			* * [Go] 			-> live_on_the_run
			* * [Don't go] 
				 	It's no good. That's only half a solution. I couldn't be happy with that.
					* * * 	[Back to the barracks] 			-> return_to_room_after_excursion
					* * * 	{ gotcomponent   && not go_to_hoopers_dorm  }  [To Hooper's dorm] -> go_to_hoopers_dorm


/*--------------------------------------------------------------------------------
	Visit Hooper's dorm overnight
--------------------------------------------------------------------------------*/


=== go_to_hoopers_dorm
	// Hooper's Dorm
	I creep around the outside of the huts towards Hooper's dorm. Time to wrap up this little game once and for all. A few guards patrol the area at night but not many — after all, very few know this place even exists.
	Our quarters are arranged away from the main house; where we sleep is of less importance than where we work. We each have our own hut, through some are less permanent than others. Hooper's is a military issue tent: quite a large canopy, with two rooms inside and a short porch area where he insists people leave their shoes. It's all zipped up for the night and no light shines from inside.
	I hang back for a moment. If Harris is keeping to the terms of our deal then someone will be watching this place. But I can see no—one.
	 * (outer_zip) [Open the outer zip] 
		 	I creep forward to the tent, intent on lifting the zip to the front porch area just a little — enough to slip the component inside, and without the risk of the noise waking Hooper from his snoring.
			The work is careful, and more than little fiddly — Hooper has tied the zips down on the inside, the fastidious little bastard! — but after a little work I manage to make a hole large enough for my hand.
			* * [Slip in the component] 		
					I slide the component into the tent, work the zip closed, and move quickly away into the shadows. It takes a few minutes for my breath to slow, and my heart to stop hammering, but I see no other movement. If anyone is watching Hooper's tent, they are asleep at their posts.
					 ~ putcomponentintent  = true
					 ~ gotcomponent = false
					 -> return_to_room_after_excursion
			* * [No, some other way] 			
					Then pause. This is too transparent. Too blatant. If I leave it here, like this, Hooper will never be seen to go looking for it: he will stumble over it in plain sight, and the men watching will wonder why it was not there when he went to bed.
					No, I must try something else — or nothing at all.
					* * * 	[On top of the tent] -> put_component_on_tent
					* * * 	[Throw the component into the long grass] 
					 	From inspiration — or desperation, I am not certain — a simple approach occurs to me. -> toss_component_into_bushes
					* * * 	[Give up] 
							There is nothing to be gained here. I have the component now; maybe it will be of some value tomorrow.
							* * * * [Return to my barrack] -> return_to_room_after_excursion
							* * * * [Escape the compound] -> live_on_the_run

	 * (wide_circuit) [Look for another opening] 
		 	Making a wide circuit I creep around the tent. It has plenty of other flaps and openings, tied down with Gordian complexity. But nothing afford itself to slipping the component inside.
			* * [Try the porch zip] 			-> outer_zip
			* * [Try on top of the tent] 		-> put_component_on_tent
			* * [Give up] 						
				It's no good. Nothing I can do will be any less than obvious — something appearing where something was not there before. The men watching Hooper will know it is a deception and Hooper's protestations will be taken at face value.
				If I can't find a way for Hooper to pick the component up, as if from a hiding place of his own devising, and be caught doing it, then I have no plan at all.
				* * * [Return to my barrack] -> return_to_room_after_excursion
				* * * [Escape the compound] -> live_on_the_run
				* * * [Toss the component into the bushes] -> toss_component_into_bushes

	 * [Hide the component somewhere] 
	 		If I leave the component here somewhere it should be somewhere I can rely on Hooper finding it, but no—one before Hooper. In particular.
			* * [Behind the tent]			 	-> wide_circuit
			* * [Inside the porch section] 		-> outer_zip
			* * [On top of the canvas] 			-> put_component_on_tent


= put_component_on_tent
	A neat idea strikes me. If I could place it on top of the canvas, somewhere in the middle where it would bow the cloth inwards, then it would be invisible to anyone passing by. But to Hooper, it would be above him: a shadow staring him in the face as he awoke. What could be more natural than getting up, coming out, and looking to see what had fallen on him during the night?
	
	It's the work of a moment. I was once an excellent bowler for the second XI back at school. This time I throw underarm, of course, but I still land the vital missing component exactly where I want it to go. 
	 ~ framedhooper  = true
	 ~ gotcomponent = false
	For a second I hold my breath, but nothing and no—one stirs. -> return_to_room_after_excursion


= toss_component_into_bushes
	I toss the component away into the bushes behind Hooper's tent and return to my barrack, wishing myself a long sleep followed by a morning, free of this business.
	 ~ gotcomponent = false
	 ~ throwncomponentaway  = true
	 -> return_to_room_after_excursion

/*--------------------------------------------------------------------------------
	Ending: Run away from the camp
--------------------------------------------------------------------------------*/


=== live_on_the_run
	Better to live on the run than die on the spit. Creeping around the edge of the compound{ gotcomponent :, the Bombe component heavy in my pocket}, I make my way to the front gate. As always, it's manned by two guards, but I slip past their box by crawling on my belly.
	And then I'm on the road. Walking, not running. Silent. Free.
	//  End - Run Away
	For the moment, at least.
	-> END

/*--------------------------------------------------------------------------------
	Return to room after slipping out
--------------------------------------------------------------------------------*/


=== return_to_room_after_excursion
	{ gotcomponent :The weight of the Bombe component safely in my jacket|Satisfied}, I return the short way up the paths between the huts to the barrack block and the broken window. 
	It's a little harder getting back through — the window is higher off the ground than the floor inside — but after a decent bit of jumping and hauling I manage to get my elbows up, and then one leg, and finally I collapse inside, quite winded and out breath.
	 *  [Wait]  	-> night_passes

/*--------------------------------------------------------------------------------
	Night passes
--------------------------------------------------------------------------------*/


=== night_passes
// In room smashed glass
	The rest of the night passes slowly. I sleep a little, dozing mostly. Then I'm woken by the rooster in the yard. The door opens, and Harris comes in. He takes one look at the broken window and frowns with puzzlement.
	{ putcomponentintent: -> put_component_inside_tent }

	"What happened there?"
 	* [Confess] 
	 	"I broke it," I reply. There doesn't seem any use in trying to lie. "I thought I could escape. But I couldn't get myself through."
		The Commander laughs. -> glad_youre_here 
	
	* (deny) [Deny] 
	 	"I'm not sure. I was asleep: I woke up when someone broke the window. I looked out to see who it was, but they were already gone."
		Harris looks at me with puzzlement. "Someone came by to break the window, and then ran off? That's absurd. That's utterly absurd. Admit it, Manning. You tried to escape and you couldn't get through."
		* * [Admit it]
	 		"All right. {forceful>1:Damn you.} That's exactly it."
	 		-> glad_youre_here

	 	* * { not framedhooper  }   [Deny it]
	 		"If I wanted to escape, I would have made damn sure that I could," I tell him sternly. 
	 		-> harris_certain_is_you

	 	* * { framedhooper  }   [Deny it] 
		 	"I tell you, someone broke it. Someone wanted to threaten me, I think."
			Harris shakes his head. "Well, we can look into that matter later. For now, you probably want to hear the more pressing news. -> found_missing_component

 	* { gotcomponent  }   [Show him the component] -> someone_threw_component

= put_component_inside_tent
	He takes one look around, and sighs, a deep, wistful sigh.		
	"Things just get worse and worse for you, Manning," he remarks. "You are your own worst enemy."
	 * [Agree] 
	 	"I've thought so before." { admitblackmail :Certainly in the matter of getting blackmailed.}
		"Let me tell you what happened this morning. <>

	 * [Disagree]
	 	"Right now, I think you take that role, Harris," I reply coolly.
	 	- - (droll)	"Very droll," he replies. "Let me tell you what happened this morning. It will take the smile off your face. <>

	 * [Evade] 
	 	"I'm looking forward to having a wash and a change of clothes; which should make me a little less evil to be around."
	 	-> droll

	-	Our men watching Hooper's tent saw Hooper wake up, get dressed, clamber out of his tent and then step on something in at the entrance of his tent."
	 	~ piecereturned  = true
	 * [Be interested] 
	 	"You mean he didn't even hide it? He put it in his shoe?"
	 	- - (not_that) "No," Harris replies. "That isn't really what I mean. <>

	 * 	[Be dismissive]
	 	"So he's an idiot, and he hid it in his shoe."
		 -> not_that

	 * [Say nothing] 
	 	I say quiet, listening, not sure how this will go.
		"In case I'm not making myself clear," Harris continues, "<>

	- 	I mean, he managed to find it, by accident, somewhere where it wasn't the night before. And at the same time, you're sitting here with your window broken. So, I rather think you've played your last hand and lost. It's utterly implausible that Hooper stole that component and then left it lying around in the doorway of his tent. So I came to tell you that the game is up, for you."
		He nods and gets to his feet. -> left_alone



= someone_threw_component
	"Someone threw this in through the window over night," I reply, and open my jacket to reveal the component from the Bombe. "I couldn't see who, it was too dark. But I know what it is."
	He reaches out and takes it. "Well, I'll be damned," he murmurs. "That's it all right. And you didn't have it on you when we put you in here. But it can't have been Hooper — I had men watching him all night. And there's no—one else it could have been." 
	He turns the component over in his hands, bemused.
	 ~ piecereturned  = true
	 * [Suggest something] 
	 	"Perhaps Hooper had an accomplice. Someone else who works on site."
		Harris shakes his head, distractedly. "That doesn't make sense," he says. "Why go to all the trouble of stealing it only to give it back? And why like this?"
		 * * [Suggest something] 
		 	"Perhaps the accomplice thought it was Hooper being kept in here. Maybe they saw the guard..."
		 	-> all_too_farfetched
		 * * [Suggest nothing] 
	 * [Suggest nothing] 
	- 	I shrug, eloquently.
	- 	-> all_too_farfetched


= glad_youre_here
	"Shame," he remarks. "I should have left that window open and put a guard on you. Might have been interesting to see where you went. Anyway, I'm glad you're still here, even if you do smell like a dog."

	* { not framedhooper  }   [Be optimistic] 
	 	-> night_falls.morning_not_saved.optimism
	* { not framedhooper  }   [Be pessimistic] 
		-> night_falls.morning_not_saved.pessimism

	* { framedhooper  }   [Be optimistic] 
	 		"I'm looking forward to having a bath."
			//  Framed Hooper
			"Well, you should enjoy it. <> 

	* { framedhooper  }   [Be pessimistic]
	 		"I imagine I'll smell worse after another couple of days of this."
			"That won't be necessary. <>
	- -> found_missing_component


= found_missing_component
	// Framed Hooper
	We found the missing component. Or rather, Hooper found it for us. He snuck out and retrieved it from on top. Of all the damnest places — you would never have known it was there. He claimed ignorance when we jumped him, of course. But it's good enough for me."
	 * (devil) [Approve]
			"I can't tell you enough, I'm glad to hear it. I've had a devil of a night."
			 His gaze flicks to the broken window, but only for a moment. I think he genuinely cannot believe I could have done it.
	 * [Disapprove] 
	 		"You should never have hired him. A below-average intelligence can't be expected to cope with the pressure of our work."
 	- 	Harris rolls his eyes, but he might almost be smiling. "You'd better get along, { devil :and work through your devils|Mr Intelligent}. There's a 24—hour—late message to be tackled and we're a genius short. So you'd better be ready to work twice as hard."
 		* 	[Thank him] 	
 			"I'll enjoy it. Thank you for helping me clear this up."
			"Don't thank me yet. There's still a war to fight. Now get a move on."
			I nod, and hurry out of the door. The air outside has never tasted fresher and more invigorating. <>

 		* 	[Argue with him] 
 				"I'll work as hard as I work."
				"Get out," Harris growls. "Before I decide to arrest you as an accessory."
				I do as he says. Outside the barrack, the air has never smelt sweeter. 
	- -> head_for_my_dorm_free


=== night_falls ===
//  Night falls
	Night falls. The clockwork of the heavens keeps turning, whatever state I might be in. No—one can steal the components that make the sun go down and the stars come out. I watch it performing its operations. I can't sleep.
	{ hooperClueType > NONE  :
		Has Hooper taken my bait?
	}
	* 	[Look of out the window] 
			I peer out of the window, but it looks out onto the little brook at the back of the compound, with no view of the other huts or the House. Who knows if there are men up, searching the base of Hut 2, following one another with flashlights...
			 {inside_hoopers_hut.back_of_hut_2:
			 	Perhaps Hooper is there, in the dark, trying to help me after all?
			 }
	* 	[Listen at the door] 	
			I put my ear to the keyhole but can make out nothing. Are there still guards posted? { hooperClueType > NONE :Perhaps, if Hooper has managed to incriminate himself, the guards have been removed?|Perhaps the component has been found and the crisis is over.}
			Perhaps the door is unlocked and they left me to sleep? 
			* * 	[Try it] I try the handle. No such luck.
			* * 	[Leave it] I don't touch it. I don't want anyone outside thinking I'm trying to escape.
			 
	* 	[Wait] 					
			There is nothing I can do to speed up time. 

	- 	The night moves at its own pace. I suppose by morning I will know my fate.
 	* 	{ hooperClueType > NONE  }   	[Wait] 
 		// Hooper now arrested
		Morning comes. I'm woken by a rooster calling from the yard behind the House. I must have slept after all. I pull myself up from the bunk, shivering slightly. There is condensation on the inside of the window. I have probably given myself a chill.
		Without knocking, Harris comes inside. "You're up," he remarks, and then, "You smell like an animal."
		* * 	[Be friendly] 
				"I suppose I do rather." I laugh, but Harris does not.
				"This damn business gets worse and worse," he says, talking as he goes over to unlock and throw open the window. <>
		* * 	[Be cold] 
				"So would you," I reply tartly. Harris shrugs.
				"I've been through worse than this," he replies matter—of—factly. "It's hardly my fault if you sleep in your clothes."
				I glare back. He goes over to the window, unlocks it and throws it open, relishing the fresh air from outside.   
		- - 	"Hooper's confessed, you know."
		* * 	[Be eager] 
				"He has? I knew he would. The worm."
				"Steady now. Matters aren't over yet. <> 
		* * 	[Be cautious] 
				"Oh, yes?"
				"Yes. For what that's worth. <> 
		- -		(hooper_didnt_give_himself_up) There's still the issue of the component. It hasn't turned up. He didn't lead us to it. I guess he figured you must have had something on him. I don't know."
 				
				He looks quite put out by the whole affair. He is not the kind of man to deal well with probabilities.
 		* * 	[Be interested] 
 				"You mean he confessed of his own accord? You didn't catch him?"
				
 		* * 	[Be disinterested] 
 				"Well, I'm glad his conscience finally caught up with him," I reply dismissively.
		- - 	"The Captain went back into that hut and he confessed immediately. We were so surprised we didn't let you go." He wrinkles his nose. "I'm rather sorry about that now. I suggest you have a wash."
				And with that he gestures to the doorway.
 			* * 	[Go] 
 			* * 	[Wait] 
 				I hang back a moment. Something does not seem quite right. After all, Hooper did not steal the component. He has no reason to confess to anything. Perhaps this is another trap?
				"Well?" Harris asks. "What are you waiting for? Please don't tell me <i>you</i> want to confess now as well, I don't think my head could stand it."
	 				* * * 	[Confess] 
	 						After a chance like this? A chance — however real — to save my neck? To hand it over — what, to save Hooper's worthless skin?
 							* * * * [Confess] 
 									I see. Perhaps you think I bullied the man into giving himself up. Perhaps he understood my little clue far enough to know it was a threat against him, but not well enough to understand where he should look to find it. So he took the easy route out and folded. Gave me the hand.
									 ~ hooperConfessed  = true
										Hardly sporting, of course.
									* * * * * [Confess]
												Well, then. I suppose this must be what it feels like to have a conscience. I suppose I had always wondered.
												"Harris, sir. I don't know what Hooper's playing at, sir. But I can't let him do this."
												"Do what?"
												"Take the rope for this. I took it, sir. 
												 ~ revealedhooperasculprit = false
												 ~ losttemper = false
												 -> reveal_location_of_component
									* * * * * [Don't confess] 
 							* * * * [Don't confess] 
	 				* * * 	[Don't confess] 
	 				- - - 	"I certainly don't. But still, I'm surprised. I had Hooper down for a full—blown double agent, a traitor. He knows he'll face the rope, doesn't he?"
							"Don't ask me to explain why he did what he did," Harris sighs. "Just be grateful that he did, and you're now off the hook."
		- - 	Curiouser and curiouser. I nod once to Harris and slip outside into the cold morning air.
				 { hooperClueType == NONE  :
					Hooper's confession only makes sense in one fashion{ hooperConfessed :, and that is his being dim—witted and slow| — if I successfully implied to him that I had him framed, but he did not unpack my little clue well enough to go looking for the component. Well, I had figured him for a more intelligent opponent, but a resignation from the game will suffice}. Or perhaps he knew he would be followed if he went to check, and decided he would be doomed either way.
				- else:
					Hooper's confession only makes sense in one way — and that's that he believed me. He reasoned that he would be followed. To try and uncover the component would have got him arrested, and to confess was the same. 
					He simply caved, and threw in his hand.
				}
				// Outside, possibly free
				Of course, however, there is only one way to be certain that Harris is telling the truth, and that is to check the breeze—block at the back of Hut 2.
			* * [Check] -> go_to_where_component_is_hidden
			* * [Don't check]
					But there will time for that later. If there is nothing there, then Hooper discovered the component after all and Harris' men will have swooped on him, and the story about his confession is just a ruse to test me out. 
					And if the component is still there — well. It will be just as valuable to my contact in a week's time, and his deadline of the 31st is not yet upon us.
					 -> head_for_my_dorm_free

 	* 	{ hooperClueType == NONE  }   	[Wait] -> morning_not_saved

= morning_not_saved
	// Not saved
	Morning comes with the call of a rooster from the yard of the House. I must have slept after all. I pull myself up off the bunk, shivering slightly. There is condensation on the inside of the window. I have probably given myself a chill.
	It's not long after that Harris enters the hut. He closes the door behind him, careful as ever, then takes a chair across from me.
	"You smell like a dog," he remarks.
	* 	(optimism) [Be optimistic] 
	 	"I'm looking forward to a long bath," I reply. "And getting back to work."
	* 	(pessimism) [Be pessimistic] 
	 	"So would you after the night I've had."

	- 	-> harris_certain_is_you


=== harris_certain_is_you
	"Well, I'm afraid it is going to get worse for you," Harris replies soberly. "We followed Hooper, and he took himself neatly to bed and slept like a boy scout. Which puts us back to square one, and you firmly in the frame. And I'm afraid I don't have time for any more games. I want you to tell me where that component is, or we will hang you as a traitor."
	 ~ revealedhooperasculprit = false
	 ~ losttemper = false
	 -> harris_threatens_lynching




/*---------------------------------------------------------------
	Ending: they don't think it was you
---------------------------------------------------------------*/


=== head_for_my_dorm_free
I head for my dorm, intent on a bath, breakfast, a glance at the crossword before the other men get to it, and then on with work. They should have replaced the component in the Bombe by now. We will only be a day behind.
 { not framedhooper  :
	And then everything will proceed as before. The component will mean nothing to the Germans — this is the one fact I could never have explained to a man like Harris, even though the principle behind the Bombe is the same as the principle behind the army. The individual pieces — the men, the components — do not matter. They are identical. It is how they are arranged that counts. 
}
I bump into Russell in the dorm hut. 
"Did you hear?" he whispers. "Terrible news about Hooper. Absolutely terrible."
 * [Yes] 
 	"Quite terrible. I would never have guessed."
	"Well." Russell harrumphs. 
	- - (quince) "Quince was saying this morning, apparently his grandfather was German. So perhaps it's to be expected. See you there?"
 		
 * [No]

	"Heard what?"
 	- - (hooper_taken) "Hooper's been taken away. They caught him, uncovering that missing Bombe component from a hiding place somewhere, apparently about to take it to his contact." Russell harrumphs. -> quince
 * [Lie] 
 	"I don't know what you're talking about." 
 	-> hooper_taken
 * [Evade]
		"If you'll excuse me, Russell. I was about to take a bath."
		"Oh, of course. Worked all night, did you? Well, you'll hear soon enough. Can hardly hide the fact there'll only be three of us from now on."

- I wave to him and move away, my thoughts turning to the young man in the village. My lover. My contact. My blackmailer. Hooper may have taken the fall for the missing component, but { not framedhooper :if he did recover it from Hut 2 then | its recovery does mean }I have nothing to sell to save my reputation{ i_met_a_young_man :, if I have any left}. 
 { not framedhooper  :
If he didn't, of course, and Harris was telling the truth about his sudden confession, then I will be able to buy my freedom once and for all.
}
 * { not framedhooper  }   [Get the component] -> go_to_where_component_is_hidden
 * { not framedhooper  }   [Leave it] 
 	I will have to leave that question for another day. To return there now, when they're probably watching my every step, would be suicide. After all, if Hooper { hooperClueType == STRAIGHT :followed|understood} my clue, he will have explained it to them to save his neck. They won't believe him — but they won't quite disbelieve him either. We're locked in a cycle now, him and me, of half—truth and probability. There's nothing either of us can do to put the other entirely into blame.
 	-> ending_return_to_normal
 * [Act normal] 
 	But there is nothing to be done about it. -> ending_return_to_normal




=== ending_return_to_normal
Nothing, that is, except to act as if there is no game being played. I'll have a bath, then start work as normal. I've got a week to find something to give my blackmailer{ i_met_a_young_man : — or give him nothing: it seems my superiors know about my indiscretions now already}. 
 * [Co-operate] 
 	Something will turn up. It always does. An opportunity will present itself, and more easily now that Hooper is out of the way.
	But for now, there's yesterday's intercept to be resolved. 

 * [Dissemble] 
 	Or perhaps I might hand my young blackmailer over my superiors instead for being the spy he is.
	Perhaps that would be the moral thing to do, even, and not just the most smart. 
	But not today. Today, there's an intercept to resolve.

 * [Lie]
 	In a week's time, this whole affair will be in the past and quite forgotten. I'm quite sure of that. -> moreimportant
 * (moreimportant) [Evade] I've more important problems to think about now. There's still yesterday's intercept to be resolved. 
-  The Bombe needs to be set up once more and set running. 
It's time I tackled a problem I can solve.
//  End - Scot Free
-> END


=== go_to_where_component_is_hidden
	It won't take a moment to settle the matter. I can justify a walk past Hut 2 as part of my morning stroll. It will be obvious in a moment if the component is still there.
	On my way across the paddocks, between the huts and the House, I catch sight of young Miss Lyon, arriving for work on her bicycle. She giggles as she sees me and waves.
 	* 	[Wave back] 
	 		I wave cheerily back and she giggles, almost drops her bicycle, then dashes away inside the House. Judging by the clock on the front gable, she's running a little late this morning.
 	* 	[Ignore her] 
 			I give no reaction. She sighs to herself, as if this kind of behaviour is normal, and trots away inside the House to begin her duties.
	- 	I turn the corner of Hut 3 and walk down the short gravel path to Hut 2. It was a good spot to choose — Hut 2 is where the electricians work, and they're generally focussed on what they're doing. They don't often come outside to smoke a cigarette so it's easy to slip past the doorway unnoticed.
 	* 	[Check inside] 		
 			I hop up the steps and put my head inside all the same. Nobody about. Still too early in the AM for sparks, I suppose. <> 
 	* 	[Go around the back] 
	
	- 	I head on around the back of the hut. The breeze—block with the cavity is on the left side.
 	* 	(check) [Check] 		
 			No time to waste. I drop to my knees and check the breeze—block. Sure enough, there's nothing there. <i>Hooper took the bait.</i>
			Suddenly, there's a movement behind me. I look up to see, first a snub pistol, and then, Harris.

 	* 	[Look around] 
 			I pause to glance around, and catch a glimpse of movement. Someone ducking around the corner of the hut. Or a canvas sheet flapping in the light breeze. Impossible to be sure.
  			* * 	[Check the breeze—block] -> check
 			* * 	[Check around the side of the hut] 
 						But too important to guess. I move back around the side of the hut.
						Harris is there, leaning in against the wall. He holds a stub pistol in his hand.

	- 	{ hooperClueType > STRAIGHT  :
			"{ hooperClueType == CHESS:Queen to rook two|Messy without one missing whatever it was}," he declares. "I wouldn't have fathomed it but Hooper did. Explained it right after we sprung him doing what you're doing now. We weren't sure what to believe but now, you seem to have resolved that for us."
		- else:
			"Hooper said you'd told him where to look. I didn't believe him. Or, well. I wasn't sure what to believe. Now I rather think you've settled it."
		}
	 * 	[Agree] 
		 	"I have, rather." I put my hands into my pockets. "I seem to have done exactly that."
			"I'm afraid my little story about Hooper confessing wasn't true. I wanted to see if you'd go to retrieve the part." Harris gestures me to start walking. "You were close, Manning, I'll give you that. I wanted to believe you. But I'm glad I didn't."
			-> done
	 * 	[Lie] 
		 	"I spoke to Russell. He said he saw Hooper doing something round here. I wanted to see what it was."

	 * 	[Evade] 
		 	"Harris, you'd better watch out. He's planted a time—bomb here."
			Harris stares at me for a moment, then laughs. "Oh, goodness. That's rich."
			I almost wish I had a way to make the hut explode, but of course I don't.

	- 	"Enough." Harris gestures for me to start walking. "This story couldn't be simpler. You took it to cover your back. You hid it. You lied to get Hooper into trouble, and when you thought you'd won, you came to scoop your prize. A good hand but ultimately, { hooperClueType <= STRAIGHT  :if it hadn't have been you who hid the component, then you wouldn't be here now|you told Hooper where to look with your little riddle}."

 	- (done) 
	//   End - Caught in AM
		He leads me across the yard. Back towards Hut 5 to be decoded, and taken to pieces, once again.
		-> END


/*---------------------------------------------------------------
	Ending: they think it was you
---------------------------------------------------------------*/

=== harris_threatens_lynching
 	{ harris_certain_is_you:He passes a hand across his eyes with a long look of despair.|He gets to his feet, and gathers his gloves from the table top.}
	"I'm going to go outside and organise a rope. That'll take about twelve minutes. That's how long you have to decide."
	 * [Protest] 
	 	"You can't do this!" I cry. "It's murder! I demand a trial, a lawyer; for God's sake, man, you can't just throw me overboard, we're not barbarians...!"
		- - (too_clever) "You leave me no choice," Harris snaps back, eyes cold as gun—metal. "You and your damn cyphers. Your damn clever problems. If men like you didn't exist, if we could just all be <i>straight</i> with one another." He gets to his feet and heads for the door. "I fear for the future of this world, with men like you in. Reich or no Reich, Mr Manning, people like you simply <i>complicate</i> matters."
		 -> left_alone
	 * { not gotcomponent   && not throwncomponentaway  }   [Confess] 
	 		I nod. "I don't need twelve minutes. -> reveal_location_of_component
	 * [Stay silent] -> my_lips_are_sealed
	 * { gotcomponent  }   			[Show him the component] 
	 		"I don't need twelve minutes. Here it is."
			I open my jacket and pull the Bombe component out of my pocket. Harris takes it from me, whistling, curious.
			"Well, I'll be. That's it all right."
			"That's it."
			"But you didn't have it on you yesterday."
			* * [Explain] 
			 	"I climbed out of the window overnight," I explain. "I went and got this from where it was hidden, and brought it back here."
			* * [Don't explain]
				"No. I didn't."
			- -> all_too_farfetched

	 * { throwncomponentaway  }   	[Confess]
		 	"I don't need twelve minutes. The component is in the long grass behind Hooper's tent. I threw it there hoping to somehow frame him, but now I see that won't be possible. I was naive, I suppose."
			 ~ piecereturned  = true
			 -> reveal_location_of_component.harris_believes

	 * { throwncomponentaway  }   [Frame Hooper] 
		 	"Look, I know where it is. The missing piece of the Bombe is in the long grasses behind Hooper's tent. I saw him throw it there right after we finished work. He knew you'd scour the camp but I suppose he thought you'd more obvious places first. I suppose he was right about that. Look there. That <i>proves</i> his guilt."
			 ~ longgrasshooperframe  = true
			 ~ piecereturned  = true
			"That doesn't prove anything," Harris returns sharply. "But we'll check what you say, all the same." He gets to his feet and heads out of the door.
			 -> left_alone



=== reveal_location_of_component
	<> The missing component of the Bombe computer is hidden in a small cavity in a breeze—block supporting the left rear post of Hut 2. I put in there anticipating a search. I intended to { revealedhooperasculprit:pass it to Hooper|dispose of it} once the fuss had died down. I suppose I was foolish to think that it might."
	~ piecereturned  = true
	-> harris_believes	
= harris_believes
 	{ not night_falls.hooper_didnt_give_himself_up  :
		"Indeed. And Mr Manning: God help you if you're lying to me." 
	- else:
		"I thought as much. I hadn't expected you to give it out so easily, however. You understand, Hooper has said nothing, of course. In fact, he went to Hut 2 directly after we released him and uncovered the component. But he told us you had instructed him where to go. Hence my little double bluff. Frankly, I'll be glad when I'm shot of the lot of you mathematicians."
	}
	Harris stands, and slips away smartly. -> left_alone



=== my_lips_are_sealed
	I say nothing, my lips tightly, firmly sealed. It's true I am a traitor, to the very laws of nature. The world has taught me that since a very early age. But not to my country — should the Reich win this war, I would hardly be treated as an honoured hero. I was doomed from the very start.
	 ~ notraitor  = true
	I explain none of this. How could a man like Harris understand?
	The Commander takes one look back from the doorway as he pulls it to.
	"It's been a pleasure working with you, Mr Manning," he declares. "You've done a great service to this country. If we come through, I'm sure they'll remember you name. I'm sorry it had to end this way and I'll do my best to keep it quiet. No—one need know what you did."
	 -> left_alone




=== all_too_farfetched
	//  Returned Component
	"This is all too far—fetched," Harris says. "I'm glad to have this back, but I need to think."
	Getting to his feet, he nods once. "You'll have to wait a little longer, I'm afraid, Manning."
	Then he steps out of the door, muttering to himself.
	 -> make_your_peace



=== left_alone
	//  Alone, about to die
	{ slam_door_shut_and_gone.time_to_move_now :The Commander holds the door for his superior, and follows him out.} Then the door closes. I am alone again, as I have been for most of my short life.
	 -> make_your_peace


=== make_your_peace
	* [Make your peace]
	-	I am waiting again. I have no God to make my peace with. I find it difficult to believe in goodness of any kind, in a world such as this. 
 		{ not notraitor:
 			~ notraitor  = true
			But I am no traitor. Not to my country. To my sex, perhaps. But how could I support the Reich? If the Nazis were to come to power, I would be worse off than ever.
		}	
 		{ harris_threatens_lynching.too_clever:
			In truth, it is men like Harris who are complex, not men like me. I live to make things ordered, systematic. I like my pencils sharpened and lined up in a row. I do not deal in difficult borders, or uncertainties, or alliances. If I could, I would reduce the world to something easier to understand, something finite. 
			But I cannot, not even here, in our little haven from the horrors of the war.
		}
		I have no place here. No way to fit. I am caught, in the middle, cryptic and understood only thinly, through my machines. 
 			* 	I must seem very calm. 			
 			* 	Perhaps I should try to escape.[] But escape to where? I am already a prisoner. Jail would be a blessing. -> monastic
	- 	<> I suppose I do not believe they will hang me. They will lock me up and continue to use my brain, if they can. I wonder what they will tell the world — perhaps that I have taken my own life. That would be simplest. The few who know me would believe it.
		Well, then. Not a bad existence, in prison. Removed from temptation. 
	-	(monastic) A monastic life, with plenty of problems to keep me going. 
		I wonder what else I might yet unravel before I'm done?
 			* The door is opening.[] Harris is returning. Our little calculation here is complete. { not piecereturned: I can only hope one of the others will be able to explain to him that the part I stole will mean nothing to the Germans.|We are just pieces in this machine; interchangeable and prone to wear.}
	- 	That is the true secret of the calculating engine, and the source of its power. It is not the components that matter, they are quite repetitive. What matters is how they are wired; the diversity of the patterns and structures they can form. Much like people — it is how they connect that determines our victories and tragedies, and not their genius.
		Which makes me wonder. Should I give { i_met_a_young_man :up my beautiful young man|the young man who put me in this spot} to them as well as myself?
		 * 	[Yes] 
		 		But of course I will. { forceful > 2:Perhaps I can persuade them to put him in my cell.|A little vengeance, disguised as doing something good.}
		 * 	[No] 
		 		No. What would be the use? He will be long gone, and the name he told me is no doubt hokum. No: I was alone before in guilt, and I am thus alone again.
		 * 	[Lie] 
		 		No. Why would I? He is no doubt an innocent himself, trapped by some dire circumstance. Forced to act the way he did. I have every sympathy for him.
				Of course I do.
		 * 	[Evade] 
		 		It depends, perhaps, on what his name his worth. If it were to prove valuable, well; perhaps I can concoct a few more such lovers with which to ease my later days.
				{ hooper_mentioned: Hooper, perhaps. He wouldn't like that. }
	- 	{ not longgrasshooperframe  :
			Harris put the cuffs around my wrists. "I still have the intercept in my pocket," I remark. "Wherever we're going, could I have a pencil?"
		- else:
			"We recovered the part, just where you said it was," Harris reports, as he puts the cuffs around my wrists. "Of course, a couple of the men swear blind they searched there yesterday, so I'm afraid, what with the broken window... we've formed a perfectly good theory which doesn't bode well for you."
		}
	 	~ piecereturned  = true
		{ longgrasshooperframe  :
		"I see." It doesn't seem worth arguing any further. "I still have the intercept in my pocket," I remark. "Wherever we're going, could I have a pencil?"
		}
		He looks me in the eye.
		{ not losttemper  :
			"Of course. And one of your computing things, if I get my way. And when we're old, and smoking pipes together in The Rag like heroes, I'll explain to you the way that decent men have affairs. 
		- else:
			"I'll give you a stone to chisel notches in the wall. And that's all the calculations you'll be doing. And as you sit there, pissing into a bucket and growing a beard down to your toes, you have a think about how a <i>smart</i> man would conduct his illicit affairs. With a bit of due decorum you could have learnt off any squaddie.
		}
		<> You scientists." 
		He drags me up to my feet. 
		"You think you have to re—invent everything."
		With that, he hustles me out of the door and I can't help thinking that, with a little more strategy, I could still have won the day. But too late now, of course.
		-> END
