// Classify discord support threads based on messages

enum Status {
  Pending @description("Only the <author> of the <first message> has sent messages.")
  Ongoing @description("The <first message> has been answered by a user that is not the <author> of the <first message>.")
  Resolved @description("The conversation has concluded with at least one message from a user different from the <author> of the <first message>. There are no follow-up questions or messages.")
}

enum ResolvedBy {
  Team @description("The initial message has been addressed by someone inside the team. The conversation does not continue.")
  Community @description("The initial message has been addressed only by community members. Either the original issue has a provided solution or there is a concluding message. The issue in question may not be resolved.")
}

class Resolution {
  status Status
  resolved ResolvedBy | null
}

function ClassifyThread(messages: Message[]) -> Resolution {
  client "openai/gpt-4o"
  prompt #"
     {{ _.role('user')}}

     Classify the following list of messages into a <Status> and an optional <ResolvedBy>.
     Specify the <Status>. If <Status> is <Resolved>, specify whether it was resolved by the team or the community, using <ResolvedBy>.
     Message list is given at the end, after <THREAD>. Messages are given in chronological order and separated by ---.

     {{ ctx.output_format }}

     <THREAD>
     ---
     {% for message in messages %}
     {% if loop.first %}
     <first message>
     {% endif %}
     <author: {{ message.author.username }}> ({{ "team" if message.author.isFromTeam else "community" }}): {{ message.text }}
     {% if not loop.last %}
     ---
     {% endif %}
     {% endfor %}

  "#
}

test ClassifyResolvedByTeam {
  functions [ClassifyThread]
  args {
    messages [
      {
        author {
          username "PrimeTimeEngineer"
          isFromTeam false
        }
        text "Does BAML have an llms.txt and/or llms-full.txt to use with Code Agents? Trying to build a couple of quick, easy abstractions for the rest of my lab to use BAML (trying to plug it to all the researchers I can) using Gemini/Claude since I want to ensure the design I have in mind doesn't go against any key decisions made in BAMLs design and implementation (if there's a link someone could provide, that would be great - haven't been able to find one myself)."
      }

      {
        author {
          username "Antonio Sarosi"
          isFromTeam true
        }
        text "We do have llms.txt and llms-full.txt but llms-full.txt is just the home page, we have to change that to make it more detailed. We're using Fern for our docs and they have some automatic things for llms.txt but I think no one set that up yet."
      }
    ]
  }
  @@assert({{ this.status == 'Resolved' and this.resolved == 'Team' }})
}


test ClassifyResolvedByCommunity {
  functions [ClassifyThread]
  args {
    messages [
      {
        author {
          username "Chengxuan"
          isFromTeam false
        }
        text "curious if anyone did native tool call as well, wondering what is your experience between that and using baml? I perosnally didn't do any native tool call before, only used baml. OpenAI doc said using their native tool call has high accuracy, but my use case has  complex schema, defining the schema in OpenAI seems  very tedious. Also I feel we still need to tune the prompt in order to make OpenAI pick correct parameter, right?"
      }
      
      {
        author {
          username "Vu Nguyen[dꚙby]"
          isFromTeam false
        }
        text "interested!"
      }
      
      {
        author {
          username "Blacklight"
          isFromTeam false
        }
        text "models are not great at correctly-generating tool call arguments since they are specified as stringified json and do not use guided decoding so the JSON is not guaranteed to be valid"
      }
      
      {
        author {
          username "Blacklight"
          isFromTeam false
        }
        text "many have observed accuracy/quality issues with tool call argument values for complex schemas"
      }
      
      {
        author {
          username "PauloRavenna"
          isFromTeam false
        }
        text "@Chengxuan 👋 yes i've used native tool calling before. main issue is that even though the model will parse out the entire data model according to your definition, it will often miss out on extracting the relevant content from the input you give it if it's complex. on top of that, there's no easy way to test the changes from my experience with the native tool calling whenever you modify the prompt. what i was doing for native tool calling is that i would run it against my evals suite. the problem is that even with batched requests it would take a few mins to run evals against a dataset with 100s of rows. finally, there's some undocumented (or at least very hard to find) frustrations with native tool calling, a few examples include: the fact that OAI strongly encourages you to pass back the tool_call_id which is a UUID they generate. it works without it but it's unclear if that is a feature or a bug OAI's schema is more relaxed but when using third party models that support the OAI SDK sometimes they have different schema requirements that are often much stricter. TLDR: when it comes to these frustrations, you're at the helm of both the model providers and OAI's SDK I'm actually putting together a few blogs on all of this since I learned these lessons the hard way 😅 happy to share them here when I get them up!"
      }
      
      {
        author {
          username "Chengxuan"
          isFromTeam false
        }
        text "ah, thanks for the valuable insights. sounds BAML outperform here. :baml:"
      }
      
      {
        author {
          username "btisback"
          isFromTeam false
        }
        text "so you guys are of the view BAML outperform all native tool calling (even with Structured Schema -pydantic) and BAML always works? Is it because under the hood BAML uses SAP technique,a bit diff than tool calling? LC folks released some open source package called Trustcall. I never used it but is it comparable to BAML? I mostly use tool calling with Pydantic schema and it has always worked for me on decent schemas"
      }
      
      {
        author {
          username "Chengxuan"
          isFromTeam false
        }
        text "looks cool. I am not sure if BAML internally retry when validation error occurs. (Looks like in LLM word, retry is the rule of thumb😀 ) for my experience with BAML, usually the json inference works well (like containing the whole valid json), but sometimes the value of some fields are wrong, it mostly comes from LLM hallucination."
      }
    ]
  }
  @@assert({{ this.status == 'Resolved' and this.resolved == 'Community' }})
}

test ClassifyPending {
  functions [ClassifyThread]
  args {
    messages [
      {
        author {
          username "monk"
          isFromTeam false
        }
        text "One suggestion for AI that works, could be to push tiny models to their limits. I think the newer ones are quite capable, and it might be interesting to see your take on how inference can be distributed based on the cognitive load of tasks."
      }
    ]
  }
  @@assert({{ this.status == 'Pending' }})
}


test ClassifyOngoing {
  functions [ClassifyThread]
  args {
    messages [
      {
        author {
          username "Wendaolee"
          isFromTeam false
        }
        text "Hello,how can I get the workflow techpreview?👀I haven't find any exact ways to reach it in the blog or documentation.\n\nAnd,when will BAML have LSP support for VSCode or Cursor?I recommend BAML to my friends but without LSP it seem a little expensive for them to learn with coding."
      },
      {
        author {
          username "rjurney"
          isFromTeam false
        }
        text "Got an example with structured input rather that output?"
      },
      {
        author {
          username "rjurney"
          isFromTeam false
        }
        text "I feed two company records in, I get one combined one out."
      }
    ]
  }
  @@assert({{ this.status == 'Ongoing' }})
}
