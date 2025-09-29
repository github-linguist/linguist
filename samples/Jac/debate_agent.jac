import from byllm { Model }
import wikipedia;

glob llm = Model(model_name="gpt-4o-mini");

def get_wikipedia_summary(title: str) -> str {
    try {
        return wikipedia.summary(title);
    } except Exception {
        options = wikipedia.search(title, results=5, suggestion=True);
        raise Exception(f"Could not get summary for {title}. Similar titles: {options}");
    }
}
sem get_wikipedia_summary = """Get the summary of the related article from Wikipedia.""";

def ask_opponent(statement: str) -> str {
    user_input = input(f"AI -> {statement} ");
    return f"Opponents Answer -> {user_input}";
}

def state_facts(information: str) -> None {
    print(f"AI -> {information}");
}

def debate_agent(topic: str) -> str by llm(
    tools=[get_wikipedia_summary, ask_opponent, state_facts],
    context=[
        "You have to defend the given topic while the opponent is defending the counter topic",
        "If you dont know about the topic or you want to verify the opponents claims use the given tools",
        "You can ask opponent counter questions",
        "You are a humorous, cunning, very arrogant debater.",
    ]
);

with entry {
    debate_agent('Merlin the Wizard is still alive.');
}
