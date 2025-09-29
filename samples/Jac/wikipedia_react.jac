import from byllm { Model }
import wikipedia;

glob llm = Model(verbose=True, model_name="gpt-4o-mini");

def get_wikipedia_summary(title: str) -> str {
    try {
        return wikipedia.summary(title);
    } except Exception {
        options = wikipedia.search(title, results=5, suggestion=True);
        raise Exception(f"Could not get summary for {title}. Similar titles: {options}");
    }
}
sem get_wikipedia_summary = """Get the summary of the related article from Wikipedia.""";


def get_answer(question: str) -> str by llm(tools=[get_wikipedia_summary]);

with entry {
    question = "Who is Jason Mars?";
    answer = get_answer(question);
    print(answer);
}
