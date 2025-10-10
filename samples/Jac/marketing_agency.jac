import from byllm { Model }

# Note that these tools are not available in byllm package and
# Should be defined by the user him/herself.
import from byllm.tools.wikipedia_utils { wikipedia_summary }
import from byllm.tools.serper { search, scrape }

glob llm = Model(model_name="gpt-4o-mini");

def save_output_as_md(output: str, filename: str) -> None {
    with open(filename, 'w') as f {
        f.write(output);
    }
}

def persona_expert(requirements: str) -> str
by llm(
    method="ReAct",
    tools=[wikipedia_summary, search, scrape],
    max_prev_react_outputs=10,
    max_react_iterations=10,
    context=["DONOT SUMMARIZE. MAKE IT DETAILED"]
);
def demographic_expert(requirements: str) -> str
by llm(
    method="ReAct",
    tools=[wikipedia_summary, search, scrape],
    max_prev_react_outputs=10,
    max_react_iterations=10,
    context=["DONOT SUMMARIZE. MAKE IT DETAILED"]
);
def market_expert(requirements: str) -> str
by llm(
    method="ReAct",
    tools=[wikipedia_summary, scrape, search],
    max_prev_react_outputs=10,
    max_react_iterations=10,
    context=["DONOT SUMMARIZE. MAKE IT DETAILED"]
);
def demand_expert(requirements: str) -> str
by llm(
    method="ReAct",
    tools=[wikipedia_summary, search, scrape],
    max_prev_react_outputs=10,
    max_react_iterations=10,
    context=["DONOT SUMMARIZE. MAKE IT DETAILED"]
);
def manager(query: str) -> str
by llm(
    method="ReAct",
    tools=[persona_expert, demographic_expert, market_expert, search, scrape],
    max_react_iterations=10,
    max_prev_react_outputs=10,
    context=["DONOT SUMMARIZE. MAKE IT DETAILED"]
);

with entry {
    query = "Jaclang is a native superset of python with a focus on AI and ML. Jaclang allows developers to"
        "prototype AI Applications with ease by providing high level abstractions for AIML Usecases."
        "Perform a detailed analysis of the market for Jaclang and provide a detailed report on the market,"
        "demand, demographics and personas for the product.";
    save_output_as_md(manager(query), "marketing_report.md");
}
