import from byllm { Model }

glob llm = Model();

def get_expert(question: str) -> str by llm(method='Reason');
def get_answer(question: str, expert: str) -> str by llm();

with entry {
    question = "What are Large Language Models?";
    expert = get_expert(question);
    answer = get_answer(question, expert);
    print(f"{expert} says: '{answer}' ");
}
