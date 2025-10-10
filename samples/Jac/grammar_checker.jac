import from byllm { Model }

glob llm = Model();

def correct_grammar(text: str) -> str by llm(temperature=0.9);

with entry {
    files_path = input("Enter the file path to the text file: ");
    with open(files_path, 'r') as file {
        text = file.read();
    }
    print("Original text:", text);
    corrected_text = correct_grammar(text);
    print("Corrected text:", corrected_text);
}
