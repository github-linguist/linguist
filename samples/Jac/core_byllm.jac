import from byllm.lib { Model }

glob llm = Model(verbose=True, model_name="gpt-4o-mini");

enum Personality {
    INTROVERT,
    EXTROVERT,
    AMBIVERT
}

obj Person {
    has full_name: str,
        yod: int,
        personality: Personality;
}
sem Person.yod = "Year of Death of the person";

glob personality_examples: dict[str, Personality] = {
    'Albert Einstein': Personality.INTROVERT,
    'Barack Obama': Personality.EXTROVERT
};

def get_person_info(name: str) -> Person by llm(
    reason=True,
    temperature=0.0,
    incl_info={"personality_examples": personality_examples}
);

with entry {
    person_obj = get_person_info('Martin Luther King Jr.');
    print(
        f"{person_obj.full_name} was a {person_obj.personality} person who died in {person_obj.yod}"
    );
}
