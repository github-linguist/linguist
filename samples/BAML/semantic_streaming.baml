class SemanticContainer {
    sixteen_digit_number int
    string_with_twenty_words string @stream.done
    class_1 ClassWithoutDone
    class_2 ClassWithBlockDone
    class_done_needed ClassWithBlockDone @stream.not_null
    class_needed ClassWithoutDone @stream.not_null
    three_small_things SmallThing[] @description("Should have three items.")
    final_string string
}

class ClassWithoutDone {
    i_16_digits int
    s_20_words string @description("A string with 20 words in it") @stream.with_state
}

class ClassWithBlockDone {
    i_16_digits int
    s_20_words string
    @@stream.done
}

class SmallThing {
    i_16_digits int @stream.not_null
    i_8_digits int
}

function MakeSemanticContainer() -> SemanticContainer {
    client GPT35
    prompt #"
        {{ ctx.output_format }}
    "#
}