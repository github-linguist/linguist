
function Completion(prefix: string, suffix: string, language: string) -> string {
  client "openai/gpt-4o"
  prompt ##"
    {{ _.role("system", cache_control={"type": "ephemeral"}) }}

    You are a programmer that suggests code completions in the %INSERT-HERE% part below with  {{ language }} code. Only output the code that replaces %INSERT-HERE% part, NOT THE SUFFIX OR PREFIX. Respond only with code, and with no markdown formatting.

    Try to complete a whole section inside curlies when you can.

    {% if language == "baml" %}
    {{ BAMLBackground2()}}

    Examples:
    INPUT:
    ---
    class MyObject {{"{"}}%INSERT-HERE%
    }
    ---
    OUTPUT:
    ---
      property string
    ---
    In this example, we just inserted one line, with tabs for a fake property to aid the user.

    INPUT:
    ---
    function foo(input: string) -> string {{"{"}} %INSERT-HERE%
      prompt #"
        {{ "{{ input }}" }}
      "#
    }
    ---
    OUTPUT:
    ---
      client "openai/gpt-4o"
    ---
    In this example, no need to add the prompt because it was part of the suffix after INSERT-HERE

    INPUT:
    OUTPUT: N/A
    In this example there was nothing to complete, so we returned N/A.

    Ignore the "---" in your outputs.
    {% endif %}


    {{ _.role("user") }}
    INPUT:
    ---
    {{ prefix }}%INSERT-HERE%{{ suffix }}
    ---
  "##
}

test CompletionTest3 {
  functions [Completion]
  args {
    prefix ##"function foo(input: string) -> string {
      client "openai/gpt-4o"
      prompt #"
    "##
    suffix ""
    language "baml"
  }
}

test CompletionTest2 {
  functions [Completion]
  args {
    prefix "function foo(input: string) -> string {\n"
    suffix "\n  prompt #\n\""
    language "baml"
  }
}
 
template_string Hi(
  hello: string,
  world: string,
) ##"
  {{ hello }} {{ world }}
"##

template_string Hi3(
  hello: string,
  world: string,
) #"
  {{ hello }} {{ world }}
"#

template_string BAMLBackground2() ##"
  <Overview>
    BAML is a domain-specific language for building LLM prompts as functions.
      client "openai/gpt-4o"
      // prompt with jinja syntax inside here. with double curly braces for variables.
      // make sure to include: {{ "{{ ctx.output_format }}"}} in the prompt, which prints the output schema instructions so the LLM returns the output in the correct format (json or string, etc.). DO NOT write the output schema manually.
      prompt #"
        
      "#
    }

      3. You do not need to specify to "answer in JSON format". Only write in the prompt brief instruction, and any other task-specific things to keep in mind for the task.
      4. Write a {{ "{{ _.role(\"user\") }}" }} tag to indicate where the user's inputs start. So if there's a convo you can write
      #"{{ "{{ _.role(\"user\") }}" }} {{ "{{ some-variable }}" }}#
    </Prompt>
  </Functions>

  The @asserts only go in the "output" types. Don't use them in inputs.
  Do NOT use numbers as confidence intervals if you need to use them. Prefer an enum with descriptions or literals like "high", "medium", "low".

  Dedent all declarations.
"##

template_string BamlTests() ##"
  // For image inputs:
  test ImageTest {
    functions [MyFunction]
    args {
      imageArg {
        file "../images/test.png"
        // Optional: media_type "image/png"
      }
      // Or using URL:
      // imageArg {
      //   url "https://example.com/image.png"
      // }
    }
  }

  // For array/object inputs:
  test ComplexTest {
    functions [MyFunction]
    args {
      input {
        name "Complex Object"
        tags [
          "tag1",
          #"
            Multi-line
            tag here
          "#
        ]
        status PENDING
        type "error"
        count 100
        enabled false
        score 7.8
      }
    }
  }
"##
