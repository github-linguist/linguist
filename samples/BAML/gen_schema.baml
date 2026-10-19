// Generate BAML schema from an image

template_string SchemaGuidelines() #"
  Available types:
    - class (mentioned above)
    - string[], int[], float[]
    - float
    - bool
    - unions can be represented as "ClassA | ClassB"
    - optionals have a question mark: e.g. "property string?"
    - enums in this format:
      enum MyEnum {
        VALUE1
        // a random comment
        VALUE2 @description("Also optional description")
        VALUE3
      }

    Not available:
    - Recursive types are not supported.
    - inline definitions are not allowed. you must declare a class for anything nested.
    - Enums must also be declared separately, not inline.

    Any freeform text goes in double slash:
    // like this.
"#
   
function GenerateBamlSchemaFromImage(pdf: image, prompt: string?) -> string {
  client Claude
  prompt #"
    {{ _.role("user") }}
    {{ pdf }}

    Rules:

    1.Extract the schema of this PDF in this kind of format:

    For objects you want to reuse in the schema, use this syntax:
    ```
    class MyObject {
      // the @description is optional, and it goes AFTER the field you're trying to describe.
      property string @description("some description")
      property2 float @description("another example")
    }
    ```

    2. Write a "class OutputSchema" that contains the final schema representation. Only write this once in the output.
 
    3. Be thorough, and ensure that every piece of data is accounted for in the schema. If there's a paragraph of text, just add a 1-2 sentence summary in the output schema.

    4. Don't add any logos or raw base64 images to the output schema.

    5. If there is a box number of some sort. Indicate that in the @description.

    6. Add comments about each section you're processing before you write out the class using "//"

    7. If a field has is not super clear (it can have possible interpretations, make it optional and add in the @description which areas of the image/document are blurry and should be avoided)

    9. Arrays cannot be optional.

    10. Don't actually fill out the schema with data.

    11. Before you write out the answer, analyze the document and write which fields are not very visible or could be confused with different values in comments.

    12. All enums should be optional with a question mark: MyEnum?

    13. All "string" fields (not string[]) should be optional with a question mark: string?

    14. Don't repeat the class models.
    
    {{ SchemaGuidelines() }}
    ---

   

    User:
    {% if prompt %}
    {{ prompt }}
    {% endif %}
   

    
 
    Answer only in the BAML format. Use comments for any freeform text. Output:
  "#
}
