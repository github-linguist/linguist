describe "Jazz grammar", ->
    grammar = null

    beforeEach ->
        waitsForPromise ->
            atom.packages.activatePackage("language-jazz")

        runs ->
            grammar = atom.grammars.grammarForScopeName("source.jazz")

    it "parses the grammar", ->
        expect(grammar).toBeDefined()
        expect(grammar.scopeName).toBe "source.jazz"

    it "tokenises keywords", ->
        tokens = grammar.tokenizeLines('func')

        expect(tokens[0][0].value).toBe 'func'
        expect(tokens[0][0].scopes).toEqual ['source.jazz', 'meta.function.jazz', 'storage.type.jazz']

    it "tokenises comments inside function parameters", ->
        tokens = grammar.tokenizeLines('func test(arg1, ;; arg2)')

        expect(tokens[0][0].value).toBe 'func'
        expect(tokens[0][4].scopes).toEqual ['source.jazz', 'meta.function.jazz', 'variable.parameter.jazz', 'comment.line.jazz']

    it "tokenizes multi-line strings", ->
        tokens = grammar.tokenizeLines('"Hello,\nWorld!"')

        # Line 0
        expect(tokens[0][0].value).toBe '"'
        expect(tokens[0][0].scopes).toEqual ['source.jazz', 'string.quoted.double.jazz', 'punctuation.definition.string.begin.jazz']

        expect(tokens[0][1].value).toBe 'Hello,'
        expect(tokens[0][1].scopes).toEqual ['source.jazz', 'string.quoted.double.jazz']

        expect(tokens[0][3]).not.toBeDefined()

        # Line 1
        expect(tokens[1][0].value).toBe 'World!'
        expect(tokens[1][0].scopes).toEqual ['source.jazz', 'string.quoted.double.jazz']

        expect(tokens[1][1].value).toBe '"'
        expect(tokens[1][1].scopes).toEqual ['source.jazz', 'string.quoted.double.jazz', 'punctuation.definition.string.end.jazz']

        expect(tokens[1][2]).not.toBeDefined()

    it "tokenizes nested object definitions", ->
        tokens = grammar.tokenizeLines('test.foo')

        expect(tokens[0][1].scopes).toEqual ['source.jazz', 'meta.nestedobject.jazz']
