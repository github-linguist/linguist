myDoc = '''
        Single-quoted heredocs allows no '#{foo}' interpolation.
        This behavior is similar to single-quoted strings.
        '''
doc2  = """
        However, double-quoted heredocs *do* allow these.
        See it in action:
            Content: "#{myDoc}"
        """

console.log doc2
